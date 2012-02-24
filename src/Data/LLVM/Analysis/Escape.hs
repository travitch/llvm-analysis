{-# LANGUAGE TemplateHaskell, ViewPatterns, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | This is a very conservative flow- and context-insensitive escape
-- analysis based on graph reachability.  The underlying graph is a used-by
-- graph.  A value gets an edge to an instruction if the instruction uses the
-- value.
--
--  * A value Escapes if a store destination uses it *OR* it is passed
--    as an argument to a function pointer.
--
--  * A value WillEscape if it does not Escape but it is used by a ret
--    instruction
--
-- This graph has a few special properties and is not exactly a
-- used-by graph.  Since we only care about escape properties, store
-- destinations do not need to be represented (store x -> y cannot
-- possibly make y escape) and the value used to call a function
-- pointer does not need to be represented.
--
-- This analysis assumes that arguments passed as varargs do *not*
-- escape.
--
-- Only pointers can escape.  If a sub-component of an object escapes,
-- as in:
--
-- > global = s->foo;
--
-- this analysis will not report that @s@ escapes.  This type of
-- escaping can be identified by a derived analysis using the results
-- of this analysis.  This analysis will identify the loaded result of
-- @s->foo@ as escaping.
--
-- Notes on precision:
--
-- This analysis does not do any sophisticated value tracking through
-- memory references and it does not distinguish stores to locals
-- (allocas) from stores to globals.  With no optimization, it will
-- provide useless results (basically everything will escape).
--
-- With simple optimizations (-mem2reg and -basicaa) it will be very
-- precise.
module Data.LLVM.Analysis.Escape (
  EscapeResult,
  escapeAnalysis,
  functionEscapeArguments,
  functionWillEscapeArguments,
  instructionEscapes,
  -- * Testing
  escapeResultToTestFormat,
  willEscapeResultToTestFormat
  ) where

import Control.Arrow
import Control.Monad ( foldM, filterM )
import Data.Graph.Inductive
import Data.HashMap.Strict ( HashMap )
import qualified Data.HashMap.Strict as M
import Data.List ( foldl' )
import Data.Map ( Map )
import qualified Data.Map as Map
import Data.Set ( Set )
import qualified Data.Set as S
import FileLocation

import Data.LLVM
import Data.LLVM.Analysis.CallGraph
import Data.LLVM.Analysis.CallGraphSCCTraversal

-- | An opaque representation of escape information for a Module.
data EscapeResult =
  EscapeResult { escapeGraphs :: HashMap Function UseGraph
               , escapeArguments :: HashMap Function (Set Argument)
               , willEscapeArguments :: HashMap Function (Set Argument)
               }

-- | Get the set of escaped arguments for a function.  This function
-- will throw an error if the function is not in the escape result set
-- since that implies a programming error.
functionEscapeArguments :: EscapeResult -> Function -> Set Argument
functionEscapeArguments er f =
  M.lookupDefault errMsg f (escapeArguments er)
  where
    errMsg = $err' ("No escape result for " ++ show (functionName f))

functionWillEscapeArguments :: EscapeResult -> Function -> Set Argument
functionWillEscapeArguments er f =
  M.lookupDefault errMsg f (willEscapeArguments er)
  where
    errMsg = $err' ("No escape result for " ++ show (functionName f))

-- | Determine if an instruction escapes from the scope of its
-- enclosing function.  This function does not include willEscape
-- results.
instructionEscapes :: EscapeResult -> Instruction -> Bool
instructionEscapes er i =
  fst $ foldr inducesEscape (False, False) reached
  where
    Just bb = instructionBasicBlock i
    f = basicBlockFunction bb
    errMsg = $err' ("Expected escape graph for " ++ show (functionName f))
    g = M.lookupDefault errMsg f (escapeGraphs er)
    reached = map ($fromJst . lab g) $ dfs [instructionUniqueId i] g

instance Eq EscapeResult where
  (EscapeResult g1 e1 w1) == (EscapeResult g2 e2 w2) =
    e1 == e2 && w1 == w2 && g1 == g2

instance Eq (Gr Value ()) where
  (==) = equal

emptyResult :: [Function] -> EscapeResult
emptyResult fs =
  let e = M.fromList (zip fs (repeat S.empty))
  in EscapeResult M.empty e e

-- Useful type synonyms to hopefully make switching to hbgl easier
-- later
type UseGraph = Gr Value ()
type UseNode = LNode Value
type UseEdge = LEdge ()
type UseContext = Context Value ()

-- | Run the escape analysis given a function summarizing the
-- arguments of external functions.  The function summary should
-- return true if the ith argument of the given external function
-- escapes.
escapeAnalysis :: (Monad m) => CallGraph -> (ExternalFunction -> Int -> m Bool)
                  -> m EscapeResult
escapeAnalysis cg extSummary =
  callGraphSCCTraversal cg (escAnalysis extSummary) s0
  where
    -- This initial state must have an entry for each function in the
    -- call graph; they will be filled in as the analysis progresses.
    s0 = emptyResult (callGraphFunctions cg)

-- | This is the underlying bottom-up analysis to identify which
-- arguments escape.  It builds a UseGraph for the function
-- (incorporating information from other functions that have already
-- been analyzed) and then checks to see which arguments escape using
-- that graph.
escAnalysis :: (Monad m)
               => (ExternalFunction -> Int -> m Bool)
               -> Function
               -> EscapeResult
               -> m EscapeResult
escAnalysis extSumm f summ = do
  g <- buildUseGraph extSumm summ f
  let summ' = summ { escapeGraphs = M.insert f g (escapeGraphs summ) }
  return $! foldl' (analyzeArgument g f) summ' args
  where
    args = functionParameters f

-- | An argument escapes if any instruction that induces an escape is
-- reachable in the UseGraph from it.  Reachability here is computed
-- using a simple depth-first search.  See 'inducesEscape' for details
-- on what we consider escaping.
analyzeArgument :: UseGraph
                   -> Function
                   -> EscapeResult
                   -> Argument
                   -> EscapeResult
analyzeArgument g f summ a =
  let (escapes, willEscape) = foldr inducesEscape (False, False) reached
  in case (escapes, willEscape) of
    (True, _) -> summ { escapeArguments = M.insertWith S.union f (S.singleton a) (escapeArguments summ) }
    (_, True) -> summ { willEscapeArguments = M.insertWith S.union f (S.singleton a) (willEscapeArguments summ) }
    _ -> summ
  where
    reached = map ($fromJst . lab g) $ dfs [argumentUniqueId a] g

-- | An instruction causes its value to escape if it is a store or a
-- Call/Invoke.  The only edges to call instructions in the UseGraph
-- are those that escape, so we don't need to try to figure out which
-- argument is involved at this stage.
--
-- If a Return instruction is reachable from a value, that value
-- *willEscape*, which is different from *escapes*.
inducesEscape :: Value -> (Bool, Bool) -> (Bool, Bool)
inducesEscape _ (True, we) = (True, we)
inducesEscape v (e, we) =
  case valueContent v of
    InstructionC StoreInst {} -> (True, we)
    InstructionC RetInst {} -> (e, True)
    InstructionC CallInst {} -> (True, we)
    InstructionC InvokeInst {} -> (True, we)
    _ -> (e, we)

-- Graph construction

-- | The nodes of the graph are all of the instructions plus all of
-- their operands.  The edges are from use to user.  The graph
-- construction only includes nodes relevant to the escape analysis.
-- Anything not involving a pointer is ignored.
--
-- The external and module summaries are required to know which call
-- instructions allow values to escape.
buildUseGraph :: (Monad m) => (ExternalFunction -> Int -> m Bool)
                 -> EscapeResult -> Function -> m UseGraph
buildUseGraph extSumm summ f = do
  ns <- foldM (addInstAndOps extSumm summ) S.empty insts
  es <- foldM (addEdges extSumm summ) S.empty insts
  return $! mkGraph (S.toList ns) (S.toList es)
  where
    insts = concatMap basicBlockInstructions (functionBody f)

addInstAndOps :: (Monad m) => (ExternalFunction -> Int -> m Bool) -> EscapeResult
                 -> Set UseNode -> Instruction -> m (Set UseNode)
addInstAndOps extSumm er s i = do
  operands <- escapeOperands extSumm er i
  let opNodes = map (valueUniqueId &&& id) operands
  return $! s `S.union` S.fromList (inode : opNodes)
  where
    inode = (instructionUniqueId i, Value i)

addEdges :: (Monad m) => (ExternalFunction -> Int -> m Bool) -> EscapeResult
            -> Set UseEdge -> Instruction -> m (Set UseEdge)
addEdges extSumm er s i = do
  operands <- escapeOperands extSumm er i
  let es = map (mkOpEdge (instructionUniqueId i)) operands
  return $! s `S.union` S.fromList es
  where
    mkOpEdge dst v = (valueUniqueId v, dst, ())

isPointer :: Value -> Bool
isPointer v =
  case valueType v of
    TypePointer _ _ -> True
    _ -> False


-- | Extract the operands relevant to the UseGraph from an
-- Instruction.
--
-- Many instructions don't matter at all since they can't lead to
-- escaping.  We only need to extract operands from the relevant ones
-- (things involving memory, addresses, and casts).
escapeOperands :: (Monad m) => (ExternalFunction -> Int -> m Bool) -> EscapeResult
                  -> Instruction -> m [Value]
escapeOperands extSumm er i = do
  operands <- case i of
    RetInst { retInstValue = Just v } -> return [v]
    RetInst {} -> return []
    -- Inserting makes the inserted thing escape.  I'm not sure if
    -- either of these instructions can ever actually operate on
    -- pointers...  Actually I have never even seen them generated.
    InsertElementInst { insertElementValue = v } -> return [v]
    InsertValueInst { insertValueValue = v } -> return [v]
    StoreInst { storeValue = v } -> return [v]
    AtomicCmpXchgInst { atomicCmpXchgNewValue = v } -> return [v]
    AtomicRMWInst { atomicRMWValue = v } -> return [v]
    PtrToIntInst { castedValue = v } -> return [v]
    IntToPtrInst { castedValue = v } -> return [v]
    BitcastInst { castedValue = v } -> return [v]
    SelectInst { selectTrueValue = t, selectFalseValue = f } -> return [t,f]
    CallInst { callArguments = args, callFunction = f } ->
      keepEscapingArgs extSumm er f (map fst args)
    InvokeInst { invokeArguments = args, invokeFunction = f } ->
      keepEscapingArgs extSumm er f (map fst args)
    PhiNode { phiIncomingValues = vs } -> return $ map fst vs
    _ -> return []
  return $! filter isPointer operands

-- | Only make nodes/edges for arguments that escape.  Indirect calls
-- let all of their pointer arguments escape.
keepEscapingArgs :: (Monad m) => (ExternalFunction -> Int -> m Bool) -> EscapeResult
                    -> Value -> [Value] -> m [Value]
keepEscapingArgs extSumm er callee args =
  case valueContent' callee of
    FunctionC f ->
      let errMsg = $err' ("Missing summary for " ++ show (functionName f))
          escArgs = M.lookupDefault errMsg f (escapeArguments er)
          indexedFormals = zip ixs (functionParameters f)
          indexedEscapes = filter ((`S.member` escArgs) . snd) indexedFormals
          escapedIndices = S.fromList $ map fst indexedEscapes
      in return $ map snd $ filter ((`S.member` escapedIndices) . fst) (zip ixs args)
    ExternalFunctionC f -> do
      escArgs <- filterM (extSumm f . fst) indexedArgs
      return $ map snd escArgs
    _ -> return args
  where
    ixs :: [Int]
    ixs = [0..]
    indexedArgs = zip ixs args

-- Testing

-- | Extract the arguments for each function that escape.  The keys of
-- the map are function names and the set elements are argument names.
-- This format exposes the internal results for testing purposes.
--
-- For actual use in a program, use one of 'functionEscapeArguments',
-- 'functionWillEscapeArguments', or 'instructionEscapes' instead.
escapeResultToTestFormat :: EscapeResult -> Map String (Set String)
escapeResultToTestFormat er =
  M.foldlWithKey' transform Map.empty m
  where
    m = escapeArguments er
    transform a f s =
      Map.insert (show (functionName f)) (S.map (show . argumentName) s) a

-- | The same as 'escapeResultToTestFormat', but for the willEscape
-- arguments.
willEscapeResultToTestFormat :: EscapeResult -> Map String (Set String)
willEscapeResultToTestFormat er =
  M.foldlWithKey' transform Map.empty m
  where
    m = willEscapeArguments er
    transform a f s =
      Map.insert (show (functionName f)) (S.map (show . argumentName) s) a
