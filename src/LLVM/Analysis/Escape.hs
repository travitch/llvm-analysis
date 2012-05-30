{-# LANGUAGE TemplateHaskell, ViewPatterns, FlexibleInstances, TypeSynonymInstances #-}
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
module LLVM.Analysis.Escape (
  EscapeResult,
  escapeAnalysis,
  argumentEscapes,
  argumentFptrEscapes,
  argumentWillEscape,
  instructionEscapes,
  instructionEscapesWith,
  instructionWillEscape,
  -- * Testing
  escapeResultToTestFormat,
  willEscapeResultToTestFormat,
  escapeUseGraphs,
  useGraphvizRepr
  ) where

import Control.Arrow
import Control.DeepSeq
import Control.Monad ( foldM, filterM )
import Data.GraphViz
import Data.HashMap.Strict ( HashMap )
import qualified Data.HashMap.Strict as M
import Data.List ( foldl' )
import Data.Map ( Map )
import qualified Data.Map as Map
import Data.Maybe ( isJust )
import Data.Monoid
import Data.Set ( Set )
import qualified Data.Set as S
import Debug.Trace.LocationTH

import Data.Graph.Interface
import Data.Graph.PatriciaTree
import Data.Graph.Algorithms.Matching.DFS

import LLVM.Analysis

-- | An opaque representation of escape information for a Module.
data EscapeResult =
  EscapeResult { escapeGraphs :: HashMap Function UseGraph
               , escapeArguments :: HashMap Argument Instruction
               , fptrEscapeArguments :: HashMap Argument Instruction
               , willEscapeArguments :: HashMap Argument Instruction
               }

instance Eq EscapeResult where
  (EscapeResult g1 e1 f1 w1) == (EscapeResult g2 e2 f2 w2) =
    e1 == e2 && f1 == f2 && w1 == w2 && g1 == g2

instance Eq UseGraph where
  (==) = graphEqual

emptyResult :: EscapeResult
emptyResult = EscapeResult mempty mempty mempty mempty

instance Monoid EscapeResult where
  mempty = emptyResult
  mappend (EscapeResult gs1 as1 f1 was1) (EscapeResult gs2 as2 f2 was2) =
    EscapeResult { escapeGraphs = M.union gs1 gs2
                 , escapeArguments = M.union as1 as2
                 , fptrEscapeArguments = M.union f1 f2
                 , willEscapeArguments = M.union was1 was2
                 }

instance NFData EscapeResult where
  rnf r@(EscapeResult gs as fs was) =
    gs `deepseq` as `deepseq` was `deepseq` fs `deepseq` r `seq` ()

-- | Get the set of escaped arguments for a function.  This function
-- will throw an error if the function is not in the escape result set
-- since that implies a programming error.
argumentEscapes :: EscapeResult -> Argument -> Maybe Instruction
argumentEscapes er a = M.lookup a (escapeArguments er)

argumentFptrEscapes :: EscapeResult -> Argument -> Maybe Instruction
argumentFptrEscapes er a = M.lookup a (fptrEscapeArguments er)

argumentWillEscape :: EscapeResult -> Argument -> Maybe Instruction
argumentWillEscape er a = M.lookup a (willEscapeArguments er)

-- | A generalization of 'instructionEscapes'.  The first argument is
-- a predicate that returns True if the input Value should be excluded
-- in the Use graph.  The set of reachable locations for the
-- input instruction is computed as normal, but the values in the
-- @ignore@ list are removed from the set before it is used to
-- determine what escapes.
--
-- This arrangement means that @ignore@d nodes do *not* affect the
-- reachability computation.  That is critical for transitive
-- assignments to be treated properly (that is, for the transitive
-- links to be included).
--
-- The intended use of this variant is to issue escape queries for
-- instructions that are known to escape via some desired means (e.g.,
-- an out parameter) and to determine if they also escape via some
-- other means.  In that case, the @ignore@ list should be just the
-- store instruction that created the known escape.
instructionEscapesWith :: (Value -> Bool) -> Instruction -> EscapeResult -> Maybe Instruction
instructionEscapesWith = instructionEscapeCore (Just . escapeWitness)

-- | Returns the instruction (if any) that causes the input
-- instruction to escape.  This does *not* cover WillEscape at all.
instructionEscapes :: Instruction -> EscapeResult -> Maybe Instruction
instructionEscapes = instructionEscapeCore extract (const False)
  where
    extract (EscapeWitness w) = Just w
    extract (FptrEscapeWitness w) = Just w
    extract _ = Nothing

-- | Returns the return instruction (if any) that causes the given
-- instruction to be marked as WillEscape.
instructionWillEscape :: Instruction -> EscapeResult -> Maybe Instruction
instructionWillEscape = instructionEscapeCore extract (const False)
  where
    extract (WillEscapeWitness w) = Just w
    extract _ = Nothing

instructionEscapeCore :: (WitnessType -> Maybe Instruction)
                         -> (Value -> Bool)
                         -> Instruction
                         -> EscapeResult
                         -> Maybe Instruction
instructionEscapeCore extractWitness ignoreValue i er = do
  escType <- foldr inducesEscape Nothing reached
  extractWitness escType
  where
    Just bb = instructionBasicBlock i
    f = basicBlockFunction bb
    errMsg = $failure ("Expected escape graph for " ++ show (functionName f))
    g = M.lookupDefault errMsg f (escapeGraphs er)
    reached = filter (not . ignoreValue . snd) $ reachableValues i g

-- | Get the list of values reachable from the given instruction in
-- the use graph.  An instruction is not reachable from itself unless
-- it is in a cycle.
reachableValues :: Instruction -> UseGraph -> [(Bool, Value)]
reachableValues i g =
  case instructionInLoop i g of
    True -> reached
    False -> filter ((/= (Value i)) . snd) reached
  where
    reached = map (safeLab $__LOCATION__ g) $ dfs [instructionUniqueId i] g

-- | Return True if the given instruction is in a cycle in the use
-- graph
instructionInLoop :: Instruction -> UseGraph -> Bool
instructionInLoop i g = any (instInNonSingleton i) (scc g)
  where
    instInNonSingleton inst component =
      case length component > 1 of
        False -> False
        True -> instructionUniqueId inst `elem` component

-- Useful type synonyms to hopefully make switching to hbgl easier
-- later
type UseGraph = Gr (Bool, Value) ()
type UseNode = LNode UseGraph
type UseEdge = LEdge UseGraph
type UseContext = Context UseGraph

-- | This is the underlying bottom-up analysis to identify which
-- arguments escape.  It builds a UseGraph for the function
-- (incorporating information from other functions that have already
-- been analyzed) and then checks to see which arguments escape using
-- that graph.
--
-- The function summary should return true if the ith argument of the
-- given external function escapes.
escapeAnalysis :: (Monad m, HasFunction funcLike)
                  => (ExternalFunction -> Int -> m Bool)
                  -> funcLike
                  -> EscapeResult
                  -> m EscapeResult
escapeAnalysis extSumm funcLike summ = do
  g <- buildUseGraph extSumm summ f
  let summ' = summ { escapeGraphs = M.insert f g (escapeGraphs summ) }
  return $! foldl' (analyzeArgument g) summ' args
  where
    f = getFunction funcLike
    args = functionParameters f

-- | An argument escapes if any instruction that induces an escape is
-- reachable in the UseGraph from it.  Reachability here is computed
-- using a simple depth-first search.  See 'inducesEscape' for details
-- on what we consider escaping.
--
-- Note that this doesn't need the more complicated reaching
-- computation as used in 'instructionEscapes' because arguments
-- cannot ever reach themselves.
analyzeArgument :: UseGraph
                   -> EscapeResult
                   -> Argument
                   -> EscapeResult
analyzeArgument g summ a =
  case foldr inducesEscape Nothing reached of
    Just (EscapeWitness i) ->
      summ { escapeArguments =
                M.insert a i (escapeArguments summ)
           }
    Just (WillEscapeWitness i) ->
      summ { willEscapeArguments =
                M.insert a i (willEscapeArguments summ)
           }
    Just (FptrEscapeWitness i) ->
      summ { fptrEscapeArguments =
                M.insert a i (fptrEscapeArguments summ)
           }
    Nothing -> summ
  where
    reached = map (safeLab $__LOCATION__ g) $ dfs [argumentUniqueId a] g

data WitnessType = EscapeWitness { escapeWitness :: Instruction }
                 | FptrEscapeWitness { escapeWitness :: Instruction }
                 | WillEscapeWitness { escapeWitness :: Instruction }
                 deriving (Show)

-- | An instruction causes its value to escape if it is a store or a
-- Call/Invoke.  The only edges to call instructions in the UseGraph
-- are those that escape, so we don't need to try to figure out which
-- argument is involved at this stage.
--
-- If a Return instruction is reachable from a value, that value
-- *willEscape*, which is different from *escapes*.
inducesEscape :: (Bool, Value) -> Maybe WitnessType -> Maybe WitnessType
inducesEscape _ w@(Just (EscapeWitness _)) = w
inducesEscape (isFptr, v) w =
  case valueContent v of
    InstructionC i@StoreInst {} -> Just (EscapeWitness i)
    InstructionC i@RetInst {} -> Just (WillEscapeWitness i)
    InstructionC i@CallInst {} ->
      case isFptr of
        False -> Just (EscapeWitness i)
        True -> Just (FptrEscapeWitness i)
    InstructionC i@InvokeInst {} ->
      case isFptr of
        False -> Just (EscapeWitness i)
        True -> Just (FptrEscapeWitness i)
    _ -> w

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
  let opNodes = map (\v -> LNode (valueUniqueId v) (False, v)) operands
  return $! s `S.union` S.fromList (inode : opNodes)
  where
    inode = LNode (instructionUniqueId i) (isIndirectCall i, Value i)

addEdges :: (Monad m) => (ExternalFunction -> Int -> m Bool) -> EscapeResult
            -> Set UseEdge -> Instruction -> m (Set UseEdge)
addEdges extSumm er s i = do
  operands <- escapeOperands extSumm er i
  let es = map (mkOpEdge (instructionUniqueId i)) operands
  return $! s `S.union` S.fromList es
  where
    mkOpEdge dst v = LEdge (Edge (valueUniqueId v) dst) ()

isPointer :: Value -> Bool
isPointer v =
  case valueType v of
    TypePointer _ _ -> True
    _ -> False

isIndirectCall :: Instruction -> Bool
isIndirectCall i =
  case i of
    CallInst { callFunction = (valueContent' -> FunctionC _) } -> False
    CallInst { callFunction = (valueContent' -> ExternalFunctionC _) } -> False
    CallInst { } -> True
    InvokeInst { invokeFunction = (valueContent' -> FunctionC _) } -> False
    InvokeInst { invokeFunction = (valueContent' -> ExternalFunctionC _) } -> False
    InvokeInst { } -> True
    _ -> False

-- | Extract the operands relevant to the UseGraph from an
-- Instruction.
--
-- Many instructions don't matter at all since they can't lead to
-- escaping.  We only need to extract operands from the relevant ones
-- (things involving memory, addresses, and casts).
--
-- FIXME: Add an extra argument here that tests whether or not an
-- instruction should be ignored.  This will be useful since sometimes
-- domain knowledge or annotations can let us rule out escapes early.
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
    SelectInst { selectTrueValue = t, selectFalseValue = f } -> return [ t, f ]
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
      let indexedEscapes = indexedEscapingArgs er f
          escapedIndices = S.fromList $ map fst indexedEscapes
          escapedArgs = filter ((`S.member` escapedIndices) . fst) (zip ixs args)
      in return $ map snd escapedArgs
    ExternalFunctionC f -> do
      escArgs <- filterM (extSumm f . fst) indexedArgs
      return $ map snd escArgs

    _ -> return args
  where
    ixs :: [Int]
    ixs = [0..]
    indexedArgs = zip ixs args

indexedEscapingArgs :: EscapeResult -> Function -> [(Int, Argument)]
indexedEscapingArgs er f =
  filter (\(_, a) -> isJust (argumentEscapes er a)) indexedArgs
  where
    ixs :: [Int]
    ixs = [0..]
    args = functionParameters f
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
  foldr transform Map.empty (M.keys m)
  where
    m = escapeArguments er `M.union` fptrEscapeArguments er
    transform a acc =
      let f = argumentFunction a
          fname = show (functionName f)
          aname = show (argumentName a)
      in Map.insertWith' S.union fname (S.singleton aname) acc

-- | The same as 'escapeResultToTestFormat', but for the willEscape
-- arguments.
willEscapeResultToTestFormat :: EscapeResult -> Map String (Set String)
willEscapeResultToTestFormat er =
  foldr transform Map.empty (M.keys m)
  where
    m = willEscapeArguments er
    transform a acc =
      let f = argumentFunction a
          fname = show (functionName f)
          aname = show (argumentName a)
      in Map.insertWith' S.union fname (S.singleton aname) acc

safeLab :: (InspectableGraph gr) => String -> gr -> Node gr -> NodeLabel gr
safeLab loc g n =
  case lab g n of
    Nothing -> error (loc ++ ": missing label for use graph node")
    Just l -> l

escapeUseGraphs :: EscapeResult -> [(String, UseGraph)]
escapeUseGraphs = map (first (show . functionName)) . M.toList . escapeGraphs

useGraphvizParams :: GraphvizParams n (Bool, Value) el () (Bool, Value)
useGraphvizParams =
  nonClusteredParams { fmtNode = \(_,(_, l)) -> [toLabel (Value l)]
                     , fmtEdge = \_ -> []
                     }

useGraphvizRepr :: UseGraph -> DotGraph Int
useGraphvizRepr g = graphElemsToDot useGraphvizParams ns es
  where
    ns = map toNodeTuple $ labNodes g
    es = map toEdgeTuple $ labEdges g
