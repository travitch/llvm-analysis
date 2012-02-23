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
import Control.Monad.Identity
import Data.Graph.Inductive
import Data.HashMap.Strict ( HashMap )
import qualified Data.HashMap.Strict as M
import Data.List ( foldl' )
import Data.Map ( Map )
import qualified Data.Map as Map
import Data.Maybe ( mapMaybe )
import Data.Set ( Set )
import qualified Data.Set as S
import FileLocation

import Data.LLVM
import Data.LLVM.Analysis.CallGraph
import Data.LLVM.Analysis.CallGraphSCCTraversal

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

-- We cheat on this instance since the escape graphs are computed
-- exactly once, we don't need to compare them for equality at each
-- step.
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

escapeAnalysis :: CallGraph -> (ExternalFunction -> Int -> Bool) -> EscapeResult
escapeAnalysis cg extSummary =
  runIdentity (callGraphSCCTraversal cg (escAnalysis extSummary) s0)
  where
    s0 = emptyResult (callGraphFunctions cg)

-- | This is the underlying bottom-up analysis to identify which
-- arguments escape.  It assumes that all of the use graphs have been
-- computed.
escAnalysis :: (Monad m)
               => (ExternalFunction -> Int -> Bool)
               -> Function
               -> EscapeResult
               -> m EscapeResult
escAnalysis extSumm f summ =
  let g = buildUseGraph extSumm summ f
      summ' = summ { escapeGraphs = M.insert f g (escapeGraphs summ) }
  in return $ foldl' (analyzeArgument extSumm g f) summ' args
  where
    args = functionParameters f

analyzeArgument :: (ExternalFunction -> Int -> Bool)
                   -> UseGraph
                   -> Function
                   -> EscapeResult
                   -> Argument
                   -> EscapeResult
analyzeArgument extSumm g f summ a =
  let (escapes, willEscape) = foldr inducesEscape (False, False) reached
  in case (escapes, willEscape) of
    (True, _) -> summ { escapeArguments = M.insertWith S.union f (S.singleton a) (escapeArguments summ) }
    (_, True) -> summ { willEscapeArguments = M.insertWith S.union f (S.singleton a) (willEscapeArguments summ) }
    _ -> summ
  where
    reached = map ($fromJst . lab g) $ dfs [argumentUniqueId a] g

instructionEscapes :: EscapeResult -> Instruction -> Bool
instructionEscapes er i =
  fst $ foldr inducesEscape (False, False) reached
  where
    Just bb = instructionBasicBlock i
    f = basicBlockFunction bb
    errMsg = $err' ("Expected escape graph for " ++ show (functionName f))
    g = M.lookupDefault errMsg f (escapeGraphs er)
    reached = map ($fromJst . lab g) $ dfs [instructionUniqueId i] g

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

addUseGraphForFunction :: (ExternalFunction -> Int -> Bool)
                          -> Function -> EscapeResult -> EscapeResult
addUseGraphForFunction extSumm f er =
  er { escapeGraphs = M.insert f g (escapeGraphs er) }
  where
    g = buildUseGraph extSumm er f

-- | The nodes of the graph are all of the instructions plus all of
-- their operands.  The edges are from use to user.
--
-- Handling loads properly may require a fixed-point calculation
buildUseGraph :: (ExternalFunction -> Int -> Bool)
                 -> EscapeResult -> Function -> UseGraph
buildUseGraph extSumm summ f = mkGraph (S.toList ns) (S.toList es)
  where
    insts = concatMap basicBlockInstructions (functionBody f)
    ns = foldr (addInstAndOps extSumm summ) S.empty insts
    es = foldr (addEdges extSumm summ) S.empty insts

addInstAndOps :: (ExternalFunction -> Int -> Bool) -> EscapeResult
                 -> Instruction -> Set UseNode -> Set UseNode
addInstAndOps extSumm er i s = s `S.union` S.fromList (inode : opNodes)
  where
    inode = (instructionUniqueId i, Value i)
    opNodes = map (valueUniqueId &&& id) (escapeOperands extSumm er i)

addEdges :: (ExternalFunction -> Int -> Bool) -> EscapeResult
            -> Instruction -> Set UseEdge -> Set UseEdge
addEdges extSumm er i s = s `S.union` S.fromList es
  where
    es = map (mkOpEdge (instructionUniqueId i)) (escapeOperands extSumm er i)
    mkOpEdge dst v = (valueUniqueId v, dst, ())

keepIfPointer :: Value -> Maybe Value
keepIfPointer v =
  case valueType v of
    TypePointer _ _ -> Just v
    _ -> Nothing

-- Many instructions don't matter at all - we only need to extract
-- operands from the relevant ones (things involving memory and
-- addresses, plus arithmetic and casts).
escapeOperands :: (ExternalFunction -> Int -> Bool) -> EscapeResult
                  -> Instruction -> [Value]
escapeOperands extSumm er i =
  mapMaybe keepIfPointer $ case i of
    RetInst { retInstValue = Just v } -> [v]
    RetInst {} -> []
    -- Extracting an element could be said to make the base object
    -- escape.  Inserting makes the inserted thing escape.
    ExtractElementInst { extractElementVector = v } -> [v]
    InsertElementInst { insertElementValue = v } -> [v]
    ExtractValueInst { extractValueAggregate = v } -> [v]
    InsertValueInst { insertValueValue = v } -> [v]
    LoadInst { loadAddress = a } -> [a]
    StoreInst { storeValue = v } -> [v]
    AtomicCmpXchgInst { atomicCmpXchgNewValue = v } -> [v]
    AtomicRMWInst { atomicRMWValue = v } -> [v]
    AddInst { binaryLhs = lhs, binaryRhs = rhs } -> [lhs, rhs]
    SubInst { binaryLhs = lhs, binaryRhs = rhs } -> [lhs, rhs]
    PtrToIntInst { castedValue = v } -> [v]
    IntToPtrInst { castedValue = v } -> [v]
    BitcastInst { castedValue = v } -> [v]
    SelectInst { selectTrueValue = t, selectFalseValue = f } -> [t,f]
    GetElementPtrInst { getElementPtrValue = v } -> [v]
    CallInst { callArguments = args, callFunction = f } ->
      keepEscapingArgs extSumm er f (map fst args)
    InvokeInst { invokeArguments = args, invokeFunction = f } ->
      keepEscapingArgs extSumm er f (map fst args)
    PhiNode { phiIncomingValues = vs } -> map fst vs
    _ -> []

-- | Only make nodes/edges for arguments that escape.  Indirect calls
-- let all of their pointer arguments escape.
keepEscapingArgs :: (ExternalFunction -> Int -> Bool) -> EscapeResult
                    -> Value -> [Value] -> [Value]
keepEscapingArgs extSumm er callee args =
  case valueContent' callee of
    FunctionC f ->
      let errMsg = $err' ("Missing summary for " ++ show (functionName f))
          escArgs = M.lookupDefault errMsg f (escapeArguments er)
          indexedFormals = zip ixs (functionParameters f)
          indexedEscapes = filter ((`S.member` escArgs) . snd) indexedFormals
          escapedIndices = S.fromList $ map fst indexedEscapes
      in map snd $ filter ((`S.member` escapedIndices) . fst) (zip ixs args)
    ExternalFunctionC f -> map snd $ filter (extSumm f . fst) indexedArgs
    _ -> args
  where
    ixs :: [Int]
    ixs = [0..]
    indexedArgs = zip ixs args

-- Testing

escapeResultToTestFormat :: EscapeResult -> Map String (Set String)
escapeResultToTestFormat er =
  M.foldlWithKey' transform Map.empty m
  where
    m = escapeArguments er
    transform a f s =
      Map.insert (show (functionName f)) (S.map (show . argumentName) s) a

willEscapeResultToTestFormat :: EscapeResult -> Map String (Set String)
willEscapeResultToTestFormat er =
  M.foldlWithKey' transform Map.empty m
  where
    m = willEscapeArguments er
    transform a f s =
      Map.insert (show (functionName f)) (S.map (show . argumentName) s) a
