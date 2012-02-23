{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses #-}
-- | An analysis to identify functions that never return to their
-- caller.  This only counts calls to exit, abort, or similar.
-- Notably, exceptions are not considered since the caller can catch
-- those.
--
-- The dataflow fact is "Function does not return".  It starts at
-- False (top) and calls to termination functions move it to True.
-- The meet operator is &&.  The final result for a function is
-- computed by meeting all of the values at the *reachable* exit
-- instructions.  If no paths can return, this will be &&(True...),
-- which is True.
module Data.LLVM.Analysis.NoReturn (
  noReturnAnalysis
  ) where

import Control.Monad.Identity
import Control.Monad.Reader
import Data.HashSet ( HashSet )
import qualified Data.HashSet as S

import Data.LLVM
import Data.LLVM.Analysis.CallGraph
import Data.LLVM.Analysis.CallGraphSCCTraversal
import Data.LLVM.Analysis.CFG
import Data.LLVM.Analysis.Dataflow

noReturnAnalysis :: CallGraph -> (ExternalFunction -> Bool) -> [Function]
noReturnAnalysis cg extSummary = S.toList res
  where
    res = runIdentity (callGraphSCCTraversal cg (noRetAnalysis extSummary) S.empty)

-- | The dataflow fact represents the fact that the "Function does not
-- return".  It is a simple wrapper around Bool
data ReturnInfo = RI { unRI :: !Bool }
                deriving (Show)
instance Eq ReturnInfo where
  (RI r1) == (RI r2) = r1 == r2

instance MeetSemiLattice ReturnInfo where
  meet (RI r1) (RI r2) = RI (r1 && r2)

instance BoundedMeetSemiLattice ReturnInfo where
  top = RI False

data AnalysisEnvironment = AE { externalSummary :: ExternalFunction -> Bool
                              , internalSummary :: HashSet Function
                              }

-- | The analysis monad is just a Reader whose environment is a function
-- to test ExternalFunctions
type AnalysisMonad = Reader AnalysisEnvironment

instance DataflowAnalysis AnalysisMonad ReturnInfo where
  transfer = returnTransfer

noRetAnalysis :: (Monad m)
                 => (ExternalFunction -> Bool)
                 -> Function
                 -> HashSet Function
                 -> m (HashSet Function)
noRetAnalysis extSummary f summ =
  let cfg = mkCFG f
      env = AE extSummary summ
      localRes = runReader (forwardDataflow top cfg) env
      exitInsts = filter (instructionReachable cfg) (functionExitInstructions f)
      exitInfos = map (dataflowResult localRes) exitInsts
      exitVal = foldr ((&&) . unRI) True exitInfos
  in case exitVal of
    False -> return summ
    True -> return $! S.insert f summ

returnTransfer :: ReturnInfo -> Instruction -> AnalysisMonad ReturnInfo
returnTransfer ri i =
  case i of
    CallInst { callFunction = calledFunc } ->
      dispatchCall ri calledFunc
    InvokeInst { invokeFunction = calledFunc } ->
      dispatchCall ri calledFunc
    _ -> return ri

dispatchCall :: ReturnInfo -> Value -> AnalysisMonad ReturnInfo
dispatchCall ri v =
  case valueContent' v of
    FunctionC f -> do
      intSumm <- asks internalSummary
      case S.member f intSumm of
        True -> return $! RI True
        False -> return ri
    ExternalFunctionC ef -> do
      extSumm <- asks externalSummary
      case extSumm ef of
        True -> return $! RI True
        False -> return ri
    _ -> return ri

-- | An instruction is reachable if its basic block has predecessors
-- *OR* (if there are no predecessors) it is the first basic block.
instructionReachable :: CFG -> Instruction -> Bool
instructionReachable cfg i =
  case null (basicBlockPredecessors cfg bb) of
    True -> bb == firstBlock
    False -> True
  where
    Just bb = instructionBasicBlock i
    f = basicBlockFunction bb
    firstBlock : _ = functionBody f
