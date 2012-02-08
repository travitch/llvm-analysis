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
data ReturnInfo = RI !Bool
                deriving (Eq, Ord, Show)

instance MeetSemiLattice ReturnInfo where
  meet (RI r1) (RI r2) = RI (r1 || r2)

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
  let env = AE extSummary summ
      localRes = runReader (forwardDataflow top f) env
      -- FIXME filter out unreachable exit instructions
      exitInsts = functionExitInstructions f
      exitInfos = runReader (mapM (dataflowResult localRes) exitInsts) env
  in case meets exitInfos of
    RI False -> return summ
    RI True -> return $! S.insert f summ

returnTransfer :: ReturnInfo -> Instruction -> [CFGEdge] -> AnalysisMonad ReturnInfo
returnTransfer ri i _ =
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