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
  NoReturnSummary,
  noReturnAnalysis
  ) where

import Control.Monad.Reader
import Data.HashSet ( HashSet )
import qualified Data.HashSet as S

import Data.LLVM
import Data.LLVM.Analysis.CFG
import Data.LLVM.Analysis.Dataflow

type NoReturnSummary = HashSet Function

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

data AnalysisEnvironment m =
  AE { externalSummary :: ExternalFunction -> m Bool
     , internalSummary :: HashSet Function
     }

-- | The analysis monad is just a Reader whose environment is a function
-- to test ExternalFunctions
type AnalysisMonad m = ReaderT (AnalysisEnvironment m) m

instance (Monad m) => DataflowAnalysis (AnalysisMonad m) ReturnInfo where
  transfer = returnTransfer

noReturnAnalysis :: (Monad m, HasCFG cfg)
                    => (ExternalFunction -> m Bool)
                    -> cfg
                    -> HashSet Function
                    -> m (HashSet Function)
noReturnAnalysis extSummary cfgLike summ = do
  let cfg = getCFG cfgLike
      f = getFunction cfg
      env = AE extSummary summ
  localRes <- runReaderT (forwardDataflow top cfg) env
  let exitInsts = filter (instructionReachable cfg) (functionExitInstructions f)
      exitInfos = map (dataflowResult localRes) exitInsts
      exitVal = foldr ((&&) . unRI) True exitInfos
  case exitVal of
    False -> return summ
    True -> return $! S.insert f summ

returnTransfer :: (Monad m) => ReturnInfo -> Instruction -> AnalysisMonad m ReturnInfo
returnTransfer ri i =
  case i of
    CallInst { callFunction = calledFunc } ->
      dispatchCall ri calledFunc
    InvokeInst { invokeFunction = calledFunc } ->
      dispatchCall ri calledFunc
    _ -> return ri

dispatchCall :: (Monad m) => ReturnInfo -> Value -> AnalysisMonad m ReturnInfo
dispatchCall ri v =
  case valueContent' v of
    FunctionC f -> do
      intSumm <- asks internalSummary
      case S.member f intSumm of
        True -> return $! RI True
        False -> return ri
    ExternalFunctionC ef -> do
      extSumm <- asks externalSummary
      isNoRet <- lift $ extSumm ef
      case isNoRet of
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
