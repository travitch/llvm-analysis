{-# LANGUAGE NoMonomorphismRestriction #-}
-- | An analysis to identify functions that never return to their
-- caller.  This only counts calls to exit, abort, or similar.
-- Notably, exceptions are not considered since the caller can catch
-- those.
--
-- The dataflow fact is "Function does not return".  It starts at
-- False (top) and calls to termination functions (or the unreachable
-- instruction) move it to True.
--
-- Meet is &&.  Functions are able to return as long as at least one
-- path can return.
module LLVM.Analysis.NoReturn (
  noReturnAnalysis
  ) where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Data.HashSet ( HashSet )
import qualified Data.HashSet as S

import LLVM.Analysis
import LLVM.Analysis.CFG
import LLVM.Analysis.Dataflow

-- | The dataflow fact; Bool is not enough.  Top is "NotReturned" -
-- the function has not returned yet.  Return instructions will
-- transfer to "Returned".  However, we want to distinguish between
-- "Hasn't returned yet" and "Can never return" in case we see a call
-- to a function we know cannot return (but LLVM does not).
--
-- If LLVM recognizes that something cannot return, it will add an
-- unreachable instruction (from which we also return NeverReturns).
--
-- If even a single Returned value is incoming to the exit node, the
-- function can return.
data ReturnInfo = NotReturned
                | Returned
                | WillNeverReturn
                deriving (Show, Eq)

meet :: ReturnInfo -> ReturnInfo -> ReturnInfo
meet Returned _ = Returned
meet _ Returned = Returned
meet WillNeverReturn _ = WillNeverReturn
meet _ WillNeverReturn = WillNeverReturn
meet NotReturned NotReturned = NotReturned

data AnalysisEnvironment m =
  AE { externalSummary :: ExternalFunction -> m Bool
     , internalSummary :: HashSet Function
     }

-- | The analysis monad is just a Reader whose environment is a function
-- to test ExternalFunctions
type AnalysisMonad m = ReaderT (AnalysisEnvironment m) m

-- | The functions in the returned set are those that do not return.
--
-- Warning, this return type may become abstract at some point.
noReturnAnalysis :: (Monad m, HasCFG cfg)
                    => (ExternalFunction -> m Bool)
                    -> cfg
                    -> HashSet Function
                    -> m (HashSet Function)
noReturnAnalysis extSummary cfgLike summ = do
  let cfg = getCFG cfgLike
      f = getFunction cfg
      env = AE extSummary summ
      analysis = dataflowAnalysis NotReturned meet returnTransfer
  localRes <- runReaderT (forwardDataflow cfg analysis NotReturned) env
  case dataflowResult localRes of
    WillNeverReturn -> return $! S.insert f summ
    NotReturned -> return $! S.insert f summ
    Returned -> return summ

returnTransfer :: (Monad m) => ReturnInfo -> Instruction -> AnalysisMonad m ReturnInfo
returnTransfer ri i =
  case i of
    CallInst { callFunction = calledFunc } ->
      dispatchCall ri calledFunc
    InvokeInst { invokeFunction = calledFunc } ->
      dispatchCall ri calledFunc
    UnreachableInst {} -> return WillNeverReturn
    ResumeInst {} -> return WillNeverReturn
    RetInst {} -> return Returned
    _ -> return ri

dispatchCall :: (Monad m) => ReturnInfo -> Value -> AnalysisMonad m ReturnInfo
dispatchCall ri v =
  case valueContent' v of
    FunctionC f -> do
      intSumm <- asks internalSummary
      case S.member f intSumm of
        True -> return WillNeverReturn
        False -> return ri
    ExternalFunctionC ef -> do
      extSumm <- asks externalSummary
      isNoRet <- lift $ extSumm ef
      case isNoRet of
        True -> return WillNeverReturn
        False -> return ri
    _ -> return ri

{-# ANN module "HLint: ignore Use if" #-}
