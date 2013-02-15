{-# LANGUAGE ViewPatterns, NoMonomorphismRestriction #-}
{-|

This analysis identifies the (memory) effects that functions have on
the scalar components of their arguments.

Only pointer parameters are interesting because only their effects can
escape the callee.  Effects are currently restricted to increments and
decrements of integral types.  The affected memory can be a struct
member; the effects are described in terms of abstract AccessPaths.

This is a must analysis.  Effects are only reported if they *MUST*
occur (modulo non-termination style effects like calls to exit or
infinite loops).

Currently, sequential effects are not composed and nothing useful will
be reported.

-}
module LLVM.Analysis.ScalarEffects (
  ScalarEffectResult,
  ScalarEffect(..),
  scalarEffectAnalysis
  ) where

import Control.DeepSeq
import Data.HashMap.Strict ( HashMap )
import qualified Data.HashMap.Strict as HM

import LLVM.Analysis
import LLVM.Analysis.AccessPath
import LLVM.Analysis.CFG
import LLVM.Analysis.Dataflow

-- | The types of effects tracked by this analysis.  This can be expanded
-- as the analysis becomes more sophisticated (it could include general
-- affine relations or even relate arguments to each other).
data ScalarEffect = EffectAdd1 AbstractAccessPath
                  | EffectSub1 AbstractAccessPath
                  deriving (Eq)

instance NFData ScalarEffect where
  rnf e@(EffectAdd1 ap) = ap `deepseq` e `seq` ()
  rnf e@(EffectSub1 ap) = ap `deepseq` e `seq` ()

type ScalarEffectResult = HashMap Argument ScalarEffect

data ScalarInfo = SI (HashMap Argument (Maybe ScalarEffect))
                | SITop

instance Eq ScalarInfo where
  (SI s1) == (SI s2) = s1 == s2
  SITop == SITop = True
  _ == _ = False

instance MeetSemiLattice ScalarInfo where
  meet SITop s = s
  meet s SITop = s
  meet (SI s1) (SI s2) = SI (HM.unionWith mergeEffect s1 s2)
    where
      -- | If there is an entry in both maps, it must be the same to be
      -- retained.
      mergeEffect e1 e2 = if e1 == e2 then e1 else Nothing

instance BoundedMeetSemiLattice ScalarInfo where
  top = SITop

-- instance (Monad m) => DataflowAnalysis m ScalarInfo where
--   transfer = scalarTransfer

analysis :: (Monad m) => DataflowAnalysis m ScalarInfo
analysis = dataflowAnalysis scalarTransfer

-- For each function, initialize all arguments to Nothing
scalarEffectAnalysis :: (Monad m, HasCFG funcLike, HasFunction funcLike)
                        => funcLike
                        -> ScalarEffectResult
                        -> m ScalarEffectResult
scalarEffectAnalysis funcLike summ = do
  let cfg = getCFG funcLike
--      f = getFunction funcLike
      -- s0 = SI $ HM.fromList (zip (functionParameters f) (repeat Nothing))

  localRes <- forwardDataflow analysis cfg
  let xi = case dataflowResult localRes of
        SITop -> HM.empty
        SI m -> HM.foldlWithKey' discardNothings HM.empty m
  return $! HM.union xi summ
  -- let exitInsts = filter (instructionReachable cfg) (functionExitInstructions f)
  --     exitInfo = meets $ map (dataflowResult localRes) exitInsts
  --     exitInfo' = case exitInfo of
  --       SITop -> HM.empty
  --       SI m -> HM.foldlWithKey' discardNothings HM.empty m
  -- return $! HM.union exitInfo' summ

discardNothings :: HashMap Argument ScalarEffect
                   -> Argument
                   -> Maybe ScalarEffect
                   -> HashMap Argument ScalarEffect
discardNothings acc _ Nothing = acc
discardNothings acc a (Just e) = HM.insert a e acc

scalarTransfer :: (Monad m) => ScalarInfo -> Instruction -> m ScalarInfo
scalarTransfer si i =
  case i of
    AtomicRMWInst { atomicRMWOperation = AOAdd
                  , atomicRMWValue =
      (valueContent -> ConstantC ConstantInt { constantIntValue = 1 })} ->
      recordIfAffectsArgument EffectAdd1 i si
    AtomicRMWInst { atomicRMWOperation = AOAdd
                  , atomicRMWValue =
      (valueContent -> ConstantC ConstantInt { constantIntValue = -1 })} ->
      recordIfAffectsArgument EffectSub1 i si
    AtomicRMWInst { atomicRMWOperation = AOSub
                  , atomicRMWValue =
      (valueContent -> ConstantC ConstantInt { constantIntValue = 1 })} ->
      recordIfAffectsArgument EffectSub1 i si
    AtomicRMWInst { atomicRMWOperation = AOSub
                  , atomicRMWValue =
      (valueContent -> ConstantC ConstantInt { constantIntValue = -1 })} ->
      recordIfAffectsArgument EffectAdd1 i si
    StoreInst { storeAddress = sa, storeValue = sv } ->
      case isNonAtomicAdd sa sv of
        False ->
          case isNonAtomicSub sa sv of
            False -> return si
            True -> recordIfAffectsArgument EffectSub1 i si
        True -> recordIfAffectsArgument EffectAdd1 i si
    _ -> return si

isNonAtomicSub :: (IsValue a) => Value -> a -> Bool
isNonAtomicSub sa sv =
  case valueContent sv of
    InstructionC AddInst {
      binaryLhs = (valueContent -> ConstantC ConstantInt { constantIntValue = -1 }),
      binaryRhs = (valueContent -> InstructionC LoadInst { loadAddress = la }) } ->
      sa == la
    InstructionC AddInst {
      binaryRhs = (valueContent -> ConstantC ConstantInt { constantIntValue = -1 }),
      binaryLhs = (valueContent -> InstructionC LoadInst { loadAddress = la }) } ->
      sa == la
    InstructionC SubInst {
      binaryRhs = (valueContent -> ConstantC ConstantInt { constantIntValue = 1 }),
      binaryLhs = (valueContent -> InstructionC LoadInst { loadAddress = la }) } ->
      sa == la
    _ -> False

isNonAtomicAdd :: (IsValue a) => Value -> a -> Bool
isNonAtomicAdd sa sv =
  case valueContent sv of
    InstructionC AddInst {
      binaryLhs = (valueContent -> ConstantC ConstantInt { constantIntValue = 1 }),
      binaryRhs = (valueContent -> InstructionC LoadInst { loadAddress = la }) } ->
      sa == la
    InstructionC AddInst {
      binaryRhs = (valueContent -> ConstantC ConstantInt { constantIntValue = 1 }),
      binaryLhs = (valueContent -> InstructionC LoadInst { loadAddress = la }) } ->
      sa == la
    InstructionC SubInst {
      binaryRhs = (valueContent -> ConstantC ConstantInt { constantIntValue = -1 }),
      binaryLhs = (valueContent -> InstructionC LoadInst { loadAddress = la }) } ->
      sa == la
    _ -> False

recordIfAffectsArgument :: (Monad m)
                           => (AbstractAccessPath -> ScalarEffect)
                           -> Instruction
                           -> ScalarInfo
                           -> m ScalarInfo
recordIfAffectsArgument con i si =
  case accessPath i of
    Nothing -> return si
    Just cap ->
      case valueContent' (accessPathBaseValue cap) of
        ArgumentC a ->
          let e = Just $ con (abstractAccessPath cap)
          in case si of
            SITop -> return $! SI $ HM.insert a e HM.empty
            SI m -> return $! SI $ HM.insert a e m
        _ -> return si

{-# ANN module "HLint: ignore Use if" #-}
