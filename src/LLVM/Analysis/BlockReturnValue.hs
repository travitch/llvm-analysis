module LLVM.Analysis.BlockReturnValue (
  BlockReturns,
  hoistReturns,
  blockReturn
  ) where

import Control.Arrow ( second )
import Data.HashMap.Strict ( HashMap )
import qualified Data.HashMap.Strict as HM
import Data.Maybe ( isJust )
import Data.Monoid

import LLVM.Analysis

-- Idea: start from the return instruction (the "bottom" of the
-- function) and propagate the branches of phi instructions backwards.
-- Collect these in a map and provide a lookup to see the return for a
-- given block.  They can only be pushed up through unconditional
-- branches.
--
-- Maybe a derived analysis could deal with aggregating possible
-- returns for unconditional branches.
--
-- For this analysis, if no return value makes it to the end of the
-- error checking block, it is error recovery code (also interesting).
data BlockReturns = BlockReturns (HashMap BasicBlock Value)

-- First pass, make a table of all unconditional block predecessors.
-- This is needed to trace backwards...  Phi nodes include this
-- information, but returns of non-phi nodes do not.
hoistReturns :: Function -> BlockReturns
hoistReturns f =
  case functionExitInstructions f of
    [] -> BlockReturns mempty
    exitInsts -> BlockReturns $ foldr pushReturnValues mempty exitInsts
  where
    upreds = foldr addPred mempty (functionBody f)
    addPred bb ps =
      case unconditionalTerminatorTarget bb of
        Just t -> HM.insertWith (++) t [bb] ps
        _ -> ps
    pushReturnValues exitInst m =
      case exitInst of
        RetInst { retInstValue = Just rv } ->
          let Just b0 = instructionBasicBlock exitInst
          in pushReturnUp (rv, b0) m
        _ -> m
    pushReturnUp (val, bb) m
      | not (hasUnconditionalTerminator bb) && not (isExitBlock bb) = m
      | otherwise =
        case valueContent' val of
          InstructionC PhiNode { phiIncomingValues = ivs } ->
            foldr pushReturnUp m (map (second toBB) ivs)
          _ ->
            let m' = HM.insert bb val m
                preds = HM.lookup bb upreds
            in maybe m' (foldr pushReturnUp m' . zip (repeat val)) preds

-- FIXME: Stopping at a conditional branch is a bit too conservative.
-- We can do better with the CFG.  If the return value being
-- propagated up post-dominates *all* of the edges of a branch, then
-- it must be the return value for all of those branches.

toBB :: Value -> BasicBlock
toBB v =
  case valueContent v of
    BasicBlockC bb -> bb
    _ -> error "LLVM.Analysis.BlockReturnValue.toBB: not a basic block"

hasUnconditionalTerminator :: BasicBlock -> Bool
hasUnconditionalTerminator = isJust . unconditionalTerminatorTarget

unconditionalTerminatorTarget :: BasicBlock -> Maybe BasicBlock
unconditionalTerminatorTarget bb =
  case basicBlockTerminatorInstruction bb of
    UnconditionalBranchInst { unconditionalBranchTarget = t } -> Just t
    _ -> Nothing

isExitBlock :: BasicBlock -> Bool
isExitBlock bb =
  case basicBlockTerminatorInstruction bb of
    RetInst {} -> True
    _ -> False

blockReturn :: BlockReturns -> BasicBlock -> Maybe Value
blockReturn (BlockReturns m) bb = HM.lookup bb m
