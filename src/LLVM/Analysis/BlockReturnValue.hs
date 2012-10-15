-- | Label each BasicBlock with the value it *must* return.
--
-- Most frontends that generate bitcode unify all of the return
-- statements of a function and return a phi node that has a return
-- value for each branch.  This pass ('labelBlockReturns') pushes
-- those returns backwards through the control flow graph as labels on
-- basic blocks.  The function 'blockReturn' gives the return value
-- for a block, if there is a value that must be returned by that
-- block.
--
-- The algorithm starts from the return instruction.  Non-phi values
-- are propagated backwards to all reachable blocks.  Phi values are
-- split and the algorithm propagates each phi incoming value back to
-- the block it came from.  A value can be propagated from a block BB
-- to its predecessor block PB if (and only if) BB postdominates PB.
-- Intuitively, the algorithm propagates a return value to a
-- predecessor block if that predecessor block *must* return that
-- value (hence postdominance).
module LLVM.Analysis.BlockReturnValue (
  BlockReturns,
  labelBlockReturns,
  blockReturn,
  blockReturns,
  instructionReturn,
  instructionReturns
  ) where

import Control.Arrow ( second )
import Data.HashMap.Strict ( HashMap )
import qualified Data.HashMap.Strict as HM
import Data.HashSet ( HashSet )
import qualified Data.HashSet as HS
import Data.Maybe ( mapMaybe )
import Data.Monoid

import LLVM.Analysis
import LLVM.Analysis.CFG

import LLVM.Analysis.Dominance

data BlockReturns = BlockReturns (HashMap BasicBlock Value) (HashMap BasicBlock (HashSet Value))

instance Show BlockReturns where
  show (BlockReturns _ m) = unlines $ map showPair (HM.toList m)
    where
      showPair (bb, vs) = show (basicBlockName bb) ++ ": " ++ show vs

instance Monoid BlockReturns where
  mempty = BlockReturns mempty mempty
  mappend (BlockReturns b1 bs1) (BlockReturns b2 bs2) =
    BlockReturns (b1 `mappend` b2) (HM.unionWith HS.union bs1 bs2)

-- | Retrieve the Value that must be returned (if any) if the given
-- BasicBlock executes.
blockReturn :: BlockReturns -> BasicBlock -> Maybe Value
blockReturn (BlockReturns m _) bb = HM.lookup bb m

-- | Builds on the results from 'blockReturn' and reports *all* of the
-- values that each block can return (results may not include the
-- final block).
blockReturns :: BlockReturns -> BasicBlock -> [Value]
blockReturns (BlockReturns _ m) bb = maybe [] HS.toList (HM.lookup bb m)

-- | Return the Value that must be returned (if any) if the given
-- Instruction is executed.
instructionReturn :: BlockReturns -> Instruction -> Maybe Value
instructionReturn brs i = do
  bb <- instructionBasicBlock i
  blockReturn brs bb

instructionReturns :: BlockReturns -> Instruction -> [Value]
instructionReturns brs i = blockReturns brs bb
  where
    Just bb = instructionBasicBlock i

-- | Label each BasicBlock with the value that it must return (if
-- any).
labelBlockReturns :: (HasFunction funcLike, HasPostdomTree funcLike, HasCFG funcLike)
                => funcLike -> BlockReturns
labelBlockReturns funcLike =
  case functionExitInstructions f of
    [] -> BlockReturns mempty mempty
    exitInsts ->
      let s0 = (mempty, mempty)
          singleBlockRets = fst $ foldr pushReturnValues s0 exitInsts
          -- Traverse the list of basic blocks in reverse order
          -- (bottom up) to accumulate as many returns as is
          -- reasonable.
          cs0 = fmap HS.singleton singleBlockRets -- convert to list values
          compositeRets = foldr accumulateSuccReturns cs0 (reverse blocks)
      in BlockReturns singleBlockRets compositeRets
  where
    f = getFunction funcLike
    pdt = getPostdomTree funcLike
    cfg = getCFG funcLike
    blocks = functionBody f

    pushReturnValues exitInst (m, vis) =
      let Just b0 = instructionBasicBlock exitInst
      in case exitInst of
        RetInst { retInstValue = Just rv } ->
          pushReturnUp Nothing (rv, b0) (m, vis)
        _ -> (m, vis)
    pushReturnUp prevBlock (val, bb) acc@(m, vis)
      | HS.member bb vis = acc
      | not (prevTerminatorPostdominates pdt prevBlock bb) =
        (m, HS.insert bb vis)
      | otherwise =
        case valueContent' val of
          InstructionC PhiNode { phiIncomingValues = ivs } ->
            let vis' = HS.insert bb vis
            in foldr (pushReturnUp (Just bb) . second toBB) (m, vis') ivs
          _ ->
            let m' = HM.insert bb val m
                vis' = HS.insert bb vis
                preds = basicBlockPredecessors cfg bb
            in foldr (pushReturnUp (Just bb)) (m', vis') (zip (repeat val) preds)

    accumulateSuccReturns b acc =
      let succs = basicBlockSuccessors cfg b
          succRets = mapMaybe (\s -> HM.lookup s acc) succs
      in case null succRets of
        True -> acc
        False -> HM.insert b (mconcat succRets) acc

-- | Return True if the terminator instruction of the previous block
-- in the traversal postdominates the terminator instruction of the
-- current block.
prevTerminatorPostdominates :: PostdominatorTree -> Maybe BasicBlock -> BasicBlock -> Bool
prevTerminatorPostdominates _ Nothing _ = True
prevTerminatorPostdominates pdt (Just prevBlock) bb =
  postdominates pdt prevTerm bbTerm
  where
    prevTerm = basicBlockTerminatorInstruction prevBlock
    bbTerm = basicBlockTerminatorInstruction bb

-- | Unconditionally convert a Value to a BasicBlock.  This should
-- always work for the second value of each Phi incoming value.  There
-- may be some cases with blockaddresses that fail...
toBB :: Value -> BasicBlock
toBB v =
  case valueContent v of
    BasicBlockC bb -> bb
    _ -> error "LLVM.Analysis.BlockReturnValue.toBB: not a basic block"
