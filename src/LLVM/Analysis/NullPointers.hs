{-# LANGUAGE DeriveDataTypeable, FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
-- | An analysis to identify the NULL state of pointers at each
-- Instruction in a Function.  Pointers can either be DefiniteNULL,
-- NotNULL, or Unknown.  Only DefiniteNULL and NotNULL are recorded -
-- all other pointers are Unknown.
module LLVM.Analysis.NullPointers (
  HasNullSummary(..),
  NullPointersSummary,
  nullPointersAnalysis,
  nullPointersAt,
  notNullPointersAt,
  branchNullInfo,
  NullInfoError(..)
  ) where

import Control.Failure
import Control.Monad.Identity
import Data.Maybe ( fromMaybe )
import Data.Monoid
import Data.HashSet ( HashSet )
import qualified Data.HashSet as HS
import Data.Typeable ( Typeable )

import LLVM.Analysis
import LLVM.Analysis.CDG
import LLVM.Analysis.Dataflow

class HasNullSummary a where
  getNullSummary :: a -> NullPointersSummary

instance HasNullSummary NullPointersSummary where
  getNullSummary = id

data NULLState = Top
               | NS (HashSet Value) (HashSet Value)
                 -- ^ Must be NULL, definitely not NULL
               deriving (Eq, Show)

-- | A record of the known NULL and known Not-NULL pointers at each
-- Instruction.
newtype NullPointersSummary =
  NPS (DataflowResult Identity NULLState)
  deriving (Eq, Show)

-- | Determine which pointers are NULL and NotNULL at each
-- Instruction.
nullPointersAnalysis :: (HasCDG cdg) => cdg -> NullPointersSummary
nullPointersAnalysis cdgLike =
  NPS $ runIdentity $ forwardDataflow cdg analysis f0
  where
    cdg = getCDG cdgLike
    f0 = NS mempty mempty

    analysis = fwdDataflowEdgeAnalysis Top meet transfer edgeTransfer
-- See Note [NULL Pointers]

nullPointersAt :: NullPointersSummary -> Instruction -> [Value]
nullPointersAt (NPS summ) i =
  case runIdentity $ dataflowResultAt summ i of
    Top -> []
    NS mustNull _ -> HS.toList mustNull

notNullPointersAt :: NullPointersSummary -> Instruction -> [Value]
notNullPointersAt (NPS summ) i =
  case runIdentity $ dataflowResultAt summ i of
    Top -> []
    NS _ notNull -> HS.toList notNull

-- | If an item is in must1 and must2, it must be null in the
-- result. Likewise not1 and not2.  The intersections are separate and
-- simple.
meet :: NULLState -> NULLState -> NULLState
meet Top other = other
meet other Top = other
meet (NS must1 not1) (NS must2 not2) =
  NS (must1 `HS.intersection` must2) (not1 `HS.intersection` not2)

-- | Add any new facts we've learned from the block state.  We could
-- have this be a no-op for non-entry instructions, but it probably
-- isn't a big deal.
transfer :: (Monad m) => NULLState -> Instruction -> m NULLState
transfer s _ = return s

addNull :: Value -> NULLState -> NULLState
addNull v s =
  case s of
    Top -> NS (HS.singleton v) mempty
    NS must notNull -> NS (HS.insert v must) notNull

addNotNull :: Value -> NULLState -> NULLState
addNotNull v s =
  case s of
    Top -> NS mempty (HS.singleton v)
    NS must notNull -> NS must (HS.insert v notNull)

edgeTransfer :: (Monad m ) => NULLState -> Instruction -> m [(BasicBlock, NULLState)]
edgeTransfer s i = return $ fromMaybe [] $ do
  (nullBlock, nullVal, notNullBlock) <- branchNullInfo i
  return [(nullBlock, addNull nullVal s),
          (notNullBlock, addNotNull nullVal s)
         ]

isNullPtr :: Value -> Bool
isNullPtr (valueContent -> ConstantC ConstantPointerNull {}) = True
isNullPtr _ = False

data NullInfoError = NotABranchInst Instruction
                   | NotANullTest Instruction
                   deriving (Typeable, Show)

-- | Given a BranchInst, return:
--
-- 1) The BasicBlock where a pointer is known to be NULL
--
-- 2) The value known to be NULL
--
-- 3) The BasicBlock where the pointer is known to be not NULL
branchNullInfo :: (Failure NullInfoError m)
                  => Instruction
                  -> m (BasicBlock, Value, BasicBlock)
branchNullInfo i@BranchInst { branchTrueTarget = tt
                            , branchFalseTarget = ft
                            , branchCondition = (valueContent -> InstructionC ci@ICmpInst { cmpPredicate = ICmpEq })
                            }
  | isNullPtr (cmpV1 ci) = return (tt, cmpV2 ci, ft)
  | isNullPtr (cmpV2 ci) = return (tt, cmpV1 ci, ft)
  | otherwise = failure (NotANullTest i)
branchNullInfo i@BranchInst { branchTrueTarget = tt
                            , branchFalseTarget = ft
                            , branchCondition = (valueContent -> InstructionC ci@ICmpInst { cmpPredicate = ICmpNe })
                            }
  | isNullPtr (cmpV1 ci) = return (ft, cmpV2 ci, tt)
  | isNullPtr (cmpV2 ci) = return (ft, cmpV1 ci, tt)
  | otherwise = failure (NotANullTest i)
branchNullInfo i = failure (NotABranchInst i)


{- Note [NULL Pointers]

The algorithm proceeds in two simple phases.

In the first, we check the direct control dependencies for each block.
The terminator instruction for each of these control dependencies
should be a conditional branch.  If the condition is a NULL check and
we can determine that the current block is on either the NULL or not
NULL path, we treat that as a fact for the current block.  This
produces a map from blocks to known NULL/Not Null states for a given
value.

We then use this initial map in the dataflow analysis.  At every
instruction whose basic block is in the map, introduce the map fact at
that point in the dataflow analysis.  It will handle meeting facts and
propagating them across other branches (if possible).

-}