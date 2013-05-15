module LLVM.Analysis.UsesOf (
  UseSummary,
  computeUsesOf,
  usedBy
  ) where

import qualified Data.Foldable as F
import Data.HashMap.Strict ( HashMap )
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.Maybe ( fromMaybe )

import LLVM.Analysis

data UseSummary = UseSummary (HashMap Value [Instruction])

-- | Compute the uses of every value in the 'Module'
--
-- This information can be used to answer the query:
--
-- > usedBy useSummary foo
--
-- which will return all of the Instructions that reference
-- the provided value @foo@.
--
-- Note that this is a simple index.  It does not look through bitcasts
-- at all.
computeUsesOf :: Module -> UseSummary
computeUsesOf m = UseSummary $ fmap HS.toList uses
  where
    uses = F.foldl' funcUses HM.empty fs
    fs = moduleDefinedFunctions m
    funcUses acc f = F.foldl' addInstUses acc (functionInstructions f)
    addInstUses acc i = F.foldl' (addUses i) acc (instructionOperands i)
    addUses i acc v = HM.insertWith HS.union v (HS.singleton i) acc

-- | > usedBy summ val
--
-- Find the instructions using @val@ in the function that @summ@ was
-- computed for.
usedBy :: UseSummary -> Value -> [Instruction]
usedBy (UseSummary m) v = fromMaybe [] $ HM.lookup v m
