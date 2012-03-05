-- | This module defines a simple worklist.
--
-- The worklist operates in phases.  First, it drains all of the items
-- in the "current" list.  Then it replaces the current list with the
-- "next" list.
--
-- Items are added to the worklist by placing them on this internal
-- "next" list.  The "next" list is treated as a set so that items
-- will only appear once to avoid repeated processing.
--
-- TODO: Provide a hook to sort the "next" worklist in some
-- user-specified way.  Topological sorts are often useful, for
-- example.
module LLVM.Analysis.Internal.Worklist (
  -- * Types
  Worklist,
  WorklistDecomp(..),
  -- * Constructors
  emptyWorklist,
  worklistFromList,
  -- * Accessors/Modifiers
  takeWorkItem,
  addWorkItem,
  addWorkItems
  ) where

import Data.Set ( Set )
import qualified Data.Set as S

-- | A worklist.
data Worklist a = Worklist ![a] !(Set a)

-- | A view of the worklist exposing the next item and the rest of the
-- worklist
data WorklistDecomp a = EmptyWorklist
                      | a :< (Worklist a)

-- | Create a new empty worklist
emptyWorklist :: Worklist a
emptyWorklist = Worklist [] S.empty

-- | Create a new worklist from an initial list of items
worklistFromList :: Ord a => [a] -> Worklist a
worklistFromList itms = Worklist (uniq itms) S.empty
  where
    uniq = S.toList . S.fromList

-- | Take the next work item.
takeWorkItem :: Ord a => Worklist a -> WorklistDecomp a
takeWorkItem (Worklist lst nxt) = case lst of
  itm : rest -> itm :< (Worklist rest nxt)
  [] -> case S.null nxt of
    True -> EmptyWorklist
    False -> takeWorkItem (Worklist (S.toList nxt) S.empty)

-- | Add a single item to the worklist
addWorkItem :: Ord a => a -> Worklist a -> Worklist a
addWorkItem itm (Worklist lst nxt) = Worklist lst (S.insert itm nxt)

-- | Add a list of items to the worklist
addWorkItems :: Ord a => [a] -> Worklist a -> Worklist a
addWorkItems itms (Worklist lst nxt) =
  Worklist lst (S.union nxt (S.fromList itms))