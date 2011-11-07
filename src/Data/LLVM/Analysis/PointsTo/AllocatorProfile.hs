{-# LANGUAGE OverloadedStrings #-}
-- | This module defines a number of allocation profiles that are
-- meant to be inputs to the points-to analyses.  These profiles
-- identify the set of instructions that allocate *fresh* memory
-- locations (e.g., @malloc@).
--
-- Different profiles are useful for different languages or setups.
-- The points-to analyses take lists of these functions so they can be
-- combined arbitrarily (and augmented with user-provided versions).
module Data.LLVM.Analysis.PointsTo.AllocatorProfile (
  standardCProfile
  ) where

import Data.LLVM

-- | This profile corresponds to the standard C library and marks
-- @malloc@, @calloc@, and @alloca@ as allocators.  @realloc@ is not
-- always an allocator (since it could return existing memory), so it
-- is not included.
--
-- This function returns True if the given instruction must be a call
-- to a standard C library allocation function.
standardCProfile :: Instruction -> Bool
standardCProfile CallInst { callFunction = cf } =
  case valueContent cf of
    ExternalFunctionC ef ->
      let ident = identifierContent (externalFunctionName ef)
      in ident == "malloc" || ident == "calloc" || ident == "alloca"
    _ -> False
standardCProfile _ = False
