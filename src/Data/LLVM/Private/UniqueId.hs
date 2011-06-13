-- | This module provides unique identifiers to be used while
-- constructing the referential 'Module'.  The IDs provide identity to
-- these objects, which can exist in cycles.
--
-- This approach to unique IDs is helpful since it doesn't require
-- threading any state.  When IDs are needed in child recursive calls
-- to some function, the current stream can just be split as many
-- times as necessary ('split2', 'split3', etc).
--
-- 'split' itself is slightly unusual, see below.  Use 'extract' to
-- get an ID out of the stream.  It is still slightly tricky to ensure
-- that streams are not accidentally re-used.
module Data.LLVM.Private.UniqueId (
  -- * Types
  IdStream,
  UniqueId,
  -- * Operations
  extract,
  initialStream,
  split,
  split2,
  split3,
  split4
  ) where

import Control.Comonad
import Data.Stream.Supply hiding ( split )
import Data.LLVM.Types
import System.IO.Unsafe

-- | The type of the unique identifier source.
type IdStream = Supply UniqueId

-- The documentation recommends forcing the compiler to not inline
-- calls involving unsafePerformIO.  It shouldn't be an issue for this
-- use, but it also doesn't hurt.

-- | The initial ID stream, starting from zero.
initialStream :: IdStream
initialStream = unsafePerformIO newNumSupply
{-# NOINLINE initialStream #-}

-- | Split the stream; the new stream contains unused identifiers.
-- The input stream is still accessible and can produce one id with
-- 'extract'.
split :: Supply a -> Supply a
split = leftSupply