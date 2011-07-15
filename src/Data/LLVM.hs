-- | This is the top-level LLVM IR module.  It provides functions to
-- construct the IR from a bitcode file and some basic methods to
-- inspect it.
--
-- This module re-exports all of the types used in the IR for
-- convenience.
module Data.LLVM (
  -- * Constructing the IR
  parseLLVMBitcodeFile,
  defaultParserOptions,
  ParserOptions(..),
  PositionPrecision(..),
  -- * Inspect the IR
  module Data.LLVM.Types
  ) where

import Data.LLVM.Private.Parser.Options
import Data.LLVM.Private.Parser.Unmarshal
import Data.LLVM.Types

