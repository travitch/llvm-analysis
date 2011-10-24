{-# LANGUAGE OverloadedStrings #-}
-- | This is the top-level LLVM IR module.  It provides functions to
-- construct the IR from a bitcode file and some basic methods to
-- inspect it.
--
-- This module re-exports all of the types used in the IR for
-- convenience.
module Data.LLVM (
  -- * Constructing the IR
  -- parseLLVMBitcodeFile,
  -- defaultParserOptions,
  -- ParserOptions(..),
  -- PositionPrecision(..),
  -- * Inspect the IR
  findFunctionByName,
  findMain,
  module Data.LLVM.Types
  ) where

import qualified Data.ByteString.Char8 as BS
import Data.List ( find )

-- import Data.LLVM.Private.Parser.Options
-- import Data.LLVM.Private.Parser.Unmarshal
import Data.LLVM.Types

-- | Find a function in the Module by its name.
findFunctionByName :: Module -> String -> Maybe Function
findFunctionByName m s = find isFunc $ moduleDefinedFunctions m
  where
    funcIdent = makeGlobalIdentifier (BS.pack s)
    isFunc f = functionName f == funcIdent

-- | Find the function named 'main' in the 'Module', if any.
findMain :: Module -> Maybe Function
findMain m = findFunctionByName m "main"
