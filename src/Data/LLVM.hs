-- |
module Data.LLVM (
  parseLLVMBitcodeFile,
  defaultParserOptions,
  ParserOptions(..),
  PositionPrecision(..)
  ) where

import Data.LLVM.Private.Parser.Options
import Data.LLVM.Private.Parser.Unmarshal


