module Data.LLVM ( parser, runLLVMParser ) where

import Data.ByteString.Lazy (ByteString)

import Data.LLVM.AssemblyParser
import Data.LLVM.Types
import Data.LLVM.Private.ParsingMonad
import Data.LLVM.Private.TieKnot

parseLLVMAsm :: ByteString -> Maybe Module
parseLLVMAsm bs = do
  parseTree <- runLLVMParser parser bs
  return $ tieKnot parseTree

