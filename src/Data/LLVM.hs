module Data.LLVM ( llvmAssemblyParser, runLLVMParser ) where

import Data.LLVM.AssemblyParser
import Data.LLVM.Lexer
import Data.LLVM.Private.ParsingMonad

runLLVMParser p bs = case res of
  Ok result -> Just result
  _ -> Nothing
  where tokens = lexer bs
        res = p tokens



