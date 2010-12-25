module Data.LLVM ( parser, runLLVMParser ) where

import Data.ByteString.Lazy (ByteString)

import Data.LLVM.AssemblyParser
import Data.LLVM.Lexer
import Data.LLVM.Private.ParsingMonad

runLLVMParser :: ([Token] -> ParsingMonad a) -> ByteString -> Maybe a
runLLVMParser p bs = case res of
  Ok result -> Just result
  _ -> Nothing
  where tokens = lexer bs
        res = p tokens



