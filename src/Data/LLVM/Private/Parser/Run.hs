module Data.LLVM.Private.Parser.Run (
  maybeRunLLVMParser,
  runLLVMParser
  ) where

import Data.ByteString.Lazy.Char8 ( ByteString )
import Text.Parsec.Error
import Text.Parsec.Prim

import Data.LLVM.Private.Lexer
import Data.LLVM.Private.Parser ( )
import Data.LLVM.Private.Parser.Primitive


runLLVMParser :: AssemblyParser a -> ByteString -> Either ParseError a
runLLVMParser p t = parse p "" tks
  where
    tks = lexer t

maybeRunLLVMParser :: AssemblyParser a -> ByteString -> Maybe a
maybeRunLLVMParser p t =
  either (const Nothing) Just (runLLVMParser p t)
