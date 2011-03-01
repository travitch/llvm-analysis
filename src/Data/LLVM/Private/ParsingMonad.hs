module Data.LLVM.Private.ParsingMonad ( -- ParsingMonad(..)
  maybeRunLLVMParser
  , runLLVMParser
  ) where

import Data.Text (Text)
import Data.LLVM.Private.Lexer
import Data.LLVM.Private.Parser

import Text.Parsec.Prim

-- runLLVMParser :: ([Token] -> ParsingMonad a) -> Text -> Either String a
runLLVMParser p t = res
  where tokens = lexer t
        res = parse p "" tokens

-- maybeRunLLVMParser :: ([Token] -> ParsingMonad a) -> Text -> Maybe a
maybeRunLLVMParser p t =
  either (const Nothing) Just (runLLVMParser p t)
