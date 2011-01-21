module Data.LLVM.Private.ParsingMonad ( ParsingMonad(..)
                                      , maybeRunLLVMParser
                                      , runLLVMParser
                                      ) where

import Data.Text (Text)
import Data.LLVM.Lexer

runLLVMParser :: ([Token] -> ParsingMonad a) -> Text -> Either String a
runLLVMParser p t = case res of
  Ok result -> Right result
  Failed e -> Left e
  where tokens = lexer t
        res = p tokens

maybeRunLLVMParser :: ([Token] -> ParsingMonad a) -> Text -> Maybe a
maybeRunLLVMParser p t = case res of
  Ok result -> Just result
  Failed _ -> Nothing
  where tokens = lexer t
        res = p tokens

data ParsingMonad a = Ok a
                    | Failed String
                    deriving (Show)

instance Monad ParsingMonad where
  return = Ok
  fail = Failed
  m >>= k = case m of
    Ok a -> k a
    Failed e -> Failed e
