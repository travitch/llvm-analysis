module Data.LLVM.Private.ParsingMonad ( ParsingMonad(..)
                                      , runLLVMParser
                                      ) where

import Data.Text (Text)
import Data.LLVM.Lexer

runLLVMParser :: ([Token] -> ParsingMonad a) -> Text -> Maybe a
runLLVMParser p bs = case res of
  Ok result -> Just result
  _ -> Nothing
  where tokens = lexer bs
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
