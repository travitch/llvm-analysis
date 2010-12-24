module Data.LLVM.Private.ParsingMonad ( ParsingMonad(..) ) where

data ParsingMonad a = Ok a
                    | Failed String
                    deriving (Show)

instance Monad ParsingMonad where
  return = Ok
  fail = Failed
  m >>= k = case m of
    Ok a -> k a
    Failed e -> Failed e
