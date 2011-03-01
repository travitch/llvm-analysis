module Data.LLVM.Private.Parser.Primitive ( AssemblyParser
                                          , tokenAs
                                          , commaP
                                          , lexTokenAs
                                          , consumeToken
                                          , consumeTokens
                                          , parseInteger
                                          , parseString
                                          , manyChain
                                          , betweenTokens
                                          ) where

import Control.Applicative hiding ((<|>), many)
import Data.List (foldl')
import Data.Text (Text)
import Text.Parsec
import Text.Parsec.Pos (newPos)

import Data.LLVM.Private.Lexer

type AssemblyParser = Parsec [Token] ()

commaP :: AssemblyParser ()
commaP = consumeToken TComma

-- | Parse a list of `p`, producing a result after each step by applying
-- f to the result and the current seed.
manyChain :: AssemblyParser a -> (b -> a -> b) -> b -> AssemblyParser b
manyChain p f initVal = do
  vs <- many p
  return $! foldl' f initVal vs

betweenTokens :: [LexerToken] -> [LexerToken] -> AssemblyParser a -> AssemblyParser a
betweenTokens open close p = consumeTokens open *> p <* consumeTokens close

toSourcePos :: AlexPosn -> SourcePos
toSourcePos (AlexPn _ line col) = newPos "" line col

tokenAs :: (LexerToken -> Maybe a) -> AssemblyParser a
tokenAs test = token showToken posToken posTest
  where showToken = show . tokenT
        posToken = toSourcePos . tokenPos
        posTest = test . tokenT

-- | Parse the given 0-argument token and return the associated final value
lexTokenAs :: (LexerToken, a) -> AssemblyParser a
lexTokenAs (t, val) = tokenAs matcher >> return val
  where matcher tok =
          if t == tok
          then Just t
          else Nothing

consumeToken :: LexerToken -> AssemblyParser ()
consumeToken t = tokenAs matcher >> return ()
  where matcher tok =
          if t == tok
          then Just t
          else Nothing

consumeTokens :: [LexerToken] -> AssemblyParser ()
consumeTokens = mapM_ consumeToken

parseInteger :: AssemblyParser Integer
parseInteger = tokenAs matcher
  where matcher x =
          case x of
            TIntLit i -> Just i
            _ -> Nothing

parseString :: AssemblyParser Text
parseString = tokenAs matcher
  where matcher x =
          case x of
            TString s -> Just s
            _ -> Nothing
