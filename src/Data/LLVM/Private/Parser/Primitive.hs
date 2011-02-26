module Data.LLVM.Private.Parser.Primitive ( AssemblyParser
                                          , tokenAs
                                          , lexTokenAs
                                          , consumeToken
                                          , parseInteger
                                          , parseString
                                          , manyChain
                                          ) where

import Data.Text (Text)
import Text.Parsec
import Text.Parsec.Pos (newPos)

import Data.LLVM.Private.Lexer

type AssemblyParser = Parsec [Token] ()


manyChain :: AssemblyParser a -> (a -> b -> b) -> b -> AssemblyParser b
manyChain p f initVal = do
  vs <- many p
  return $ foldr f initVal vs

toSourcePos :: AlexPosn -> SourcePos
toSourcePos (AlexPn _ line col) = newPos "" line col

tokenAs :: (LexerToken -> Maybe a) -> AssemblyParser a
tokenAs test = token showToken posToken posTest
  where showToken = show . snd
        posToken = toSourcePos . fst
        posTest = test . snd

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
