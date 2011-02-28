module Data.LLVM.Private.Parser.Types ( typeP ) where

import Control.Applicative hiding ((<|>))
import Text.Parsec

import Data.LLVM.Private.AttributeTypes
import Data.LLVM.Private.Lexer
import Data.LLVM.Private.PlaceholderTypes
import Data.LLVM.Private.Parser.Primitive

typeP :: AssemblyParser Type
typeP = do
  -- Every type starts with some _base_ type.  These are primitive
  -- types upon which the others are built. We include the basic
  -- aggregates here since they can be unambiguously parsed.
  baseType <- choice [ baseParser
                     , try $ arrayTypeP TypeArray [TLSquare] [TRSquare]
                     , try $ arrayTypeP TypeVector [TLAngle] [TRAngle]
                     , try $ aggregateTypeP TypeStruct [TLCurl] [TRCurl]
                     , try $ aggregateTypeP TypePackedStruct [TLAngle, TLCurl] [TRCurl, TRAngle]
                     ]
  -- Since this is a top-down parser, we can't use left recursion to
  -- pick up pointer modifiers or function types, so we have to do it
  -- all in one go here using iteration.  We start with pointer
  -- annotations (since this could be the return type of a function
  -- type).
  pointerType <- manyChain (consumeToken TStar) pointerAccum baseType
  typeArgList <- optionMaybe funcArgFragment
  case typeArgList of
    Nothing -> return pointerType
    Just (argTypes, isVa) -> return $ TypeFunction pointerType argTypes isVa

baseParser :: AssemblyParser Type
baseParser = tokenAs matcher
  where matcher x =
          case x of
            TIntegralT i -> Just (TypeInteger i)
            TFloatT -> Just TypeFloat
            TDoubleT -> Just TypeDouble
            TX86_FP80T -> Just TypeX86FP80
            TFP128T -> Just TypeFP128
            TPPC_FP128T -> Just TypePPCFP128
            TX86mmxT -> Just TypeX86MMX
            TVoidT -> Just TypeVoid
            TMetadataT -> Just TypeMetadata
            TOpaqueT -> Just TypeOpaque
            TUprefT i -> Just (TypeUpref i)
            TLabelT -> Just TypeLabel
            TLocalIdent i -> Just (TypeNamed $ makeLocalIdentifier i)
            _ -> Nothing

-- Parse a list of comma-separated types bracketed by
-- the l and r parsers.
aggregateTypeP :: ([Type] -> Type) -> [LexerToken] -> [LexerToken] ->
                  AssemblyParser Type
aggregateTypeP c l r = c <$> betweenTokens l r p
  where p = sepBy typeP (consumeToken TComma)

arrayTypeP :: (Integer -> Type -> Type) -> [LexerToken] -> [LexerToken] ->
              AssemblyParser Type
arrayTypeP c l r = c <$> prefx <*> postfx
  where prefx = consumeTokens l *> parseInteger
        postfx = consumeToken TAggLen *> typeP <* consumeTokens r

pointerAccum :: a -> Type -> Type
pointerAccum _ t = TypePointer t

funcArgFragment :: AssemblyParser ([Type], Bool)
funcArgFragment = try $ do
  consumeToken TLParen
  -- Comma separated types.  Use sepEndBy instead of sepBy to consume
  -- the comma separating the main list from the va-marker, if there
  -- is one.
  funcArgTypes <- sepEndBy typeP (consumeToken TComma)
  -- Now, we could have a ", ..." for a valist signature
  isVa <- option False $ True <$ consumeToken TDotDotDot
  consumeToken TRParen
  return (funcArgTypes, isVa)
