module Data.LLVM.Private.Parser (parser) where

import Control.Applicative hiding ((<|>))
import Text.Parsec

import Data.LLVM.Private.AttributeTypes
import Data.LLVM.Private.Lexer
import Data.LLVM.Private.PlaceholderTypes
import Data.LLVM.Private.PlaceholderBuilders
import Data.LLVM.Private.Parser.Attributes
import Data.LLVM.Private.Parser.Primitive

-- | To parse a full constant, we need to apply a partial constant to
-- its type.
constantP :: AssemblyParser Constant
constantP = do
  t <- typeP
  c <- partialConstantP
  return $ c t

-- | A partial constant is either an identifier or one of the actual
-- constant types.
partialConstantP :: AssemblyParser PartialConstant
partialConstantP = (valueRef <$> identifier) <|> (ConstValue <$> simpleConstant)

simpleConstant :: AssemblyParser ConstantT
simpleConstant = tokenAs matcher
  where matcher x =
          case x of
            TTrueLit -> Just (ConstantInt 1)
            TFalseLit -> Just (ConstantInt 0)
            TIntLit i -> Just (ConstantInt i)
            TFloatLit f -> Just (ConstantFP f)
            TStringLit s -> Just (ConstantString s)
            TMetadataString m -> Just (MDString m)
            TNullLit -> Just ConstantPointerNull
            TUndefLit -> Just UndefValue
            TZeroInitializer -> Just ConstantAggregateZero
            _ -> Nothing



typeP :: AssemblyParser Type
typeP = do
  -- Every type starts with some _base_ type.  These are primitive
  -- types upon which the others are built. We include the basic
  -- aggregates here since they can be unambiguously parsed.
  baseType <- choice [ baseParser
                     , arrayTypeP TypeArray [TLSquare] [TRSquare]
                     , arrayTypeP TypeVector [TLAngle] [TRAngle]
                     , aggregateTypeP TypeStruct [TLCurl] [TRCurl]
                     , aggregateTypeP TypePackedStruct [TLAngle, TLCurl] [TRCurl, TRAngle]
                     ]
  -- Since this is a top-down parser, we can't use left recursion to
  -- pick up pointer modifiers or function types, so we have to do it
  -- all in one go here using iteration.  We start with pointer
  -- annotations (since this could be the return type of a function
  -- type).
  pointerType <- manyChain (consumeToken TStar) pointerAccum baseType
  typeArgList <- funcArgFragment <|> (parserReturn Nothing)
  case typeArgList of
    Nothing -> return pointerType
    Just (argTypes, isVa) -> return $ TypeFunction pointerType argTypes isVa

  where baseParser = tokenAs matcher
        matcher x =
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
        aggregateTypeP c l r = do
          mapM_ consumeToken l
          ts <- sepBy typeP (consumeToken TComma)
          mapM_ consumeToken r
          return $ c ts
        arrayTypeP c l r = do
          mapM_ consumeToken l
          i <- parseInteger
          consumeToken TAggLen
          t <- typeP
          mapM_ consumeToken r
          return $ c i t
        pointerAccum _ t = TypePointer t
        funcArgFragment = try $ do
          consumeToken TLParen
          -- Comma separated types
          funcArgTypes <- sepBy typeP (consumeToken TComma)
          -- Now, we could have a ", ..." FIXME: check
          consumeToken TRParen
          return $ Just (funcArgTypes, False)

typeDeclaration :: AssemblyParser GlobalDeclaration
typeDeclaration = NamedType <$> localIdentifierP <*> (syntaxP *> typeP)
  where eqTok = consumeToken TAssign
        typeTok = consumeToken TType
        syntaxP = eqTok *> typeTok

moduleLevelAssembly :: AssemblyParser GlobalDeclaration
moduleLevelAssembly = (ModuleAssembly . Assembly) <$> (syntaxP *> parseString)
  where syntaxP = consumeToken TModule *> consumeToken TAsm

targetTriple :: AssemblyParser TargetTriple
targetTriple = mkTriple <$> (syntaxP *> parseString)
  where syntaxP = consumeToken TTarget *> consumeToken TTriple *> consumeToken TAssign

dataLayout :: AssemblyParser DataLayout
dataLayout = mkDataLayout <$> (syntaxP *> parseString)
  where syntaxP = consumeToken TTarget *> consumeToken TDataLayout *> consumeToken TAssign

parser = undefined

-- satisfy f