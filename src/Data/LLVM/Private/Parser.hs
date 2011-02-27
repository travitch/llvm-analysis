module Data.LLVM.Private.Parser (parser) where

import Control.Applicative hiding ((<|>), many)
import Text.Parsec

import Data.LLVM.Private.AttributeTypes
import Data.LLVM.Private.Lexer
import Data.LLVM.Private.PlaceholderTypes
import Data.LLVM.Private.PlaceholderBuilders
import Data.LLVM.Private.Parser.Attributes
import Data.LLVM.Private.Parser.Constants
import Data.LLVM.Private.Parser.Instructions
import Data.LLVM.Private.Parser.Primitive
import Data.LLVM.Private.Parser.Types

-- These are the top-level parsers.  The GlobalEntity parser will
-- need some fanciness with lookahead to dispatch to the correct
-- parsers.

-- GlobalDecl and GlobalFunction are the hard ones.

externalDecl :: AssemblyParser GlobalDeclaration
externalDecl = do
  consumeToken TDeclare
  -- FIXME: We don't currently attach these to the constructed values.
  -- that sucks.
  paramAttrs <- many paramAttributeP
  t <- typeP
  name <- globalIdentifierP
  funcParts <- optionMaybe (try funcPartsP)
  case funcParts of
    Nothing -> return $ ExternalValueDecl t name
    Just (args, fattrs) -> return $ mkExternalFuncDecl t name args fattrs
  where funcPartsP = (,) <$> p1 <*> p2
        p1 = betweenTokens [TLParen] [TRParen] funcTypeArgList
        p2 = many functionAttributeP

-- FIXME: Ignoring parameter attributes here for now...
funcTypeArgList :: AssemblyParser ([Type], Bool)
funcTypeArgList = do
  ts <- sepBy1 tArgP (consumeToken TComma)
  isVa <- option False vaTailP
  return (ts, isVa)
  where tArgP = typeP <* many paramAttributeP
        vaTailP = True <$ consumeTokens [TComma, TDotDotDot]

globalAlias :: AssemblyParser GlobalDeclaration
globalAlias = mkGlobalAlias <$> i <*> lt <*> vis <*> ty <*> c
  where i = globalIdentifierP <* consumeTokens [TAssign, TAlias]
        lt = linkageTypeP
        vis = visibilityStyleP
        ty = typeP
        c = partialConstantP

typeDeclaration :: AssemblyParser GlobalDeclaration
typeDeclaration = NamedType <$> localIdentifierP <*> (syntaxP *> typeP)
  where syntaxP = consumeTokens [TAssign, TType]

moduleLevelAssembly :: AssemblyParser GlobalDeclaration
moduleLevelAssembly = (ModuleAssembly . Assembly) <$> (syntaxP *> parseString)
  where syntaxP = consumeTokens [TModule, TAsm]

targetTriple :: AssemblyParser TargetTriple
targetTriple = mkTriple <$> (syntaxP *> parseString)
  where syntaxP = consumeTokens [TTarget, TTriple, TAssign]

dataLayout :: AssemblyParser DataLayout
dataLayout = mkDataLayout <$> (syntaxP *> parseString)
  where syntaxP = consumeTokens [TTarget, TDataLayout, TAssign]


-- These are the top-level metadata bits that just list the metadata
-- that should be attached to global entities.  There seem to just be
-- two or so per module.
unnamedMetadataP :: AssemblyParser GlobalDeclaration
unnamedMetadataP = mkMDNode <$> metadataIdentifierP <*> (infx *> metadataNodeContentsP)
  where infx = consumeTokens [TAssign, TMetadataT]

-- These are the very common metadata nodes referenced from everywhere
-- else.
namedMetadataP :: AssemblyParser GlobalDeclaration
namedMetadataP = mkNamedMetadata <$> metadataIdentifierP <*> (infx *> p <* postfx)
  where infx = consumeTokens [TAssign, TBang, TLCurl]
        p = sepBy1 metadataIdentifierP (consumeToken TComma)
        postfx = consumeToken TRCurl


parser = undefined

-- satisfy f