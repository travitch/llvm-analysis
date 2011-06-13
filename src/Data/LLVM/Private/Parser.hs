module Data.LLVM.Private.Parser ( parser ) where

import Control.Applicative hiding ( (<|>), many )
import Text.Parsec

import Data.LLVM.Private.AttributeTypes
import Data.LLVM.Private.Lexer
import Data.LLVM.Private.PlaceholderTypes
import Data.LLVM.Private.Parser.Attributes
import Data.LLVM.Private.Parser.Constants
import Data.LLVM.Private.Parser.Instructions
import Data.LLVM.Private.Parser.Primitive
import Data.LLVM.Private.Parser.Types


parser :: AssemblyParser Module
parser = Module <$> dataLayoutP <*> targetTripleP <*> many globalEntityP

-- | Dispatch top-level parses as efficiently as possible.  Several
-- cases can be uniquely identified by their first token, but four
-- cannot.  These four are divided into two groups.  The most likely
-- parse in each case is listed first, since the choice combinator
-- tries its arguments in order.
globalEntityP :: AssemblyParser GlobalDeclaration
globalEntityP = do
  realParser <- lookAhead dispatcher
  gd <- realParser
  return $! gd
  where dispatcher = tokenAs matcher
        matcher x =
          case x of
            TModule -> Just moduleLevelAssemblyP
            TLocalIdent _ -> Just typeDeclarationP
            TDeclare -> Just externalDeclP
            TDefine -> Just functionDefinitionP
            TGlobalIdent _ -> Just $! choice [try globalDeclP, globalAliasP]
            TMetadataName _ -> Just $! choice [try namedMetadataP, unnamedMetadataP]
            _ -> Just (parserFail "Expected a global entity")


-- These are the top-level parsers.  The GlobalEntity parser will
-- need some fanciness with lookahead to dispatch to the correct
-- parsers.

functionDefinitionP :: AssemblyParser GlobalDeclaration
functionDefinitionP = do
  consumeToken TDefine
  lt <- linkageTypeP
  vis <- visibilityStyleP
  cc <- callingConventionP
  pattrs <- many paramAttributeP
  t <- typeP
  name <- globalIdentifierP
  (funcArgs, isva) <- funcArgListP
  fattrs <- many functionAttributeP
  sec <- sectionNameP
  falign <- alignmentP
  gcname <- optionMaybe gcNameP
  body <- functionBodyP
  return $! FunctionDefinition { funcLinkage = lt
                               , funcVisibility = vis
                               , funcCC = cc
                               , funcRetAttrs = pattrs
                               , funcRetType = t
                               , funcName = name
                               , funcParams = funcArgs
                               , funcAttrs = fattrs
                               , funcSection = sec
                               , funcAlign = falign
                               , funcGCName = gcname
                               , funcBody = body
                               , funcIsVararg = isva
                               }

funcArgListP :: AssemblyParser ([FormalParameter], Bool)
funcArgListP = do
  consumeToken TLParen
  namedArgs <- sepEndBy arg commaP
  isVa <- option False $! True <$ consumeToken TDotDotDot
  consumeToken TRParen
  return $! (namedArgs, isVa)
  where arg = FormalParameter <$> typeP <*> many paramAttributeP <*> localIdentifierP

globalAliasP :: AssemblyParser GlobalDeclaration
globalAliasP = do
  name <- globalIdentifierP
  consumeTokens [TAssign, TAlias]
  lt <- linkageTypeP
  vis <- visibilityStyleP
  t <- typeP
  target <- partialConstantP
  return $! GlobalAlias name lt vis t (target t)

globalDeclP :: AssemblyParser GlobalDeclaration
globalDeclP = do
  name <- globalIdentifierP
  consumeToken TAssign
  addrSpace <- addrSpaceP
  lt <- linkageTypeP
  vis <- visibilityStyleP
  ga <- globalAnnotationP
  t <- typeP
  -- Initializers are optional.  We only want to take the initializer if it is NOT
  -- followed by a TAssign, though -- the next constant might actually be the name
  -- of the NEXT global declaration.  This is one place where the grammar is LR(2)
  initializer <- optionMaybe globalInitP
  -- Now we have optional sections and alignments; this is also easier
  -- here than in an LR parser since we don't have to worry about the
  -- reduce/reduce conflicts.
  section <- optionMaybe $! try $! consumeTokens [TComma, TSection] *> parseString
  align <- option 0 (commaP *> alignmentP)
  let i = case initializer of
        Just i' -> Just $ i' t
        Nothing -> Nothing
      pType = TypePointer t
  return $! GlobalDeclaration name addrSpace lt vis ga pType i align section
  where globalInitP = try (partialConstantP <* notFollowedBy (consumeToken TAssign))

externalDeclP :: AssemblyParser GlobalDeclaration
externalDeclP = do
  consumeToken TDeclare
  -- FIXME: We don't currently attach these to the constructed values.
  -- that sucks.
  paramAttrs <- many paramAttributeP
  t <- typeP
  name <- globalIdentifierP
  funcParts <- optionMaybe $! try funcPartsP
  case funcParts of
    Nothing -> return $! ExternalValueDecl t name
    Just (args, fattrs) -> return $! mk t name args fattrs
  where funcPartsP = (,) <$> p1 <*> p2
        p1 = betweenTokens [TLParen] [TRParen] funcTypeArgListP
        p2 = many functionAttributeP
        mk r i (as, va) atts =
          let t = case r of
                TypeFunction _ _ _ -> r
                _ -> TypeFunction r as va
          in ExternalFuncDecl t i atts

-- FIXME: Ignoring parameter attributes here for now...
funcTypeArgListP :: AssemblyParser ([Type], Bool)
funcTypeArgListP = do
  ts <- sepEndBy tArgP commaP
  isVa <- option False $ True <$ consumeToken TDotDotDot
  return $! (ts, isVa)
  where tArgP = typeP <* many paramAttributeP

typeDeclarationP :: AssemblyParser GlobalDeclaration
typeDeclarationP = NamedType <$> localIdentifierP <*> (syntaxP *> typeP)
  where syntaxP = consumeTokens [TAssign, TType]

moduleLevelAssemblyP :: AssemblyParser GlobalDeclaration
moduleLevelAssemblyP = (ModuleAssembly . Assembly) <$> (syntaxP *> parseString)
  where syntaxP = consumeTokens [TModule, TAsm]

targetTripleP :: AssemblyParser TargetTriple
targetTripleP = TargetTriple <$> (syntaxP *> parseString)
  where syntaxP = consumeTokens [TTarget, TTriple, TAssign]

dataLayoutP :: AssemblyParser DataLayout
dataLayoutP = mk <$> (syntaxP *> parseString)
  where syntaxP = consumeTokens [TTarget, TDataLayout, TAssign]
        mk s = defaultDataLayout


-- These are the top-level metadata bits that just list the metadata
-- that should be attached to global entities.  There seem to just be
-- two or so per module.
unnamedMetadataP :: AssemblyParser GlobalDeclaration
unnamedMetadataP = UnnamedMetadata <$> metadataIdentifierP <*> (infx *> metadataNodeContentsP)
  where infx = consumeTokens [TAssign, TMetadataT]

-- These are the very common metadata nodes referenced from everywhere
-- else.
namedMetadataP :: AssemblyParser GlobalDeclaration
namedMetadataP = mk <$> metadataIdentifierP <*> (infx *> p <* postfx)
  where infx = consumeTokens [TAssign, TBang, TLCurl]
        p = sepBy1 metadataIdentifierP commaP
        postfx = consumeToken TRCurl
        mk name names = let vals = map ValueRef names
                        in NamedMetadata name vals

basicBlockP :: AssemblyParser BasicBlock
basicBlockP = mk <$> labelP <*> many instructionP
  where mk lbl = BasicBlock ((Just . makeLocalIdentifier) lbl)

functionBodyP :: AssemblyParser [BasicBlock]
functionBodyP = do
  consumeToken TLCurl
  realParser <- lookAhead dispatcher
  bbs <- realParser
  consumeToken TRCurl
  return $! bbs
  where dispatcher = tokenAs matcher
        matcher x =
          case x of
            TLabel _ -> Just (many1 basicBlockP)
            -- ^ Parse a list of basic blocks since we have at least
            -- one labeled block here.
            _ -> Just ((:[]) <$> (BasicBlock <$> (return Nothing) <*> many1 instructionP))
            -- ^ This alternative form is invoked when a function has
            -- only a single basic block with no label.  Create a list
            -- of instructions with no label and then just wrap it
            -- into a singleton list.

