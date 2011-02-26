module Data.LLVM.Private.Parser (parser) where

import Control.Applicative hiding ((<|>))
import Text.Parsec

import Data.LLVM.Private.AttributeTypes
import Data.LLVM.Private.Lexer
import Data.LLVM.Private.PlaceholderTypes
import Data.LLVM.Private.PlaceholderBuilders
import Data.LLVM.Private.Parser.Attributes
import Data.LLVM.Private.Parser.Constants
import Data.LLVM.Private.Parser.Primitive
import Data.LLVM.Private.Parser.Types



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