module Data.LLVM.Private.Parser.Constants ( constantP ) where

import Control.Applicative hiding ((<|>))
import Text.Parsec

import Data.LLVM.Private.Lexer
import Data.LLVM.Private.PlaceholderTypes
import Data.LLVM.Private.PlaceholderBuilders
import Data.LLVM.Private.Parser.Attributes
import Data.LLVM.Private.Parser.Primitive
import Data.LLVM.Private.Parser.Types

-- | The first choice consumes at most one token (Failing otherwise).
-- Don't need a try for it.
metadataConstantLit :: AssemblyParser (Maybe Constant)
metadataConstantLit = (Nothing <$ tokenAs matcher) <|> (Just <$> constantP)
  where matcher x =
          case x of
            TNullLit -> Just ()
            _ -> Nothing

-- | Parse a basic metadata node:  !{ <comma-separated-metadata-constants> }
-- This is re-used in some other productions
metadataNodeContents :: AssemblyParser [Maybe Constant]
metadataNodeContents = prefx *> p <* postfx
  where prefx = consumeToken TBang *> consumeToken TLCurl
        p = sepBy metadataConstantLit (consumeToken TComma)
        postfx = consumeToken TRCurl

unnamedMetadataP :: AssemblyParser GlobalDeclaration
unnamedMetadataP = mkMDNode <$> metadataIdentifierP <*> (infx *> metadataNodeContents)
  where infx = consumeTokens [TAssign, TMetadataT]

namedMetadataP :: AssemblyParser GlobalDeclaration
namedMetadataP = mkNamedMetadata <$> metadataIdentifierP <*> (infx *> p <* postfx)
  where infx = consumeTokens [TAssign, TBang, TLCurl]
        p = sepBy1 metadataIdentifierP (consumeToken TComma)
        postfx = consumeToken TRCurl

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
partialConstantP = (valueRef <$> identifier) <|> (ConstValue <$> untypedConstant)

-- | Match any single-token constants.  These are split out just for more efficient
-- handling
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

complexConstant :: AssemblyParser ConstantT
complexConstant = do
  -- lookAhead runs its parser without consuming the argument.  We
  -- don't want to consume the argument since each of the real parsers
  -- will consume it (it is better for documentation purposes for each
  -- sub-parser to be complete and not have to ignore its first
  -- token).  This is uglier than the choice combinator, but we don't
  -- want to have to ever try a long sequence of alternatives to find
  -- the right one.
  --
  -- This is essentially predictive parsing.
  realParser <- lookAhead dispatcher
  aConstant <- realParser
  return aConstant
  where dispatcher = tokenAs matcher
        matcher x =
          case x of
            TLCurl -> Just structConstantP
            TLSquare -> Just arrayConstantP
            TLAngle -> Just vectorConstantP
            TBlockAddress -> Just blockAddressP
            TBang -> Just metadataConstantP
            TAsm -> Just inlineAsmP
            TTrunc -> Just (conversionConstantP TruncInst TTrunc)
            TZext -> Just (conversionConstantP ZExtInst TZext)
            TSext -> Just (conversionConstantP SExtInst TSext)
            TFpTrunc -> Just (conversionConstantP FPTruncInst TFpTrunc)
            TFpExt -> Just (conversionConstantP FPExtInst TFpExt)
            TFpToUI -> Just (conversionConstantP FPToUIInst TFpToUI)
            TFpToSI -> Just (conversionConstantP FPToSIInst TFpToSI)
            TUIToFp -> Just (conversionConstantP UIToFPInst TUIToFp)
            TSIToFp -> Just (conversionConstantP SIToFPInst TSIToFp)
            TPtrToInt -> Just (conversionConstantP PtrToIntInst TPtrToInt)
            TIntToPtr -> Just (conversionConstantP IntToPtrInst TIntToPtr)
            TBitCast -> Just (conversionConstantP BitcastInst TBitCast)
            TGetElementPtr -> Just getElementPtrConstantP

-- | Parse getelementptr [inbounds] ( <constant> , <indices> )
getElementPtrConstantP :: AssemblyParser ConstantT
getElementPtrConstantP = ConstantExpr <$> (GetElementPtrInst <$> ib <*> ptr <*> is)
  where ib = consumeToken TGetElementPtr *> inBoundsP
        ptr = consumeToken TLParen *> constantP
        is = betweenTokens [TComma] [TRParen] $ sepBy constantP (consumeToken TComma)

-- | A general constructor for all of the constant conversion
-- operations: <instruction> ( <constant> to <type> )
conversionConstantP :: (Constant -> Type -> InstructionT) -> LexerToken ->
                       AssemblyParser ConstantT
conversionConstantP constructor tk =
  ConstantExpr <$> (constructor <$> c <*> t)
  where c = consumeTokens [tk, TLParen] *> constantP
        t = consumeToken TTo *> typeP <* consumeToken TRParen

inlineAsmP :: AssemblyParser ConstantT
inlineAsmP = InlineAsm <$> asm <*> constraints
  where asm = consumeToken TAsm *> parseString
        constraints = consumeToken TComma *> parseString

metadataConstantP :: AssemblyParser ConstantT
metadataConstantP = MDNode <$> metadataNodeContents

blockAddressP :: AssemblyParser ConstantT
blockAddressP = BlockAddress <$> (prefx *> globalIdentifierP <* infx) <*> (localIdentifierP <* postfx)
  where prefx = consumeToken TBlockAddress *> consumeToken TLParen
        infx = consumeToken TComma
        postfx = consumeToken TRParen

vectorConstantP :: AssemblyParser ConstantT
vectorConstantP = ConstantVector <$> between l r p
  where p = sepBy constantP (consumeToken TComma)
        l = consumeToken TLSquare
        r = consumeToken TRSquare

arrayConstantP :: AssemblyParser ConstantT
arrayConstantP = ConstantArray <$> between l r p
  where p = sepBy constantP (consumeToken TComma)
        l = consumeToken TLSquare
        r = consumeToken TRSquare

structConstantP :: AssemblyParser ConstantT
structConstantP = ConstantStruct <$> between l r p
  where p = sepBy constantP (consumeToken TComma)
        l = consumeToken TLCurl
        r = consumeToken TRCurl



-- | Here, simpleConstant will either succeed by consuming 1 token or
-- fail consuming none.  We can have it first in the alternation
-- without a try.
untypedConstant :: AssemblyParser ConstantT
untypedConstant = simpleConstant <|> complexConstant
