module Data.LLVM.Private.Parser.Constants ( constantP
                                          , partialConstantP
                                          , metadataNodeContentsP
                                          ) where

import Control.Applicative hiding ( (<|>) )
import Text.Parsec

import Data.LLVM.Private.Lexer
import Data.LLVM.Private.PlaceholderTypes
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
metadataNodeContentsP :: AssemblyParser [Maybe Constant]
metadataNodeContentsP = prefx *> p <* postfx
  where prefx = consumeToken TBang *> consumeToken TLCurl
        p = sepBy metadataConstantLit commaP
        postfx = consumeToken TRCurl

-- | To parse a full constant, we need to apply a partial constant to
-- its type.
constantP :: AssemblyParser Constant
constantP = do
  t <- typeP
  c <- partialConstantP
  return $! c t

-- | Here, simpleConstant will either succeed by consuming 1 token or
-- fail consuming none.  We can have it first in the alternation
-- without a try.
untypedConstant :: AssemblyParser ConstantT
untypedConstant = simpleConstant <|> complexConstant

-- | A partial constant is either an identifier or one of the actual
-- constant types.
partialConstantP :: AssemblyParser PartialConstant
partialConstantP = (valueRef <$> identifierP) <|> (ConstValue <$> untypedConstant)

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

-- | Parse all of the non-single-token constants.  This is actually
-- a reasonably efficient predictive parser.
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
  return $! aConstant
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
            TSelect -> Just selectConstantP
            TIcmp -> Just (cmpConstantP icmpConditionP ICmpInst TIcmp)
            TFcmp -> Just (cmpConstantP fcmpConditionP FCmpInst TFcmp)
            TExtractElement -> Just extractElementConstantP
            TInsertElement -> Just insertElementConstantP
            TShuffleVector -> Just shuffleVectorConstantP
            TExtractValue -> Just extractValueConstantP
            TInsertValue -> Just insertValueConstantP
            TAdd -> Just $ binaryConstantP TAdd (AddInst [])
            TFadd -> Just $ binaryConstantP TFadd (AddInst [])
            TSub -> Just $ binaryConstantP TSub (SubInst [])
            TFsub -> Just $ binaryConstantP TFsub (SubInst [])
            TMul -> Just $ binaryConstantP TMul (MulInst [])
            TFmul -> Just $ binaryConstantP TFmul (MulInst [])
            TUdiv -> Just $ binaryConstantP TUdiv DivInst
            TSdiv -> Just $ binaryConstantP TSdiv DivInst
            TFdiv -> Just $ binaryConstantP TFdiv DivInst
            TUrem -> Just $ binaryConstantP TUrem RemInst
            TSrem -> Just $ binaryConstantP TSrem RemInst
            TFrem -> Just $ binaryConstantP TFrem RemInst
            TShl -> Just $ binaryConstantP TShl ShlInst
            TLshr -> Just $ binaryConstantP TLshr LshrInst
            TAshr -> Just $ binaryConstantP TAshr AshrInst
            TAnd -> Just $ binaryConstantP TAnd AndInst
            TOr -> Just $ binaryConstantP TOr OrInst
            TXor -> Just $ binaryConstantP TXor XorInst
            _ -> Nothing

-- Below here are the helper parsers for the complex constants.

binaryConstantP :: LexerToken -> (Constant -> Constant -> InstructionT) ->
                   AssemblyParser ConstantT
binaryConstantP t c = ConstantExpr <$> (c <$> c1 <*> c2)
  where c1 = consumeTokens [t, TLParen] *> constantP
        c2 = betweenTokens [TComma] [TRParen] constantP

insertValueConstantP :: AssemblyParser ConstantT
insertValueConstantP =
  ConstantExpr <$> (InsertValueInst <$> val <*> elt <*> idxs)
  where val = consumeTokens [TInsertValue, TLParen] *> constantP
        elt = commaP *> constantP
        idxs = betweenTokens [TComma] [TRParen] idxListParser
        idxListParser = sepBy1 parseInteger commaP

extractValueConstantP :: AssemblyParser ConstantT
extractValueConstantP =
  ConstantExpr <$> (ExtractValueInst <$> val <*> idxs)
  where val = consumeTokens [TExtractValue, TLParen] *> constantP
        idxs = betweenTokens [TComma] [TRParen] idxListParser
        idxListParser = sepBy1 parseInteger commaP

shuffleVectorConstantP :: AssemblyParser ConstantT
shuffleVectorConstantP =
  ConstantExpr <$> (ShuffleVectorInst <$> vec1 <*> vec2 <*> mask)
  where vec1 = consumeTokens [TShuffleVector, TLParen] *> constantP
        vec2 = commaP *> constantP
        mask = betweenTokens [TComma] [TRParen] constantP

insertElementConstantP :: AssemblyParser ConstantT
insertElementConstantP =
  ConstantExpr <$> (InsertElementInst <$> val <*> elt <*> idx)
  where val = consumeTokens [TInsertElement, TLParen] *> constantP
        elt = commaP *> constantP
        idx = betweenTokens [TComma] [TRParen] constantP

extractElementConstantP :: AssemblyParser ConstantT
extractElementConstantP =
  ConstantExpr <$> (ExtractElementInst <$> val <*> idx)
  where val = consumeTokens [TExtractElement, TLParen] *> constantP
        idx = betweenTokens [TComma] [TRParen] constantP

cmpConstantP :: AssemblyParser a -> (a -> Constant -> Constant -> InstructionT) ->
                LexerToken -> AssemblyParser ConstantT
cmpConstantP condP constructor tok =
  ConstantExpr <$> (constructor <$> cond condP tok <*> v1 <*> v2)
  where cond p t = consumeToken t *> p
        v1 = consumeToken TLParen *> constantP
        v2 = betweenTokens [TComma] [TRParen] constantP

selectConstantP :: AssemblyParser ConstantT
selectConstantP = ConstantExpr <$> (SelectInst <$> cond <*> v1 <*> v2)
  where cond = consumeTokens [TSelect, TLParen] *> constantP
        v1 = commaP *> constantP
        v2 = betweenTokens [TComma] [TRParen] constantP

-- | Parse getelementptr [inbounds] ( <constant> , <indices> )
getElementPtrConstantP :: AssemblyParser ConstantT
getElementPtrConstantP = ConstantExpr <$> (GetElementPtrInst <$> ib <*> ptr <*> is)
  where ib = consumeToken TGetElementPtr *> inBoundsP
        ptr = consumeToken TLParen *> constantP
        is = betweenTokens [TComma] [TRParen] $ sepBy constantP commaP

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
        constraints = commaP *> parseString

metadataConstantP :: AssemblyParser ConstantT
metadataConstantP = MDNode <$> metadataNodeContentsP

blockAddressP :: AssemblyParser ConstantT
blockAddressP = BlockAddress <$> (prefx *> globalIdentifierP <* commaP) <*> (localIdentifierP <* postfx)
  where prefx = consumeToken TBlockAddress *> consumeToken TLParen
        postfx = consumeToken TRParen

vectorConstantP :: AssemblyParser ConstantT
vectorConstantP = ConstantVector <$> between l r p
  where p = sepBy constantP commaP
        l = consumeToken TLSquare
        r = consumeToken TRSquare

arrayConstantP :: AssemblyParser ConstantT
arrayConstantP = ConstantArray <$> between l r p
  where p = sepBy constantP commaP
        l = consumeToken TLSquare
        r = consumeToken TRSquare

structConstantP :: AssemblyParser ConstantT
structConstantP = ConstantStruct <$> between l r p
  where p = sepBy constantP commaP
        l = consumeToken TLCurl
        r = consumeToken TRCurl

