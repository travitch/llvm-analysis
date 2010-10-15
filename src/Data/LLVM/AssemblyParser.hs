{-# LANGUAGE OverloadedStrings #-}
-- module Data.LLVM.AssemblyParser () where

import Data.Char
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Map (Map)
import Data.Set (Set)
import Prelude hiding (takeWhile)
import Control.Applicative hiding (many)
import Data.Attoparsec
import Data.Attoparsec.Combinator
import Data.Attoparsec.Char8 (char8, isHorizontalSpace, isEndOfLine, isDigit_w8, endOfLine, decimal)
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import Data.Word (Word8)

import System.Environment ( getArgs )

data Module = Module Int

c = fromIntegral . ord
pChar ch = word8 (c ch)
pCharT ch = token (pChar ch)
sToken s = token (string s)
parseTokens tokens = sequence_ $ map token tokens
between lp p rp = lp *> p <* rp

data Identifier = LocalIdentifier ByteString
                | GlobalIdentifier ByteString
                  deriving (Show)

-- TODO: constants
parseIdentifier :: Parser Identifier
parseIdentifier = makeIdentifier <$> parseSigil <*> choice idents
  where idents = [parseIdentifierString, parseIdentifierName, parseUnnamedValue]
        parseSigil = satisfy (\w -> w == c '@' || w == c '%')
        parseQuote = skip (\w -> w == c '"')
        startChar = inClass "a-zA-Z$._"
        identChar = inClass "a-zA-Z0-9$._"
        parseIdentifierString = parseQuote *> takeWhile (\w -> w /= c '"') <* parseQuote
        parseIdentifierName = BS.cons <$> satisfy startChar <*> takeWhile identChar
        parseUnnamedValue = takeWhile isDigit_w8
        makeIdentifier sigil body = if sigil == (c '@')
                                    then GlobalIdentifier body
                                    else LocalIdentifier body

data LinkageType = LTPrivate
                 | LTLinkerPrivate
                 | LTLinkerPrivateWeak
                 | LTLinkerPrivateWeakDefAuto
                 | LTInternal
                 | LTAvailableExternally
                 | LTLinkOnce
                 | LTWeak
                 | LTCommon
                 | LTAppending
                 | LTExternWeak
                 | LTLinkOnceODR
                 | LTWeakODR
                 | LTExtern -- Default
                 | LTDLLImport
                 | LTDLLExport
                   deriving (Show)

mappingToParser (str, constr) = constr <$ string str

-- The default linkage type is LTExtern if there is no other specified.
-- The internal function handles parsing an explicit linkage type.
parseLinkageType :: Parser LinkageType
parseLinkageType = option LTExtern parseLinkageType'
  where parseLinkageType' :: Parser LinkageType
        parseLinkageType' = (choice $ map mappingToParser mapping) >>= return
        mapping = [ ("private", LTPrivate)
                  , ("linker_private", LTLinkerPrivate)
                  , ("linker_private_weak", LTLinkerPrivateWeak)
                  , ("linker_private_weak_def_auto", LTLinkerPrivateWeakDefAuto)
                  , ("internal", LTInternal)
                  , ("available_externally", LTAvailableExternally)
                  , ("linkonce", LTLinkOnce)
                  , ("weak", LTWeak)
                  , ("common", LTCommon)
                  , ("appending", LTAppending)
                  , ("extern_weak", LTExternWeak)
                  , ("linkonce_odr", LTLinkOnceODR)
                  , ("weak_odr", LTWeakODR)
                  , ("dllimport", LTDLLImport)
                  , ("dllexport", LTDLLExport)]

data CallingConvention = CCC
                       | CCFastCC
                       | CCColdCC
                       | CCGHC
                       | CCN Int
                       deriving (Show)

parseCallingConvention :: Parser CallingConvention
parseCallingConvention = option CCC parseCallingConvention'
  where parseCallingConvention' = choice convParsers
        convParsers = parseNumberedCC : (map mappingToParser mapping)
        mapping = [ ("ccc", CCC)
                  , ("fastcc", CCFastCC)
                  , ("coldcc", CCColdCC)
                  , ("cc 10", CCGHC)]
        parseNumberedCC = CCN <$> (string "cc " *> decimal)

data VisibilityStyle = VisibilityDefault
                     | VisibilityHidden
                     | VisibilityProtected
                       deriving (Show)

parseVisibilityStyle :: Parser VisibilityStyle
parseVisibilityStyle = option VisibilityDefault parseVisibilityStyle'
  where parseVisibilityStyle' = choice visParsers
        visParsers = map mappingToParser mapping
        mapping = [ ("default", VisibilityDefault)
                  , ("hidden", VisibilityHidden)
                  , ("protected", VisibilityProtected)]

data ParamAttr = PAZeroExt
               | PASignExt
               | PAInReg
               | PAByVal
               | PASRet
               | PANoAlias
               | PANoCapture
               | PANest
               deriving (Show)

-- These are optional for each parameter, and any number can be
-- specified (space separated)
parseParameterAttributes :: Parser [ParamAttr]
parseParameterAttributes = sepBy parseParameterAttributes' skipWhitespace
  where parseParameterAttributes' = choice attrParsers
        attrParsers = map mappingToParser mapping
        mapping = [ ("zeroext", PAZeroExt)
                  , ("signext", PASignExt)
                  , ("inreg", PAInReg)
                  , ("byval", PAByVal)
                  , ("sret", PASRet)
                  , ("noalias", PANoAlias)
                  , ("nocapture", PANoCapture)
                  , ("nest", PANest)]

data FunctionAttr = FAAlignStack Int
                  | FAAlwaysInline
                  | FAInlineHint
                  | FANaked
                  | FANoImplicitFloat
                  | FANoInline
                  | FANoRedZone
                  | FANoReturn
                  | FANoUnwind
                  | FAOptSize
                  | FAReadNone
                  | FAReadOnly
                  | FASSP
                  | FASSPReq
                    deriving (Show)

-- Again, zero or more separated by spaces
parseFunctionAttributes :: Parser [FunctionAttr]
parseFunctionAttributes = sepBy parseFunctionAttributes' skipWhitespace
  where parseFunctionAttributes' = choice attrParsers
        attrParsers = parseAlignStack : (map mappingToParser mapping)
        mapping = [ ("alwaysinline", FAAlwaysInline)
                  , ("inlinehint", FAInlineHint)
                  , ("naked", FANaked)
                  , ("noimplicitfloat", FANoImplicitFloat)
                  , ("noinline", FANoInline)
                  , ("noredzone", FANoRedZone)
                  , ("noreturn", FANoReturn)
                  , ("nounwind", FANoUnwind)
                  , ("optsize", FAOptSize)
                  , ("readnone", FAReadNone)
                  , ("readonly", FAReadOnly)
                  , ("ssp", FASSP)
                  , ("sspreq", FASSPReq)]
        parseAlignStack = FAAlignStack <$> (string "alignstack(" *> decimal <* word8 (c ')'))

parseModuleAsm :: Parser ByteString
parseModuleAsm = parsePrefix *> takeWhile (\w -> w /= c '"') <* word8 (c '"')
  where parsePrefix = parseTokens [string "module", string "asm", string "\""]

data Endian = EBig
            | ELittle
              deriving (Show)

-- Track the ABI alignment and preferred alignment
data AlignSpec = AlignSpec Int Int
                 deriving (Show)

data DataLayout = DataLayout { endianness :: Endian
                             , pointerAlign :: (Int, AlignSpec)
                             , intAlign :: Map Int AlignSpec
                             , vectorAlign :: Map Int AlignSpec
                             , floatAlign :: Map Int AlignSpec
                             , aggregateAlign :: Map Int AlignSpec
                             , stackAlign :: Map Int AlignSpec
                             , nativeWidths :: Set Int
                             }
                  deriving (Show)

-- Defaults specified by LLVM.  I think there can only be one pointer
-- align specification, though it isn't explicitly stated
defaultDataLayout = DataLayout { endianness = EBig
                               , pointerAlign = (64, AlignSpec 64 64)
                               , intAlign = Map.fromList [ (1, AlignSpec 8 8)
                                                         , (8, AlignSpec 8 8)
                                                         , (16, AlignSpec 16 16)
                                                         , (32, AlignSpec 32 32)
                                                         , (64, AlignSpec 32 64)
                                                         ]
                               , vectorAlign = Map.fromList [ (64, AlignSpec 64 64)
                                                            , (128, AlignSpec 128 128)
                                                            ]
                               , floatAlign = Map.fromList [ (32, AlignSpec 32 32)
                                                           , (64, AlignSpec 64 64)
                                                           ]
                               , aggregateAlign = Map.fromList [ (0, AlignSpec 0 1) ]
                               , stackAlign = Map.fromList [ (0, AlignSpec 64 64) ]
                               , nativeWidths = Set.empty
                               }

-- target datalayout = "<spec>"
parseDataLayout :: Parser DataLayout
parseDataLayout = parsePrefix *> parseLayoutSpec <* word8 (c '"') <* parseLineEnd
  where parsePrefix = parseTokens [string "target", string "datalayout", string "=", string "\""]

-- The datalayout spec.  Covers endianness, pointer, int, float,
-- vector, aggregate, and stack size/alignment
parseLayoutSpec :: Parser DataLayout
parseLayoutSpec = parseSpecifiers defaultDataLayout
  where parseSpecifier lyt = choice (parseNative lyt : parseEndian lyt : map (parseAlign lyt) alignTypes)
        alignTypes = [ ("p:", \lyt bits spec -> lyt { pointerAlign = (bits, spec) } )
                     , ("i", \lyt bits spec -> lyt { intAlign = Map.insert bits spec (intAlign lyt) } )
                     , ("v", \lyt bits spec -> lyt { vectorAlign = Map.insert bits spec (vectorAlign lyt) } )
                     , ("f", \lyt bits spec -> lyt { floatAlign = Map.insert bits spec (floatAlign lyt) } )
                     , ("a", \lyt bits spec -> lyt { aggregateAlign = Map.insert bits spec (aggregateAlign lyt) } )
                     , ("s", \lyt bits spec -> lyt { stackAlign = Map.insert bits spec (aggregateAlign lyt) } )
                     ]
        parseEndian lyt = modEndian lyt <$> satisfy (\w -> w == c 'e' || w == c 'E')
        modEndian lyt w = lyt { endianness = if w == c 'e' then ELittle else EBig }
        parseNative lyt = modNative lyt <$> (word8 (c 'n') *> sepBy1 decimal (word8 (c ':')))
        modNative lyt digits = lyt { nativeWidths = Set.union (nativeWidths lyt) (Set.fromList digits) }
        parseAlign lyt (st, modFunc) = do
          string st
          size <- decimal
          word8 (c ':')
          abiAlign <- decimal
          -- The preffered alignment is optional and defaults to
          -- abiAlign if not specified; the : is omitted if the
          -- prefalign is.
          prefAlign <- option abiAlign (word8 (c ':') *> decimal)
          return $ modFunc lyt size $ AlignSpec abiAlign prefAlign
        parseSpecifiers lyt = do
          lyt' <- parseSpecifier lyt
          more <- option False (satisfyWith (\w -> w == c '-') id)
          if more
            then parseSpecifiers lyt'
            else return lyt'

data Type = TypeInteger Int -- bits
          | TypeFloat
          | TypeDouble
          | TypeFP128
          | TypeX86FP80
          | TypePPCFP128
          | TypeX86MMX
          | TypeVoid
          | TypeLabel
          | TypeMetadata
          | TypeArray Int Type
          | TypeVector Int Type
          | TypeFunction Type [Type] Bool -- Return type, arg types, vararg
          | TypeOpaque
          | TypePointer Type
          | TypeStruct [Type]
          | TypePackedStruct [Type]
          | TypeUpref Int
          deriving (Show)

-- This is an internal-only data type used to track fragmentary parts
-- of type decls; see the comments below but this is needed due to
-- the left recursion in the type grammar
data TypeFragment = FuncFragment [Type] Bool
                  | PointerFragment Int

-- The type grammar is left recursive for pointer types; use the
-- normal trick of parsing the recursive part and just slurping up all
-- pointer chars with a many combinator
parseType :: Parser Type
parseType = applyFragmentTypes <$> parseOneType <*> parseTypeModTail
  where parseOneType = choice (complexParsers ++ (map mappingToParser mapping))
        complexParsers = [ parseIntType
                         , parseArrayType
                         , parseVectorType
                         , parseStructType
                         , parsePackedStructType
                         , parseUprefType
                         ]
        mapping = [ ("float", TypeFloat)
                  , ("double", TypeDouble)
                  , ("fp128", TypeFP128)
                  , ("x86_fp80", TypeX86FP80)
                  , ("ppc_fp128", TypePPCFP128)
                  , ("x86mmx", TypeX86MMX)
                  , ("void", TypeVoid)
                  , ("label", TypeLabel)
                  , ("metadata", TypeMetadata)
                  , ("opaque", TypeOpaque)
                  ]
        -- Trivial helpers to parse the left and right parts of a
        -- vector or array type
        pVArrayLeft ch = pCharT ch *> decimal
        pVArrayRight ch = pCharT 'x' *> parseType <* pCharT ch

        -- Simple types
        parseIntType = TypeInteger <$> (pChar 'i' *> decimal)
        parseArrayType = TypeArray <$> pVArrayLeft '[' <*> pVArrayRight ']'
        parseVectorType = TypeVector <$> pVArrayLeft '<' <*> pVArrayRight '>'
        parseStructType = TypeStruct <$> (pCharT '{' *> parseTypeList <* pCharT '}')
        parsePackedStructType =
          TypePackedStruct <$> (parseTokens [pChar '<', pChar '{'] *> parseTypeList <* parseTokens [pChar '}', pChar '>'])
        parseUprefType = TypeUpref <$> (pChar '\\' *> decimal)

        -- Applies type specifier fragments (for pointers and function
        -- types).  This is required because the type grammar is
        -- left-recursive and we can't directly use left recursion
        -- with parser combinator parsers.  This function will wrap
        -- function or pointer types around a base type as required
        applyFragmentTypes :: Type -> [TypeFragment] -> Type
        applyFragmentTypes baseType [] = baseType
        applyFragmentTypes baseType ((PointerFragment 0) : rest) =
          applyFragmentTypes baseType rest
        applyFragmentTypes baseType ((PointerFragment cnt) : rest) =
          applyFragmentTypes (TypePointer baseType) ((PointerFragment (cnt-1)) : rest)
        applyFragmentTypes baseType ((FuncFragment types vararg) : rest) =
          applyFragmentTypes (TypeFunction baseType types vararg) rest

        -- Parses a comma separated list of types
        parseTypeList = sepBy parseType (pCharT ',')
        -- This is a type list followed by an optional comma and
        -- ... for the vararg parameter
        parseFuncTypeList :: Parser TypeFragment
        parseFuncTypeList = FuncFragment <$> parseTypeList <*> option False (const True <$> (pCharT ',' *> sToken "..."))
        -- This is a func type list surrounded by parens
        parseFuncFragment = pCharT '(' *> parseFuncTypeList <* pCharT ')'
        parsePointerFragment :: Parser TypeFragment
        parsePointerFragment = (PointerFragment . length) <$> (many1 $ pCharT '*')
        -- This is the main part of the fragment parser; it uses
        -- alternation and both of the parsers need to be able to
        -- fail, otherwise you get infinite recursion due to the many
        parseTypeModTail :: Parser [TypeFragment]
        parseTypeModTail = many (parseFuncFragment <|> parsePointerFragment)

parseGCName :: Parser ByteString
parseGCName = string "gc \"" *> takeWhile (\w -> w /= c '"') <* pChar '"'

-- FIXME: Do this after type parsers are defined
-- data NamedType
-- parseNamedType :: Parser NamedType

skipWhitespace = skipWhile isHorizontalSpace

parseLineEnd :: Parser ()
parseLineEnd = skipWhitespace *> option () parseComment *> skipWhile isEndOfLine
  where parseComment = pChar ';' *> skipWhile notEOL
        notEOL c = not $ isEndOfLine c

token :: Parser a -> Parser a
token p = skipWhitespace *> p <* skipWhitespace

-- testParser = many (token parseIdentifier)
--testParser :: Parser [LinkageType]
-- testParser = token parseLinkageType
-- testParser = ((token parseCallingConvention) <* parseLineEnd)
testParser = parseType

main = do
  [ filename ] <- getArgs

  bs <- BS.readFile filename

  let res = parse testParser bs
      res' = feed res ""
  case res' of
    Done remaining result -> putStrLn (show result) >> putStrLn ("Remaining input: " ++ show remaining)
    Fail bs contexts msg -> putStrLn ("Failed: " ++ msg)
    _ -> putStrLn ("Partial: " ++ show res)