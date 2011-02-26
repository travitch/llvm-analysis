{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances #-}
module Data.LLVM.Private.Parser (parser) where

import Control.Applicative hiding ((<|>))
import Data.Monoid
import Data.Text (Text)
import Text.Parsec

import Data.LLVM.Private.AttributeTypes
import Data.LLVM.Private.Lexer
import Data.LLVM.Private.PlaceholderTypes
import Data.LLVM.Private.PlaceholderBuilders
import Data.LLVM.Private.Parser.Primitive


paramAttribute :: AssemblyParser ParamAttribute
paramAttribute = tokenAs matcher
  where matcher x =
          case x of
            TPAZeroExt -> Just PAZeroExt
            TPASignExt -> Just PASignExt
            TPAInReg -> Just PAInReg
            TPAByVal -> Just PAByVal
            TPASRet -> Just PASRet
            TPANoAlias -> Just PANoAlias
            TPANoCapture -> Just PANoCapture
            TPANest -> Just PANest
            _ -> Nothing

functionAttribute :: AssemblyParser FunctionAttribute
functionAttribute = tokenAs matcher
  where matcher x =
          case x of
            TFAAlignStack a -> Just $ FAAlignStack a
            TFAAlwaysInline -> Just FAAlwaysInline
            TFAHotPatch -> Just FAHotPatch
            TFAInlineHint -> Just FAInlineHint
            TFANaked -> Just FANaked
            TFANoImplicitFloat -> Just FANoImplicitFloat
            TFANoInline -> Just FANoInline
            TFANoRedZone -> Just FANoRedZone
            TFANoReturn -> Just FANoReturn
            TFANoUnwind -> Just FANoUnwind
            TFAOptSize -> Just FAOptSize
            TFAReadNone -> Just FAReadNone
            TFAReadOnly -> Just FAReadOnly
            TFASSP -> Just FASSP
            TFASSPReq -> Just FASSPReq
            _ -> Nothing

visibilityStyle :: AssemblyParser VisibilityStyle
visibilityStyle = tokenAs matcher
  where matcher x =
          case x of
            TVisDefault -> Just VisibilityDefault
            TVisHidden -> Just VisibilityHidden
            TVisProtected -> Just VisibilityProtected
            _ -> Just VisibilityDefault

linkageType :: AssemblyParser LinkageType
linkageType = tokenAs matcher
  where matcher x =
          case x of
            TPrivate -> Just LTPrivate
            TLinkerPrivate -> Just LTLinkerPrivate
            TLinkerPrivateWeak -> Just LTLinkerPrivateWeak
            TLinkerPrivateWeakDefAuto -> Just LTLinkerPrivateWeakDefAuto
            TInternal -> Just LTInternal
            TAvailableExternally -> Just LTAvailableExternally
            TLinkOnce -> Just LTLinkOnce
            TWeak -> Just LTWeak
            TCommon -> Just LTCommon
            TAppending -> Just LTAppending
            TExternWeak -> Just LTExternWeak
            TLinkOnceODR -> Just LTLinkOnceODR
            TWeakODR -> Just LTWeakODR
            TDLLImport -> Just LTDLLImport
            TDLLExport -> Just LTDLLExport
            _ -> Just LTExtern


callingConvention :: AssemblyParser CallingConvention
callingConvention = tokenAs matcher
  where matcher x =
          case x of
            TCCN n -> Just (CCN n)
            TCCCCC -> Just CCC
            TCCFastCC -> Just CCFastCC
            TCCColdCC -> Just CCColdCC
            TCCGHC -> Just CCGHC
            _ -> Just CCC

stringParser:: AssemblyParser Text
stringParser = tokenAs stringMatcher

gcName :: AssemblyParser GCName
gcName = consumeToken TGC >> (GCName <$> stringParser)

sectionName :: AssemblyParser (Maybe Text)
sectionName = option Nothing p
  where p = Just <$> stringParser

addrSpace :: AssemblyParser Int
addrSpace = option 0 (tokenAs matcher)
  where matcher x =
          case x of
            TAddrspace n -> Just n
            _ -> Nothing

globalAnnotation :: AssemblyParser GlobalAnnotation
globalAnnotation = tokenAs matcher
  where matcher x =
          case x of
            TConstant -> Just GAConstant
            TGlobal -> Just GAGlobal
            _ -> Nothing

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

globalIdentifier :: AssemblyParser Identifier
globalIdentifier = tokenAs matcher
  where matcher x =
          case x of
            TGlobalIdent i -> Just (makeGlobalIdentifier i)
            _ -> Nothing

localIdentifierP :: AssemblyParser Identifier
localIdentifierP = tokenAs matcher
  where matcher x =
          case x of
            TLocalIdent i -> Just (makeLocalIdentifier i)
            _ -> Nothing

metadataIdentifier :: AssemblyParser Identifier
metadataIdentifier = tokenAs matcher
  where matcher x =
          case x of
            TMetadataName i -> Just (makeMetaIdentifier i)
            _ -> Nothing

-- | Combined form which can match any identifier with just one
-- pattern match (no choice combinator required)
identifier :: AssemblyParser Identifier
identifier = tokenAs matcher
  where matcher x =
          case x of
            TGlobalIdent i -> Just (makeGlobalIdentifier i)
            TLocalIdent i -> Just (makeLocalIdentifier i)
            TMetadataName i -> Just (makeMetaIdentifier i)
            _ -> Nothing

instructionMetadata :: AssemblyParser Identifier
instructionMetadata = consumeToken TDbg >> metadataIdentifier

branchTarget :: AssemblyParser Constant
branchTarget = ValueRef <$> localIdentifierP

inBounds :: AssemblyParser Bool
inBounds = tokenAs matcher
  where matcher x =
          case x of
            TInbounds -> Just True
            _ -> Just False

icmpCondition :: AssemblyParser ICmpCondition
icmpCondition = tokenAs matcher
  where matcher x =
          case x of
            Teq -> Just ICmpEq
            Tne -> Just ICmpNe
            Tugt -> Just ICmpUgt
            Tuge -> Just ICmpUge
            Tult -> Just ICmpUlt
            Tule -> Just ICmpUle
            Tsgt -> Just ICmpSgt
            Tsge -> Just ICmpSge
            Tslt -> Just ICmpSlt
            Tsle -> Just ICmpSle
            _ -> Nothing

fcmpCondition :: AssemblyParser FCmpCondition
fcmpCondition = tokenAs matcher
  where matcher x =
          case x of
            TFalseLit -> Just FCmpFalse
            Toeq -> Just FCmpOeq
            Togt -> Just FCmpOgt
            Toge -> Just FCmpOge
            Tolt -> Just FCmpOlt
            Tole -> Just FCmpOle
            Tone -> Just FCmpOne
            Tord -> Just FCmpOrd
            Tueq -> Just FCmpUeq
            Tugt -> Just FCmpUgt
            Tuge -> Just FCmpUge
            Tult -> Just FCmpUlt
            Tule -> Just FCmpUle
            Tune -> Just FCmpUne
            Tuno -> Just FCmpUno
            TTrueLit -> Just FCmpTrue
            _ -> Nothing

volatileFlag :: AssemblyParser Bool
volatileFlag = tokenAs matcher
  where matcher x =
          case x of
            TVolatile -> Just True
            _ -> Just False

-- | Parse ", align N" and return N.  Defaults to 0 if not specified.
alignmentSpec :: AssemblyParser Integer
alignmentSpec = option 0 (consumeToken TComma *> basicAlignmentSpec)

functionAlignment :: AssemblyParser Integer
functionAlignment = basicAlignmentSpec

basicAlignmentSpec :: AssemblyParser Integer
basicAlignmentSpec = consumeToken TAlign *> tokenAs matcher
  where matcher x =
          case x of
            TIntLit i -> Just i
            _ -> Nothing

addInst :: AssemblyParser ()
addInst = tokenAs matcher
  where matcher x =
          case x of
            TAdd -> Just ()
            TFadd -> Just ()
            _ -> Nothing

subInst :: AssemblyParser ()
subInst = tokenAs matcher
  where matcher x =
          case x of
            TSub -> Just ()
            TFsub -> Just ()
            _ -> Nothing

mulInst :: AssemblyParser ()
mulInst = tokenAs matcher
  where matcher x =
          case x of
            TMul -> Just ()
            TFmul -> Just ()
            _ -> Nothing

divInst :: AssemblyParser ()
divInst = tokenAs matcher
  where matcher x =
          case x of
            TUdiv -> Just ()
            TSdiv -> Just ()
            TFdiv -> Just ()
            _ -> Nothing

remInst :: AssemblyParser ()
remInst = tokenAs matcher
  where matcher x =
          case x of
            TUrem -> Just ()
            TSrem -> Just ()
            TFrem -> Just ()
            _ -> Nothing

arithFlag :: AssemblyParser ArithFlag
arithFlag = tokenAs matcher
  where matcher x =
          case x of
            TNSW -> Just AFNSW
            TNUW -> Just AFNUW
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


parser = undefined

-- satisfy f