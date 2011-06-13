module Data.LLVM.Private.Parser.Attributes ( paramAttributeP
                                           , functionAttributeP
                                           , visibilityStyleP
                                           , linkageTypeP
                                           , callingConventionP
                                           , gcNameP
                                           , sectionNameP
                                           , addrSpaceP
                                           , globalAnnotationP
                                           , globalIdentifierP
                                           , localIdentifierP
                                           , metadataIdentifierP
                                           , labelP
                                           , identifierP
                                           , instructionMetadata
                                           , branchTarget
                                           , inBoundsP
                                           , icmpConditionP
                                           , fcmpConditionP
                                           , volatileFlag
                                           , alignmentSpecP
                                           , alignmentP
                                           , addInst
                                           , subInst
                                           , mulInst
                                           , divInst
                                           , remInst
                                           , arithFlagP
                                           ) where

import Control.Applicative hiding ( (<|>) )
import Data.ByteString.Char8 ( ByteString )
import Text.Parsec

import Data.LLVM.Private.Lexer
import Data.LLVM.Private.Parser.Primitive
import Data.LLVM.Private.Types.Attributes
import Data.LLVM.Private.Types.Identifiers
import Data.LLVM.Private.Types.Placeholder


paramAttributeP :: AssemblyParser ParamAttribute
paramAttributeP = tokenAs matcher
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
            TAlign i -> Just (PAAlign i)
            _ -> Nothing

functionAttributeP :: AssemblyParser FunctionAttribute
functionAttributeP = tokenAs matcher
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

visibilityStyleP :: AssemblyParser VisibilityStyle
visibilityStyleP = option VisibilityDefault $ tokenAs matcher
  where matcher x =
          case x of
            TVisDefault -> Just VisibilityDefault
            TVisHidden -> Just VisibilityHidden
            TVisProtected -> Just VisibilityProtected
            _ -> Nothing

linkageTypeP :: AssemblyParser LinkageType
linkageTypeP = option LTExtern $ tokenAs matcher
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
            TExternal -> Just LTExtern
            _ -> Nothing


callingConventionP :: AssemblyParser CallingConvention
callingConventionP = option CCC $ tokenAs matcher
  where matcher x =
          case x of
            TCCN n -> Just (CCN n)
            TCCCCC -> Just CCC
            TCCFastCC -> Just CCFastCC
            TCCColdCC -> Just CCColdCC
            TCCGHC -> Just CCGHC
            _ -> Nothing

gcNameP :: AssemblyParser GCName
gcNameP = consumeToken TGC >> (GCName <$> parseString)

sectionNameP :: AssemblyParser (Maybe ByteString)
sectionNameP = optionMaybe parseString

addrSpaceP :: AssemblyParser Int
addrSpaceP = option 0 (tokenAs matcher)
  where matcher x =
          case x of
            TAddrspace n -> Just n
            _ -> Nothing

globalAnnotationP :: AssemblyParser GlobalAnnotation
globalAnnotationP = tokenAs matcher
  where matcher x =
          case x of
            TConstant -> Just GAConstant
            TGlobal -> Just GAGlobal
            _ -> Nothing

globalIdentifierP :: AssemblyParser Identifier
globalIdentifierP = tokenAs matcher
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

metadataIdentifierP :: AssemblyParser Identifier
metadataIdentifierP = tokenAs matcher
  where matcher x =
          case x of
            TMetadataName i -> Just (makeMetaIdentifier i)
            _ -> Nothing

labelP :: AssemblyParser ByteString
labelP = tokenAs matcher
  where matcher x =
          case x of
            TLabel l -> Just l
            _ -> Nothing

-- | Combined form which can match any identifier with just one
-- pattern match (no choice combinator required)
identifierP :: AssemblyParser Identifier
identifierP = tokenAs matcher
  where matcher x =
          case x of
            TGlobalIdent i -> Just (makeGlobalIdentifier i)
            TLocalIdent i -> Just (makeLocalIdentifier i)
            TMetadataName i -> Just (makeMetaIdentifier i)
            _ -> Nothing

instructionMetadata :: AssemblyParser Identifier
instructionMetadata = consumeToken TDbg >> metadataIdentifierP

branchTarget :: AssemblyParser Constant
branchTarget = ValueRef <$> localIdentifierP

inBoundsP :: AssemblyParser Bool
inBoundsP = option False $ tokenAs matcher
  where matcher x =
          case x of
            TInbounds -> Just True
            _ -> Nothing

icmpConditionP :: AssemblyParser ICmpCondition
icmpConditionP = tokenAs matcher
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

fcmpConditionP :: AssemblyParser FCmpCondition
fcmpConditionP = tokenAs matcher
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
alignmentSpecP :: AssemblyParser Integer
alignmentSpecP = option 0 (commaP *> basicAlignmentSpec)

alignmentP :: AssemblyParser Integer
alignmentP = option 0 basicAlignmentSpec

basicAlignmentSpec :: AssemblyParser Integer
basicAlignmentSpec = tokenAs matcher
  where matcher x =
          case x of
            TAlign i -> Just (fromIntegral i)
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

arithFlagP :: AssemblyParser ArithFlag
arithFlagP = tokenAs matcher
  where matcher x =
          case x of
            TNSW -> Just AFNSW
            TNUW -> Just AFNUW
            _ -> Nothing
