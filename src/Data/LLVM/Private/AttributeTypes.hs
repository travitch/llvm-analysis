module Data.LLVM.Private.AttributeTypes ( LinkageType(..)
                                        , CallingConvention(..)
                                        , VisibilityStyle(..)
                                        , ParamAttribute(..)
                                        , FunctionAttribute(..)
                                        , Endian(..)
                                        , ArithFlag(..)
                                        , DataLayout(..)
                                        , TargetTriple(..)
                                        , AlignSpec(..)
                                        , defaultDataLayout
                                        , GCName(..)
                                        , ICmpCondition(..)
                                        , FCmpCondition(..)
                                        , GlobalAnnotation(..)
                                        , Assembly(..)
                                        , module Data.LLVM.Private.Identifiers
                                        ) where

import Control.DeepSeq
import Data.ByteString.Char8 ( ByteString, unpack )

import Data.LLVM.Private.Identifiers

data Assembly = Assembly ByteString
                deriving (Eq, Ord)

instance Show Assembly where
  show (Assembly txt) = unpack txt

instance NFData Assembly where
  rnf a@(Assembly txt) = txt `seq` a `seq` ()

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
                   deriving (Eq, Ord)

-- Only trivial constructors so the default is fine
instance NFData LinkageType

instance Show LinkageType where
  show LTPrivate = "private"
  show LTLinkerPrivate = "linker_private"
  show LTLinkerPrivateWeak = "linker_private_weak"
  show LTLinkerPrivateWeakDefAuto = "linker_private_weak_def_auto"
  show LTInternal = "internal"
  show LTAvailableExternally = "available_externally"
  show LTLinkOnce = "link_once"
  show LTWeak = "weak"
  show LTCommon = "common"
  show LTAppending = "appending"
  show LTExternWeak = "extern_weak"
  show LTLinkOnceODR = "link_once_odr"
  show LTWeakODR = "weak_odr"
  show LTExtern = ""
  show LTDLLImport = "dllimport"
  show LTDLLExport = "dllexport"

data CallingConvention = CCC
                       | CCFastCC
                       | CCColdCC
                       | CCGHC
                       | CCN !Int
                       deriving (Eq, Ord)

instance NFData CallingConvention

instance Show CallingConvention where
  show CCC = ""
  show CCFastCC = "fastcc"
  show CCColdCC = "coldcc"
  show CCGHC = "cc 10"
  show (CCN n) = "cc " ++ show n

data VisibilityStyle = VisibilityDefault
                     | VisibilityHidden
                     | VisibilityProtected
                       deriving (Eq, Ord)

-- Again, default since there are only trivial constructors
instance NFData VisibilityStyle

instance Show VisibilityStyle where
  show VisibilityDefault = ""
  show VisibilityHidden = "hidden"
  show VisibilityProtected = "protected"

data ParamAttribute = PAZeroExt
                    | PASignExt
                    | PAInReg
                    | PAByVal
                    | PASRet
                    | PANoAlias
                    | PANoCapture
                    | PANest
                    | PAAlign !Int
                    deriving (Eq, Ord)

instance NFData ParamAttribute

instance Show ParamAttribute where
  show PAZeroExt = "zeroext"
  show PASignExt = "signext"
  show PAInReg = "inreg"
  show PAByVal = "byval"
  show PASRet = "sret"
  show PANoAlias = "noalias"
  show PANoCapture = "nocapture"
  show PANest = "nest"
  show (PAAlign i) = "align " ++ show i

data FunctionAttribute = FAAlignStack !Int
                       | FAAlwaysInline
                       | FAHotPatch
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
                       deriving (Eq, Ord)

instance NFData FunctionAttribute

instance Show FunctionAttribute where
  show (FAAlignStack n) = "alignstack(" ++ show n ++ ")"
  show FAAlwaysInline = "alwaysinline"
  show FAHotPatch = "hotpatch"
  show FAInlineHint = "inlinehint"
  show FANaked = "naked"
  show FANoImplicitFloat = "noimplicitfloat"
  show FANoInline = "noinline"
  show FANoRedZone = "noredzone"
  show FANoReturn = "noreturn"
  show FANoUnwind = "nounwind"
  show FAOptSize = "optsize"
  show FAReadNone = "readnone"
  show FAReadOnly = "readonly"
  show FASSP = "ssp"
  show FASSPReq = "sspreq"

data Endian = EBig
            | ELittle
              deriving (Eq, Ord)

instance NFData Endian

instance Show Endian where
  show EBig = "E"
  show ELittle = "e"

-- Track the ABI alignment and preferred alignment
data AlignSpec = AlignSpec !Int !Int
                 deriving (Show, Eq, Ord)

instance NFData AlignSpec where
  rnf a@(AlignSpec i1 i2) = i1 `seq` i2 `seq` a `seq` ()

data TargetTriple = TargetTriple ByteString
                    deriving (Eq)

instance Show TargetTriple where
  show (TargetTriple t) = unpack t

instance NFData TargetTriple where
  rnf t@(TargetTriple t') = t' `seq` t `seq` ()

data DataLayout = DataLayout { endianness :: Endian
                             , pointerAlign :: (Int, AlignSpec)
                             , intAlign :: [ (Int, AlignSpec) ]
                             , vectorAlign :: [ (Int, AlignSpec) ]
                             , floatAlign :: [ (Int, AlignSpec) ]
                             , aggregateAlign :: [ (Int, AlignSpec) ]
                             , stackAlign :: [ (Int, AlignSpec) ]
                             , nativeWidths :: [ Int ]
                             }
                  deriving (Show, Eq)

instance NFData DataLayout where
  rnf d = endianness d `deepseq` pointerAlign d `deepseq`
           intAlign d `deepseq` vectorAlign d `deepseq`
           floatAlign d `deepseq` aggregateAlign d `deepseq`
           stackAlign d `deepseq` nativeWidths d `deepseq` d `seq` ()

-- Defaults specified by LLVM.  I think there can only be one pointer
-- align specification, though it isn't explicitly stated
defaultDataLayout :: DataLayout
defaultDataLayout = DataLayout { endianness = EBig
                               , pointerAlign = (64, AlignSpec 64 64)
                               , intAlign = [ (1, AlignSpec 8 8)
                                            , (8, AlignSpec 8 8)
                                            , (16, AlignSpec 16 16)
                                            , (32, AlignSpec 32 32)
                                            , (64, AlignSpec 32 64)
                                            ]
                               , vectorAlign = [ (64, AlignSpec 64 64)
                                               , (128, AlignSpec 128 128)
                                               ]
                               , floatAlign = [ (32, AlignSpec 32 32)
                                              , (64, AlignSpec 64 64)
                                              ]
                               , aggregateAlign = [ (0, AlignSpec 0 1) ]
                               , stackAlign = [ (0, AlignSpec 64 64) ]
                               , nativeWidths = [] -- Set.empty
                               }

data GCName = GCName ByteString deriving (Eq, Ord)

instance NFData GCName where
  rnf n@(GCName s) = s `seq` n `seq` ()

instance Show GCName where
  show (GCName t) = "gc \"" ++ unpack t ++ "\""

data ICmpCondition = ICmpEq
                   | ICmpNe
                   | ICmpUgt
                   | ICmpUge
                   | ICmpUlt
                   | ICmpUle
                   | ICmpSgt
                   | ICmpSge
                   | ICmpSlt
                   | ICmpSle
                     deriving (Eq, Ord)

instance Show ICmpCondition where
  show ICmpEq = "eq"
  show ICmpNe = "ne"
  show ICmpUgt = "ugt"
  show ICmpUge = "uge"
  show ICmpUlt = "ult"
  show ICmpUle = "ule"
  show ICmpSgt = "sgt"
  show ICmpSge = "sge"
  show ICmpSlt = "slt"
  show ICmpSle = "sle"

data FCmpCondition = FCmpFalse
                   | FCmpOeq
                   | FCmpOgt
                   | FCmpOge
                   | FCmpOlt
                   | FCmpOle
                   | FCmpOne
                   | FCmpOrd
                   | FCmpUeq
                   | FCmpUgt
                   | FCmpUge
                   | FCmpUlt
                   | FCmpUle
                   | FCmpUne
                   | FCmpUno
                   | FCmpTrue
                     deriving (Eq, Ord)

instance Show FCmpCondition where
  show FCmpFalse = "false"
  show FCmpOeq = "oeq"
  show FCmpOgt = "ogt"
  show FCmpOge = "oge"
  show FCmpOlt = "olt"
  show FCmpOle = "ole"
  show FCmpOne = "one"
  show FCmpOrd = "ord"
  show FCmpUeq = "ueq"
  show FCmpUgt = "ugt"
  show FCmpUge = "uge"
  show FCmpUlt = "ult"
  show FCmpUle = "ule"
  show FCmpUne = "une"
  show FCmpUno = "uno"
  show FCmpTrue = "true"

data GlobalAnnotation = GAConstant
                      | GAGlobal
                        deriving (Eq, Ord)

instance Show GlobalAnnotation where
  show GAConstant = "constant"
  show GAGlobal = "global"

data ArithFlag = AFNSW | AFNUW
               deriving (Eq, Ord)

instance NFData ArithFlag

instance Show ArithFlag where
  show AFNSW = "nsw"
  show AFNUW = "nuw"
