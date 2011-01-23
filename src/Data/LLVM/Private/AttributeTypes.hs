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
                                        -- , Type(..)
                                        , GCName(..)
                                        , ICmpCondition(..)
                                        , FCmpCondition(..)
                                        , GlobalAnnotation(..)
                                        , Identifier(..)
                                        , Assembly(..)
                                        ) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Map (Map)
import Data.Set (Set)
import Data.Text (Text, unpack)

data Assembly = Assembly Text
                deriving (Eq)

instance Show Assembly where
  show (Assembly txt) = unpack txt

data Identifier = LocalIdentifier Text
                | GlobalIdentifier Text
                | MetaIdentifier Text
                  deriving (Eq, Ord)

instance Show Identifier where
  show (LocalIdentifier t) = "%" ++ unpack t
  show (GlobalIdentifier t) = "@" ++ unpack t
  show (MetaIdentifier t) = "!" ++ unpack t

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
                   deriving (Eq)

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
                       | CCN Int
                       deriving (Eq)

instance Show CallingConvention where
  show CCC = "ccc"
  show CCFastCC = "fastcc"
  show CCColdCC = "coldcc"
  show CCGHC = "cc 10"
  show (CCN n) = "cc " ++ show n

data VisibilityStyle = VisibilityDefault
                     | VisibilityHidden
                     | VisibilityProtected
                       deriving (Eq)

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
                    deriving (Eq)

instance Show ParamAttribute where
  show PAZeroExt = "zeroext"
  show PASignExt = "signext"
  show PAInReg = "inreg"
  show PAByVal = "byval"
  show PASRet = "sret"
  show PANoAlias = "noalias"
  show PANoCapture = "nocapture"
  show PANest = "nest"

data FunctionAttribute = FAAlignStack Int
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
                       deriving (Eq)

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
              deriving (Show, Eq)

-- Track the ABI alignment and preferred alignment
data AlignSpec = AlignSpec Int Int
                 deriving (Show, Eq)

data TargetTriple = TargetTriple Text
                    deriving (Eq)

instance Show TargetTriple where
  show (TargetTriple t) = unpack t

data DataLayout = DataLayout { endianness :: Endian
                             , pointerAlign :: (Int, AlignSpec)
                             , intAlign :: Map Int AlignSpec
                             , vectorAlign :: Map Int AlignSpec
                             , floatAlign :: Map Int AlignSpec
                             , aggregateAlign :: Map Int AlignSpec
                             , stackAlign :: Map Int AlignSpec
                             , nativeWidths :: Set Int
                             }
                  deriving (Show, Eq)

-- Defaults specified by LLVM.  I think there can only be one pointer
-- align specification, though it isn't explicitly stated
defaultDataLayout :: DataLayout
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

data GCName = GCName Text deriving (Eq)

instance Show GCName where
  show (GCName t) = "gc \"" ++ (unpack t) ++ "\""

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
                     deriving (Eq)

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
                     deriving (Eq)

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
                      | GACommon
                      | GAPrivate
                      | GAExternal
                        deriving (Eq)

instance Show GlobalAnnotation where
  show GAConstant = "constant"
  show GAGlobal = "global"
  show GACommon = "common"
  show GAPrivate = "private"
  show GAExternal = "external"

data ArithFlag = AFNSW | AFNUW deriving (Eq)

instance Show ArithFlag where
  show AFNSW = "nsw"
  show AFNUW = "nuw"
