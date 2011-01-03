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
import Data.Text (Text)

data Assembly = Assembly Text
                deriving (Show, Eq)

data Identifier = LocalIdentifier Text
                | GlobalIdentifier Text
                | MetaIdentifier Text
                  deriving (Show, Eq, Ord)

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
                   deriving (Show, Eq)

data CallingConvention = CCC
                       | CCFastCC
                       | CCColdCC
                       | CCGHC
                       | CCN Int
                       deriving (Show, Eq)

data VisibilityStyle = VisibilityDefault
                     | VisibilityHidden
                     | VisibilityProtected
                       deriving (Show, Eq)

data ParamAttribute = PAZeroExt
                    | PASignExt
                    | PAInReg
                    | PAByVal
                    | PASRet
                    | PANoAlias
                    | PANoCapture
                    | PANest
                    deriving (Show, Eq)

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
                       deriving (Show, Eq)

data Endian = EBig
            | ELittle
              deriving (Show, Eq)

-- Track the ABI alignment and preferred alignment
data AlignSpec = AlignSpec Int Int
                 deriving (Show, Eq)

data TargetTriple = TargetTriple Text
                    deriving (Show, Eq)

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

data GCName = GCName Text deriving (Show, Eq)

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
                     deriving (Show, Eq)

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
                     deriving (Show, Eq)

data GlobalAnnotation = GAConstant
                      | GAGlobal
                      | GACommon
                      | GAPrivate
                      | GAExternal
                        deriving (Show, Eq)


data ArithFlag = AFNSW | AFNUW deriving (Show, Eq)
