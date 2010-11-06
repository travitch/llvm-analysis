module Data.LLVM.AttributeTypes ( LinkageType(..)
                                , CallingConvention(..)
                                , VisibilityStyle(..)
                                , ParamAttribute(..)
                                , FunctionAttribute(..)
                                , Endian(..)
                                , DataLayout(..)
                                , AlignSpec(..)
                                , defaultDataLayout
                                , Type(..)
                                , GCName(..)
                                , ModuleAssembly(..)
                                ) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Map (Map)
import Data.Set (Set)
import Data.ByteString.Lazy (ByteString)

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

data CallingConvention = CCC
                       | CCFastCC
                       | CCColdCC
                       | CCGHC
                       | CCN Int
                       deriving (Show)

data VisibilityStyle = VisibilityDefault
                     | VisibilityHidden
                     | VisibilityProtected
                       deriving (Show)

data ParamAttribute = PAZeroExt
                    | PASignExt
                    | PAInReg
                    | PAByVal
                    | PASRet
                    | PANoAlias
                    | PANoCapture
                    | PANest
                    deriving (Show)

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
                       deriving (Show)

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
          | TypeArray Integer Type
          | TypeVector Integer Type
          | TypeFunction Type [Type] Bool -- Return type, arg types, vararg
          | TypeOpaque
          | TypePointer Type -- (Maybe Int) -- Address Space
          | TypeStruct [Type]
          | TypePackedStruct [Type]
          | TypeUpref Int
          deriving (Show, Eq)

data GCName = GCName ByteString
data ModuleAssembly = ModuleAssembly ByteString
