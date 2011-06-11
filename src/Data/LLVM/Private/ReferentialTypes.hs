{-# LANGUAGE StandaloneDeriving #-}
module Data.LLVM.Private.ReferentialTypes ( Metadata(..)
                                          , MetadataT(..)
                                          , Type(..)
                                          , Value(..)
                                          , ValueT(..)
                                          , UniqueId
                                          , valueIsFunction
                                          , llvmDebugVersion
                                          ) where

import Data.ByteString.Char8 ( ByteString )
import Data.Dwarf
import Data.Hashable
import Data.Int

import Data.LLVM.Private.AttributeTypes
import Data.LLVM.Private.DwarfHelpers

deriving instance Ord DW_LANG
deriving instance Ord DW_VIRTUALITY
deriving instance Ord DW_ATE
deriving instance Ord DW_TAG
deriving instance Ord DW_VAR_TAG

-- | This is the version of LLVM's debug information that this library
-- supports.
llvmDebugVersion :: Integer
llvmDebugVersion = 524288

data Type = TypeInteger !Int -- bits
          | TypeFloat
          | TypeDouble
          | TypeFP128
          | TypeX86FP80
          | TypePPCFP128
          | TypeX86MMX
          | TypeVoid
          | TypeLabel
          | TypeMetadata
          | TypeArray !Integer !Type
          | TypeVector !Integer !Type
          | TypeFunction !Type [Type] !Bool -- Return type, arg types, vararg
          | TypeOpaque
          | TypePointer !Type -- (Maybe Int) -- Address Space
          | TypeStruct [Type]
          | TypePackedStruct [Type]
          | TypeNamed !String !Type
          deriving (Ord, Eq)

data MetadataT =
  MetaSourceLocation { metaSourceRow :: !Int32
                     , metaSourceCol :: !Int32
                     , metaSourceScope :: Metadata
                     }
  | MetaDWLexicalBlock { metaLexicalBlockRow :: !Int32
                       , metaLexicalBlockCol :: !Int32
                       , metaLexicalBlockContext :: Metadata
                       , metaLexicalBlockFile :: Metadata
                       , metaLexicalBlockDepth :: !Int32
                       }
  | MetaDWCompileUnit { metaCompileUnitLanguage :: !DW_LANG
                      , metaCompileUnitSourceFile :: ByteString
                      , metaCompileUnitCompileDir :: ByteString
                      , metaCompileUnitProducer :: ByteString
                      , metaCompileUnitIsMain :: !Bool
                      , metaCompileUnitIsOpt :: !Bool
                      , metaCompileUnitFlags :: ByteString
                      , metaCompileUnitVersion :: !Int32
                      }
  | MetaDWFile { metaFileSourceFile :: ByteString
               , metaFileSourceDir :: ByteString
               , metaFileCompileUnit :: Metadata
               }
  | MetaDWVariable { metaGlobalVarContext :: Metadata
                   , metaGlobalVarName :: ByteString
                   , metaGlobalVarDisplayName :: ByteString
                   , metaGlobalVarLinkageName :: ByteString
                   , metaGlobalVarFile :: Metadata
                   , metaGlobalVarLine :: !Int32
                   , metaGlobalVarType :: Metadata
                   , metaGlobalVarStatic :: !Bool
                   , metaGlobalVarNotExtern :: !Bool
                   }
  | MetaDWSubprogram { metaSubprogramContext :: Metadata
                     , metaSubprogramName :: ByteString
                     , metaSubprogramDisplayName :: ByteString
                     , metaSubprogramLinkageName :: ByteString
                     , metaSubprogramFile :: Metadata
                     , metaSubprogramLine :: !Int32
                     , metaSubprogramType :: Metadata
                     , metaSubprogramStatic :: !Bool
                     , metaSubprogramNotExtern :: !Bool
                     , metaSubprogramVirtuality :: !DW_VIRTUALITY
                     , metaSubprogramVirtIndex :: !Int32
                     , metaSubprogramBaseType :: Maybe Metadata
                     , metaSubprogramArtificial :: !Bool
                     , metaSubprogramOptimized :: !Bool
                     }
  | MetaDWBaseType { metaBaseTypeContext :: Metadata
                   , metaBaseTypeName :: ByteString
                   , metaBaseTypeFile :: Maybe Metadata
                   , metaBaseTypeLine :: !Int32
                   , metaBaseTypeSize :: !Int64
                   , metaBaseTypeAlign :: !Int64
                   , metaBaseTypeOffset :: !Int64
                   , metaBaseTypeFlags :: !Int32
                   , metaBaseTypeEncoding :: !DW_ATE
                   }
  | MetaDWDerivedType { metaDerivedTypeTag :: !DW_TAG
                      , metaDerivedTypeContext :: Metadata
                      , metaDerivedTypeName :: ByteString
                      , metaDerivedTypeFile :: Maybe Metadata
                      , metaDerivedTypeLine :: !Int32
                      , metaDerivedTypeSize :: !Int64
                      , metaDerivedTypeAlign :: !Int64
                      , metaDerivedTypeOffset :: !Int64
                      , metaDerivedTypeParent :: Maybe Metadata
                      }
  | MetaDWCompositeType { metaCompositeTypeTag :: !DW_TAG
                        , metaCompositeTypeContext :: Metadata
                        , metaCompositeTypeName :: ByteString
                        , metaCompositeTypeFile :: Maybe Metadata
                        , metaCompositeTypeLine :: !Int32
                        , metaCompositeTypeSize :: !Int64
                        , metaCompositeTypeAlign :: !Int64
                        , metaCompositeTypeOffset :: !Int64
                        , metaCompositeTypeFlags :: !Int32
                        , metaCompositeTypeParent :: Maybe Metadata
                        , metaCompositeTypeMembers :: Maybe Metadata
                        , metaCompositeTypeRuntime :: !Int32
                        }
  | MetaDWSubrange { metaSubrangeLow :: !Int64
                   , metaSubrangeHigh :: !Int64
                   }
  | MetaDWEnumerator { metaEnumeratorName :: ByteString
                     , metaEnumeratorValue :: !Int64
                     }
  | MetaDWLocal { metaLocalTag :: !DW_VAR_TAG
                , metaLocalContext :: Metadata
                , metaLocalName :: ByteString
                , metaLocalFile :: Metadata
                , metaLocalLine :: !Int32
                , metaLocalType :: Metadata
                }
  | MetadataList [Metadata]
  | MetadataValueConstant Value
  | MetadataUnknown
  deriving (Ord, Eq)

type UniqueId = Int

-- | A wrapper for 'Metadata' values that tracks an Identifier and a
-- unique identifier (similar to the 'Value' wrapper).  Almost all
-- 'Metadata' has an 'Identifier'.  The only exception seems to be a
-- few 'Value' constants (such as Ints and null).
data Metadata = Metadata { metaValueName :: Maybe Identifier
                         , metaValueContent :: MetadataT
                         , metaValueUniqueId :: UniqueId
                         }

instance Eq Metadata where
  mv1 == mv2 = metaValueUniqueId mv1 == metaValueUniqueId mv2

instance Ord Metadata where
  mv1 `compare` mv2 = metaValueUniqueId mv1 `compare` metaValueUniqueId mv2

-- | A wrapper around 'ValueT' values that tracks the 'Type', name,
-- and attached metadata. valueName is mostly informational at this
-- point.  All references will be resolved as part of the graph, but
-- the name will be useful for visualization purposes and
-- serialization.
data Value = Value { valueType :: Type
                   , valueName :: Maybe Identifier
                   , valueMetadata :: Maybe Metadata
                   , valueContent :: ValueT
                   , valueUniqueId :: UniqueId
                   }

instance Eq Value where
  v1 == v2 = valueUniqueId v1 == valueUniqueId v2

instance Ord Value where
  v1 `compare` v2 = valueUniqueId v1 `compare` valueUniqueId v2

maxInt :: UniqueId
maxInt = fromIntegral (maxBound :: Int)

instance Hashable Value where
  hash Value { valueUniqueId = i } = fromIntegral $ (i `mod` maxInt)

-- Functions have parameters if they are not external
data ValueT = Function { functionType :: Type
                       , functionParameters :: [Value] -- A list of arguments
                       , functionBody :: [Value] -- A list of basic blocks
                       , functionLinkage :: LinkageType
                       , functionVisibility :: VisibilityStyle
                       , functionCC :: CallingConvention
                       , functionRetAttrs :: [ParamAttribute]
                       , functionAttrs :: [FunctionAttribute]
                       , functionName :: Identifier
                       , functionSection :: Maybe ByteString
                       , functionAlign :: Integer
                       , functionGCName :: Maybe GCName
                       , functionIsVararg :: Bool
                       }
            | GlobalDeclaration { globalVariableAddressSpace :: Int
                                , globalVariableLinkage :: LinkageType
                                , globalVariableAnnotation :: GlobalAnnotation
                                , globalVariableInitializer :: Maybe Value
                                , globalVariableAlignment :: Integer
                                , globalVariableSection :: Maybe ByteString
                                }
            | GlobalAlias { globalAliasLinkage :: LinkageType
                          , globalAliasVisibility :: VisibilityStyle
                          , globalAliasValue :: Value
                          }
            | ExternalValue
            | ExternalFunction [FunctionAttribute]
            | BasicBlock [Value]
            | Argument [ParamAttribute]
            | RetInst (Maybe Value)
            | UnconditionalBranchInst Value
            | BranchInst { branchCondition :: Value
                         , branchTrueTarget :: Value
                         , branchFalseTarget :: Value
                         }
            | SwitchInst { switchValue :: Value
                         , switchDefaultTarget :: Value
                         , switchCases :: [(Value, Value)]
                         }
              -- The target must be derived from a blockaddress constant
              -- The list is a list of possible target destinations
            | IndirectBranchInst { indirectBranchAddress :: Value
                                 , indirectBranchTargets :: [Value]
                                 }
            | UnwindInst
            | UnreachableInst
            | AddInst [ArithFlag] Value Value
            | SubInst [ArithFlag] Value Value
            | MulInst [ArithFlag] Value Value
            | DivInst Value Value -- Does not encode the exact flag of sdiv.  Convince me to
            | RemInst Value Value
            | ShlInst Value Value
            | LshrInst Value Value
            | AshrInst Value Value
            | AndInst Value Value
            | OrInst Value Value
            | XorInst Value Value
            | ExtractElementInst { extractElementVector :: Value
                                 , extractElementIndex :: Value
                                 }
            | InsertElementInst { insertElementVector :: Value
                                , insertElementValue :: Value
                                , insertElementIndex :: Value
                                }
            | ShuffleVectorInst { shuffleVectorV1 :: Value
                                , shuffleVectorV2 :: Value
                                , shuffleVectorMask :: Value
                                }
            | ExtractValueInst { extractValueAggregate :: Value
                               , extractValueIndices :: [Integer]
                               }
            | InsertValueInst { insertValueAggregate :: Value
                              , insertValueValue :: Value
                              , insertValueIndices :: [Integer]
                              }
            | AllocaInst Type Value Integer -- Type, NumElems, align
            | LoadInst Bool Value Integer -- Volatile? Type Dest align
            | StoreInst Bool Value Value Integer -- Volatile? Val Dest align
            | TruncInst Value Type -- The value being truncated, and the type truncted to
            | ZExtInst Value Type
            | SExtInst Value Type
            | FPTruncInst Value Type
            | FPExtInst Value Type
            | FPToUIInst Value Type
            | FPToSIInst Value Type
            | UIToFPInst Value Type
            | SIToFPInst Value Type
            | PtrToIntInst Value Type
            | IntToPtrInst Value Type
            | BitcastInst Value Type
            | ICmpInst ICmpCondition Value Value
            | FCmpInst FCmpCondition Value Value
            | PhiNode [(Value, Value)]
            | SelectInst Value Value Value
            | GetElementPtrInst { getElementPtrInBounds :: Bool
                                , getElementPtrValue :: Value
                                , getElementPtrIndices :: [Value]
                                }
            | CallInst { callIsTail :: Bool
                       , callConvention :: CallingConvention
                       , callParamAttrs :: [ParamAttribute]
                       , callRetType :: Type
                       , callFunction :: Value
                       , callArguments :: [(Value, [ParamAttribute])]
                       , callAttrs :: [FunctionAttribute]
                       , callHasSRet :: Bool
                       }
            | InvokeInst { invokeConvention :: CallingConvention
                         , invokeParamAttrs :: [ParamAttribute]
                         , invokeRetType :: Type
                         , invokeFunction :: Value
                         , invokeArguments :: [(Value, [ParamAttribute])]
                         , invokeAttrs :: [FunctionAttribute]
                         , invokeNormalLabel :: Value
                         , invokeUnwindLabel :: Value
                         , invokeHasSRet :: Bool
                         }
            | VaArgInst Value Type
            | UndefValue
            | BlockAddress Value Value -- Function, block -- type i8*, constant
            | ConstantAggregateZero
            | ConstantArray [Value]
            | ConstantFP Double
            | ConstantInt Integer
            | ConstantString ByteString
            | ConstantPointerNull
            | ConstantStruct [Value]
            | ConstantVector [Value]
            | ConstantValue ValueT
            | InlineAsm ByteString ByteString
            deriving (Ord, Eq)

valueIsFunction :: Value -> Bool
valueIsFunction Value { valueContent = Function {} } = True
valueIsFunction _ = False

