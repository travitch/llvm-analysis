{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Data.LLVM.Private.Types.Referential (
  Type(..),
  UniqueId,
  Value(..),
  ValueT(..),
  Metadata(..),
  MetadataT(..),
  valueIsFunction,
  blockInstructions,
  llvmDebugVersion
  ) where

import Data.ByteString.Char8 ( ByteString )
import Data.Dwarf
import Data.GraphViz
import Data.Hashable
import Data.Int
import Text.Printf

import Data.LLVM.Private.Types.Attributes
import Data.LLVM.Private.Types.CAttributes
import Data.LLVM.Private.Types.Dwarf
import Data.LLVM.Private.Types.Identifiers

-- | This is the version of LLVM's debug information that this library
-- supports.
llvmDebugVersion :: Integer
llvmDebugVersion = 524288

-- | The type system of LLVM
data Type = TypeInteger !Int
            -- ^ Integral types; the parameter holds the number of
            -- bits required to represent the type.
          | TypeFloat
          | TypeDouble
          | TypeFP128
          | TypeX86FP80
          | TypePPCFP128
          | TypeX86MMX
          | TypeVoid
          | TypeLabel
          | TypeMetadata
          | TypeArray !Int !Type
            -- ^ Fixed-length arrays, where the Int holds the number
            -- of elements in arrays of this type.
          | TypeVector !Int !Type
            -- ^ Vectors with a fixed length.  These are vectors in
            -- the SSE sense.
          | TypeFunction !Type [Type] !Bool
            -- ^ Functions with a return type, list of argument types,
            -- and a flag that denotes whether or not the function
            -- accepts varargs
          | TypeOpaque
          | TypePointer !Type !Int
          | TypeStruct [Type]
          | TypePackedStruct [Type]
          | TypeNamed !String !Type
            -- ^ A wrapper for typedefs

instance Hashable Type where
  hash (TypeInteger i) = 1 `combine` hash i
  hash TypeFloat = 2
  hash TypeDouble = 3
  hash TypeFP128 = 4
  hash TypeX86FP80 = 5
  hash TypePPCFP128 = 6
  hash TypeX86MMX = 7
  hash TypeVoid = 8
  hash TypeLabel = 9
  hash TypeMetadata = 10
  hash (TypeArray i t) = 11 `combine` hash i `combine` hash t
  hash (TypeVector i t) = 12 `combine` hash i `combine` hash t
  hash (TypeFunction r ts v) = 13 `combine` hash r `combine` hash ts `combine` hash v
  hash TypeOpaque = 14
  hash (TypePointer t as) = 15 `combine` hash t `combine` as
  hash (TypeStruct ts) = 16 `combine` hash ts
  hash (TypePackedStruct ts) = 17 `combine` hash ts
  hash (TypeNamed s _) = 18 `combine` hash s

instance Eq Type where
  TypeInteger i1 == TypeInteger i2 = i1 == i2
  TypeFloat == TypeFloat = True
  TypeDouble == TypeDouble = True
  TypeFP128 == TypeFP128 = True
  TypeX86FP80 == TypeX86FP80 = True
  TypePPCFP128 == TypePPCFP128 = True
  TypeX86MMX == TypeX86MMX = True
  TypeVoid == TypeVoid = True
  TypeLabel == TypeLabel = True
  TypeMetadata == TypeMetadata = True
  TypeArray i1 t1 == TypeArray i2 t2 = i1 == i2 && t1 == t2
  TypeVector i1 t1 == TypeVector i2 t2 = i1 == i2 && t1 == t2
  TypeFunction r1 ts1 v1 == TypeFunction r2 ts2 v2 =
    v1 == v2 && r1 == r2 && ts1 == ts2
  TypeOpaque == TypeOpaque = True
  TypePointer t1 as1 == TypePointer t2 as2 = t1 == t2 && as1 == as2
  TypeStruct ts1 == TypeStruct ts2 = ts1 == ts2
  TypePackedStruct ts1 == TypePackedStruct ts2 = ts1 == ts2
  TypeNamed s1 _ == TypeNamed s2 _ = s1 == s2
  _ == _ = False

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
                      , metaCompileUnitSourceFile :: !ByteString
                      , metaCompileUnitCompileDir :: !ByteString
                      , metaCompileUnitProducer :: !ByteString
                      , metaCompileUnitIsMain :: !Bool
                      , metaCompileUnitIsOpt :: !Bool
                      , metaCompileUnitFlags :: !ByteString
                      , metaCompileUnitVersion :: !Int32
                      }
  | MetaDWFile { metaFileSourceFile :: !ByteString
               , metaFileSourceDir :: !ByteString
               , metaFileCompileUnit :: Metadata
               }
  | MetaDWVariable { metaGlobalVarContext :: Metadata
                   , metaGlobalVarName :: !ByteString
                   , metaGlobalVarDisplayName :: !ByteString
                   , metaGlobalVarLinkageName :: !ByteString
                   , metaGlobalVarFile :: Metadata
                   , metaGlobalVarLine :: !Int32
                   , metaGlobalVarType :: Metadata
                   , metaGlobalVarStatic :: !Bool
                   , metaGlobalVarNotExtern :: !Bool
                   }
  | MetaDWSubprogram { metaSubprogramContext :: Metadata
                     , metaSubprogramName :: !ByteString
                     , metaSubprogramDisplayName :: !ByteString
                     , metaSubprogramLinkageName :: !ByteString
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
                   , metaBaseTypeName :: !ByteString
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
                      , metaDerivedTypeName :: !ByteString
                      , metaDerivedTypeFile :: Maybe Metadata
                      , metaDerivedTypeLine :: !Int32
                      , metaDerivedTypeSize :: !Int64
                      , metaDerivedTypeAlign :: !Int64
                      , metaDerivedTypeOffset :: !Int64
                      , metaDerivedTypeParent :: Maybe Metadata
                      }
  | MetaDWCompositeType { metaCompositeTypeTag :: !DW_TAG
                        , metaCompositeTypeContext :: Metadata
                        , metaCompositeTypeName :: !ByteString
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
  | MetaDWEnumerator { metaEnumeratorName :: !ByteString
                     , metaEnumeratorValue :: !Int64
                     }
  | MetaDWLocal { metaLocalTag :: !DW_VAR_TAG
                , metaLocalContext :: Metadata
                , metaLocalName :: !ByteString
                , metaLocalFile :: Metadata
                , metaLocalLine :: !Int32
                , metaLocalType :: Metadata
                }
  | MetadataList [Metadata]
  | MetadataValueConstant Value
    -- ^ A reference to a 'Value' in metadata
  | MetadataDiscarded
    -- ^ Metadata that has been discarded due to the parser
    -- configuration
  | MetadataUnknown
    -- ^ Unrecognized type of metadata
  deriving (Ord, Eq)

-- | The type of the unique identifiers that let us to work with
-- 'Value's and 'Metadata`, despite the cycles in the object graph.
-- These ids are typically used as hash keys and give objects of these
-- types identity.
type UniqueId = Int

-- | A wrapper for 'Metadata' values that tracks an Identifier and a
-- unique identifier (similar to the 'Value' wrapper).  Almost all
-- 'Metadata' has an 'Identifier'.  The only exception seems to be a
-- few 'Value' constants (such as Ints and null).
data Metadata = Metadata { metaValueName :: Maybe Identifier
                         , metaValueContent :: MetadataT
                         , metaValueUniqueId :: !UniqueId
                         }

instance Eq Metadata where
  mv1 == mv2 = metaValueUniqueId mv1 == metaValueUniqueId mv2

instance Ord Metadata where
  mv1 `compare` mv2 = metaValueUniqueId mv1 `compare` metaValueUniqueId mv2

instance Hashable Metadata where
  hash md = fromIntegral $ metaValueUniqueId md

-- | A wrapper around 'ValueT' values that tracks the 'Type', name,
-- and attached metadata. valueName is mostly informational at this
-- point.  All references will be resolved as part of the graph, but
-- the name will be useful for visualization purposes and
-- serialization.
data Value = Value { valueType :: Type
                   , valueName :: !(Maybe Identifier)
                   , valueMetadata :: Maybe Metadata
                   , valueContent :: ValueT
                   , valueUniqueId :: !UniqueId
                   }

instance Eq Value where
  v1 == v2 = valueUniqueId v1 == valueUniqueId v2

instance Ord Value where
  v1 `compare` v2 = valueUniqueId v1 `compare` valueUniqueId v2

maxInt :: UniqueId
maxInt = fromIntegral (maxBound :: Int)

instance Hashable Value where
  hash Value { valueUniqueId = i } = fromIntegral $ (i `mod` maxInt)

instance Labellable Value where
  toLabel = (Label . StrLabel) . show . valueName

-- Functions have parameters if they are not external
data ValueT = Function { functionParameters :: [Value] -- A list of arguments
                       , functionBody :: [Value] -- A list of basic blocks
                       , functionLinkage :: !LinkageType
                       , functionVisibility :: !VisibilityStyle
                       , functionCC :: !CallingConvention
                       , functionRetAttrs :: [ParamAttribute]
                       , functionAttrs :: [FunctionAttribute]
                       , functionSection :: !(Maybe ByteString)
                       , functionAlign :: !Int64
                       , functionGCName :: !(Maybe GCName)
                       , functionIsVararg :: !Bool
                       }
            | GlobalDeclaration { globalVariableLinkage :: !LinkageType
                                , globalVariableVisibility :: !VisibilityStyle
                                , globalVariableInitializer :: Maybe Value
                                , globalVariableAlignment :: !Int64
                                , globalVariableSection :: !(Maybe ByteString)
                                , globalVariableIsThreadLocal :: !Bool
                                }
            | GlobalAlias { globalAliasLinkage :: !LinkageType
                          , globalAliasVisibility :: !VisibilityStyle
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
            | AddInst !ArithFlags Value Value
            | SubInst !ArithFlags Value Value
            | MulInst !ArithFlags Value Value
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
                               , extractValueIndices :: [Int]
                               }
            | InsertValueInst { insertValueAggregate :: Value
                              , insertValueValue :: Value
                              , insertValueIndices :: [Int]
                              }
            | AllocaInst Value !Int64
              -- ^ Type being allocated, number of elements, alignment
            | LoadInst !Bool Value !Int64
              -- ^ Volatile flag, address being loaded, alignment
            | StoreInst !Bool Value Value !Int64
              -- ^ Volatile flag, value being stored, address of the destination, alignment
            | TruncInst Value
              -- ^ Value being truncated, result type
            | ZExtInst Value
              -- ^ Value being truncated, result type
            | SExtInst Value
              -- ^ Value being truncated, result type
            | FPTruncInst Value
              -- ^ Value being truncated, result type
            | FPExtInst Value
              -- ^ Value being truncated, result type
            | FPToUIInst Value
              -- ^ Value being truncated, result type
            | FPToSIInst Value
              -- ^ Value being truncated, result type
            | UIToFPInst Value
              -- ^ Value being truncated, result type
            | SIToFPInst Value
              -- ^ Value being truncated, result type
            | PtrToIntInst Value
              -- ^ Value being truncated, result type
            | IntToPtrInst Value
              -- ^ Value being truncated, result type
            | BitcastInst Value
              -- ^ Value being truncated, result type
            | ICmpInst !CmpPredicate Value Value
              -- ^ Type of comparison, values being compared
            | FCmpInst !CmpPredicate Value Value
              -- ^ Type of comparison, values being compared
            | PhiNode [(Value, Value)]
            | SelectInst Value Value Value
            | GetElementPtrInst { getElementPtrInBounds :: !Bool
                                , getElementPtrValue :: Value
                                , getElementPtrIndices :: [Value]
                                }
            | CallInst { callIsTail :: !Bool
                       , callConvention :: !CallingConvention
                       , callParamAttrs :: [ParamAttribute]
                       , callFunction :: Value
                       , callArguments :: [(Value, [ParamAttribute])]
                       , callAttrs :: [FunctionAttribute]
                       , callHasSRet :: !Bool
                       }
            | InvokeInst { invokeConvention :: !CallingConvention
                         , invokeParamAttrs :: [ParamAttribute]
                         , invokeFunction :: Value
                         , invokeArguments :: [(Value, [ParamAttribute])]
                         , invokeAttrs :: [FunctionAttribute]
                         , invokeNormalLabel :: Value
                         , invokeUnwindLabel :: Value
                         , invokeHasSRet :: !Bool
                         }
            | VaArgInst Value
            | UndefValue
            | BlockAddress Value Value -- Function, block -- type i8*, constant
            | ConstantAggregateZero
            | ConstantArray [Value]
            | ConstantFP !Double
            | ConstantInt !Integer
            | ConstantString !ByteString
            | ConstantPointerNull
            | ConstantStruct [Value]
            | ConstantVector [Value]
            | ConstantValue ValueT
            | InlineAsm !ByteString !ByteString
            deriving (Eq)


-- | Get the instructions for a BasicBlock.
blockInstructions :: Value -> [Value]
blockInstructions Value { valueContent = BasicBlock is } = is
blockInstructions v = error $ printf "Value is not a basic block: %d" uid
  where
    uid :: Integer
    uid = fromIntegral $ valueUniqueId v

-- | This simple helper tests whether or not the given 'Value' is a
-- Function definition
valueIsFunction :: Value -> Bool
valueIsFunction Value { valueContent = Function {} } = True
valueIsFunction _ = False

