{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TypeSynonymInstances, ExistentialQuantification #-}
module Data.LLVM.Private.Types.Referential (
  Type(..),
  UniqueId,
  IsValue(..),
  Value(..),
  ValueContent(..),
  Function(..),
  BasicBlock(..),
  Argument(..),
  Instruction(..),
  GlobalVariable(..),
  GlobalAlias(..),
  ExternalValue(..),
  ExternalFunction(..),
  Constant(..),
  Metadata(..),
  MetadataContent(..),
  functionIsVararg,
  functionEntryInstruction,
  functionExitInstruction,
  instructionIsTerminator,
  llvmDebugVersion
  ) where

import Data.ByteString.Char8 ( ByteString )
import Data.Hashable
import Data.Int
import Data.Ord ( comparing )
import Text.Printf

import Data.LLVM.Attributes
import Data.LLVM.Dwarf
import Data.LLVM.Identifiers

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
          | TypeArray !Int Type
            -- ^ Fixed-length arrays, where the Int holds the number
            -- of elements in arrays of this type.
          | TypeVector !Int Type
            -- ^ Vectors with a fixed length.  These are vectors in
            -- the SSE sense.
          | TypeFunction Type [Type] !Bool
            -- ^ Functions with a return type, list of argument types,
            -- and a flag that denotes whether or not the function
            -- accepts varargs
          | TypeOpaque
          | TypePointer Type !Int
          | TypeStruct [Type] !Bool -- isPacked
          | TypeNamed !String Type
            -- ^ A wrapper for typedefs

-- Deriving an Ord instance won't work because Type is a cyclic data
-- structure and the derived instances end up stuck in infinite loops.
-- Defining a more traditional one that just breaks cycles is really
-- tedious here, so just base Ord off of equality and then make the
-- ordering arbitrary (but consistent) based on the Hashable instance.
instance Ord Type where
  t1 `compare` t2 = case t1 == t2 of
    True -> EQ
    False -> comparing hash t1 t2

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
  hash (TypeStruct ts p) = 16 `combine` hash ts `combine` hash p
  hash (TypeNamed s _) = 17 `combine` hash s

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
  TypeStruct ts1 p1 == TypeStruct ts2 p2 = ts1 == ts2 && p1 == p2
  TypeNamed s1 _ == TypeNamed s2 _ = s1 == s2
  _ == _ = False

data MetadataContent =
  MetaSourceLocation { metaSourceRow :: !Int32
                     , metaSourceCol :: !Int32
                     , metaSourceScope :: Metadata
                     }
  | MetaDWLexicalBlock { metaLexicalBlockRow :: !Int32
                       , metaLexicalBlockCol :: !Int32
                       , metaLexicalBlockContext :: Metadata
                       }
  | MetaDWNamespace { metaNamespaceContext :: Metadata
                    , metaNamespaceName :: !ByteString
                    , metaNamespaceCompileUnit :: Metadata
                    , metaNamespaceLine :: !Int32
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
                     , metaSubprogramIsExplicit :: !Bool
                     , metaSubprogramIsPrototyped :: !Bool
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
                      , metaDerivedTypeCompileUnit :: Maybe Metadata
                      , metaDerivedTypeName :: !ByteString
                      , metaDerivedTypeFile :: Maybe Metadata
                      , metaDerivedTypeLine :: !Int32
                      , metaDerivedTypeSize :: !Int64
                      , metaDerivedTypeAlign :: !Int64
                      , metaDerivedTypeOffset :: !Int64
                      , metaDerivedTypeIsArtificial :: !Bool
                      , metaDerivedTypeIsVirtual :: !Bool
                      , metaDerivedTypeIsForward :: !Bool
                      , metaDerivedTypeIsPrivate :: !Bool
                      , metaDerivedTypeIsProtected :: !Bool
                      , metaDerivedTypeParent :: Maybe Metadata
                      }
  | MetaDWCompositeType { metaCompositeTypeTag :: !DW_TAG
                        , metaCompositeTypeContext :: Metadata
                        , metaCompositeTypeName :: !ByteString
                        , metaCompositeTypeFile :: Maybe Metadata
                        , metaCompositeTypeCompileUnit :: Maybe Metadata
                        , metaCompositeTypeLine :: !Int32
                        , metaCompositeTypeSize :: !Int64
                        , metaCompositeTypeAlign :: !Int64
                        , metaCompositeTypeOffset :: !Int64
                        , metaCompositeTypeFlags :: !Int32
                        , metaCompositeTypeParent :: Maybe Metadata
                        , metaCompositeTypeMembers :: Maybe Metadata
                        , metaCompositeTypeRuntime :: !Int32
                        , metaCompositeTypeContainer :: Maybe Metadata
                        , metaCompositeTypeTemplateParams :: Maybe Metadata
                        , metaCompositeTypeIsArtificial :: !Bool
                        , metaCompositeTypeIsVirtual :: !Bool
                        , metaCompositeTypeIsForward :: !Bool
                        , metaCompositeTypeIsProtected :: !Bool
                        , metaCompositeTypeIsPrivate :: !Bool
                        , metaCompositeTypeIsByRefStruct :: !Bool
                        }
  | MetaDWSubrange { metaSubrangeLow :: !Int64
                   , metaSubrangeHigh :: !Int64
                   }
  | MetaDWEnumerator { metaEnumeratorName :: !ByteString
                     , metaEnumeratorValue :: !Int64
                     }
  | MetaDWLocal { metaLocalTag :: !DW_TAG
                , metaLocalContext :: Metadata
                , metaLocalName :: !ByteString
                , metaLocalFile :: Metadata
                , metaLocalLine :: !Int32
                , metaLocalArgNo :: !Int32
                , metaLocalType :: Metadata
                , metaLocalIsArtificial :: !Bool
                , metaLocalIsBlockByRefVar :: !Bool
                , metaLocalAddrElements :: [Int64]
                }
  | MetaDWTemplateTypeParameter { metaTemplateTypeParameterContext :: Metadata
                                , metaTemplateTypeParameterType :: Metadata
                                , metaTemplateTypeParameterLine :: !Int32
                                , metaTemplateTypeParameterCol :: !Int32
                                , metaTemplateTypeParameterName :: !ByteString
                                }
  | MetaDWTemplateValueParameter { metaTemplateValueParameterContext :: Metadata
                                 , metaTemplateValueParameterType :: Metadata
                                 , metaTemplateValueParameterLine :: !Int32
                                 , metaTemplateValueParameterCol :: !Int32
                                 , metaTemplateValueParameterValue :: !Int64
                                 , metaTemplateValueParameterName :: !ByteString
                                 }
  | MetadataList [Metadata]
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
data Metadata = Metadata { metaValueContent :: MetadataContent
                         , metaValueUniqueId :: !UniqueId
                         }

instance Eq Metadata where
  mv1 == mv2 = metaValueUniqueId mv1 == metaValueUniqueId mv2

instance Ord Metadata where
  mv1 `compare` mv2 = comparing metaValueUniqueId mv1 mv2

instance Hashable Metadata where
  hash = fromIntegral . metaValueUniqueId

-- | A wrapper around 'ValueT' values that tracks the 'Type', name,
-- and attached metadata. valueName is mostly informational at this
-- point.  All references will be resolved as part of the graph, but
-- the name will be useful for visualization purposes and
-- serialization.
data Value = forall a . IsValue a => Value a

class IsValue a where
  valueType :: a -> Type
  valueName :: a -> Maybe Identifier
  valueMetadata :: a -> [Metadata]
  valueContent :: a -> ValueContent
  valueUniqueId :: a -> UniqueId

instance IsValue Value where
  valueType (Value a) = valueType a
  valueName (Value a) = valueName a
  valueMetadata (Value a) = valueMetadata a
  valueContent (Value a) = valueContent a
  valueUniqueId (Value a) = valueUniqueId a

instance Eq Value where
  v1 == v2 = valueUniqueId v1 == valueUniqueId v2

instance Ord Value where
  v1 `compare` v2 = comparing valueUniqueId v1 v2

instance Hashable Value where
  hash = fromIntegral . valueUniqueId

data Function = Function { functionType :: Type
                         , functionName :: !Identifier
                         , functionMetadata :: [Metadata]
                         , functionUniqueId :: !UniqueId
                         , functionParameters :: [Argument]
                         , functionBody :: [BasicBlock]
                         , functionLinkage :: !LinkageType
                         , functionVisibility :: !VisibilityStyle
                         , functionCC :: !CallingConvention
                         , functionRetAttrs :: [ParamAttribute]
                         , functionAttrs :: [FunctionAttribute]
                         , functionSection :: !(Maybe ByteString)
                         , functionAlign :: !Int64
                         , functionGCName :: !(Maybe ByteString)
                         }
functionIsVararg :: Function -> Bool
functionIsVararg Function { functionType = TypeFunction _ _ isva } = isva
functionIsVararg v = error $ printf "Value %d is not a function" (valueUniqueId v)

functionEntryInstruction :: Function -> Instruction
functionEntryInstruction f = e1
  where
    (bb1:_) = functionBody f
    (e1:_) = basicBlockInstructions bb1

functionExitInstruction :: Function -> Instruction
functionExitInstruction f = e
  where
    (bb:_) = reverse (functionBody f)
    (e:_) = reverse (basicBlockInstructions bb)

instance IsValue Function where
  valueType = functionType
  valueName = Just . functionName
  valueMetadata = functionMetadata
  valueContent = FunctionC
  valueUniqueId = functionUniqueId

instance Eq Function where
  f1 == f2 = functionUniqueId f1 == functionUniqueId f2

instance Hashable Function where
  hash = fromIntegral . functionUniqueId

instance Ord Function where
  f1 `compare` f2 = comparing functionUniqueId f1 f2

data Argument = Argument { argumentType :: Type
                         , argumentName :: !Identifier
                         , argumentMetadata :: [Metadata]
                         , argumentUniqueId :: !UniqueId
                         , argumentParamAttrs :: [ParamAttribute]
                         , argumentFunction :: Function
                         }

instance IsValue Argument where
  valueType = argumentType
  valueName = Just . argumentName
  valueMetadata = argumentMetadata
  valueContent = ArgumentC
  valueUniqueId = argumentUniqueId

instance Hashable Argument where
  hash = fromIntegral . argumentUniqueId

instance Eq Argument where
  a1 == a2 = argumentUniqueId a1 == argumentUniqueId a2

instance Ord Argument where
  a1 `compare` a2 = comparing argumentUniqueId a1 a2

data BasicBlock = BasicBlock { basicBlockType :: Type
                             , basicBlockName :: !Identifier
                             , basicBlockMetadata :: [Metadata]
                             , basicBlockUniqueId :: !UniqueId
                             , basicBlockInstructions :: [Instruction]
                             , basicBlockFunction :: Function
                             }

instance IsValue BasicBlock where
  valueType = basicBlockType
  valueName = Just . basicBlockName
  valueMetadata = basicBlockMetadata
  valueContent = BasicBlockC
  valueUniqueId = basicBlockUniqueId

instance Hashable BasicBlock where
  hash = fromIntegral . basicBlockUniqueId

instance Eq BasicBlock where
  f1 == f2 = basicBlockUniqueId f1 == basicBlockUniqueId f2

instance Ord BasicBlock where
  b1 `compare` b2 = comparing basicBlockUniqueId b1 b2

data GlobalVariable = GlobalVariable { globalVariableType :: Type
                                     , globalVariableName :: !Identifier
                                     , globalVariableMetadata :: [Metadata]
                                     , globalVariableUniqueId :: !UniqueId
                                     , globalVariableLinkage :: !LinkageType
                                     , globalVariableVisibility :: !VisibilityStyle
                                     , globalVariableInitializer :: Maybe Value
                                     , globalVariableAlignment :: !Int64
                                     , globalVariableSection :: !(Maybe ByteString)
                                     , globalVariableIsThreadLocal :: !Bool
                                     , globalVariableIsConstant :: !Bool
                                     }

instance IsValue GlobalVariable where
  valueType = globalVariableType
  valueName = Just . globalVariableName
  valueMetadata = globalVariableMetadata
  valueContent = GlobalVariableC
  valueUniqueId = globalVariableUniqueId

instance Eq GlobalVariable where
  f1 == f2 = globalVariableUniqueId f1 == globalVariableUniqueId f2

instance Hashable GlobalVariable where
  hash = fromIntegral . globalVariableUniqueId

instance Ord GlobalVariable where
  g1 `compare` g2 = comparing globalVariableUniqueId g1 g2

data GlobalAlias = GlobalAlias { globalAliasTarget :: Value
                               , globalAliasLinkage :: !LinkageType
                               , globalAliasName :: !Identifier
                               , globalAliasVisibility :: !VisibilityStyle
                               , globalAliasMetadata :: [Metadata]
                               , globalAliasUniqueId :: !UniqueId
                               }

instance IsValue GlobalAlias where
  valueType = valueType . globalAliasTarget
  valueName = Just . globalAliasName
  valueMetadata = globalAliasMetadata
  valueContent = GlobalAliasC
  valueUniqueId = globalAliasUniqueId

instance Eq GlobalAlias where
  f1 == f2 = globalAliasUniqueId f1 == globalAliasUniqueId f2

instance Hashable GlobalAlias where
  hash = fromIntegral . globalAliasUniqueId

instance Ord GlobalAlias where
  g1 `compare` g2 = comparing globalAliasUniqueId g1 g2

data ExternalValue = ExternalValue { externalValueType :: Type
                                   , externalValueName :: !Identifier
                                   , externalValueMetadata :: [Metadata]
                                   , externalValueUniqueId :: !UniqueId
                                   }

instance IsValue ExternalValue where
  valueType = externalValueType
  valueName = Just . externalValueName
  valueMetadata = externalValueMetadata
  valueContent = ExternalValueC
  valueUniqueId = externalValueUniqueId

instance Eq ExternalValue where
  f1 == f2 = externalValueUniqueId f1 == externalValueUniqueId f2

instance Hashable ExternalValue where
  hash = fromIntegral . externalValueUniqueId

instance Ord ExternalValue where
  e1 `compare` e2 = comparing externalValueUniqueId e1 e2

data ExternalFunction = ExternalFunction { externalFunctionType :: Type
                                         , externalFunctionName :: !Identifier
                                         , externalFunctionMetadata :: [Metadata]
                                         , externalFunctionUniqueId :: !UniqueId
                                         , externalFunctionAttrs :: [FunctionAttribute]
                                         }

instance Show ExternalFunction where
  show = show . externalFunctionName

instance IsValue ExternalFunction where
  valueType = externalFunctionType
  valueName = Just . externalFunctionName
  valueMetadata = externalFunctionMetadata
  valueContent = ExternalFunctionC
  valueUniqueId = externalFunctionUniqueId

instance Eq ExternalFunction where
  f1 == f2 = externalFunctionUniqueId f1 == externalFunctionUniqueId f2

instance Hashable ExternalFunction where
  hash = fromIntegral . externalFunctionUniqueId

instance Ord ExternalFunction where
  f1 `compare` f2 = comparing externalFunctionUniqueId f1 f2

-- | Determine if an instruction is a Terminator instruction (i.e.,
-- ends a BasicBlock)
instructionIsTerminator :: Instruction -> Bool
instructionIsTerminator RetInst {} = True
instructionIsTerminator UnconditionalBranchInst {} = True
instructionIsTerminator BranchInst {} = True
instructionIsTerminator SwitchInst {} = True
instructionIsTerminator IndirectBranchInst {} = True
instructionIsTerminator UnwindInst {} = True
instructionIsTerminator UnreachableInst {} = True
instructionIsTerminator InvokeInst {} = True
instructionIsTerminator _ = False
-- Note, the new ResumeInst needs to be handled

data Instruction = RetInst { instructionType :: Type
                           , instructionName :: !(Maybe Identifier)
                           , instructionMetadata :: [Metadata]
                           , instructionUniqueId :: !UniqueId
                           , instructionBasicBlock :: Maybe BasicBlock
                           , retInstValue :: Maybe Value
                           }
                 | UnconditionalBranchInst { instructionType :: Type
                                           , instructionName :: !(Maybe Identifier)
                                           , instructionMetadata :: [Metadata]
                                           , instructionUniqueId :: !UniqueId
                                           , instructionBasicBlock :: Maybe BasicBlock
                                           , unconditionalBranchTarget :: BasicBlock
                                           }
                 | BranchInst { instructionType :: Type
                              , instructionName :: !(Maybe Identifier)
                              , instructionMetadata :: [Metadata]
                              , instructionUniqueId :: !UniqueId
                              , instructionBasicBlock :: Maybe BasicBlock
                              , branchCondition :: Value
                              , branchTrueTarget :: BasicBlock
                              , branchFalseTarget :: BasicBlock
                              }
                 | SwitchInst { instructionType :: Type
                              , instructionName :: !(Maybe Identifier)
                              , instructionMetadata :: [Metadata]
                              , instructionUniqueId :: !UniqueId
                              , instructionBasicBlock :: Maybe BasicBlock
                              , switchValue :: Value
                              , switchDefaultTarget :: BasicBlock
                              , switchCases :: [(Value, BasicBlock)]
                              }
                 | IndirectBranchInst { instructionType :: Type
                                      , instructionName :: !(Maybe Identifier)
                                      , instructionMetadata :: [Metadata]
                                      , instructionUniqueId :: !UniqueId
                                      , instructionBasicBlock :: Maybe BasicBlock
                                      , indirectBranchAddress :: Value
                                      , indirectBranchTargets :: [BasicBlock]
                                      }
                   -- ^ The target must be derived from a blockaddress constant
                   -- The list is a list of possible target destinations
                 | UnwindInst { instructionType :: Type
                              , instructionName :: !(Maybe Identifier)
                              , instructionMetadata :: [Metadata]
                              , instructionUniqueId :: !UniqueId
                              , instructionBasicBlock :: Maybe BasicBlock
                              }
                 | UnreachableInst { instructionType :: Type
                                   , instructionName :: !(Maybe Identifier)
                                   , instructionMetadata :: [Metadata]
                                   , instructionUniqueId :: !UniqueId
                                   , instructionBasicBlock :: Maybe BasicBlock
                                   }
                 | ExtractElementInst { instructionType :: Type
                                      , instructionName :: !(Maybe Identifier)
                                      , instructionMetadata :: [Metadata]
                                      , instructionUniqueId :: !UniqueId
                                      , instructionBasicBlock :: Maybe BasicBlock
                                      , extractElementVector :: Value
                                      , extractElementIndex :: Value
                                      }
                 | InsertElementInst { instructionType :: Type
                                     , instructionName :: !(Maybe Identifier)
                                     , instructionMetadata :: [Metadata]
                                     , instructionUniqueId :: !UniqueId
                                     , instructionBasicBlock :: Maybe BasicBlock
                                     , insertElementVector :: Value
                                     , insertElementValue :: Value
                                     , insertElementIndex :: Value
                                     }
                 | ShuffleVectorInst { instructionType :: Type
                                     , instructionName :: !(Maybe Identifier)
                                     , instructionMetadata :: [Metadata]
                                     , instructionUniqueId :: !UniqueId
                                     , instructionBasicBlock :: Maybe BasicBlock
                                     , shuffleVectorV1 :: Value
                                     , shuffleVectorV2 :: Value
                                     , shuffleVectorMask :: Value
                                     }
                 | ExtractValueInst { instructionType :: Type
                                    , instructionName :: !(Maybe Identifier)
                                    , instructionMetadata :: [Metadata]
                                    , instructionUniqueId :: !UniqueId
                                    , instructionBasicBlock :: Maybe BasicBlock
                                    , extractValueAggregate :: Value
                                    , extractValueIndices :: [Int]
                                    }
                 | InsertValueInst { instructionType :: Type
                                   , instructionName :: !(Maybe Identifier)
                                   , instructionMetadata :: [Metadata]
                                   , instructionUniqueId :: !UniqueId
                                   , instructionBasicBlock :: Maybe BasicBlock
                                   , insertValueAggregate :: Value
                                   , insertValueValue :: Value
                                   , insertValueIndices :: [Int]
                                   }
                 | AllocaInst { instructionType :: Type
                              , instructionName :: !(Maybe Identifier)
                              , instructionMetadata :: [Metadata]
                              , instructionUniqueId :: !UniqueId
                              , instructionBasicBlock :: Maybe BasicBlock
                              , allocaNumElements :: Value
                              , allocaAlign :: !Int64
                              }
                 | LoadInst { instructionType :: Type
                            , instructionName :: !(Maybe Identifier)
                            , instructionMetadata :: [Metadata]
                            , instructionUniqueId :: !UniqueId
                            , instructionBasicBlock :: Maybe BasicBlock
                            , loadIsVolatile :: !Bool
                            , loadAddress :: Value
                            , loadAlignment :: !Int64
                            }
                 | StoreInst { instructionType :: Type
                             , instructionName :: !(Maybe Identifier)
                             , instructionMetadata :: [Metadata]
                             , instructionUniqueId :: !UniqueId
                             , instructionBasicBlock :: Maybe BasicBlock
                             , storeIsVolatile :: !Bool
                             , storeValue :: Value
                             , storeAddress :: Value
                             , storeAlignment :: !Int64
                             , storeAddressSpace :: !Int
                             }
                 | AddInst { instructionType :: Type
                           , instructionName :: !(Maybe Identifier)
                           , instructionMetadata :: [Metadata]
                           , instructionUniqueId :: !UniqueId
                           , instructionBasicBlock :: Maybe BasicBlock
                           , binaryArithFlags :: !ArithFlags
                           , binaryLhs :: Value
                           , binaryRhs :: Value
                           }
                 | SubInst { instructionType :: Type
                           , instructionName :: !(Maybe Identifier)
                           , instructionMetadata :: [Metadata]
                           , instructionUniqueId :: !UniqueId
                           , instructionBasicBlock :: Maybe BasicBlock
                           , binaryArithFlags :: !ArithFlags
                           , binaryLhs :: Value
                           , binaryRhs :: Value
                           }
                 | MulInst { instructionType :: Type
                           , instructionName :: !(Maybe Identifier)
                           , instructionMetadata :: [Metadata]
                           , instructionUniqueId :: !UniqueId
                           , instructionBasicBlock :: Maybe BasicBlock
                           , binaryArithFlags :: !ArithFlags
                           , binaryLhs :: Value
                           , binaryRhs :: Value
                           }
                 | DivInst { instructionType :: Type
                           , instructionName :: !(Maybe Identifier)
                           , instructionMetadata :: [Metadata]
                           , instructionUniqueId :: !UniqueId
                           , instructionBasicBlock :: Maybe BasicBlock
                           , binaryLhs :: Value
                           , binaryRhs :: Value
                           }
                 | RemInst { instructionType :: Type
                           , instructionName :: !(Maybe Identifier)
                           , instructionMetadata :: [Metadata]
                           , instructionUniqueId :: !UniqueId
                           , instructionBasicBlock :: Maybe BasicBlock
                           , binaryLhs :: Value
                           , binaryRhs :: Value
                           }
                 | ShlInst { instructionType :: Type
                           , instructionName :: !(Maybe Identifier)
                           , instructionMetadata :: [Metadata]
                           , instructionUniqueId :: !UniqueId
                           , instructionBasicBlock :: Maybe BasicBlock
                           , binaryLhs :: Value
                           , binaryRhs :: Value
                           }
                 | LshrInst { instructionType :: Type
                            , instructionName :: !(Maybe Identifier)
                            , instructionMetadata :: [Metadata]
                            , instructionUniqueId :: !UniqueId
                            , instructionBasicBlock :: Maybe BasicBlock
                            , binaryLhs :: Value
                            , binaryRhs :: Value
                            }
                 | AshrInst { instructionType :: Type
                            , instructionName :: !(Maybe Identifier)
                            , instructionMetadata :: [Metadata]
                            , instructionUniqueId :: !UniqueId
                            , instructionBasicBlock :: Maybe BasicBlock
                            , binaryLhs :: Value
                            , binaryRhs :: Value
                           }
                 | AndInst { instructionType :: Type
                           , instructionName :: !(Maybe Identifier)
                           , instructionMetadata :: [Metadata]
                           , instructionUniqueId :: !UniqueId
                           , instructionBasicBlock :: Maybe BasicBlock
                           , binaryLhs :: Value
                           , binaryRhs :: Value
                           }
                 | OrInst { instructionType :: Type
                          , instructionName :: !(Maybe Identifier)
                          , instructionMetadata :: [Metadata]
                          , instructionUniqueId :: !UniqueId
                          , instructionBasicBlock :: Maybe BasicBlock
                          , binaryLhs :: Value
                          , binaryRhs :: Value
                          }
                 | XorInst { instructionType :: Type
                           , instructionName :: !(Maybe Identifier)
                           , instructionMetadata :: [Metadata]
                           , instructionUniqueId :: !UniqueId
                           , instructionBasicBlock :: Maybe BasicBlock
                           , binaryLhs :: Value
                           , binaryRhs :: Value
                           }
                 | TruncInst { instructionType :: Type
                             , instructionName :: !(Maybe Identifier)
                             , instructionMetadata :: [Metadata]
                             , instructionUniqueId :: !UniqueId
                             , instructionBasicBlock :: Maybe BasicBlock
                             , castedValue :: Value
                             }
                 | ZExtInst { instructionType :: Type
                            , instructionName :: !(Maybe Identifier)
                            , instructionMetadata :: [Metadata]
                            , instructionUniqueId :: !UniqueId
                            , instructionBasicBlock :: Maybe BasicBlock
                            , castedValue :: Value
                            }
                 | SExtInst { instructionType :: Type
                            , instructionName :: !(Maybe Identifier)
                            , instructionMetadata :: [Metadata]
                            , instructionUniqueId :: !UniqueId
                            , instructionBasicBlock :: Maybe BasicBlock
                            , castedValue :: Value
                            }
                 | FPTruncInst { instructionType :: Type
                               , instructionName :: !(Maybe Identifier)
                               , instructionMetadata :: [Metadata]
                               , instructionUniqueId :: !UniqueId
                               , instructionBasicBlock :: Maybe BasicBlock
                               , castedValue :: Value
                               }
                 | FPExtInst { instructionType :: Type
                             , instructionName :: !(Maybe Identifier)
                             , instructionMetadata :: [Metadata]
                             , instructionUniqueId :: !UniqueId
                             , instructionBasicBlock :: Maybe BasicBlock
                             , castedValue :: Value
                             }
                 | FPToSIInst { instructionType :: Type
                              , instructionName :: !(Maybe Identifier)
                              , instructionMetadata :: [Metadata]
                              , instructionUniqueId :: !UniqueId
                              , instructionBasicBlock :: Maybe BasicBlock
                              , castedValue :: Value
                              }
                 | FPToUIInst { instructionType :: Type
                              , instructionName :: !(Maybe Identifier)
                              , instructionMetadata :: [Metadata]
                              , instructionUniqueId :: !UniqueId
                              , instructionBasicBlock :: Maybe BasicBlock
                              , castedValue :: Value
                              }
                 | SIToFPInst { instructionType :: Type
                              , instructionName :: !(Maybe Identifier)
                              , instructionMetadata :: [Metadata]
                              , instructionUniqueId :: !UniqueId
                              , instructionBasicBlock :: Maybe BasicBlock
                              , castedValue :: Value
                              }
                 | UIToFPInst { instructionType :: Type
                              , instructionName :: !(Maybe Identifier)
                              , instructionMetadata :: [Metadata]
                              , instructionUniqueId :: !UniqueId
                              , instructionBasicBlock :: Maybe BasicBlock
                              , castedValue :: Value
                              }
                 | PtrToIntInst { instructionType :: Type
                                , instructionName :: !(Maybe Identifier)
                                , instructionMetadata :: [Metadata]
                                , instructionUniqueId :: !UniqueId
                                , instructionBasicBlock :: Maybe BasicBlock
                                , castedValue :: Value
                                }
                 | IntToPtrInst { instructionType :: Type
                                , instructionName :: !(Maybe Identifier)
                                , instructionMetadata :: [Metadata]
                                , instructionUniqueId :: !UniqueId
                                , instructionBasicBlock :: Maybe BasicBlock
                                , castedValue :: Value
                                }
                 | BitcastInst { instructionType :: Type
                               , instructionName :: !(Maybe Identifier)
                               , instructionMetadata :: [Metadata]
                               , instructionUniqueId :: !UniqueId
                               , instructionBasicBlock :: Maybe BasicBlock
                               , castedValue :: Value
                               }
                 | ICmpInst { instructionType :: Type
                            , instructionName :: !(Maybe Identifier)
                            , instructionMetadata :: [Metadata]
                            , instructionUniqueId :: !UniqueId
                            , instructionBasicBlock :: Maybe BasicBlock
                            , cmpPredicate :: !CmpPredicate
                            , cmpV1 :: Value
                            , cmpV2 :: Value
                            }
                 | FCmpInst { instructionType :: Type
                            , instructionName :: !(Maybe Identifier)
                            , instructionMetadata :: [Metadata]
                            , instructionUniqueId :: !UniqueId
                            , instructionBasicBlock :: Maybe BasicBlock
                            , cmpPredicate :: !CmpPredicate
                            , cmpV1 :: Value
                            , cmpV2 :: Value
                            }
                 | SelectInst { instructionType :: Type
                              , instructionName :: !(Maybe Identifier)
                              , instructionMetadata :: [Metadata]
                              , instructionUniqueId :: !UniqueId
                              , instructionBasicBlock :: Maybe BasicBlock
                              , selectCondition :: Value
                              , selectTrueValue :: Value
                              , selectFalseValue :: Value
                              }
                 | CallInst { instructionType :: Type
                            , instructionName :: !(Maybe Identifier)
                            , instructionMetadata :: [Metadata]
                            , instructionUniqueId :: !UniqueId
                            , instructionBasicBlock :: Maybe BasicBlock
                            , callIsTail :: !Bool
                            , callConvention :: !CallingConvention
                            , callParamAttrs :: [ParamAttribute]
                            , callFunction :: Value
                            , callArguments :: [(Value, [ParamAttribute])]
                            , callAttrs :: [FunctionAttribute]
                            , callHasSRet :: !Bool
                            }
                 | GetElementPtrInst { instructionType :: Type
                                     , instructionName :: !(Maybe Identifier)
                                     , instructionMetadata :: [Metadata]
                                     , instructionUniqueId :: !UniqueId
                                     , instructionBasicBlock :: Maybe BasicBlock
                                     , getElementPtrInBounds :: !Bool
                                     , getElementPtrValue :: Value
                                     , getElementPtrIndices :: [Value]
                                     , getElementPtrAddrSpace :: !Int
                                     }
                 | InvokeInst { instructionType :: Type
                              , instructionName :: !(Maybe Identifier)
                              , instructionMetadata :: [Metadata]
                              , instructionUniqueId :: !UniqueId
                              , instructionBasicBlock :: Maybe BasicBlock
                              , invokeConvention :: !CallingConvention
                              , invokeParamAttrs :: [ParamAttribute]
                              , invokeFunction :: Value
                              , invokeArguments :: [(Value, [ParamAttribute])]
                              , invokeAttrs :: [FunctionAttribute]
                              , invokeNormalLabel :: BasicBlock
                              , invokeUnwindLabel :: BasicBlock
                              , invokeHasSRet :: !Bool
                              }
                 | VaArgInst { instructionType :: Type
                             , instructionName :: !(Maybe Identifier)
                             , instructionMetadata :: [Metadata]
                             , instructionUniqueId :: !UniqueId
                             , instructionBasicBlock :: Maybe BasicBlock
                             , vaArgValue :: Value
                             }
                 | PhiNode { instructionType :: Type
                           , instructionName :: !(Maybe Identifier)
                           , instructionMetadata :: [Metadata]
                           , instructionUniqueId :: !UniqueId
                           , instructionBasicBlock :: Maybe BasicBlock
                           , phiIncomingValues :: [(Value, Value)]
                           }
instance IsValue Instruction where
  valueType = instructionType
  valueName = instructionName
  valueMetadata = instructionMetadata
  valueContent = InstructionC
  valueUniqueId = instructionUniqueId

instance Eq Instruction where
  i1 == i2 = instructionUniqueId i1 == instructionUniqueId i2

instance Hashable Instruction where
  hash = fromIntegral . instructionUniqueId

instance Ord Instruction where
  i1 `compare` i2 = comparing instructionUniqueId i1 i2

data Constant = UndefValue { constantType :: Type
                           , constantUniqueId :: !UniqueId
                           }
              | ConstantAggregateZero { constantType :: Type
                                      , constantUniqueId :: !UniqueId
                                      }
              | ConstantPointerNull { constantType :: Type
                                    , constantUniqueId :: !UniqueId
                                    }
              | BlockAddress { constantType :: Type
                             , constantUniqueId :: !UniqueId
                             , blockAddressFunction :: Function
                             , blockAddressBlock :: BasicBlock
                             }
              | ConstantArray { constantType :: Type
                              , constantUniqueId :: !UniqueId
                              , constantArrayValues :: [Value]
                              }
              | ConstantFP { constantType :: Type
                           , constantUniqueId :: !UniqueId
                           , constantFPValue :: !Double
                           }
              | ConstantInt { constantType :: Type
                            , constantUniqueId :: !UniqueId
                            , constantIntValue :: !Integer
                            }
              | ConstantString { constantType :: Type
                               , constantUniqueId :: !UniqueId
                               , constantStringValue :: !ByteString
                               }
              | ConstantStruct { constantType :: Type
                               , constantUniqueId :: !UniqueId
                               , constantStructValues :: [Value]
                               }
              | ConstantVector { constantType :: Type
                               , constantUniqueId :: !UniqueId
                               , constantVectorValues :: [Value]
                               }
              | ConstantValue { constantType :: Type
                              , constantUniqueId :: !UniqueId
                              , constantInstruction :: Instruction
                              }
              | InlineAsm { constantType :: Type
                          , constantUniqueId :: !UniqueId
                          , inlineAsmString :: !ByteString
                          , inlineAsmConstraints :: !ByteString
                          }

instance IsValue Constant where
  valueType = constantType
  valueName _ = Nothing
  valueMetadata _ = []
  valueContent = ConstantC
  valueUniqueId = constantUniqueId

instance Eq Constant where
  c1 == c2 = constantUniqueId c1 == constantUniqueId c2

instance Hashable Constant where
  hash = fromIntegral . constantUniqueId

instance Ord Constant where
  c1 `compare` c2 = comparing constantUniqueId c1 c2

-- Functions have parameters if they are not external
data ValueContent = FunctionC Function
                  | ArgumentC Argument
                  | BasicBlockC BasicBlock
                  | GlobalVariableC GlobalVariable
                  | GlobalAliasC GlobalAlias
                  | ExternalValueC ExternalValue
                  | ExternalFunctionC ExternalFunction
                  | InstructionC Instruction
                  | ConstantC Constant


