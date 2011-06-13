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

import Control.DeepSeq
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
          | TypeArray !Int !Type
          | TypeVector !Int !Type
          | TypeFunction !Type [Type] !Bool -- Return type, arg types, vararg
          | TypeOpaque
          | TypePointer !Type -- (Maybe Int) -- Address Space
          | TypeStruct [Type]
          | TypePackedStruct [Type]
          | TypeNamed !String !Type
          deriving (Ord, Eq)

instance NFData Type where
  rnf t@(TypeInteger i) = i `deepseq` t `deepseq` ()
  rnf t@(TypeArray i t') = t' `deepseq` i `seq` t `seq` ()
  rnf t@(TypeVector i t') = t' `deepseq` i `seq` t `seq` ()
  rnf t@(TypeFunction r ts b) = r `deepseq` ts `deepseq` b `seq` t `seq` ()
  rnf t@(TypePointer t') = t' `deepseq` t `seq` ()
  rnf t@(TypeStruct ts) = ts `deepseq` t `seq` ()
  rnf t@(TypePackedStruct ts) = ts `deepseq` t `seq` ()
  rnf t@(TypeNamed s t') = t' `deepseq` s `seq` t `seq` ()
  rnf t = t `seq` ()

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
    -- ^ A reference to a 'Value' in metadata
  | MetadataDiscarded
    -- ^ Metadata that has been discarded due to the parser
    -- configuration
  | MetadataUnknown
    -- ^ Unrecognized type of metadata
  deriving (Ord, Eq)

instance NFData MetadataT where
  rnf m@(MetaSourceLocation {}) = metaSourceScope m `deepseq` m `seq` ()
  rnf m@(MetaDWLexicalBlock {}) = metaLexicalBlockContext m `deepseq`
                                   metaLexicalBlockFile m `deepseq` m `seq` ()
  rnf m@(MetaDWCompileUnit {}) = metaCompileUnitSourceFile m `seq`
                                  metaCompileUnitCompileDir m `seq`
                                  metaCompileUnitProducer m `seq`
                                  metaCompileUnitFlags m `seq` m `seq` ()
  rnf m@(MetaDWFile {}) = metaFileCompileUnit m `deepseq`
                          metaFileSourceFile m `seq`
                          metaFileSourceDir m `seq` m `seq` ()
  rnf m@(MetaDWVariable {}) = metaGlobalVarContext m `deepseq`
                               metaGlobalVarFile m `deepseq`
                               metaGlobalVarType m `deepseq`
                               metaGlobalVarName m `seq`
                               metaGlobalVarDisplayName m `seq`
                               metaGlobalVarLinkageName m `seq` m `seq` ()
  rnf m@(MetaDWSubprogram {}) = metaSubprogramContext m `deepseq`
                                 metaSubprogramFile m `deepseq`
                                 metaSubprogramType m `deepseq`
                                 metaSubprogramBaseType m `deepseq`
                                 metaSubprogramName m `seq`
                                 metaSubprogramDisplayName m `seq`
                                 metaSubprogramLinkageName m `seq` m `seq` ()
  rnf m@(MetaDWBaseType {}) = metaBaseTypeContext m `deepseq`
                               metaBaseTypeFile m `deepseq`
                               metaBaseTypeName m `seq` m `seq` ()
  rnf m@(MetaDWDerivedType {}) = metaDerivedTypeContext m `deepseq`
                                  metaDerivedTypeFile m `deepseq`
                                  metaDerivedTypeParent m `deepseq`
                                  metaDerivedTypeName m `seq` m `seq` ()
  rnf m@(MetaDWCompositeType {}) = metaCompositeTypeContext m `deepseq`
                                    metaCompositeTypeFile m `deepseq`
                                    metaCompositeTypeParent m `deepseq`
                                    metaCompositeTypeMembers m `deepseq`
                                    metaCompositeTypeName m `seq` m `seq` ()
  rnf m@(MetaDWSubrange {}) = m `seq` ()
  rnf m@(MetaDWEnumerator {}) = metaEnumeratorName m `seq` m `seq` ()
  rnf m@(MetaDWLocal {}) = metaLocalContext m `deepseq`
                            metaLocalFile m `deepseq`
                            metaLocalType m `deepseq`
                            metaLocalName m `seq` m `seq` ()
  rnf m@(MetadataList ms) = ms `deepseq` m `seq` ()
  rnf m@(MetadataValueConstant v) = v `deepseq` m `seq` ()
  rnf m@MetadataDiscarded = m `seq` ()
  rnf m@MetadataUnknown = m `seq` ()

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
  hash md = metaValueUniqueId md

instance NFData Metadata where
  rnf m = metaValueName m `deepseq` metaValueContent m `seq` m `seq` ()

-- | A wrapper around 'ValueT' values that tracks the 'Type', name,
-- and attached metadata. valueName is mostly informational at this
-- point.  All references will be resolved as part of the graph, but
-- the name will be useful for visualization purposes and
-- serialization.
data Value = Value { valueType :: Type
                   , valueName :: Maybe Identifier
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

instance NFData Value where
  rnf v = valueType v `deepseq` valueName v `deepseq` valueMetadata v `deepseq` valueContent v `deepseq` ()

-- Functions have parameters if they are not external
data ValueT = Function { functionType :: Type
                       , functionParameters :: [Value] -- A list of arguments
                       , functionBody :: [Value] -- A list of basic blocks
                       , functionLinkage :: !LinkageType
                       , functionVisibility :: !VisibilityStyle
                       , functionCC :: !CallingConvention
                       , functionRetAttrs :: [ParamAttribute]
                       , functionAttrs :: [FunctionAttribute]
                       , functionName :: Identifier
                       , functionSection :: Maybe ByteString
                       , functionAlign :: !Int64
                       , functionGCName :: Maybe GCName
                       , functionIsVararg :: !Bool
                       }
            | GlobalDeclaration { globalVariableAddressSpace :: !Int
                                , globalVariableLinkage :: !LinkageType
                                , globalVariableAnnotation :: !GlobalAnnotation
                                , globalVariableInitializer :: Maybe Value
                                , globalVariableAlignment :: !Int64
                                , globalVariableSection :: Maybe ByteString
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
            | AllocaInst Type Value !Int64 -- Type, NumElems, align
            | LoadInst !Bool Value !Int64 -- Volatile? Type Dest align
            | StoreInst !Bool Value Value !Int64 -- Volatile? Val Dest align
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
            | ICmpInst !ICmpCondition Value Value
            | FCmpInst !FCmpCondition Value Value
            | PhiNode [(Value, Value)]
            | SelectInst Value Value Value
            | GetElementPtrInst { getElementPtrInBounds :: !Bool
                                , getElementPtrValue :: Value
                                , getElementPtrIndices :: [Value]
                                }
            | CallInst { callIsTail :: !Bool
                       , callConvention :: !CallingConvention
                       , callParamAttrs :: [ParamAttribute]
                       , callRetType :: Type
                       , callFunction :: Value
                       , callArguments :: [(Value, [ParamAttribute])]
                       , callAttrs :: [FunctionAttribute]
                       , callHasSRet :: !Bool
                       }
            | InvokeInst { invokeConvention :: !CallingConvention
                         , invokeParamAttrs :: [ParamAttribute]
                         , invokeRetType :: Type
                         , invokeFunction :: Value
                         , invokeArguments :: [(Value, [ParamAttribute])]
                         , invokeAttrs :: [FunctionAttribute]
                         , invokeNormalLabel :: Value
                         , invokeUnwindLabel :: Value
                         , invokeHasSRet :: !Bool
                         }
            | VaArgInst Value Type
            | UndefValue
            | BlockAddress Value Value -- Function, block -- type i8*, constant
            | ConstantAggregateZero
            | ConstantArray [Value]
            | ConstantFP !Double
            | ConstantInt !Integer
            | ConstantString ByteString
            | ConstantPointerNull
            | ConstantStruct [Value]
            | ConstantVector [Value]
            | ConstantValue ValueT
            | InlineAsm ByteString ByteString
            deriving (Ord, Eq)

instance NFData ValueT where
  rnf f@(Function {}) = functionType f `deepseq` functionParameters f `deepseq`
                          functionBody f `deepseq` functionLinkage f `deepseq`
                          functionVisibility f `deepseq` functionCC f `deepseq`
                          functionRetAttrs f `deepseq` functionAttrs f `deepseq`
                          functionName f `deepseq` functionSection f `seq`
                          functionAlign f `deepseq` functionGCName f `deepseq`
                          functionIsVararg f `deepseq` ()
  rnf g@(GlobalDeclaration {}) = globalVariableInitializer g `deepseq` globalVariableSection g `seq` ()
  rnf g@(GlobalAlias {}) = globalAliasValue g `deepseq` ()
  rnf v@ExternalValue = v `seq` ()
  rnf e@(ExternalFunction atts) = atts `deepseq` e `seq` ()
  rnf b@(BasicBlock vals) = vals `deepseq` b `seq` ()
  rnf a@(Argument atts) = atts `deepseq` a `seq` ()
  rnf r@(RetInst mv) = mv `deepseq` r `seq` ()
  rnf u@(UnconditionalBranchInst v) = v `deepseq` u `seq` ()
  rnf b@(BranchInst {}) = branchCondition b `deepseq` branchTrueTarget b `deepseq`
                            branchFalseTarget b `deepseq` b `seq` ()
  rnf s@(SwitchInst {}) =  switchValue s `deepseq` switchDefaultTarget s `deepseq`
                             switchCases s `deepseq` s `seq` ()

  rnf i@(IndirectBranchInst {}) = indirectBranchAddress i `deepseq` indirectBranchTargets i `deepseq`
                                    i `seq` ()
  rnf v@UnwindInst = v `seq` ()
  rnf v@UnreachableInst = v `seq` ()
  rnf i@(AddInst flags v1 v2) = flags `deepseq` v1 `deepseq` v2 `deepseq` i `seq` ()
  rnf i@(SubInst flags v1 v2) = flags `deepseq` v1 `deepseq` v2 `deepseq` i `seq` ()
  rnf i@(MulInst flags v1 v2) = flags `deepseq` v1 `deepseq` v2 `deepseq` i `seq` ()
  rnf i@(DivInst v1 v2) = v1 `deepseq` v2 `deepseq` i `seq` ()
  rnf i@(RemInst v1 v2) = v1 `deepseq` v2 `deepseq` i `seq` ()
  rnf i@(ShlInst v1 v2) = v1 `deepseq` v2 `deepseq` i `seq` ()
  rnf i@(LshrInst v1 v2) = v1 `deepseq` v2 `deepseq` i `seq` ()
  rnf i@(AndInst v1 v2) = v1 `deepseq` v2 `deepseq` i `seq` ()
  rnf i@(OrInst v1 v2) = v1 `deepseq` v2 `deepseq` i `seq` ()
  rnf i@(XorInst v1 v2) = v1 `deepseq` v2 `deepseq` i `seq` ()
  rnf i@(ExtractElementInst {}) = extractElementVector i `deepseq` extractElementIndex i `deepseq` i `seq` ()
  rnf i@(InsertElementInst {}) = insertElementVector i `deepseq`
                                  insertElementValue i `deepseq`
                                  insertElementIndex i `deepseq` i `seq` ()
  rnf i@(ShuffleVectorInst {}) = shuffleVectorV1 i `deepseq`
                                  shuffleVectorV2 i `deepseq`
                                  shuffleVectorMask i `deepseq` i `seq` ()
  rnf i@(ExtractValueInst {}) = extractValueAggregate i `deepseq`
                                 extractValueIndices i `deepseq` i `seq` ()
  rnf i@(InsertValueInst {}) = insertValueAggregate i `deepseq`
                                insertValueValue i `deepseq`
                                insertValueIndices i `deepseq` i `seq` ()
  rnf i@(AllocaInst t v _) = t `deepseq` v `deepseq` i `seq` ()
  rnf i@(LoadInst _ v _) = v `deepseq` i `seq` ()
  rnf i@(StoreInst _ v1 v2 _) = v1 `deepseq` v2 `deepseq` i `seq` ()
  rnf i@(TruncInst v t) = v `deepseq` t `deepseq` i `seq` ()
  rnf i@(ZExtInst v t) = v `deepseq` t `deepseq` i `seq` ()
  rnf i@(SExtInst v t) = v `deepseq` t `deepseq` i `seq` ()
  rnf i@(FPTruncInst v t) = v `deepseq` t `deepseq` i `seq` ()
  rnf i@(FPExtInst v t) = v `deepseq` t `deepseq` i `seq` ()
  rnf i@(FPToUIInst v t) = v `deepseq` t `deepseq` i `seq` ()
  rnf i@(FPToSIInst v t) = v `deepseq` t `deepseq` i `seq` ()
  rnf i@(UIToFPInst v t) = v `deepseq` t `deepseq` i `seq` ()
  rnf i@(SIToFPInst v t) = v `deepseq` t `deepseq` i `seq` ()
  rnf i@(PtrToIntInst v t) = v `deepseq` t `deepseq` i `seq` ()
  rnf i@(IntToPtrInst v t) = v `deepseq` t `deepseq` i `seq` ()
  rnf i@(BitcastInst v t) = v `deepseq` t `deepseq` i `seq` ()
  rnf i@(ICmpInst _ v1 v2) = v1 `deepseq` v2 `deepseq` i `seq` ()
  rnf i@(FCmpInst _ v1 v2) = v1 `deepseq` v2 `deepseq` i `seq` ()
  rnf i@(PhiNode vs) = vs `deepseq` i `seq` ()
  rnf i@(SelectInst v1 v2 v3) = v1 `deepseq` v2 `deepseq` v3 `deepseq` i `seq` ()
  rnf i@(GetElementPtrInst {}) = getElementPtrValue i `deepseq` getElementPtrIndices i `deepseq` i `seq` ()
  rnf i@(CallInst {}) = callParamAttrs i `deepseq` callRetType i `deepseq`
                          callFunction i `deepseq` callArguments i `deepseq`
                          callAttrs i `deepseq` i `seq` ()
  rnf i@(InvokeInst {}) =  invokeParamAttrs i `deepseq` invokeRetType i `deepseq`
                             invokeFunction i `deepseq` invokeArguments i `deepseq`
                             invokeAttrs i `deepseq` invokeNormalLabel i `deepseq`
                             invokeUnwindLabel i `deepseq` i `seq` ()
  rnf i@(VaArgInst v t) = v `deepseq` t `deepseq` i `seq` ()
  rnf i@UndefValue = i `seq` ()
  rnf i@(BlockAddress v1 v2) = v1 `deepseq` v2 `deepseq` i `seq` ()
  rnf i@ConstantAggregateZero = i `seq` ()
  rnf i@(ConstantArray vs) = vs `deepseq` i `seq` ()
  rnf i@(ConstantFP n) = n `seq` i `seq` ()
  rnf i@(ConstantInt n) = n `seq` i `seq` ()
  rnf i@(ConstantString s) = s `seq` i `seq` ()
  rnf i@ConstantPointerNull = i `seq` ()
  rnf i@(ConstantStruct vs) = vs `deepseq` i `seq` ()
  rnf i@(ConstantVector vs) = vs `deepseq` i `seq` ()
  rnf i@(ConstantValue v) = v `deepseq` i `seq` ()
  rnf i@(InlineAsm s1 s2) = s1 `seq` s2 `seq` i `seq` ()


valueIsFunction :: Value -> Bool
valueIsFunction Value { valueContent = Function {} } = True
valueIsFunction _ = False

