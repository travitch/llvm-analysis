module Data.LLVM.Types ( Module(..)
                       , Metadata(..)
                       , Type(..)
                       , Value(..)
                       , ValueT(..)
                       , isExternalFunction
                       , functionAttributes
                       ) where

import Data.Dwarf
import Data.Text (Text)

import Data.LLVM.Private.AttributeTypes
import Data.LLVM.Private.DwarfHelpers

data Module = Module { moduleDataLayout :: DataLayout
                     , moduleTarget :: TargetTriple
                     , moduleAssembly :: [Assembly]
                     , moduleGlobals :: [Value]
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
          | TypeFunction Type [Type] Bool [FunctionAttribute] -- Return type, arg types, vararg
          | TypeOpaque
          | TypePointer Type -- (Maybe Int) -- Address Space
          | TypeStruct [Type]
          | TypePackedStruct [Type]
          deriving (Show, Eq)


data Metadata =
  MetaSourceLocation { metaSourceRow :: Integer
                     , metaSourceCol :: Integer
                     , metaSourceScope :: Metadata
                     }
  | MetaNewValue Value
  | MetaDWLexicalBlock { metaLexicalBlockRow :: Integer
                       , metaLexicalBlockCol :: Integer
                       , metaLexicalBlockContext :: Metadata
                       }
  | MetaDWAutoVariable
  | MetaDWCompileUnit { metaCompileUnitLanguage :: DW_LANG
                      , metaCompileUnitSourceFile :: Text
                      , metaCompileUnitCompileDir :: Text
                      , metaCompileUnitProducer :: Text
                      , metaCompileUnitIsMain :: Bool
                      , metaCompileUnitIsOpt :: Bool
                      , metaCompileUnitFlags :: Text
                      , metaCompileUnitVersion :: Integer
                      }
  | MetaDWFile { metaFileSourceFile :: Text
               , metaFileSourceDir :: Text
               , metaFileCompileUnit :: Metadata
               }
  | MetaDWVariable { metaGlobalVarContext :: Metadata
                   , metaGlobalVarName :: Text
                   , metaGlobalVarDisplayName :: Text
                   , metaGlobalVarLinkageName :: Text
                   , metaGlobalVarFile :: Metadata
                   , metaGlobalVarLine :: Integer
                   , metaGlobalVarType :: Metadata
                   , metaGlobalVarStatic :: Bool
                   , metaGlobalVarNotExtern :: Bool
                   -- , metaGlobalVarRef :: Value
                   }
  | MetaDWSubprogram { metaSubprogramContext :: Metadata
                     , metaSubprogramName :: Text
                     , metaSubprogramDisplayName :: Text
                     , metaSubprogramLinkageName :: Text
                     , metaSubprogramFile :: Metadata
                     , metaSubprogramLine :: Integer
                     , metaSubprogramType :: Metadata
                     , metaSubprogramStatic :: Bool
                     , metaSubprogramNotExtern :: Bool
                     , metaSubprogramVirtuality :: DW_VIRTUALITY
                     , metaSubprogramVirtIndex :: Integer
                     , metaSubprogramBaseType :: Maybe Metadata
                     , metaSubprogramArtificial :: Bool
                     , metaSubprogramOptimized :: Bool
                     -- , metaSubprogramFunction :: Value
                     }
  | MetaDWBaseType { metaBaseTypeContext :: Metadata
                   , metaBaseTypeName :: Text
                   , metaBaseTypeFile :: Maybe Metadata
                   , metaBaseTypeLine :: Integer
                   , metaBaseTypeSize :: Integer
                   , metaBaseTypeAlign :: Integer
                   , metaBaseTypeOffset :: Integer
                   , metaBaseTypeFlags :: Integer
                   , metaBaseTypeEncoding :: DW_ATE
                   }
  | MetaDWDerivedType { metaDerivedTypeTag :: DW_TAG
                      , metaDerivedTypeContext :: Metadata
                      , metaDerivedTypeName :: Text
                      , metaDerivedTypeFile :: Maybe Metadata
                      , metaDerivedTypeLine :: Integer
                      , metaDerivedTypeSize :: Integer
                      , metaDerivedTypeAlign :: Integer
                      , metaDerivedTypeOffset :: Integer
                      , metaDerivedTypeParent :: Metadata
                      }
  | MetaDWCompositeType { metaCompositeTypeTag :: DW_TAG
                        , metaCompositeTypeContext :: Metadata
                        , metaCompositeTypeName :: Text
                        , metaCompositeTypeFile :: Maybe Metadata
                        , metaCompositeTypeLine :: Integer
                        , metaCompositeTypeSize :: Integer
                        , metaCompositeTypeAlign :: Integer
                        , metaCompositeTypeOffset :: Integer
                        , metaCompositeTypeFlags :: Integer
                        , metaCompositeTypeParent :: Metadata
                        , metaCompositeTypeMembers :: Metadata
                        , metaCompositeTypeRuntime :: Integer
                        }
  | MetaDWSubrange { metaSubrangeLow :: Integer
                   , metaSubrangeHigh :: Integer
                   }
  | MetaDWEnumerator { metaEnumeratorName :: Text
                     , metaEnumeratorValue :: Integer
                     }
  | MetaDWLocal { metaLocalTag :: DW_VAR_TAG
                , metaLocalContext :: Metadata
                , metaLocalName :: Text
                , metaLocalFile :: Metadata
                , metaLocalLine :: Integer
                , metaLocalType :: Metadata
                }
  | MetadataList [Metadata]
  | MetadataValueConstant
  deriving (Show, Eq)

-- valueName is mostly informational at this point.  All references
-- will be resolved as part of the graph, but the name will be useful
-- for visualization purposes
data Value = Value { valueType :: Type
                   , valueName :: Maybe Identifier
                   , valueMetadata :: Maybe Metadata
                   , valueContent :: ValueT
                   }
           deriving (Show, Eq)

isExternalFunction :: Value -> Bool
isExternalFunction Value { valueContent = Function { functionParameters = Just _
                                                   , functionBody = Just _
                                                   }
                         } = False
isExternalFunction _ = True

functionAttributes :: Value -> Maybe [FunctionAttribute]
functionAttributes Value { valueType = TypeFunction _ _ _ l } = Just l
functionAttributes _ = Nothing

-- Functions have parameters if they are not external
data ValueT = Function { functionType :: Type
                       , functionParameters :: Maybe [Value] -- A list of arguments
                       , functionBody :: Maybe [Value] -- A list of basic blocks
                       }
            deriving (Show, Eq)

