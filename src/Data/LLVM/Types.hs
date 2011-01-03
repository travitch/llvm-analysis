module Data.LLVM.Types ( Module(..)
                       , Metadata(..)
                       , Type(..)
                       , Value(..)
                       , ValueT(..)
                       , mkDwarfVirtuality
                       , mkDwarfLang
                       , mkDwarfEncoding
                       , isExternalFunction
                       , functionAttributes
                       ) where

import Data.Dwarf
import Data.Text (Text)
import Data.LLVM.Private.AttributeTypes

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

mkDwarfVirtuality i = case i of
  1 -> DW_VIRTUALITY_none
  2 -> DW_VIRTUALITY_virtual
  3 -> DW_VIRTUALITY_pure_virtual
  _ -> error "Invalid virtuality"

mkDwarfLang i = case i of
  1 -> DW_LANG_C89
  2 -> DW_LANG_C
  3 -> DW_LANG_Ada83
  4 -> DW_LANG_C_plus_plus
  5 -> DW_LANG_Cobol74
  6 -> DW_LANG_Cobol85
  7 -> DW_LANG_Fortran77
  8 -> DW_LANG_Fortran90
  9 -> DW_LANG_Pascal83
  10 -> DW_LANG_Modula2
  11 -> DW_LANG_Java
  12 -> DW_LANG_C99
  13 -> DW_LANG_Ada95
  14 -> DW_LANG_Fortran95
  15 -> DW_LANG_PLI
  16 -> DW_LANG_ObjC
  17 -> DW_LANG_ObjC_plus_plus
  18 -> DW_LANG_UPC
  0x8765 -> DW_LANG_UPC
  19 -> DW_LANG_D
  -- 20 -> DW_LANG_Python
  -- _ -> DW_LANG_Other i
  _ -> error "Invalid virtuality"

mkDwarfEncoding i = case i of
  1 -> DW_ATE_address
  2 -> DW_ATE_boolean
  3 -> DW_ATE_complex_float
  4 -> DW_ATE_float
  5 -> DW_ATE_signed
  6 -> DW_ATE_signed_char
  7 -> DW_ATE_unsigned
  8 -> DW_ATE_unsigned_char
  9 -> DW_ATE_imaginary_float
  10 -> DW_ATE_packed_decimal
  11 -> DW_ATE_numeric_string
  12 -> DW_ATE_edited
  13 -> DW_ATE_signed_fixed
  14 -> DW_ATE_unsigned_fixed
  15 -> DW_ATE_decimal_float

data Metadata = MetaSourceLocation { metaSourceRow :: Integer
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
                               , metaGlobalVarRef :: Value
                               }
              | MetaDWSubprogram { metaSubprogramContext :: Metadata
                                 , metaSubprogramName :: Text
                                 , metaSubprogramDisplayName :: Text
                                 , metaSubprogramLinkageName :: Text
                                 , metaSubprogramLine :: Integer
                                 , metaSubprogramType :: Metadata
                                 , metaSubprogramStatic :: Bool
                                 , metaSubprogramNotExtern :: Bool
                                 , metaSubprogramVirtuality :: DW_VIRTUALITY
                                 , metaSubprogramVirtIndex :: Integer
                                 , metaSubprogramBaseType :: Metadata
                                 , metaSubprogramArtificial :: Bool
                                 , metaSubprogramOptimized :: Bool
                                 , metaSubprogramFunction :: Value
                                 }
              | MetaDWBaseType { metaBaseTypeContext :: Metadata
                               , metaBaseTypeName :: Text
                               , metaBaseTypeFile :: Metadata
                               , metaBaseTypeLine :: Integer
                               , metaBaseTypeSize :: Integer
                               , metaBaseTypeAlign :: Integer
                               , metaBaseTypeOffset :: Integer
                               , metaBaseTypeFlags :: Integer
                               , metaBaseTypeEncoding :: DW_ATE
                               }
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
                         } = True
isExternalFunction _ = False

functionAttributes :: Value -> Maybe [FunctionAttribute]
functionAttributes Value { valueType = TypeFunction _ _ _ l } = Just l
functionAttributes _ = Nothing

-- Functions have parameters if they are not external
data ValueT = Function { functionType :: Type
                       , functionParameters :: Maybe [Value] -- A list of arguments
                       , functionBody :: Maybe [Value] -- A list of basic blocks
                       }
            deriving (Show, Eq)

