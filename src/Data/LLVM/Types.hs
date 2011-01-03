module Data.LLVM.Types ( Module(..)
                       , Metadata(..)
                       , Type(..)
                       , Value(..)
                       , ValueT(..)
                       , DwarfLanguage(..)
                       , DwarfVirtuality(..)
                       , mkDwarfVirtuality
                       , mkDwarfLang
                       , isExternalFunction
                       , functionAttributes
                       ) where

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

data DwarfVirtuality = DWVirtNone        -- 1
                     | DWVirtVirtual     -- 2
                     | DWVirtPureVirtual -- 3
                     deriving (Show, Eq)

mkDwarfVirtuality i = case i of
  1 -> DWVirtNone
  2 -> DWVirtVirtual
  3 -> DWVirtPureVirtual
  _ -> error "Invalid virtuality"

data DwarfLanguage = DWLangC89        -- 1
                   | DWLangC          -- 2
                   | DWLangAda83      -- 3
                   | DWLangCpp        -- 4
                   | DWLangCobol74    -- 5
                   | DWLangCobol85    -- 6
                   | DWLangFortran77  -- 7
                   | DWLangFortran90  -- 8
                   | DWLangPascal83   -- 9
                   | DWLangModula2    -- 10
                   | DWLangJava       -- 11
                   | DWLangC99        -- 12
                   | DWLangAda95      -- 13
                   | DWLangFortran95  -- 14
                   | DWLangPLI        -- 15
                   | DWLangObjC       -- 16
                   | DWLangObjCpp     -- 17
                   | DWLangUPC        -- 18 (also 0x8765)
                   | DWLangD          -- 19
                   | DWLangPython     -- 20
                   | DWLangOther Integer
                   deriving (Show, Eq)

mkDwarfLang i = case i of
  1 -> DWLangC89
  2 -> DWLangC
  3 -> DWLangAda83
  4 -> DWLangCpp
  5 -> DWLangCobol74
  6 -> DWLangCobol85
  7 -> DWLangFortran77
  8 -> DWLangFortran90
  9 -> DWLangPascal83
  10 -> DWLangModula2
  11 -> DWLangJava
  12 -> DWLangC99
  13 -> DWLangAda95
  14 -> DWLangFortran95
  15 -> DWLangPLI
  16 -> DWLangObjC
  17 -> DWLangObjCpp
  18 -> DWLangUPC
  0x8765 -> DWLangUPC
  19 -> DWLangD
  20 -> DWLangPython
  _ -> DWLangOther i

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
              | MetaDWCompileUnit { metaCompileUnitLanguage :: DwarfLanguage
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
              | MetaDWGlobalVar { metaGlobalVarContext :: Metadata
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
                                 , metaSubprogramVirtuality :: DwarfVirtuality
                                 , metaSubprogramVirtIndex :: Integer
                                 , metaSubprogramBaseType :: Metadata
                                 , metaSubprogramArtificial :: Bool
                                 , metaSubprogramOptimized :: Bool
                                 , metaSubprogramFunction :: Value
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

