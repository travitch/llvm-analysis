module Data.LLVM.Types ( Module(..)
                       , Metadata(..)
                       , Type(..)
                       , Value(..)
                       , ValueT(..)
                       , isExternalFunction
                       , functionAttributes
                       ) where

import Data.ByteString.Lazy (ByteString)
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

data Metadata = MetaSourceLocation { metaSourceRow :: Integer
                                   , metaSourceCol :: Integer
                                   , metaSourceScope :: Metadata
                                   }
              | MetaDWLexicalBlock
              | MetaDWAutoVariable
                deriving (Show, Eq)

-- valueName is mostly informational at this point.  All references
-- will be resolved as part of the graph, but the name will be useful
-- for visualization purposes
data Value = Value { valueType :: Type
                   , valueName :: Maybe Identifier
                   , valueMetadata :: Maybe Metadata
                   , valueContent :: ValueT
                   }

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

