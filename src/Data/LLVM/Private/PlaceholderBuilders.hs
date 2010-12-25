module Data.LLVM.Private.PlaceholderBuilders ( mkExtractElement
                                             , mkDataLayout
                                             , mkShuffleVector
                                             ) where

import Data.LLVM.Private.AttributeTypes
import Data.LLVM.Private.PlaceholderTypes

mkExtractElement :: (Monad m) => Identifier -> Type -> Value -> Value -> m Value
mkExtractElement name ty v1 v2 =
  case ty of
    TypeVector _ t -> return Value { valueName = name, valueType = t, valueContent = ExtractElementInst v1 v2 }
    _ -> fail "Non-vector type in extractelement"

-- FIXME: Parse the bytestring - have the code for this already in the
-- old attoparsec-based parser
mkDataLayout :: a -> DataLayout
mkDataLayout s = defaultDataLayout

mkShuffleVector :: (Monad m) => Identifier -> Type -> Value -> Value -> Type -> Value -> m Value
mkShuffleVector name t1 val1 val2 t2 mask =
  case (t1, t2) of
    (TypeVector _ t, TypeVector n _) -> return Value { valueName = name, valueType = (TypeVector n t), valueContent = ShuffleVectorInst val1 val2 mask }
    _ -> fail "Non-vector type for vec or mask in shufflevector"

-- mkExtractValue name aggT val idx