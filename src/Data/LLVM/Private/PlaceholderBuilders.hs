module Data.LLVM.Private.PlaceholderBuilders ( mkExtractElement
                                             , mkDataLayout
                                             , mkShuffleVector
                                             , mkLoadInst
                                             , mkStoreInst
                                             , mkConversionInst
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

-- The type of a load is ty with a layer of pointer type unwrapped.
-- The input *must* be a pointer type
mkLoadInst :: (Monad m) => Identifier -> Bool -> Type -> Value -> Integer -> m Value
mkLoadInst ident volatile ty val align =
  return $ Value { valueName = ident
                 , valueType = ty'
                 , valueContent = LoadInst volatile ty val align
                 }
  where (TypePointer ty') = ty

mkStoreInst :: (Monad m) => Bool -> Value -> Type -> Identifier -> Integer -> m Value
mkStoreInst volatile val t1 ident align =
  return $ UnnamedValue $ StoreInst volatile t1 val align


-- Build a conversion instruction using the provided type constructor.
-- Examples: TruncInst, ZextInst
-- These all follow the same pattern: convert a value of t1 to t2.
mkConversionInst :: (Monad m) => (Value -> Type -> ValueT) -> Identifier -> Type -> Value -> Type -> m Value
mkConversionInst inst ident t1 val t2 =
  return $ Value { valueName = ident
                 , valueType = t2
                 , valueContent = inst val t2
                 }
