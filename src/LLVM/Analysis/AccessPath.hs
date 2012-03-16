{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, FlexibleContexts #-}
module LLVM.Analysis.AccessPath (
  -- * Types
  AccessPath(..),
  AbstractAccessPath(..),
  AccessType(..),
  AccessPathError,
  -- * Constructor
  accessPath,
  abstractAccessPath,
  appendAccessPath,
  followAccessPath
  ) where

import Control.DeepSeq
import Control.Exception
import Control.Failure hiding ( failure )
import qualified Control.Failure as F
import Data.Typeable
import Debug.Trace.LocationTH

import LLVM.Analysis

data AccessPathError = NoPathError Value
                     | NotMemoryInstruction Instruction
                     | CannotFollowPath AbstractAccessPath Value
                     | BaseTypeMismatch Type Type
                     | NonConstantInPath AbstractAccessPath Value
                     deriving (Typeable, Show)

instance Exception AccessPathError

-- | The sequence of field accesses used to reference a field
-- structure.
data AbstractAccessPath =
  AbstractAccessPath { abstractAccessPathBaseType :: Type
                     , abstractAccessPathEndType :: Type
                     , abstractAccessPathComponents :: [AccessType]
                     }
  deriving (Show, Eq, Ord)

appendAccessPath :: AbstractAccessPath
                    -> AbstractAccessPath
                    -> Maybe AbstractAccessPath
appendAccessPath (AbstractAccessPath bt1 et1 cs1) (AbstractAccessPath bt2 et2 cs2) =
  case et1 == bt2 of
    True -> Just $ AbstractAccessPath bt1 et2 (cs1 ++ cs2)
    False -> Nothing

instance NFData AbstractAccessPath where
  rnf a@(AbstractAccessPath _ _ ts) = ts `deepseq` a `seq` ()

data AccessPath =
  AccessPath { accessPathBaseValue :: Value
             , accessPathEndValue :: Value
             , accessPathComponents :: [AccessType]
             }
  deriving (Show, Eq, Ord)

instance NFData AccessPath where
  rnf a@(AccessPath _ _ ts) = ts `deepseq` a `seq` ()

data AccessType = AccessField !Int
                | AccessArray
                | AccessDeref
                deriving (Read, Show, Eq, Ord)

instance NFData AccessType where
  rnf a@(AccessField i) = i `seq` a `seq` ()
  rnf _ = ()

followAccessPath :: (Failure AccessPathError m) => AbstractAccessPath -> Value -> m Value
followAccessPath aap@(AbstractAccessPath bt _ components) val =
  case derefPointerType bt /= valueType val of
    True -> F.failure (BaseTypeMismatch bt (valueType val))
    False -> walk components val
  where
    walk [] v = return v
    walk (AccessField ix : rest) v =
      case valueContent' v of
        ConstantC ConstantStruct { constantStructValues = vs } ->
          case ix < length vs of
            False -> $failure ("Invalid access path: " ++ show aap ++ " / " ++ show val)
            True -> walk rest (vs !! ix)
        _ -> F.failure (NonConstantInPath aap val)
    walk _ _ = F.failure (CannotFollowPath aap val)

abstractAccessPath :: AccessPath -> AbstractAccessPath
abstractAccessPath (AccessPath v v0 p) =
  AbstractAccessPath (valueType v) (valueType v0) p

accessPath :: (Failure AccessPathError m) => Instruction -> m AccessPath
accessPath i =
  case i of
    LoadInst { loadAddress = la } ->
      return $! go (AccessPath la la []) la
    StoreInst { storeAddress = sa } ->
      return $! go (AccessPath sa sa []) sa
    AtomicCmpXchgInst { atomicCmpXchgPointer = p } ->
      return $! go (AccessPath p p []) p
    AtomicRMWInst { atomicRMWPointer = p } ->
      return $! go (AccessPath p p []) p
    GetElementPtrInst {} ->
      return $! go (AccessPath (Value i) (Value i) []) (Value i)
    _ -> F.failure (NotMemoryInstruction i)
  where
    go p v =
      case valueContent v of
        InstructionC GetElementPtrInst { getElementPtrValue = base
                                       , getElementPtrIndices = ixs
                                       } ->
          let p' = p { accessPathBaseValue = base
                     , accessPathComponents =
                       gepIndexFold base ixs ++ accessPathComponents p
                     }
          in go p' base
        ConstantC ConstantValue { constantInstruction =
          GetElementPtrInst { getElementPtrValue = base
                            , getElementPtrIndices = ixs
                            } } ->
          let p' = p { accessPathBaseValue = base
                     , accessPathComponents =
                       gepIndexFold base ixs ++ accessPathComponents p
                     }
          in go p' base
        InstructionC LoadInst { loadAddress = la } ->
          let p' = p { accessPathBaseValue  = la
                     , accessPathComponents =
                          AccessDeref : accessPathComponents p
                     }
          in go p' la
        _ -> p { accessPathBaseValue = v }

derefPointerType :: Type -> Type
derefPointerType (TypePointer p _) = p
derefPointerType t = $failure ("Type is not a pointer type: " ++ show t)

gepIndexFold :: Value -> [Value] -> [AccessType]
gepIndexFold base (ptrIx : ixs) =
  -- GEPs always have a pointer as the base operand
  let TypePointer baseType _ = valueType base
  in case valueContent ptrIx of
    ConstantC ConstantInt { constantIntValue = 0 } ->
      snd $ foldr walkGep (baseType, []) ixs
    _ ->
      snd $ foldr walkGep (baseType, [AccessArray]) ixs
  where
    walkGep ix (ty, acc) =
      case ty of
        -- If the current type is a pointer, this must be an array
        -- access; that said, this wouldn't even be allowed because a
        -- pointer would have to go through a Load...  check this
        TypePointer ty' _ -> (ty', AccessArray : acc)
        TypeArray _ ty' -> (ty', AccessArray : acc)
        TypeStruct _ ts _ ->
          case valueContent ix of
            ConstantC ConstantInt { constantIntValue = fldNo } ->
              let fieldNumber = fromIntegral fldNo
              in (ts !! fieldNumber, AccessField fieldNumber : acc)
            _ -> $failure ("Invalid non-constant GEP index for struct: " ++ show ty)
        _ -> $failure ("Unexpected type in GEP: " ++ show ty)
gepIndexFold v [] =
  $failure ("GEP instruction/base with empty index list: " ++ show v)