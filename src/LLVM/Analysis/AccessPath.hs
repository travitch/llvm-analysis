{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, FlexibleContexts #-}
module LLVM.Analysis.AccessPath (
  -- * Types
  AccessPath(..),
  AbstractAccessPath(..),
  AccessType(..),
  AccessPathError,
  -- * Constructor
  accessPath,
  abstractAccessPath
  ) where

import Control.Exception
import Control.Failure hiding ( failure )
import qualified Control.Failure as F
import Data.Typeable
import Debug.Trace.LocationTH

import LLVM.Analysis

data AccessPathError = NoPathError Value
                     | NotMemoryInstruction Instruction
                     deriving (Typeable, Show)

instance Exception AccessPathError

-- | The sequence of field accesses used to reference a field
-- structure.
data AbstractAccessPath =
  AbstractAccessPath { abstractAccessPathBaseType :: Type
                     , abstractAccessPathComponents :: [AccessType]
                     }
  deriving (Show)

data AccessPath =
  AccessPath { accessPathBaseValue :: Value
             , accessPathComponents :: [AccessType]
             }
  deriving (Show)

data AccessType = AccessField !Int
                | AccessArray
                | AccessDeref
                deriving (Read, Show, Eq, Ord)

abstractAccessPath :: AccessPath -> AbstractAccessPath
abstractAccessPath (AccessPath v p) =
  AbstractAccessPath (derefPointerType (valueType v)) p

accessPath :: (Failure AccessPathError m) => Instruction -> m AccessPath
accessPath i =
  case i of
    LoadInst { loadAddress = la } ->
      return $! go (AccessPath la []) la
    StoreInst { storeAddress = sa } ->
      return $! go (AccessPath sa []) sa
    AtomicCmpXchgInst { atomicCmpXchgPointer = p } ->
      return $! go (AccessPath p []) p
    AtomicRMWInst { atomicRMWPointer = p } ->
      return $! go (AccessPath p []) p
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