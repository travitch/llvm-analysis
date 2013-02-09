{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, DeriveGeneric #-}
{-# LANGUAGE ViewPatterns #-}
-- | This module defines an abstraction over field accesses of
-- structures called AccessPaths.  A concrete access path is rooted at
-- a value, while an abstract access path is rooted at a type.  Both
-- include a list of 'AccessType's that denote dereferences of
-- pointers, field accesses, and array references.
module LLVM.Analysis.AccessPath (
  -- * Types
  AccessPath(..),
  accessPathComponents,
  AbstractAccessPath(..),
  abstractAccessPathComponents,
  AccessType(..),
  AccessPathError(..),
  -- * Constructor
  accessPath,
  abstractAccessPath,
  appendAccessPath,
  followAccessPath,
  reduceAccessPath,
  externalizeAccessPath
  ) where

import Control.DeepSeq
import Control.Exception
import Control.Failure hiding ( failure )
import qualified Control.Failure as F
import Data.Hashable
import qualified Data.List as L
import Data.Typeable
import Text.PrettyPrint.GenericPretty

import LLVM.Analysis

-- import Text.Printf
-- import Debug.Trace
-- debug = flip trace

data AccessPathError = NoPathError Value
                     | NotMemoryInstruction Instruction
                     | CannotFollowPath AbstractAccessPath Value
                     | BaseTypeMismatch Type Type
                     | NonConstantInPath AbstractAccessPath Value
                     | EndpointTypeMismatch Type Type
                     | IrreducableAccessPath AbstractAccessPath
                     | CannotExternalizeType Type
                     deriving (Typeable, Show)

instance Exception AccessPathError

-- | The sequence of field accesses used to reference a field
-- structure.
data AbstractAccessPath =
  AbstractAccessPath { abstractAccessPathBaseType :: Type
                     , abstractAccessPathEndType :: Type
                     , abstractAccessPathTaggedComponents :: [(Type, AccessType)]
                     }
  deriving (Eq, Ord, Generic)

abstractAccessPathComponents :: AbstractAccessPath -> [AccessType]
abstractAccessPathComponents = map snd . abstractAccessPathTaggedComponents

instance Out AbstractAccessPath
instance Show AbstractAccessPath where
  show = pretty

instance Hashable AbstractAccessPath where
  hashWithSalt s (AbstractAccessPath bt et cs) =
    s `hashWithSalt` bt `hashWithSalt` et `hashWithSalt` cs

appendAccessPath :: (Failure AccessPathError m)
                    => AbstractAccessPath
                    -> AbstractAccessPath
                    -> m AbstractAccessPath
appendAccessPath (AbstractAccessPath bt1 et1 cs1) (AbstractAccessPath bt2 et2 cs2) =
  case et1 == bt2 of
    True -> return $ AbstractAccessPath bt1 et2 (cs1 ++ cs2)
    False -> F.failure $ EndpointTypeMismatch et1 bt2

-- | If the access path has more than one field access component, take
-- the first field access and the base type to compute a new base type
-- (the type of the indicated field) and the rest of the path
-- components.  Also allows for the discarding of array accesses.
--
-- Each call reduces the access path by one component
reduceAccessPath :: (Failure AccessPathError m)
                    => AbstractAccessPath -> m AbstractAccessPath
reduceAccessPath (AbstractAccessPath (TypePointer t _) et ((_, AccessDeref):cs)) =
  return $! AbstractAccessPath t et cs
-- FIXME: Some times (e.g., pixmap), the field number is out of range.
-- Have to figure out what could possibly cause that. Until then, just
-- ignore those cases.  Users of this are working at best-effort anyway.
reduceAccessPath p@(AbstractAccessPath (TypeStruct _ ts _) et ((_,AccessField fldNo):cs)) =
  case fldNo < length ts of
    True -> return $! AbstractAccessPath (ts !! fldNo) et cs
    False -> F.failure $ IrreducableAccessPath p
reduceAccessPath (AbstractAccessPath (TypeArray _ t) et ((_,AccessArray):cs)) =
  return $! AbstractAccessPath t et cs
reduceAccessPath p = F.failure $ IrreducableAccessPath p

instance NFData AbstractAccessPath where
  rnf a@(AbstractAccessPath _ _ ts) = ts `deepseq` a `seq` ()

data AccessPath =
  AccessPath { accessPathBaseValue :: Value
             , accessPathBaseType :: Type
               -- ^ If there are some wonky bitcasts in play, this
               -- type records the real type of this path, even if the
               -- base was something unrelated and bitcast.  The real
               -- type is the type casted /to/.
             , accessPathEndType :: Type
             , accessPathTaggedComponents :: [(Type, AccessType)]
             }
  deriving (Generic, Eq, Ord)

accessPathComponents :: AccessPath -> [AccessType]
accessPathComponents = map snd . accessPathTaggedComponents

instance Out AccessPath
instance Show AccessPath where
  show = pretty

instance NFData AccessPath where
  rnf a@(AccessPath _ _ _ ts) = ts `deepseq` a `seq` ()

instance Hashable AccessPath where
  hashWithSalt s (AccessPath bv bt ev cs) =
    s `hashWithSalt` bv `hashWithSalt` bt `hashWithSalt` ev `hashWithSalt` cs

data AccessType = AccessField !Int
                  -- ^ Field access of the field with this index
                | AccessUnion
                  -- ^ A union access.  The union discriminator is the
                  -- /type/ that this AccessType is tagged with in the
                  -- AccessPath.  Unions in LLVM do not have an
                  -- explicit representation of their fields, so there
                  -- is no index possible here.
                | AccessArray
                  -- ^ An array access; all array elements are treated
                  -- as a unit
                | AccessDeref
                  -- ^ A plain pointer dereference
                deriving (Read, Show, Eq, Ord, Generic)

instance Out AccessType

instance NFData AccessType where
  rnf a@(AccessField i) = i `seq` a `seq` ()
  rnf _ = ()

instance Hashable AccessType where
  hashWithSalt s (AccessField ix) =
    s `hashWithSalt` (1 :: Int) `hashWithSalt` ix
  hashWithSalt s AccessUnion = s `hashWithSalt` (154 :: Int)
  hashWithSalt s AccessArray = s `hashWithSalt` (26 :: Int)
  hashWithSalt s AccessDeref = s `hashWithSalt` (300 :: Int)

followAccessPath :: (Failure AccessPathError m) => AbstractAccessPath -> Value -> m Value
followAccessPath aap@(AbstractAccessPath bt _ components) val =
  case derefPointerType bt /= valueType val of
    True -> F.failure (BaseTypeMismatch bt (valueType val))
    False -> walk components val
  where
    walk [] v = return v
    walk ((_, AccessField ix) : rest) v =
      case valueContent' v of
        ConstantC ConstantStruct { constantStructValues = vs } ->
          case ix < length vs of
            False -> error $ concat [ "LLVM.Analysis.AccessPath.followAccessPath.walk: "
                                    ," Invalid access path: ", show aap, " / ", show val
                                    ]
            True -> walk rest (vs !! ix)
        _ -> F.failure (NonConstantInPath aap val)
    walk _ _ = F.failure (CannotFollowPath aap val)

abstractAccessPath :: AccessPath -> AbstractAccessPath
abstractAccessPath (AccessPath _ vt t p) =
  AbstractAccessPath vt t p

-- | For Store, RMW, and CmpXchg instructions, the returned access
-- path describes the field /stored to/.  For Load instructions, the
-- returned access path describes the field loaded.  For
-- GetElementPtrInsts, the returned access path describes the field
-- whose address was taken/computed.
accessPath :: (Failure AccessPathError m) => Instruction -> m AccessPath
accessPath i =
  case i of
    StoreInst { storeAddress = sa, storeValue = sv } ->
      return $! addDeref $ go (AccessPath sa (valueType sa) (valueType sv) []) (valueType sa) sa
    LoadInst { loadAddress = la } ->
      return $! addDeref $ go (AccessPath la (valueType la) (valueType i) []) (valueType la) la
    AtomicCmpXchgInst { atomicCmpXchgPointer = p
                      , atomicCmpXchgNewValue = nv
                      } ->
      return $! addDeref $ go (AccessPath p (valueType p) (valueType nv) []) (valueType p) p
    AtomicRMWInst { atomicRMWPointer = p
                  , atomicRMWValue = v
                  } ->
      return $! addDeref $ go (AccessPath p (valueType p) (valueType v) []) (valueType p) p
    GetElementPtrInst {} ->
      -- FIXME: Should this really get a deref tag?  Unclear...
      return $! addDeref $ go (AccessPath (toValue i) (valueType i) (valueType i) []) (valueType i) (toValue i)
    -- If this is an argument to a function call, it could be a
    -- bitcasted GEP or Load
    BitcastInst { castedValue = (valueContent' -> InstructionC i') } ->
      accessPath i'
    _ -> F.failure (NotMemoryInstruction i)
  where
    addDeref p =
      let t = valueType (accessPathBaseValue p)
          cs' = (t, AccessDeref) : accessPathTaggedComponents p
      in p { accessPathTaggedComponents = cs' }
    go p vt v =
      -- Note that @go@ does not need to update the accessPathBaseType
      -- until the end (fallthrough) case.
      case valueContent v of
        -- Unions have no representation in the IR; the only way we
        -- can identify a union is by looking for instances where a
        -- struct pointer type beginning with '%union.' is being cast
        -- into something else.  This lets us know the union variant
        -- being accessed.
        InstructionC BitcastInst { castedValue = cv }
          | isUnionPointerType (valueType cv) ->
            let p' = p { accessPathTaggedComponents =
                            (valueType v, AccessUnion) : accessPathTaggedComponents p
                       }
            in go p' (valueType v) cv
          | otherwise -> go p (valueType v) cv
        ConstantC ConstantValue { constantInstruction = BitcastInst { castedValue = cv } } ->
          go p (valueType v) cv
        InstructionC GetElementPtrInst { getElementPtrValue = base
                                       , getElementPtrIndices = [_]
                                       } ->
          let p' = p { accessPathBaseValue = base
                     , accessPathTaggedComponents = (valueType v, AccessArray) : accessPathTaggedComponents p
                     }
          in go p' (valueType base) base
        InstructionC GetElementPtrInst { getElementPtrValue = base
                                       , getElementPtrIndices = ixs
                                       } ->
          let p' = p { accessPathBaseValue = base
                     , accessPathTaggedComponents =
                       gepIndexFold base ixs ++ accessPathTaggedComponents p
                     }
          in go p' (valueType base) base
        ConstantC ConstantValue { constantInstruction =
          GetElementPtrInst { getElementPtrValue = base
                            , getElementPtrIndices = ixs
                            } } ->
          let p' = p { accessPathBaseValue = base
                     , accessPathTaggedComponents =
                       gepIndexFold base ixs ++ accessPathTaggedComponents p
                     }
          in go p' (valueType base) base
        InstructionC LoadInst { loadAddress = la } ->
          let p' = p { accessPathBaseValue  = la
                     , accessPathTaggedComponents =
                          (valueType v, AccessDeref) : accessPathTaggedComponents p
                     }
          in go p' (valueType la) la
        _ -> p { accessPathBaseValue = v
               , accessPathBaseType = vt
               }

isUnionPointerType :: Type -> Bool
isUnionPointerType t =
  case t of
    TypePointer (TypeStruct (Just name) _ _) _ ->
      L.isPrefixOf "union." name
    _ -> False

-- | Convert an 'AbstractAccessPath' to a format that can be written
-- to disk and read back into another process.  The format is the pair
-- of the base name of the structure field being accessed (with
-- struct. stripped off) and with any numeric suffixes (which are
-- added by llvm) chopped off.  The actually list of 'AccessType's is
-- preserved.
--
-- The struct name mangling here basically assumes that the types
-- exposed via the access path abstraction have the same definition in
-- all compilation units.  Ensuring this between runs is basically
-- impossible, but it is pretty much always the case.
externalizeAccessPath :: (Failure AccessPathError m)
                         => AbstractAccessPath
                         -> m (String, [AccessType])
externalizeAccessPath accPath =
  maybe (F.failure (CannotExternalizeType bt)) return $ do
    baseName <- structTypeToName (stripPointerTypes bt)
    return (baseName, abstractAccessPathComponents accPath)
  where
    bt = abstractAccessPathBaseType accPath

-- Internal Helpers


derefPointerType :: Type -> Type
derefPointerType (TypePointer p _) = p
derefPointerType t = error ("LLVM.Analysis.AccessPath.derefPointerType: Type is not a pointer type: " ++ show t)

gepIndexFold :: Value -> [Value] -> [(Type, AccessType)]
gepIndexFold base (ptrIx : ixs) =
  -- GEPs always have a pointer as the base operand
  let ty@(TypePointer baseType _) = valueType base
  in case valueContent ptrIx of
    ConstantC ConstantInt { constantIntValue = 0 } ->
      snd $ L.foldl' walkGep (baseType, []) ixs
    _ ->
      snd $ L.foldl' walkGep (baseType, [(ty, AccessArray)]) ixs
  where
    walkGep (ty, acc) ix =
      case ty of
        -- If the current type is a pointer, this must be an array
        -- access; that said, this wouldn't even be allowed because a
        -- pointer would have to go through a Load...  check this
        TypePointer ty' _ -> (ty', (ty, AccessArray) : acc)
        TypeArray _ ty' -> (ty', (ty, AccessArray) : acc)
        TypeStruct _ ts _ ->
          case valueContent ix of
            ConstantC ConstantInt { constantIntValue = fldNo } ->
              let fieldNumber = fromIntegral fldNo
                  ty' = ts !! fieldNumber
              in (ty', (ty', AccessField fieldNumber) : acc)
            _ -> error ("LLVM.Analysis.AccessPath.gepIndexFold.walkGep: Invalid non-constant GEP index for struct: " ++ show ty)
        _ -> error ("LLVM.Analysis.AccessPath.gepIndexFold.walkGep: Unexpected type in GEP: " ++ show ty)
gepIndexFold v [] =
  error ("LLVM.Analysis.AccessPath.gepIndexFold: GEP instruction/base with empty index list: " ++ show v)

{-# ANN module "HLint: ignore Use if" #-}