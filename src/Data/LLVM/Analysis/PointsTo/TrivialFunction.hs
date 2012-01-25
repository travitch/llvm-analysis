{-# LANGUAGE TemplateHaskell #-}
-- | This module implements a trivial points-to analysis that is
-- intended only for fast conservative callgraph construction.  All
-- function pointers can point to all functions with compatible types.
--
-- Other pointers are considered to alias if they are of the same
-- type.  The 'pointsTo' function only returns empty sets for
-- non-function pointers.
module Data.LLVM.Analysis.PointsTo.TrivialFunction (
  -- * Types
  TrivialFunction,
  -- * Constructor
  runPointsToAnalysis
  ) where

import Data.HashMap.Strict ( HashMap )
import Data.Set ( Set )
import qualified Data.HashMap.Strict as M
import qualified Data.Set as S
import FileLocation

import Data.LLVM.Types
import Data.LLVM.Analysis.PointsTo

-- | The result of the TrivialFunction points-to analysis.  It is an
-- instance of the 'PointsToAnalysis' typeclass and is intended to be
-- queried through that interface.
--
-- Again, note that this analysis is not precise (just fast) and does
-- not provide points-to sets for non-function types.  It provides
-- only type-based answers and does not respect typecasts at all.
newtype TrivialFunction = TrivialFunction (HashMap Type (Set Value))

instance PointsToAnalysis TrivialFunction where
  mayAlias = trivialMayAlias
  pointsTo = trivialPointsTo

-- | Run the points-to analysis and return its results in an opaque
-- handle.
runPointsToAnalysis :: Module -> TrivialFunction
runPointsToAnalysis m = TrivialFunction finalMap
  where
    externMap = foldr buildMap M.empty (moduleExternalFunctions m)
    finalMap = foldr buildMap externMap (moduleDefinedFunctions m)

-- | Add function-typed values to the result map.
buildMap :: (IsValue a) => a -> HashMap Type (Set Value) -> HashMap Type (Set Value)
buildMap v m = case M.lookup vtype m of
  Nothing -> M.insert vtype (S.singleton (Value v)) m
  Just s -> M.insert vtype (S.insert (Value v) s) m
  where
    vtype = valueType v

trivialMayAlias :: TrivialFunction -> Value -> Value -> Bool
trivialMayAlias _ v1 v2 = valueType v1 == valueType v2

-- Note, don't use the bitcast stripping functions here since we need
-- the surface types of functions.  This affects cases where function
-- pointers are stored generically in a struct and then taken out and
-- casted back to their original type.
trivialPointsTo :: TrivialFunction -> Value -> PTResult PTRel
trivialPointsTo p@(TrivialFunction m) v =
  case valueContent v of
    FunctionC f -> PTSet $ S.singleton (Direct (Value f))
    ExternalFunctionC ef -> PTSet $ S.singleton (Direct (Value ef))
    GlobalAliasC ga -> trivialPointsTo p (Value ga)
    InstructionC BitcastInst { castedValue = c } ->
      case valueContent c of
        FunctionC _ -> trivialPointsTo p c
        ExternalFunctionC _ -> trivialPointsTo p c
        GlobalAliasC _ -> trivialPointsTo p c
        _ -> PTSet $ S.map Direct $ M.lookupDefault S.empty (derefPointer v) m
    _ -> PTSet $ S.map Direct $ M.lookupDefault S.empty (derefPointer v) m

derefPointer :: Value -> Type
derefPointer v = case valueType v of
  TypePointer p _ -> p
  _ -> $err' ("Non-pointer type given to trivalPointsTo: " ++ show v)
