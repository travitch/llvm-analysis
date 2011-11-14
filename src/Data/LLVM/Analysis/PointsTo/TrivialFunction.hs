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

trivialPointsTo :: TrivialFunction -> Value -> PTResult PTRel
trivialPointsTo p@(TrivialFunction m) v =
  case valueContent v of
    FunctionC _ -> PTSet $ S.singleton (Direct v)
    ExternalFunctionC _ -> PTSet $ S.singleton (Direct v)
    GlobalAliasC ga -> trivialPointsTo p (Value ga)
    InstructionC BitcastInst { castedValue = c } -> trivialPointsTo p c
    _ -> PTSet $ S.map Direct $ M.lookupDefault S.empty (valueType v) m
