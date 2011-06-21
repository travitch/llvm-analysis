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
runPointsToAnalysis m =
  TrivialFunction $ foldr buildMap M.empty (moduleGlobals m)

-- | Add function-typed values to the result map.
buildMap :: Value -> HashMap Type (Set Value) -> HashMap Type (Set Value)
buildMap v@Value { valueContent = ExternalValue } m =
  case M.lookup (valueType v) m of
    Nothing -> M.insert (valueType v) (S.singleton v) m
    Just s -> M.insert (valueType v) (S.insert v s) m
buildMap v@Value { valueContent = Function {} } m =
  case M.lookup (valueType v) m of
    Nothing -> M.insert (valueType v) (S.singleton v) m
    Just s -> M.insert (valueType v) (S.insert v s) m
buildMap _ m = m

trivialMayAlias :: TrivialFunction -> Value -> Value -> Bool
trivialMayAlias _ v1 v2 = valueType v1 == valueType v2

trivialPointsTo :: TrivialFunction -> Value -> Set Value
trivialPointsTo (TrivialFunction m) v =
  M.lookupDefault S.empty (valueType v) m
