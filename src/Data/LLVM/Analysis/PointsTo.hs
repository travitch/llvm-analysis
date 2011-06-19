module Data.LLVM.Analysis.PointsTo (
  -- * Classes
  PointsToAnalysis(..)
  ) where

import Data.Set ( Set )
import Data.LLVM.Types

-- | The interface to any points-to analysis.
class PointsToAnalysis a where
  mayAlias :: a -> Value -> Value -> Bool
  -- ^ Check whether or not two values may alias
  pointsTo :: a -> Value -> Set Value
  -- ^ Retrieve the set of values that a value may point to
