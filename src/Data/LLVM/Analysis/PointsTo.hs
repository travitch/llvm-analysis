-- | This module defines the interface to points-to analysis in this
-- analysis framework.  Each points-to analysis returns a result
-- object that is an instance of the 'PointsToAnalysis' typeclass; the
-- results are intended to be consumed through this interface.
--
-- All of the points-to analysis implementations expose a single function:
--
-- > runPointsToAnalysis :: (PointsToAnalysis a) => Module -> a
--
-- This makes it easy to change the points-to analysis you are using:
-- just modify your imports.  If you need multiple points-to analyses
-- in the same module (for example, to support command-line selectable
-- points-to analysis precision), use qualified imports.
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
