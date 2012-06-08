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
module LLVM.Analysis.PointsTo (
  -- * Classes
  PointsToAnalysis(..),
  ) where

import Data.Maybe ( mapMaybe )
import LLVM.Analysis

-- | The interface to any points-to analysis.
class PointsToAnalysis a where
  mayAlias :: a -> Value -> Value -> Bool
  -- ^ Check whether or not two values may alias
  pointsTo :: a -> Value -> [Value]
  -- ^ Return the list of values that a LoadInst may return.  May
  -- return targets for other values too (e.g., say that a Function
  -- points to itself), but nothing is guaranteed.
  resolveIndirectCall :: a -> Instruction -> [Function]
  -- ^ Given a Call instruction, determine its possible callees.  The
  -- default implementation just delegates the called function value
  -- to 'pointsTo' and .
  resolveIndirectCall pta i =
    case i of
      CallInst { callFunction = f } -> mapMaybe toFunction $ pointsTo pta f
      InvokeInst { invokeFunction = f } -> mapMaybe toFunction $ pointsTo pta f
      _ -> []

toFunction :: Value -> Maybe Function
toFunction v =
  case valueContent' v of
    FunctionC f -> Just f
    _ -> Nothing