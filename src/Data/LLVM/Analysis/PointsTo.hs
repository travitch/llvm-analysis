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
  PointsToAnalysis(..),
  PTRel(..),
  ExternPointerDescriptor(..),
  ExternFunctionDescriptor(..)
  ) where

import Data.Set ( Set )
import qualified Data.Set as S
import Data.LLVM.Types

-- | A data type to describe complex points-to relationships.
data PTRel = Direct !Value
           | ArrayElt PTRel -- ^ The pointed-to entity is at an unknown index into an array
           | FieldAccess !Int PTRel -- ^ The pointed-to entity is a field
           deriving (Eq, Ord)

instance Show PTRel where
  show = showRel

data PTResult a = PTSet (Set a) -- ^ A set of known targets
                | UniversalSet (Set a) -- ^ The pointer can point to anything (but the set of known targets is also provided)

-- | These are data tags used in the user hook to provide information
-- about pointer parameters to external functions.
data ExternPointerDescriptor = ExternNoAlias
                             | ExternAnyAlias
                             | ExternNewLocation
                             | ExternNotPointer
data ExternFunctionDescriptor = EFD { argumentEffects :: [ExternPointerDescriptor]
                                    , returnEffect :: ExternPointerDescriptor
                                    }

showRel :: PTRel -> String
showRel (Direct v) = case valueName v of
  Just n -> show n
  Nothing -> error "Value has no name in points-to request"
showRel (ArrayElt p) = show p ++ "[*]"
showRel (FieldAccess ix p) = concat [show p, ".<", show ix, ">"]

-- | The interface to any points-to analysis.
class PointsToAnalysis a where
  mayAlias :: a -> Value -> Value -> Bool
  -- ^ Check whether or not two values may alias
  pointsTo :: a -> Value -> Set PTRel
  -- ^ Retrieve the set of locations that a value may point to
  pointsToValues :: a -> Value -> Set Value
  pointsToValues pta v =
    let rels = pointsTo pta v
    in S.fold justDirect S.empty rels

justDirect :: PTRel -> Set Value -> Set Value
justDirect (Direct v) acc = S.insert v acc
justDirect _ acc = acc
