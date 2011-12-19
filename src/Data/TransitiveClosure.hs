{-# LANGUAGE ScopedTypeVariables #-}
-- | Module defines helpers for creating transitive-closure style algorithms
module Data.TransitiveClosure ( markVisited ) where

import Data.Foldable ( foldMap )
import Data.Traversable ( Traversable, mapAccumR )
import Data.Monoid
import Data.Set ( Set )
import qualified Data.Set as S

-- | This is a combinator to allow easy implementation of
-- transitive-closure style algorithms.  Given a function @f@ and a
-- seed value, apply @f@ to the seed and transitively to all of the
-- results of that application, collecting all of the intermediate and
-- final results into some Foldable container.
markVisited :: forall a t . (Ord a, Traversable t, Monoid (t a)) => (a -> t a) -> a -> t a
markVisited f = snd . mark' S.empty
  where
    mark' :: Set a -> a -> (Set a, t a)
    mark' visited a =
      case a `S.member` visited of
        True -> (visited, mempty)
        False ->
          let vis' = S.insert a visited
              newVals = f a
              -- (vis'', transVals) = mapAccumR mark' vis' newVals
              (vis'', transVals) = foldr mark' (vis', newVals) newVals
          in (vis'', newVals `mappend` fold transVals)
