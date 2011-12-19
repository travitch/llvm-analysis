{-# LANGUAGE ScopedTypeVariables, BangPatterns #-}
-- | Module defines helpers for creating transitive-closure style algorithms
module Data.TransitiveClosure ( markVisited ) where

import Prelude hiding ( notElem )

import Data.Foldable ( Foldable, notElem, toList, foldMap )
import Data.List ( foldl' )
import Data.Monoid
import Data.Hashable
import Data.HashSet ( HashSet )
import qualified Data.HashSet as S

-- | This is a combinator to allow easy implementation of
-- transitive-closure style algorithms.  Given a function @f@ and a
-- seed value, apply @f@ to the seed and transitively to all of the
-- results of that application, collecting all of the intermediate and
-- final results into some Foldable container.
markVisited :: forall a t . (Hashable a, Eq a, Foldable t, Monoid (t a)) => (a -> t a) -> t a -> t a
markVisited f as = mappend as $ snd $ (foldMap (mark' S.empty) as)
  where
    mark' :: HashSet a -> a -> (HashSet a, t a)
    mark' !visited a =
      let vis' = visited `seq` S.insert a visited
          newVals = f a
          unvisited = filter (`notElem` vis') (toList newVals)
      in case length unvisited of
        0 -> (vis', mempty)
        _ -> foldl' applyMark (vis', newVals) unvisited
    applyMark (!vis, !vals) a =
      let (!vis', !newVals) = mark' vis a
          allVals = vals `mappend` newVals
      in (S.size vis' `seq` allVals `seq` vis', allVals)
