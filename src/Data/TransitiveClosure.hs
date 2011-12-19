{-# LANGUAGE ScopedTypeVariables, BangPatterns #-}
-- | Module defines helpers for creating transitive-closure style algorithms
module Data.TransitiveClosure ( markVisited ) where

import Prelude hiding ( notElem )

import Data.Foldable ( Foldable, notElem, toList, foldMap )
--import Data.List ( mapAccumR )
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
        _ -> followChildren vis' unvisited newVals
    followChildren :: HashSet a -> [a] -> t a -> (HashSet a, t a)
    followChildren visited unvisited newVals =
      let (vis', transVals) = mapAccumL mark' visited unvisited
          flatVals = mconcat transVals
      in (vis', newVals `mappend` flatVals)

x = foldl f
  where
    f (vis, vals) a =
      let (vis', newVals) = mark' vis a
      in (vis', vals `mappend` newVals)
-- mapAccumL :: (acc -> x -> (acc, y))
--              -> acc            -- Initial accumulator
--              -> [x]            -- Input list
--              -> (acc, [y])     -- Final accumulator and result list
mapAccumL _ !s []        =  (s, [])
mapAccumL f !s (x:xs)    =  (s'',y:ys)
  where
    (s', y ) = f s x
    (s'',ys) = mapAccumL f s' xs