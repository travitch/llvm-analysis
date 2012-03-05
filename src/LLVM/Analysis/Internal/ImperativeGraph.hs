module LLVM.Analysis.Internal.ImperativeGraph (
  -- * Types
  ImperativeGraph,
  Node,
  -- * Constructors
  newGraph,
  newGraphSized,
  -- * Mutators
  addNode,
  addEdges,
  nodeLabels,
  nodeSuccessors,
  -- * Extract
  toInductive
  ) where

import Control.Monad ( forM_, forM, when )
import Control.Monad.ST ( ST )
import Data.HashTable.ST.Linear ( HashTable )
import qualified Data.HashTable.ST.Linear as HT
import Data.Maybe ( fromJust, isJust, isNothing )
import Data.Vector.Unboxed.Mutable ( STVector )
import qualified Data.Vector.Unboxed.Mutable as V
import qualified Data.Vector.Unboxed as IV

import Data.Graph.Inductive ( LNode, LEdge, mkGraph )
import LLVM.Analysis.Internal.PatriciaTree ( Gr )

type Node = Int

-- | The information stored for each node in the graph - a label plus
-- a list of outgoing edge destinations.
data Context s lbl = EmptyContext lbl
                   | Context lbl !Int (STVector s Node) -- label, used entries in vector, vector

-- | The type of the graph.  These graphs represent each node as an
-- Int with a mandatory label and a list of outgoing edges.
data ImperativeGraph s lbl = IG (HashTable s Node (Context s lbl))

-- | Create a new graph with the default size
newGraph :: ST s (ImperativeGraph s lbl)
newGraph = do
  ht <- HT.newSized 10000
  return (IG ht)

newGraphSized :: Int -> ST s (ImperativeGraph s lbl)
newGraphSized s = do
  ht <- HT.newSized s
  return (IG ht)

addNode :: ImperativeGraph s lbl -> Node -> lbl -> ST s ()
addNode (IG ht) n lbl = do
  existingBinding <- HT.lookup ht n
  when (isJust existingBinding) (error ("Node already exists in graph: " ++ show n))
  let newCtxt = EmptyContext lbl
  HT.insert ht n newCtxt

-- | The call
--
-- > addEdges g src tgts
--
-- Adds edges from src to all of the given tgts.
addEdges :: ImperativeGraph s lbl -> Node -> [Node] -> ST s ()
addEdges (IG ht) src tgts = do
  srcCtxt <- HT.lookup ht src
  when (isNothing srcCtxt) (error ("Source node does not exist in graph: " ++ show src))
  forM_ tgts $ \tgt -> do
    tgtCtxt <- HT.lookup ht tgt
    when (isNothing tgtCtxt) (error ("Target node does not exist in graph: " ++ show tgt))
  case fromJust srcCtxt of
    -- The node has an empty context - we have to swap this out for a
    -- context with a small vector in it.
    EmptyContext lbl -> do
      v <- V.new (nTargets * 2)
      forM_ (zip [0..] tgts) $ \(ix, tgt) -> do
        V.write v ix tgt
      let newCtxt = Context lbl nTargets v
      HT.insert ht src newCtxt
    -- Otherwise, look at the vector and determine if we need to
    -- resize it.  Then add entries
    Context lbl usedEntries vec -> do
      let vlen = V.length vec
      newCtxt@(Context _ _ theVec) <- case vlen < usedEntries + nTargets of
        -- We have enough space
        False -> return $ Context lbl (usedEntries + nTargets) vec
        True -> do
          -- If doubling will fit the new elements, double the vector
          -- length.  Otherwise, add enough space to accommodate them
          -- all
          let extraEntries = max vlen nTargets
          newV <- V.grow vec extraEntries
          return $ Context lbl (usedEntries + nTargets) newV

      forM_ (zip [usedEntries..] tgts) $ \(ix, tgt) -> do
        V.write theVec ix tgt
      HT.insert ht src newCtxt
  return ()
  where
    nTargets = length tgts

-- | Return a list of the successor nodes
nodeSuccessors :: ImperativeGraph s lbl -> Node -> ST s [Node]
nodeSuccessors (IG ht) n = do
  ctxt <- HT.lookup ht n
  case ctxt of
    Nothing -> error ("Node does not exist in graph (and so has no successors): " ++ show n)
    Just (EmptyContext _) -> return []
    Just (Context _ usedEntries v) -> do
      immv <- IV.unsafeFreeze v
      return $! IV.foldl' (flip (:)) [] (IV.take usedEntries immv)

      -- forM [0..(usedEntries-1)] $ \x -> V.read v x

-- | Retrieve the label of a node
nodeLabels :: ImperativeGraph s lbl -> Node -> ST s (Maybe lbl)
nodeLabels (IG ht) n = do
  ctxt <- HT.lookup ht n
  case ctxt of
    Nothing -> return Nothing -- error ("Node does not exist in graph (and so has no label): " ++ show n)
    Just (EmptyContext lbl) -> return (Just lbl)
    Just (Context lbl _ _) -> return (Just lbl)

-- | Convert the imperative graph to a nice inductive graph (from FGL)
-- that can be exported from the ST monad.
toInductive :: ImperativeGraph s lbl -> ST s (Gr lbl ())
toInductive (IG ht) = do
  (lnodes, ledges) <- HT.foldM extractNodesAndEdges ([], []) ht
  return $! mkGraph lnodes (concat ledges)

-- | Fold helper to build the inductive graph
extractNodesAndEdges :: ([LNode lbl], [[LEdge ()]])
                      -> (Node, Context s lbl)
                      -> ST s ([LNode lbl], [[LEdge ()]])
extractNodesAndEdges (nacc, eacc) (k, ctxt) = case ctxt of
  EmptyContext lbl -> return $ ((k, lbl) : nacc, eacc)
  Context lbl usedEntries v -> do
    tgts <- forM [0..(usedEntries-1)] $ \x -> V.read v x
    let localEdges = zip3 (repeat k) tgts (repeat ())
    return $ ((k, lbl) : nacc, localEdges : eacc)