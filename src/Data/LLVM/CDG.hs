{-# LANGUAGE TemplateHaskell #-}
-- | Control Dependence Graphs for the LLVM IR
--
-- This module follows the definition of control dependence of Cytron et al
-- (http://dl.acm.org/citation.cfm?doid=115372.115320):
--
-- Let X and Y be nodes in the CFG.  If X appears on every path from Y
-- to Exit, then X postdominates Y.  If X postdominates Y but X != Y,
-- then X strictly postdominates Y.
--
-- A CFG node Y is control dependent on a CFG node X if both:
--
--  * There is a non-null path p from X->Y such that Y postdominates
--    every node *after* X on p.
--
--  * The node Y does not strictly postdominate the node X.
module Data.LLVM.CDG (
  -- * Types
  CDG,
  -- * Constructor
  controlDependenceGraph,
  -- * Queries
  controlDependencies,
  controlDependentOn,
  -- * Visualization
  cdgGraphvizRepr
  ) where

import Control.Arrow
import Data.Graph.Inductive
import Data.GraphViz
import Data.HashMap.Strict ( HashMap )
import qualified Data.HashMap.Strict as M
import Data.List ( foldl' )
import FileLocation
import Text.Printf

import Data.LLVM
import Data.LLVM.CFG
import Data.LLVM.Analysis.Dominance

import Debug.Trace
debug' = flip trace

-- | The internal representation of the CDG.  Instructions are
-- control-dependent on other instructions, so they are the nodes in
-- the graph.
type CDGType = Gr Instruction ()

data CDG = CDG { cdgGraph :: CDGType
               }

controlDependentOn :: CDG -> Instruction -> Instruction -> Bool
controlDependentOn = undefined

controlDependencies :: CDG -> Instruction -> [Instruction]
controlDependencies = undefined

-- | The typical construction augments the CFG with a fake start node.
-- Doing that here would be a bit complicated, so the graph just isn't
-- connected by a fake Start node.
controlDependenceGraph :: CFG -> CDG
controlDependenceGraph cfg = CDG $ mkGraph ns es
  where
    ns = labNodes g
    es = M.foldlWithKey' toEdge [] controlDeps
    g = cfgGraph cfg
    pdt = postdominatorTree (reverseCFG cfg)
    cfgEdges = map (($fromJst . lab g) *** ($fromJst . lab g)) (edges g)
    -- | All of the edges in the CFG m->n such that n does not
    -- postdominate m
    s = filter (isNotPostdomEdge pdt) cfgEdges
    controlDeps = foldr (extractDeps pdt) M.empty s `debug'` printf "S = (%s)" (show s)

-- | Determine if an edge belongs in the set S
isNotPostdomEdge :: PostdominatorTree -> (Instruction, Instruction) -> Bool
isNotPostdomEdge pdt (m, n) = not (postdominates pdt n m)

-- | n is control dependent on m, so add an edge from n->m
toEdge :: [LEdge ()] -> Instruction -> Instruction -> [LEdge ()]
toEdge acc n m = (instructionUniqueId n,
                  instructionUniqueId m,
                  ()) : acc

-- | Record control dependencies into a map (based on edges in S and
-- the postdominator tree).  The map is from instructions to the
-- nearest instruction that they are control dependent on.
extractDeps :: PostdominatorTree
               -> (Instruction, Instruction)
               -> HashMap Instruction Instruction
               -> HashMap Instruction Instruction
extractDeps pdt (m, n) cdeps =
  foldl' (addDep m) cdeps dependOnM
  where
    l = nearestCommonPostdominator pdt m n
    npdoms = instructionPostdominators pdt n
    -- All of the nodes from n to l in the postdominator tree
    dependOnM = takeWhile (/=l) npdoms

addDep :: Instruction
          -> HashMap Instruction Instruction
          -> Instruction
          -> HashMap Instruction Instruction
addDep m deps n =
  case M.lookup n deps of
    Nothing -> M.insert n m deps
    Just exMap -> $err' $
      printf "Already have a control dep mapping for [%s] -> [%s] (adding %s)" (show n) (show exMap) (show m)

-- toInst :: CFGType -> Node -> Instruction
-- toInst gr n =
--   let (_, _, i, _) = context gr n
--   in i

cdgGraphvizParams :: GraphvizParams n Instruction el () Instruction
cdgGraphvizParams =
  nonClusteredParams { fmtNode = \(_,l) -> [ toLabel (Value l) ]
                     }

cdgGraphvizRepr :: CDG -> DotGraph Node
cdgGraphvizRepr = graphToDot cdgGraphvizParams . cdgGraph

-- Note: can use graphElemsToDot to deal with non fgl graphs