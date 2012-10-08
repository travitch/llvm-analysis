{-# LANGUAGE FlexibleContexts #-}
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
--
-- This CDG formulation does not insert a dummy Start node to link
-- together all of the top-level nodes.  This just means that the set
-- of control dependencies can be empty if code will be executed
-- unconditionally.
module LLVM.Analysis.CDG (
  -- * Types
  CDG,
  HasCDG(..),
  -- * Constructor
  controlDependenceGraph,
  -- * Queries
  directControlDependencies,
  controlDependencies,
  controlDependentOn,
  -- * Visualization
  cdgGraphvizRepr
  ) where

import Data.GraphViz
import Data.HashMap.Strict ( HashMap )
import qualified Data.HashMap.Strict as M
import Data.HashSet ( HashSet )
import qualified Data.HashSet as S
import Data.List ( foldl' )
import Data.Maybe ( fromMaybe )

import Data.Graph.Interface
import Data.Graph.LazyHAMT
import Data.Graph.Algorithms.Marking.DFS

import LLVM.Analysis
import LLVM.Analysis.CFG
import LLVM.Analysis.Dominance

-- | The internal representation of the CDG.  Instructions are
-- control-dependent on other instructions, so they are the nodes in
-- the graph.
type CDGType = Gr Instruction ()
type NodeType = Node CDGType
type LEdgeType = LEdge CDGType
type LNodeType = LNode CDGType

-- | A control depenence graph
data CDG = CDG { cdgGraph :: CDGType
               , cdgCFG :: CFG
               }

class HasCDG a where
  getCDG :: a -> CDG

instance HasCFG CDG where
  getCFG = cdgCFG

-- | Return True if @n@ is control dependent on @m@.
--
-- > controlDependentOn cdg m n
controlDependentOn :: CDG -> Instruction -> Instruction -> Bool
controlDependentOn cdg m n = m `elem` controlDependencies cdg n

-- | Get the list of instructions that @i@ is control dependent upon.
-- This list does not include @i@.  As noted above, the list will be
-- empty if @i@ is executed unconditionally.
--
-- > controlDependences cdg i
controlDependencies :: CDG -> Instruction -> [Instruction]
controlDependencies (CDG g _) i =
  case deps of
    _ : rest -> rest
    _ -> error ("LLVM.Analysis.CDG.controlDependencies: Instruction should at least be reachable from itself: " ++ show i)
  where
    deps = map (safeLab "LLVM.Analysis.CDG.controlDependnecies.deps" g) $ dfs [instructionUniqueId i] g

safeLab :: (Show (Node gr), InspectableGraph gr)
           => String -> gr -> Node gr -> NodeLabel gr
safeLab loc g n = fromMaybe errMsg (lab g n)
  where
    errMsg = error (loc ++ ": missing label for CDG node " ++ show n)

-- | Get the list of instructions that @i@ is directly control
-- dependent upon.
directControlDependencies :: CDG -> Instruction -> [Instruction]
directControlDependencies (CDG g _) i =
  map (safeLab "LLVM.Analysis.CDG.directControlDependencies" g) $ suc g (instructionUniqueId i)

-- | Construct the control dependence graph for a function (from its
-- CFG).  This follows the construction from chapter 9 of the
-- Munchnick Compiler Design and Implementation book.
--
-- For an input function F:
--
-- 1) Construct the CFG G for F
--
-- 2) Construct the postdominator tree PT for F
--
-- 3) Let S be the set of edges m->n in G such that n does not
--    postdominate m
--
-- 4) For each edge m->n in S, find the lowest common ancestor l of m
--    and n in the postdominator tree.  All nodes on the path from
--    l->n (not including l) in PT are control dependent on m.
--
-- Note: the typical construction augments the CFG with a fake start
-- node.  Doing that here would be a bit complicated, so the graph
-- just isn't connected by a fake Start node.
controlDependenceGraph :: CFG -> CDG
controlDependenceGraph cfg = CDG (mkGraph ns es) cfg
  where
    ns = map (\(LNode n l) -> LNode n l) $ labNodes g
    es = M.foldlWithKey' toEdge [] controlDeps

    g = cfgGraph cfg
    pdt = postdominatorTree (reverseCFG cfg)
    eloc = "LLVM.Analysis.CDG.controlDependenceGraph.cfgEdges"
    cfgEdges = map (\(Edge src dst) -> (safeLab eloc g src, safeLab eloc g dst)) (edges g)
    -- cfgEdges = map ((safeLab $__LOCATION__ g) *** (safeLab $__LOCATION__ g)) (edges g)
    -- | All of the edges in the CFG m->n such that n does not
    -- postdominate m
    s = filter (isNotPostdomEdge pdt) cfgEdges
    controlDeps = foldr (extractDeps pdt) M.empty s

-- | Determine if an edge belongs in the set S
isNotPostdomEdge :: PostdominatorTree -> (Instruction, Instruction) -> Bool
isNotPostdomEdge pdt (m, n) = not (postdominates pdt n m)

-- | Add an edge from @dependent@ to each @m@ it is control dependent on
toEdge :: [LEdgeType] -> Instruction -> HashSet Instruction -> [LEdgeType]
toEdge acc dependent = S.foldr (toE dependent) acc
  where
    toE n m a = LEdge (Edge (instructionUniqueId n) (instructionUniqueId m)) () : a

-- | A private type to describe what instructions the keys of the map
-- are control dependent upon.
type DepMap = HashMap Instruction (HashSet Instruction)

-- | Record control dependencies into a map (based on edges in S and
-- the postdominator tree).  The map is from instructions to the
-- nearest instruction that they are control dependent on.
extractDeps :: PostdominatorTree
               -> (Instruction, Instruction)
               -> DepMap
               -> DepMap
extractDeps pdt (m, n) cdeps =
  foldl' (addDep m) cdeps dependOnM
  where
    l = nearestCommonPostdominator pdt m n
    npdoms = instructionPostdominators pdt n
    -- All of the nodes from n to l in the postdominator tree,
    -- ignoring l.  If there was no common ancestor (e.g., there were
    -- multiple exit instructions), take all of the postdominators of
    -- n.
    dependOnM = case l of
      Just l' -> takeWhile (/=l') npdoms
      Nothing -> npdoms

-- | Multiple predecessors *ARE* allowed.  Consider
--
-- > void acl_create_entry(int *other_p, int *entry_p) {
-- >   if(!other_p || !entry_p) {
-- >     if(entry_p)
-- >       *entry_p = 5;
--
-- >     return;
-- >   }
--
-- >   *entry_p = 6;
-- > }
--
-- The second comparison of entry_p against NULL directly depends on
-- both conditions above it.  If !other_p is true, that is the
-- immediate dependency.  Otherwise, if !entry_p is true (but !other_p
-- is false), it is also a direct dependency.
addDep :: Instruction -> DepMap -> Instruction -> DepMap
addDep m deps n = M.insertWith S.union n (S.singleton m) deps



-- Visualization

cdgGraphvizParams :: GraphvizParams n Instruction el BasicBlock Instruction
cdgGraphvizParams =
  nonClusteredParams { fmtNode = \(_,l) -> [ toLabel (toValue l) ]
                     , clusterID = Int . basicBlockUniqueId
                     , clusterBy = nodeCluster
                     , fmtCluster = formatCluster
                     }
  where
    nodeCluster l@(_, i) =
      let Just bb = instructionBasicBlock i
      in C bb (N l)
    formatCluster bb = [GraphAttrs [toLabel (show (basicBlockName bb))]]

cdgGraphvizRepr :: CDG -> DotGraph (Node CDGType)
cdgGraphvizRepr cdg = graphElemsToDot cdgGraphvizParams ns es
  where
    g = cdgGraph cdg
    ns = map toNodeTuple (labNodes g)
    es = map toEdgeTuple (labEdges g)
