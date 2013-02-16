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
  -- -- * Constructor
  controlDependenceGraph,
  -- -- * Queries
  directControlDependencies,
  controlDependencies,
  -- controlDependentOn,
  -- -- * Visualization
  -- cdgGraphvizRepr
  ) where

-- import Data.GraphViz

import qualified Data.Foldable as F
import Data.Map ( Map )
import qualified Data.Map as M
import Data.Monoid
import Data.Set ( Set )
import qualified Data.Set as S

import LLVM.Analysis
import LLVM.Analysis.CFG
import LLVM.Analysis.Dominance

class HasCDG a where
  getCDG :: a -> CDG

instance HasCDG CDG where
  getCDG = id

-- | Warning, this is an expensive instance to invoke as it constructs
-- the CDG.
instance HasCDG PostdominatorTree where
  getCDG = controlDependenceGraph

instance HasPostdomTree CDG where
  getPostdomTree (CDG pdt _) = pdt

instance HasCFG CDG where
  getCFG = getCFG . getPostdomTree

instance HasFunction CDG where
  getFunction = getFunction . getCFG

data CDG = CDG PostdominatorTree (Map BasicBlock [BasicBlock])

{- Note [CDG Format]

The CDG is a mapping BasicBlocks to the other BasicBlocks that they
are /directly/ control dependent on.

-}

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
--    l->n (not including l) in PT are control dependent on m.  If
--    there is no common ancestor (disconnected PDT because of
--    multiple exit nodes), the lowest common ancestor is then the
--    virtual exit node, so /all/ of the postdominators of n are
--    control dependent on m.
--
-- Note: the typical construction augments the CFG with a fake start
-- node.  Doing that here would be a bit complicated, so the graph
-- just isn't connected by a fake Start node.
controlDependenceGraph :: (HasCFG f, HasPostdomTree f) => f -> CDG
controlDependenceGraph flike =
  CDG pdt $ fmap S.toList $ foldr addPairs mempty (functionBody f)
  where
    cfg = getCFG flike
    f = getFunction cfg
    pdoms = M.fromList $ postdominators pdt
    pdt = getPostdomTree flike
    addPairs bM acc =
      foldr (addCDGEdge pdt pdoms bM) acc (basicBlockSuccessors cfg bM)


-- | Get the list of instructions that @i@ is control dependent upon.
-- This list does not include @i@.  As noted above, the list will be
-- empty if @i@ is executed unconditionally.
--
-- > controlDependences cdg i
controlDependencies :: (HasCDG cdg) => cdg -> Instruction -> [Instruction]
controlDependencies cdgLike i =
  map basicBlockTerminatorInstruction (S.toList res)
  where
    CDG _ cdg = getCDG cdgLike
    Just bb = instructionBasicBlock i
    res = undefined

-- | Get the list of instructions that @i@ is directly control
-- dependent upon.
directControlDependencies :: (HasCDG cdg) => cdg -> Instruction -> [Instruction]
directControlDependencies cdgLike i =
  maybe [] (map basicBlockTerminatorInstruction) (M.lookup bb m)
  where
    CDG _ m = getCDG cdgLike
    Just bb = instructionBasicBlock i

-- Implementation


-- | For each block M and each successor of M, N, add (M,N) if the
-- first instruction of N does not postdominate the terminator
-- instruction of M.
addCDGEdge :: PostdominatorTree -- ^ The postdominator tree
              -> Map Instruction [Instruction] -- ^ The entire postdom relation
              -> BasicBlock -- ^ M
              -> BasicBlock -- ^ N
              -> Map BasicBlock (Set BasicBlock)
              -> Map BasicBlock (Set BasicBlock)
addCDGEdge pdt pdoms bM bN acc
  -- If it is a postdominator, this is not an edge in S
  | postdominates pdt nEntry mTerm = acc
  -- Otherwise it is and we need to find a common ancestor in the
  -- PDT
  | otherwise = case commonAncestor mpdoms npdoms of
    Just l ->
      let cdepsOnM = bN : postdomBlocks (filter (/=l) npdoms)
      in foldr addControlDep acc cdepsOnM
    -- If there is no common ancestor, then all of the
    -- postdominators of n are control dependent on m.
    Nothing ->
      let deps = bN : postdomBlocks npdoms
      in foldr addControlDep acc deps
  where
    addControlDep b = M.insertWith S.union b (S.singleton bM)
    mTerm = basicBlockTerminatorInstruction bM
    nEntry : _ = basicBlockInstructions bN
    -- These lookups should never fail (unless the caller provided
    -- the postdominator tree for a different function).  the
    -- postdominators function just returns empty sets, and the
    -- function handles /every/ instruction in the input function.
    Just mpdoms = M.lookup mTerm pdoms
    Just npdoms = M.lookup nEntry pdoms

-- | Convert a list of Instructions into the list of their
-- BasicBlocks.  There are no repetitions in the result.
postdomBlocks :: [Instruction] -> [BasicBlock]
postdomBlocks = S.toList . foldr addInstBlock mempty
  where
    addInstBlock i acc =
      let Just bb = instructionBasicBlock i
      in S.insert bb acc

-- | Given two lists, find the first element they share in common (if
-- any).
commonAncestor :: [Instruction] -> [Instruction] -> Maybe Instruction
commonAncestor l1 = F.find (`elem` l1)

{- Note [CDG]

We can compute the CDG based on just the blocks in the graph.  All of
the instructions in a given basic block are always at the same level
in the CDG and depend on the same control decisions as the first
instruction in the block.

We also only need to store the blocks, since any instruction looked up
has a back-pointer to its block, which will let us look it up in the
CDG.

Start by finding the set S, where we just consider connected
BasicBlocks.

-}

{-

-- | The internal representation of the CDG.  Instructions are
-- control-dependent on other instructions, so they are the nodes in
-- the graph.
type CDGType = DenseDigraph Instruction ()
type LEdgeType = Edge CDGType

-- | A control depenence graph
data CDG = CDG { cdgGraph :: CDGType
               , cdgCFG :: CFG
               }


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
  filter (/= i) deps
  where
    deps = map (safeLab "LLVM.Analysis.CDG.controlDependnecies.deps" g) $ dfs [instructionUniqueId i] g

safeLab :: (InspectableGraph gr)
           => String -> gr -> Vertex -> VertexLabel gr
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
    ns = map (\(n, l) -> (n, l)) $ labeledVertices g
    es = M.foldlWithKey' toEdge [] controlDeps

    g = cfgGraph cfg
    pdt = postdominatorTree (reverseCFG cfg)
    eloc = "LLVM.Analysis.CDG.controlDependenceGraph.cfgEdges"
    cfgEdges = map (\(Edge src dst _) -> (safeLab eloc g src, safeLab eloc g dst)) (edges g)
    -- cfgEdges = map ((safeLab $__LOCATION__ g) *** (safeLab $__LOCATION__ g)) (edges g)
    -- | All of the edges in the CFG m->n such that n does not
    -- postdominate m
    s = filter (isNotPostdomEdge pdt) cfgEdges
    controlDeps = foldr (extractDeps pdt) M.empty s

-- | Determine if an edge belongs in the set S
isNotPostdomEdge :: PostdominatorTree -> (Instruction, Instruction) -> Bool
isNotPostdomEdge pdt (m, n) = instructionIsTerminator m && not (postdominates pdt n m)

-- | Add an edge from @dependent@ to each @m@ it is control dependent on
toEdge :: [LEdgeType] -> Instruction -> HashSet Instruction -> [LEdgeType]
toEdge acc dependent = S.foldr (toE dependent) acc
  where
    toE n m a = Edge (instructionUniqueId n) (instructionUniqueId m) () : a

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
  defaultParams { fmtNode = \(_,l) -> [ toLabel (toValue l) ]
                , clusterID = Int . basicBlockUniqueId
                , clusterBy = nodeCluster
                , fmtCluster = formatCluster
                }
  where
    nodeCluster l@(_, i) =
      let Just bb = instructionBasicBlock i
      in C bb (N l)
    formatCluster bb = [GraphAttrs [toLabel (show (basicBlockName bb))]]

cdgGraphvizRepr :: CDG -> DotGraph Vertex
cdgGraphvizRepr cdg = graphElemsToDot cdgGraphvizParams ns es
  where
    g = cdgGraph cdg
    ns = labeledVertices g
    es = map (\(Edge s d l) -> (s, d, l)) (edges g)
-}