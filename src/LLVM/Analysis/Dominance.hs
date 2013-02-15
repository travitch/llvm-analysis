-- | Tools to compute dominance information for functions.  Includes
-- postdominators.
--
-- A node @m@ postdominates a node @n@ iff every path from @n@ to
-- @exit@ passes through @m@.
--
-- This implementation is based on the dominator implementation in fgl,
-- which is based on the algorithm from Cooper, Harvey, and Kennedy:
--
--   http://www.cs.rice.edu/~keith/Embed/dom.pdf
module LLVM.Analysis.Dominance (
  -- * Types
  DominatorTree,
  PostdominatorTree,
  HasDomTree(..),
  HasPostdomTree(..),
  -- -- * Constructors
  -- dominators,
  -- immediateDominators,
  -- postdominators,
  -- immediatePostdominators,
  dominatorTree,
  postdominatorTree,
  -- -- * Queries
  dominates,
  postdominates,
  -- nearestCommonPostdominator,
  -- instructionPostdominators,
  -- -- * Visualization
  -- domTreeGraphvizRepr,
  -- postdomTreeGraphvizRepr
  ) where

import Control.Monad.Identity
import qualified Data.Foldable as F
import Data.Map ( Map )
import qualified Data.Map as M
import Data.Maybe ( fromMaybe )
import Data.Monoid
import Data.Set ( Set )
import qualified Data.Set as S
-- import Data.GraphViz

import LLVM.Analysis
import LLVM.Analysis.CFG
import LLVM.Analysis.Dataflow


data DominatorTree = DT CFG (Map Instruction Instruction)

class HasDomTree a where
  getDomTree :: a -> DominatorTree

instance HasDomTree DominatorTree where
  getDomTree = id

instance HasCFG DominatorTree where
  getCFG (DT cfg _) = cfg

instance HasFunction DominatorTree where
  getFunction = getFunction . getCFG

dominatorTree :: (HasCFG cfg) => cfg -> DominatorTree
dominatorTree f = DT cfg (toImmediateDominators doms)
  where
    cfg = getCFG f
    doms = dominatorAnalysis cfg

-- | Check whether n dominates m
dominates :: (HasDomTree t) => t -> Instruction -> Instruction -> Bool
dominates dt n m = checkDom m
  where
    (DT _ t) = getDomTree dt
    -- Walk backwards in the dominator tree looking for n
    checkDom i
      | i == n = True
      | otherwise = maybe False checkDom (M.lookup i t)

data PostdominatorTree = PDT CFG (Map Instruction Instruction)

class HasPostdomTree a where
  getPostdomTree :: a -> PostdominatorTree

instance HasPostdomTree CFG where
  getPostdomTree = postdominatorTree

instance HasCFG PostdominatorTree where
  getCFG (PDT cfg _) = cfg

instance HasFunction PostdominatorTree where
  getFunction = getFunction . getCFG

postdominatorTree :: (HasCFG f) => f -> PostdominatorTree
postdominatorTree f = PDT cfg (toImmediateDominators pdoms)
  where
    cfg = getCFG f
    pdoms = postdominatorAnalysis cfg

-- | Does n postdominate m?
postdominates :: (HasPostdomTree t) => t -> Instruction -> Instruction -> Bool
postdominates pdt n m = checkPDom m
  where
    PDT _ t = getPostdomTree pdt
    checkPDom i
      | i == n = True
      | otherwise = maybe False checkPDom (M.lookup i t)

-- Internal builder code


type Fact = Set Instruction

domAnalysis :: (Monad m) => Fact -> DataflowAnalysis m Fact
domAnalysis top = dataflowAnalysis top meet transfer
  where
    meet = S.intersection
    transfer doms i = return (S.insert i doms)

-- | Compute the set of dominators for each instruction in the CFG.
--
-- This is a simple dataflow analysis where top is the universal set
-- (all instructions in the function) and meet is set intersection.
-- The transfer function simply adds the current instruction to the
-- dataflow fact.
dominatorAnalysis :: CFG -> Map Instruction (Set Instruction)
dominatorAnalysis cfg = foldr (addInstFact dfr) mempty allInsts
  where
    s0 = S.singleton entryInst
    top = S.fromList allInsts
    allInsts@(entryInst:_)= functionInstructions (getFunction cfg)
    analysis = domAnalysis top
    dfr = runIdentity $ forwardDataflow cfg analysis s0

addInstFact :: DataflowResult Identity a
               -> Instruction
               -> Map Instruction a
               -> Map Instruction a
addInstFact dfr i acc =
  let f = runIdentity (dataflowResultAt dfr i)
  in M.insert i f acc

-- | In this case, we don't have an instruction that is a unique exit
-- point.  However, we know perfectly well that the virtual exit node
-- in the CFG postdominates everything so there isn't much need to
-- explicitly track that.
--
-- Thus, we start off with the empty set in the beginning.
--
-- Everything should actually remain the same here, despite this,
-- since users can't query based on the virtual exit node anyway.
postdominatorAnalysis :: CFG -> Map Instruction (Set Instruction)
postdominatorAnalysis cfg = foldr (addInstFact dfr) mempty allInsts
  where
    s0 = S.empty
    top = S.fromList allInsts
    allInsts = functionInstructions (getFunction cfg)
    analysis = domAnalysis top
    dfr = runIdentity $ backwardDataflow cfg analysis s0

-- | Compute the immediate dominators from the set of all dominators.
-- The entry instruction is not in the map because it has no immediate
-- dominator.
--
-- m `idom` n  IFF  m `sdom` n  AND  (p sdom n => p dom m)
toImmediateDominators :: Map Instruction (Set Instruction) -> Map Instruction Instruction
toImmediateDominators allDoms =
  foldr (addIdom allDoms) mempty $ M.toList allDoms

-- | Find m such that, for each node p in sdom n, p must also be in
-- dom[m]
addIdom :: Map Instruction (Set Instruction)
           -> (Instruction, Set Instruction)
           -> Map Instruction Instruction
           -> Map Instruction Instruction
addIdom allDoms (n, doms) acc =
  fromMaybe acc $ do
    m <- F.find tryOneM sdoms
    return $ M.insert n m acc
  where
    sdoms = S.delete n doms
    -- For each p in sdom, p must also be in dom[m].  We could
    -- probably cache some of these results to speed things up.
    tryOneM m =
      let Just mdom = M.lookup m allDoms
      in F.all (flip S.member mdom) sdoms

-- See Note [Immediate Dominators]

{- Note [Immediate Dominators]



-}

{-

type DomTreeType = DenseDigraph Instruction ()



instance HasDomTree CFG where
  getDomTree = dominatorTree

instance HasPostdomTree RCFG where
  getPostdomTree = postdominatorTree

-- | The standard dominator tree
data DominatorTree = DT { dtTree :: DomTreeType
                        , dtRoot :: Instruction
                        }

-- | The dominator tree of the reversed CFG.
data PostdominatorTree = PDT { pdtTree :: DomTreeType
                             , pdtRoots :: [Instruction]
                             }

-- | Compute the immediate dominators for a given CFG
immediateDominators :: CFG -> [(Instruction, Instruction)]
immediateDominators CFG { cfgGraph = g, cfgEntryNode = root } =
  map (toInst g *** toInst g) idoms
  where
    Just idoms = iDom g root

-- | Compute the full set of dominators for the given CFG
dominators :: CFG -> [(Instruction, [Instruction])]
dominators CFG { cfgGraph = g, cfgEntryNode = root } =
  map (toInst g *** map (toInst g)) doms
  where
    Just doms = dom g root

immediatePostdominators :: RCFG -> [(Instruction, Instruction)]
immediatePostdominators RCFG { rcfgGraph = g, rcfgFunction = f } =
  map (toInst g *** toInst g) (concat gs)
  where
    roots = functionExitInstructions f
    getIdoms r =
      let Just idoms = iDom g (instructionUniqueId r)
      in idoms
    gs = map getIdoms roots

-- | Get a mapping from Instructions to each of their postdominators
postdominators :: RCFG -> [(Instruction, [Instruction])]
postdominators RCFG { rcfgGraph = g, rcfgFunction = f } =
  map (toInst g *** map (toInst g)) (concat gs)
  where
    roots = functionExitInstructions f
    getDoms r =
      let Just doms = dom g (instructionUniqueId r)
      in doms
    gs = map getDoms roots

toInst :: (InspectableGraph gr) => gr -> Vertex -> VertexLabel gr
toInst gr n =
  let Just (Context _ _ i _) = context gr n
  in i

-- | Construct a dominator tree from a CFG
dominatorTree :: CFG -> DominatorTree
dominatorTree cfg =
  DT { dtTree = mkGraph ns (buildEdges idoms)
     , dtRoot = cfgEntryValue cfg
     }
  where
    idoms = immediateDominators cfg
    g = cfgGraph cfg
    ns = map (\(n, l) -> (n, l)) (labeledVertices g)

-- | Construct a postdominator tree from a reversed CFG
postdominatorTree :: RCFG -> PostdominatorTree
postdominatorTree cfg =
  PDT { pdtTree = mkGraph ns (buildEdges idoms)
      , pdtRoots = functionExitInstructions (rcfgFunction cfg)
      }
  where
    idoms = immediatePostdominators cfg
    g = rcfgGraph cfg
    ns = map (\(n, l) -> (n, l)) (labeledVertices g)

-- buildEdges :: [(Instruction, Instruction)] -> [LEdge ()]
buildEdges :: (Graph gr, EdgeLabel gr ~ ())
              => [(Instruction, Instruction)]
              -> [Edge gr]
buildEdges =
  map (toLEdge . (instructionUniqueId *** instructionUniqueId))
--  map (\(a,b) -> LEdge (Edge a b) ()) . map (instructionUniqueId *** instructionUniqueId)
  where
    toLEdge (a,b) = Edge a b () -- LEdge (Edge a b) ()

-- | Check whether n dominates m
dominates :: DominatorTree -> Instruction -> Instruction -> Bool
dominates  (DT t _) n m =
  instructionUniqueId n `elem` dfs [instructionUniqueId m] t

-- | Check whether n postdominates m
--
-- > postdominates pdt n m
--
-- n postdominates m if there is a path from n to m in the
-- postdominator tree.
postdominates :: PostdominatorTree -> Instruction -> Instruction -> Bool
postdominates (PDT t _) n m =
  instructionUniqueId n `elem` dfs [instructionUniqueId m] t

-- | Given two instructions, find their nearest common postdominator.
-- This uses a DFS search from each instruction to the root.
nearestCommonPostdominator :: PostdominatorTree -> Instruction -> Instruction -> Maybe Instruction
nearestCommonPostdominator (PDT t _) n m =
  case commonPrefix (reverse npdoms) (reverse mpdoms) of
    -- This case should really be impossible since this is a tree
    [] -> Nothing
    commonPostdom : _ -> Just $! toInst t commonPostdom
  where
    npdoms = dfs [instructionUniqueId n] t
    mpdoms = dfs [instructionUniqueId m] t

-- | Compute the transitive postdominators of a single Instruction.
-- Instructions postdominate themselves, and the list of
-- postdominators begins with the input instruction and ends with the
-- root of the postdominator tree (usually the ret node).
instructionPostdominators :: PostdominatorTree -> Instruction -> [Instruction]
instructionPostdominators (PDT t _) i =
  map (toInst t) $ dfs [instructionUniqueId i] t

-- | Returns the common prefix of two lists (reversed - the last
-- common element appears first)
commonPrefix :: (Eq a) => [a] -> [a] -> [a]
commonPrefix l1 l2 = reverse $ go l1 l2
  where
    go [] _ = []
    go _ [] = []
    go (e1:rest1) (e2:rest2) =
      case e1 == e2 of
        True -> e1 : go rest1 rest2
        False -> []

domTreeParams :: GraphvizParams n Instruction el () Instruction
domTreeParams =
  nonClusteredParams { fmtNode = \(_, l) -> [ toLabel (toValue l) ] }

-- Visualization

domTreeGraphvizRepr :: DominatorTree -> DotGraph Vertex
domTreeGraphvizRepr dt = graphElemsToDot domTreeParams ns es
  where
    g = dtTree dt
    ns = labeledVertices g
    es = map (\(Edge s d l) -> (s, d, l)) (edges g)

postdomTreeGraphvizRepr :: PostdominatorTree -> DotGraph Vertex
postdomTreeGraphvizRepr dt = graphElemsToDot domTreeParams ns es
  where
    g = pdtTree dt
    ns = labeledVertices g
    es = map (\(Edge s d l) -> (s, d, l)) (edges g)
-}
{-# ANN module "HLint: ignore Use if" #-}
