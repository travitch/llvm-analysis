{-# LANGUAGE TemplateHaskell #-}
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
  -- * Constructors
  dominators,
  immediateDominators,
  postdominators,
  immediatePostdominators,
  dominatorTree,
  postdominatorTree,
  -- * Queries
  dominates,
  postdominates,
  nearestCommonPostdominator,
  instructionPostdominators,
  -- * Visualization
  domTreeGraphvizRepr,
  postdomTreeGraphvizRepr
  ) where

import Control.Arrow
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Query.DFS
import Data.Graph.Inductive.Query.Dominators
import Data.GraphViz

import LLVM.Analysis
import LLVM.Analysis.CFG
import LLVM.Analysis.Internal.PatriciaTree

-- | The standard dominator tree
data DominatorTree = DT { dtTree :: Gr Instruction ()
                        , dtRoot :: Instruction
                        }

-- | The dominator tree of the reversed CFG.
data PostdominatorTree = PDT { pdtTree :: Gr Instruction ()
                             , pdtRoots :: [Instruction]
                             }

-- | Compute the immediate dominators for a given CFG
immediateDominators :: CFG -> [(Instruction, Instruction)]
immediateDominators CFG { cfgGraph = g, cfgEntryNode = root } =
  map (toInst g *** toInst g) (iDom g root)

-- | Compute the full set of dominators for the given CFG
dominators :: CFG -> [(Instruction, [Instruction])]
dominators CFG { cfgGraph = g, cfgEntryNode = root } =
  map (toInst g *** map (toInst g)) (dom g root)

immediatePostdominators :: RCFG -> [(Instruction, Instruction)]
immediatePostdominators RCFG { rcfgGraph = g, rcfgFunction = f } =
  map (toInst g *** toInst g) (concat gs) -- (iDom g root)
  where
    roots = functionExitInstructions f
    gs = map (\r -> iDom g (instructionUniqueId r)) roots

-- | Get a mapping from Instructions to each of their postdominators
postdominators :: RCFG -> [(Instruction, [Instruction])]
postdominators RCFG { rcfgGraph = g, rcfgFunction = f } =
  map (toInst g *** map (toInst g)) (concat gs) -- (dom g root)
  where
    roots = functionExitInstructions f
    gs = map (\r -> dom g (instructionUniqueId r)) roots

toInst :: (Graph gr) => gr Instruction a -> Node -> Instruction
toInst gr n =
  let (_, _, i, _) = context gr n
  in i

-- | Construct a dominator tree from a CFG
dominatorTree :: CFG -> DominatorTree
dominatorTree cfg =
  DT { dtTree = mkGraph (labNodes g) (buildEdges idoms)
     , dtRoot = cfgEntryValue cfg
     }
  where
    idoms = immediateDominators cfg
    g = cfgGraph cfg

-- | Construct a postdominator tree from a reversed CFG
postdominatorTree :: RCFG -> PostdominatorTree
postdominatorTree cfg =
  PDT { pdtTree = mkGraph (labNodes g) (buildEdges idoms)
      , pdtRoots = functionExitInstructions (rcfgFunction cfg)
      }
  where
    idoms = immediatePostdominators cfg
    g = rcfgGraph cfg

buildEdges :: [(Instruction, Instruction)] -> [LEdge ()]
buildEdges =
  map (\(a,b) -> (a, b, ())) . map (instructionUniqueId *** instructionUniqueId)

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
  nonClusteredParams { fmtNode = \(_, l) -> [ toLabel (Value l) ] }

-- Visualization

domTreeGraphvizRepr :: DominatorTree -> DotGraph Node
domTreeGraphvizRepr dt = graphToDot domTreeParams (dtTree dt)

postdomTreeGraphvizRepr :: PostdominatorTree -> DotGraph Node
postdomTreeGraphvizRepr dt = graphToDot domTreeParams (pdtTree dt)