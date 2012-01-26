{-# LANGUAGE TemplateHaskell #-}
module Data.LLVM.Analysis.Dominance (
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
import FileLocation

import Data.LLVM
import Data.LLVM.Internal.PatriciaTree
import Data.LLVM.CFG

data DominatorTree = DT { dtTree :: Gr Instruction ()
                        , dtRoot :: Instruction
                        }

data PostdominatorTree = PDT { pdtTree :: Gr Instruction ()
                             , pdtRoot :: Instruction
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
immediatePostdominators RCFG { rcfgGraph = g, rcfgEntryNode = root } =
  map (toInst g *** toInst g) (iDom g root)

-- | Get a mapping from Instructions to each of their postdominators
postdominators :: RCFG -> [(Instruction, [Instruction])]
postdominators RCFG { rcfgGraph = g, rcfgEntryNode = root } =
  map (toInst g *** map (toInst g)) (dom g root)

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
      , pdtRoot = rcfgEntryValue cfg
      }
  where
    idoms = immediatePostdominators cfg
    g = rcfgGraph cfg

buildEdges :: [(Instruction, Instruction)] -> [LEdge ()]
buildEdges =
  map (\(a,b) -> (a, b, ())) . map (instructionUniqueId *** instructionUniqueId)

dominates :: DominatorTree -> Instruction -> Instruction -> Bool
dominates = undefined

-- | Check whether n postdominates m
--
-- > postdominates pdt n m
--
-- n postdominates m if there is a path from n to m in the
-- postdominator tree.
postdominates :: PostdominatorTree -> Instruction -> Instruction -> Bool
postdominates (PDT t _) n m =
  instructionUniqueId n `elem` rdfs [instructionUniqueId m] t

-- | Given two instructions, find their nearest common postdominator.
-- This uses a reverse DFS search from both instructions for
-- efficiency (since the graph is a tree, this will be a smaller set
-- than a forward DFS).
nearestCommonPostdominator :: PostdominatorTree -> Instruction -> Instruction -> Instruction
nearestCommonPostdominator (PDT t _) n m =
  case commonPrefix (reverse npdoms) (reverse mpdoms) of
    -- This case should really be impossible since this is a tree
    [] -> $err' ("No common postdominator for " ++ show n ++ " and " ++ show m)
    commonPostdom : _ -> toInst t commonPostdom
  where
    npdoms = rdfs [instructionUniqueId n] t
    mpdoms = rdfs [instructionUniqueId m] t

-- | Compute the transitive postdominators of a single Instruction.
-- Instructions postdominate themselves, and the list of
-- postdominators begins with the input instruction and ends with the
-- root of the postdominator tree (usually the ret node).
instructionPostdominators :: PostdominatorTree -> Instruction -> [Instruction]
instructionPostdominators (PDT t _) i =
  map (toInst t) $ rdfs [instructionUniqueId i] t

-- | Returns the common prefix of two lists (reversed - the last
-- common element appears first)
commonPrefix :: (Eq a) => [a] -> [a] -> [a]
commonPrefix [] _ = []
commonPrefix _ [] = []
commonPrefix (e1:rest1) (e2:rest2) =
  case e1 == e2 of
    True -> e1 : commonPrefix rest1 rest2
    False -> []

domTreeParams :: GraphvizParams n Instruction el () Instruction
domTreeParams =
  nonClusteredParams { fmtNode = \(_, l) -> [ toLabel (Value l) ] }

domTreeGraphvizRepr :: DominatorTree -> DotGraph Node
domTreeGraphvizRepr dt = graphToDot domTreeParams (dtTree dt)

postdomTreeGraphvizRepr :: PostdominatorTree -> DotGraph Node
postdomTreeGraphvizRepr dt = graphToDot domTreeParams (pdtTree dt)