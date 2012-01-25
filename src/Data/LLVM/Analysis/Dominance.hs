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
  postdominates
  ) where

import Control.Arrow
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Query.Dominators

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

toInst :: CFGType -> Node -> Instruction
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
postdominates :: PostdominatorTree -> Instruction -> Instruction -> Bool
postdominates = undefined

nearestCommonPostdominator :: PostdominatorTree -> Instruction -> Instruction -> Instruction
nearestCommonPostdominator = undefined