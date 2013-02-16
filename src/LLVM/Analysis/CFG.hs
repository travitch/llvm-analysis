-- | This module defines control flow graphs over the LLVM IR.
module LLVM.Analysis.CFG (
  -- * Types
  CFG,
  HasCFG(..),
  -- -- * Constructors
  mkCFG,
  -- -- * Accessors
  basicBlockPredecessors,
  basicBlockSuccessors,
  -- -- * Visualization
  -- cfgGraphvizRepr
  ) where

import LLVM.Analysis.CFG.Internal

{-
-- Visualization

cfgGraphvizParams :: GraphvizParams n Instruction CFGEdge BasicBlock Instruction
cfgGraphvizParams =
  defaultParams { fmtNode = \(_,l) -> [toLabel (toValue l)]
                , fmtEdge = formatEdge
                , clusterID = Int . basicBlockUniqueId
                , fmtCluster = formatCluster
                , clusterBy = nodeCluster
                }
  where
    nodeCluster l@(_, i) =
      let Just bb = instructionBasicBlock i
      in C bb (N l)
    formatCluster bb = [GraphAttrs [toLabel (show (basicBlockName bb))]]
    formatEdge (_, _, l) =
      let lbl = toLabel l
      in case l of
        TrueEdge _ -> [lbl, color ForestGreen]
        FalseEdge _ -> [lbl, color Crimson]
        EqualityEdge _ _ -> [lbl, color DeepSkyBlue]
        IndirectEdge _ -> [lbl, color Indigo, style dashed]
        UnwindEdge _ -> [lbl, color Tomato4, style dotted]
        _ -> [lbl]

cfgGraphvizRepr :: CFG -> DotGraph NodeType
cfgGraphvizRepr cfg = graphElemsToDot cfgGraphvizParams ns es
  where
    g = cfgGraph cfg
    ns = labeledVertices g
    es = map (\(Edge s d l) -> (s, d, l)) (edges g)

-}
{-# ANN module "HLint: ignore Use if" #-}
