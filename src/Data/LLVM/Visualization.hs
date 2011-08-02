{-# OPTIONS_GHC -fno-warn-orphans -funbox-strict-fields #-}
module Data.LLVM.Visualization ( viewCFG, viewCG, viewICFG, icfgParams ) where

import Data.GraphViz

import Data.LLVM
import Data.LLVM.CFG
import Data.LLVM.CallGraph
import Data.LLVM.ICFG

viewCFG :: CFG -> IO ()
viewCFG cfg = do
  let params = nonClusteredParams { fmtNode = \(_,l) -> [toLabel (Value l)]
                                  , fmtEdge = \(_,_,l) -> [toLabel (l)]
                                  }
      dg = graphToDot params (cfgGraph cfg)
  _ <- runGraphvizCanvas' dg Gtk
  return ()

data ICFGCluster = CUnknown
                 | CExternalFunction !ExternalFunction
                 | CFunction !Function
                 | CBlock !BasicBlock
                 deriving (Eq, Ord)

-- | Generate a Graph Identifier for each cluster node for the ICFG
clusterIdent :: ICFGCluster -> GraphID
clusterIdent CUnknown = Str "unknown"
clusterIdent (CExternalFunction ef) = Int $ externalFunctionUniqueId ef
clusterIdent (CFunction f) = Int $ functionUniqueId f
clusterIdent (CBlock b) = Int $ basicBlockUniqueId b

instance Show ICFGCluster where
  show CUnknown = "Unknown"
  show (CExternalFunction ef) = show (externalFunctionName ef)
  show (CFunction f) = show (functionName f)
  show (CBlock b) = show (basicBlockName b)

-- | A set of graphviz parameters suitable for visualizing an 'ICFG'.
-- These graph settings group instructions into basic blocks and basic
-- blocks into functions using GraphViz clusters.
icfgParams :: GraphvizParams ICFGNode ICFGEdge ICFGCluster ICFGNode
icfgParams =
  defaultParams { fmtNode = formatNode
                , fmtEdge = formatEdge
                , clusterBy = nodeCluster
                , clusterID = Just . clusterIdent
                , fmtCluster = formatCluster
                }
  where
    formatCluster CUnknown = [GraphAttrs { attrs = [toLabel "UnknownFunction"] } ]
    formatCluster (CExternalFunction ef) = [GraphAttrs { attrs = [toLabel (show (externalFunctionName ef))] } ]
    formatCluster (CFunction f) = [GraphAttrs { attrs = [toLabel (show (functionName f))] } ]
    formatCluster (CBlock b) = [GraphAttrs { attrs = [toLabel (show (basicBlockName b))] } ]
    nodeCluster l@(_, ExternalNode Nothing) = C CUnknown (N l)
    nodeCluster l@(_, ExternalNode (Just ef)) = C (CExternalFunction ef) (N l)
    nodeCluster l@(_, ReturnNode i) = C (CFunction f) (C (CBlock bb) (N l))
      where
        Just bb = instructionBasicBlock i
        f = basicBlockFunction bb
    nodeCluster l@(_, InstNode i) = C (CFunction f) (C (CBlock bb) (N l))
      where
        Just bb = instructionBasicBlock i
        f = basicBlockFunction bb
    formatNode (_,l) = case l of
      ExternalNode Nothing -> [toLabel "UnknownFunction"]
      ExternalNode (Just ef) -> [toLabel ("ExternalFunction: " ++ show (externalFunctionName ef))]
      InstNode i -> [toLabel (Value i), Shape BoxShape]
      ReturnNode _ -> [toLabel "ReturnNode", Shape BoxShape]
    formatEdge (_,_,l) =
      let lab = toLabel l
      in case l of
        CallToEntry _ -> [lab, Style [SItem Dashed []], Color [X11Color DeepSkyBlue]]
        ReturnToCall _ -> [lab, Style [SItem Dashed []], Color [X11Color Crimson]]
        CallToReturn -> [lab, Style [SItem Dotted []], Color [X11Color ForestGreen]]
        IntraEdge _ -> [lab]

viewICFG :: ICFG -> IO ()
viewICFG icfg = do
  let dg = graphToDot icfgParams (icfgGraph icfg)
  _ <- runGraphvizCanvas' dg Gtk
  return ()

viewCG :: CallGraph -> IO ()
viewCG cg = do
  let params = nonClusteredParams { fmtNode = \(_,l) -> [toLabel (l)]
                                  , fmtEdge = \(_,_,l) -> [toLabel (l)]
                                  }
      dg = graphToDot params (callGraphRepr cg)
  _ <- runGraphvizCanvas' dg Gtk
  return ()
