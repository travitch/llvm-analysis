{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.LLVM.Visualization ( viewCFG, viewCG, viewICFG, icfgParams ) where

import Data.GraphViz hiding ( EdgeType )

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

icfgParams :: GraphvizParams NodeType EdgeType () NodeType
icfgParams =
  nonClusteredParams {
    fmtNode = \(_,l) -> case l of
       ExternalEntry Nothing -> [toLabel "UnknownEntry"]
       ExternalExit Nothing -> [toLabel "UnknownExit"]
       ExternalEntry (Just ef) -> [toLabel ("ExternalEntry: " ++ show (externalFunctionName ef))]
       ExternalExit (Just ef) -> [toLabel ("ExternalExit: " ++ show (externalFunctionName ef))]
       InstNode i -> [toLabel (Value i), Shape BoxShape],
    fmtEdge = \(_,_,l) -> let lab = toLabel l
                          in case l of
                            CallToEntry _ -> [lab, Style [SItem Dashed []]]
                            ReturnToCall _ -> [lab, Style [SItem Dashed []]]
                            CallToReturn -> [lab, Style [SItem Dotted []]]
                            IntraEdge _ -> [lab]
    }

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
