{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.LLVM.Visualization ( viewCFG, viewCG, viewICFG ) where

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

viewICFG :: ICFG -> IO ()
viewICFG icfg = do
  let params = nonClusteredParams {
        fmtNode = \(_,l) -> case l of
           ExternalEntry Nothing -> [toLabel "UnknownEntry"]
           ExternalExit Nothing -> [toLabel "UnknownExit"]
           ExternalEntry (Just ef) -> [toLabel ("ExternalEntry: " ++ show (externalFunctionName ef))]
           ExternalExit (Just ef) -> [toLabel ("ExternalExit: " ++ show (externalFunctionName ef))]
           InstNode i -> [toLabel (Value i)],
        fmtEdge = \(_,_,l) -> [toLabel l]
        }
      dg = graphToDot params (icfgGraph icfg)
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
