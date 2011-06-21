{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.LLVM.Visualization ( viewCFG, viewCG ) where

import Data.GraphViz

import Data.LLVM.CFG
import Data.LLVM.CallGraph

viewCFG :: CFG -> IO ()
viewCFG cfg = do
  preview (cfgGraph cfg)
  let dg = graphToDot nonClusteredParams (cfgGraph cfg)
  s <- prettyPrint dg
  putStrLn s
  putStrLn "\n\n"

viewCG :: CallGraph -> IO ()
viewCG (CallGraph cg) = preview cg
