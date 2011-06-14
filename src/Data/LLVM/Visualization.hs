{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.LLVM.Visualization ( viewCFG ) where

import Data.GraphViz

import Data.LLVM.CFG
import Data.LLVM.Types

instance Labellable Value where
  toLabel _ = (Label . StrLabel) "" --  . show

instance Labellable EdgeCondition where
  toLabel _ = (Label . StrLabel) "" -- . show


viewCFG :: CFG -> IO ()
viewCFG cfg = do
  -- preview cfg
  let dg = graphToDot nonClusteredParams (cfgGraph cfg)
  s <- prettyPrint dg
  putStrLn s
  putStrLn "\n\n"

