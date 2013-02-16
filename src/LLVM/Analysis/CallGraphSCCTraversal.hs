{-# LANGUAGE RankNTypes, ExistentialQuantification #-}
module LLVM.Analysis.CallGraphSCCTraversal (
  -- * Traversals
  callGraphSCCTraversal,
  parallelCallGraphSCCTraversal,

  -- * Types
  ComposableAnalysis,
  Lens,

  -- * Adaptors
  callGraphAnalysis,
  callGraphAnalysisM,
  callGraphComposeAnalysis,
  composableAnalysis,
  composableDependencyAnalysis,
  composableAnalysisM,
  composableDependencyAnalysisM,
  ) where

import LLVM.Analysis.CallGraph.Internal
