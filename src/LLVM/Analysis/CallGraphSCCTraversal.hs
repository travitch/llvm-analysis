-- | This module provides a framework for analyzing LLVM Modules
-- bottom-up with regard to the call graph.  The analysis starts at
-- the leaves and propagates summary information up the call graph.
-- Strongly-connected components (hence the SCC in the module name)
-- are analyzed until a fixed-point is reached.
--
-- Analysis functions can be either pure or monadic; the adaptors take
-- summary functions of various shapes and convert them into a form
-- suitable for the traversal engine.
--
-- The traversal also processes independent strongly-connected
-- components in parallel with as many cores as the process has been
-- allocated.
module LLVM.Analysis.CallGraphSCCTraversal (
  -- * Traversals
  callGraphSCCTraversal,
  parallelCallGraphSCCTraversal,

  -- * Types
  ComposableAnalysis,

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
