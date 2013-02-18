-- | This module defines an interface for intra-procedural dataflow
-- analysis (forward and backward).
--
-- The user simply defines a type to represent the state of their
-- dataflow analysis as an instance of 'DataflowAnalysis'.  This class
-- adds one function, 'transfer', to the semilattices defined in the
-- lattices package.
--
-- To use this dataflow analysis framework, pass it an initial
-- analysis state and either a control flow graph or a function.  The
-- analysis then returns a function that maps instructions to the
-- dataflow value at that instruction.  For example,
--
-- > let initialState = ...
-- >     cfg = mkCFG f
-- >     results = forwardDataflow initialState cfg
-- > in results (cfgFinalValue cfg)
--
-- gives the dataflow value for the return instruction in function
-- @f@.  Any instruction in @f@ can be used as an argument to the
-- @result@ function.
module LLVM.Analysis.Dataflow (
  -- * Dataflow analysis
  DataflowAnalysis,
  dataflowAnalysis,
  fwdDataflowEdgeAnalysis,
  forwardDataflow,
  backwardDataflow,
  -- -- * Dataflow results
  DataflowResult,
  dataflowResult,
  dataflowResultAt
  ) where

import LLVM.Analysis.CFG.Internal
