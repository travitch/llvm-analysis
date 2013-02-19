-- | This module defines an interface for intra-procedural dataflow
-- analysis (forward and backward).
--
-- The user defines an analysis with the 'dataflowAnalysis' function,
-- which can be constructed from a 'top' value, a 'meet' operator, and
-- a 'transfer' function (which is run as needed for 'Instruction's).
--
-- To use this dataflow analysis framework, pass it an initial
-- analysis state (which may be @top@ or a different value) and
-- something providing a control flow graph, along with the opaque
-- analysis object.  The analysis then returns an abstract result that
-- represents dataflow facts at each 'Instruction' in the 'Function'.
-- For example,
--
-- > let initialState = ...
-- >     a = dataflowAnalysis top meet transfer
-- >     results = forwardDataflow initialState analysis cfg
-- > in dataflowResult results
--
-- gives the dataflow value for the virtual exit node (to which all
-- other function termination instructions flow).  To get results at
-- other instructions, see 'dataflowResultAt'.
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
