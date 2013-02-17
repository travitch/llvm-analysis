-- | This module defines a call graph and related functions.  The call
-- graph is a static view of the calls between functions in a
-- 'Module'.  The nodes of the graph are global functions and the
-- edges are calls made to other functions.
--
-- This call graph attempts to provide as much information as possible
-- about calls through function pointers.  Direct calls have a single
-- outgoing edge.  Indirect calls that can be augmented with
-- information from a points-to analysis can induce many IndirectCall
-- edges.
--
-- For now, all indirect calls also induce an UnknownCall edge, under
-- the assumption that externally-obtained function pointers may also
-- be called somehow.  This restriction will eventually be lifted and
-- indirect calls that can be identified as completely internal will
-- not have the UnknownCall edge.  The preconditions for this will be:
--
-- * The 'Module' must have an entry point (otherwise it is a library)
--
-- * The function pointer must not be able to alias the result of a
--   dlopen or similar call
--
-- Again, the more sophisticated callgraph is still pending.
module LLVM.Analysis.CallGraph (
  -- * Types
  CallGraph,
  -- * Constructor
  callGraph,
  -- * Accessors
  callValueTargets,
  callSiteTargets,
  callGraphFunctions,
  functionCallees,
  allFunctionCallees,
  functionCallers,
  allFunctionCallers
  ) where

import LLVM.Analysis.CallGraph.Internal
