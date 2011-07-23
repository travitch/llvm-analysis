{-# OPTIONS_GHC -funbox-strict-fields #-}
module Data.LLVM.Analysis.IFDS where

import Data.Graph.Inductive hiding ( (><) )
import Data.LLVM.ICFG

class IFDSAnalysis a domElem where
  flow :: a -> domElem -> Instruction -> [CFGEdge] -> [domElem]
  -- ^ Compute local flow information for the domain element after this
  -- instruction is executed.  The returned list is the list of domain
  -- variables reachable from this domain element after the statement is
  -- executed.  This models local control flow.
  callFlow :: a -> domElem -> Instruction -> [CFGEdge] -> [domElem]
  -- ^ Similar to 'flow', but models local information flow across
  -- call->return edges.
  passArgs :: a -> domElem -> [domElem]
  returnVal :: a -> domElem -> [domElem]
  analysisBandwidth :: a -> Int

-- | An edge from <s_p, d_1> to <n, d_2> noting that <n, d_2> is
-- reachable in the exploded supergraph from <s_p, d_1>.  s_p is not
-- explicitly recorded because it is uniquely determined by n.
data PathEdge domElem = PathEdge !domElem !Node !domElem

-- After the analysis is done, reduce the Set PathEdge -> Map (Node,
-- domElem) [domElem] to start answering queries.  Really, only the
-- second two members are useful (reachable vs not reachable).

-- | An edge summarizing the flow information for the call site
-- identified by the node.
data SummaryEdge domElem = SummaryEdge !Node !domElem !domElem

{-

For the main switch statement, there are three cases:

 * Call node

   Can distinguish these InstNodes because it will always be a call or
   invoke instruction

 * Exit node

   These will always be ret instructions

 * Everything else


The current function is always accessible in constant time because any
instruction has constant-time access to its enclosing function.
Matching edges is a simple equality test on the set of all outgoing
edges from ret nodes.

Don't bother building any of the exploded supergraph explicitly.  Just
keep a Set of the PathEdges.  Reachability queries are then of the form:

  S.member (s_p, nodeid) pathEdge

For nodeid in procedure p.

-}