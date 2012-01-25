-- | Control Dependence Graphs for the LLVM IR
--
-- This module follows the definition of control dependence of Cytron et al
-- (http://dl.acm.org/citation.cfm?doid=115372.115320):
--
-- Let X and Y be nodes in the CFG.  If X appears on every path from Y
-- to Exit, then X postdominates Y.  If X postdominates Y but X != Y,
-- then X strictly postdominates Y.
--
-- A CFG node Y is control dependent on a CFG node X if both:
--
--  * There is a non-null path p from X->Y such that Y postdominates
--    every node *after* X on p.
--
--  * The node Y does not strictly postdominate the node X.
module Data.LLVM.CDG (
  -- * Types
  CDG,
  -- * Constructor
  controlDependenceGraph,
  -- * Queries
  controlDependencies,
  controlDependentOn
  ) where

import Data.Graph.Inductive

import Data.LLVM
import Data.LLVM.CFG
import Data.LLVM.Analysis.Dominance

-- | The internal representation of the CDG.  Instructions are
-- control-dependent on other instructions, so they are the nodes in
-- the graph.
type CDGType = Gr Instruction ()

data CDG = CDG { cdgGraph :: CDGType
               }

controlDependentOn :: CDG -> Instruction -> Instruction -> Bool
controlDependentOn = undefined

controlDependencies :: CDG -> Instruction -> [Instruction]
controlDependencies = undefined

controlDependenceGraph :: CFG -> CDG
controlDependenceGraph cfg = undefined
  where
    rcfg = reverseCFG cfg
    pdt = postdominatorTree rcfg