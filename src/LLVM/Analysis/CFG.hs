-- | This module defines control flow graphs over the LLVM IR.
module LLVM.Analysis.CFG (
  -- * Types
  CFG,
  HasCFG(..),
  -- * Constructors
  controlFlowGraph,
  -- * Accessors
  basicBlockPredecessors,
  basicBlockSuccessors
  ) where

import LLVM.Analysis.CFG.Internal
