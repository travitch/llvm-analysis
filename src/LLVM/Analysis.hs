-- | This is the top-level LLVM IR module.  It provides definitions of
-- the IR and some basic functions to inspect it.  More advanced analysis
-- is provided by the modules in LLVM.Analysis.*.  They are not exported
-- here due to name collisions and to avoid namespace pollution.
module LLVM.Analysis (
  module Data.LLVM.Types
  ) where


import Data.LLVM.Types
