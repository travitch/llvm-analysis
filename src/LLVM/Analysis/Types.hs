-- | Base types used in analysis components
module LLVM.Analysis.Types (
  FuncLike(..)
  ) where

import LLVM.Analysis

-- | A class for types that can be derived from a Function.
class FuncLike a where
  fromFunction :: Function -> a

instance FuncLike Function where
  fromFunction = id
