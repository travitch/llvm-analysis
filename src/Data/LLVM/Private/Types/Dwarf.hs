{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE StandaloneDeriving #-}
module Data.LLVM.Private.Types.Dwarf (
    -- * Types
  DW_VAR_TAG(..)
  ) where

import Data.Dwarf

-- | This is a temporary type until the dwarf package supports DWARF4
-- extensions
data DW_VAR_TAG = DW_TAG_auto_variable
                | DW_TAG_arg_variable
                | DW_TAG_return_variable
                deriving (Show, Eq)

deriving instance Ord DW_LANG
deriving instance Ord DW_VIRTUALITY
deriving instance Ord DW_ATE
deriving instance Ord DW_TAG
deriving instance Ord DW_VAR_TAG

