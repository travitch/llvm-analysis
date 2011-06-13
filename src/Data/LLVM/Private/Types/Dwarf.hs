module Data.LLVM.Private.Types.Dwarf (
    -- * Types
  DW_VAR_TAG(..)
  ) where

-- | This is a temporary type until the dwarf package supports DWARF4
-- extensions
data DW_VAR_TAG = DW_TAG_auto_variable
                | DW_TAG_arg_variable
                | DW_TAG_return_variable
                deriving (Show, Eq)


