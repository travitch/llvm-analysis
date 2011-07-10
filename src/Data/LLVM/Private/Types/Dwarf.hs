{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE StandaloneDeriving #-}
module Data.LLVM.Private.Types.Dwarf (
    -- * Types
  module Data.Dwarf,
  DW_TAG(..),
  dw_tag
  ) where

import Data.Dwarf hiding ( DW_TAG(..) )

deriving instance Ord DW_LANG
deriving instance Ord DW_VIRTUALITY
deriving instance Ord DW_ATE
deriving instance Ord DW_TAG

data DW_TAG
    = DW_TAG_array_type
    | DW_TAG_class_type
    | DW_TAG_entry_point
    | DW_TAG_enumeration_type
    | DW_TAG_formal_parameter
    | DW_TAG_imported_declaration
    | DW_TAG_label
    | DW_TAG_lexical_block
    | DW_TAG_member
    | DW_TAG_pointer_type
    | DW_TAG_reference_type
    | DW_TAG_compile_unit
    | DW_TAG_string_type
    | DW_TAG_structure_type
    | DW_TAG_subroutine_type
    | DW_TAG_typedef
    | DW_TAG_union_type
    | DW_TAG_unspecified_parameters
    | DW_TAG_variant
    | DW_TAG_common_block
    | DW_TAG_common_inclusion
    | DW_TAG_inheritance
    | DW_TAG_inlined_subroutine
    | DW_TAG_module
    | DW_TAG_ptr_to_member_type
    | DW_TAG_set_type
    | DW_TAG_subrange_type
    | DW_TAG_with_stmt
    | DW_TAG_access_declaration
    | DW_TAG_base_type
    | DW_TAG_catch_block
    | DW_TAG_const_type
    | DW_TAG_constant
    | DW_TAG_enumerator
    | DW_TAG_file_type
    | DW_TAG_friend
    | DW_TAG_namelist
    | DW_TAG_namelist_item
    | DW_TAG_packed_type
    | DW_TAG_subprogram
    | DW_TAG_template_type_parameter
    | DW_TAG_template_value_parameter
    | DW_TAG_thrown_type
    | DW_TAG_try_block
    | DW_TAG_variant_part
    | DW_TAG_variable
    | DW_TAG_volatile_type
    | DW_TAG_dwarf_procedure
    | DW_TAG_restrict_type
    | DW_TAG_interface_type
    | DW_TAG_namespace
    | DW_TAG_imported_module
    | DW_TAG_unspecified_type
    | DW_TAG_partial_unit
    | DW_TAG_imported_unit
    | DW_TAG_condition
    | DW_TAG_shared_type
       -- Added these since Data.Dwarf doesn't seem to support them
    | DW_TAG_auto_variable
    | DW_TAG_arg_variable
    | DW_TAG_return_variable
    deriving (Show, Eq)

-- | Copied from Data.Dwarf since it is not exported
dw_tag :: (Num a, Show a, Ord a) => a -> DW_TAG
dw_tag 0x01 = DW_TAG_array_type
dw_tag 0x02 = DW_TAG_class_type
dw_tag 0x03 = DW_TAG_entry_point
dw_tag 0x04 = DW_TAG_enumeration_type
dw_tag 0x05 = DW_TAG_formal_parameter
dw_tag 0x08 = DW_TAG_imported_declaration
dw_tag 0x0a = DW_TAG_label
dw_tag 0x0b = DW_TAG_lexical_block
dw_tag 0x0d = DW_TAG_member
dw_tag 0x0f = DW_TAG_pointer_type
dw_tag 0x10 = DW_TAG_reference_type
dw_tag 0x11 = DW_TAG_compile_unit
dw_tag 0x12 = DW_TAG_string_type
dw_tag 0x13 = DW_TAG_structure_type
dw_tag 0x15 = DW_TAG_subroutine_type
dw_tag 0x16 = DW_TAG_typedef
dw_tag 0x17 = DW_TAG_union_type
dw_tag 0x18 = DW_TAG_unspecified_parameters
dw_tag 0x19 = DW_TAG_variant
dw_tag 0x1a = DW_TAG_common_block
dw_tag 0x1b = DW_TAG_common_inclusion
dw_tag 0x1c = DW_TAG_inheritance
dw_tag 0x1d = DW_TAG_inlined_subroutine
dw_tag 0x1e = DW_TAG_module
dw_tag 0x1f = DW_TAG_ptr_to_member_type
dw_tag 0x20 = DW_TAG_set_type
dw_tag 0x21 = DW_TAG_subrange_type
dw_tag 0x22 = DW_TAG_with_stmt
dw_tag 0x23 = DW_TAG_access_declaration
dw_tag 0x24 = DW_TAG_base_type
dw_tag 0x25 = DW_TAG_catch_block
dw_tag 0x26 = DW_TAG_const_type
dw_tag 0x27 = DW_TAG_constant
dw_tag 0x28 = DW_TAG_enumerator
dw_tag 0x29 = DW_TAG_file_type
dw_tag 0x2a = DW_TAG_friend
dw_tag 0x2b = DW_TAG_namelist
dw_tag 0x2c = DW_TAG_namelist_item
dw_tag 0x2d = DW_TAG_packed_type
dw_tag 0x2e = DW_TAG_subprogram
dw_tag 0x2f = DW_TAG_template_type_parameter
dw_tag 0x30 = DW_TAG_template_value_parameter
dw_tag 0x31 = DW_TAG_thrown_type
dw_tag 0x32 = DW_TAG_try_block
dw_tag 0x33 = DW_TAG_variant_part
dw_tag 0x34 = DW_TAG_variable
dw_tag 0x35 = DW_TAG_volatile_type
dw_tag 0x36 = DW_TAG_dwarf_procedure
dw_tag 0x37 = DW_TAG_restrict_type
dw_tag 0x38 = DW_TAG_interface_type
dw_tag 0x39 = DW_TAG_namespace
dw_tag 0x3a = DW_TAG_imported_module
dw_tag 0x3b = DW_TAG_unspecified_type
dw_tag 0x3c = DW_TAG_partial_unit
dw_tag 0x3d = DW_TAG_imported_unit
dw_tag 0x3f = DW_TAG_condition
dw_tag 0x40 = DW_TAG_shared_type
dw_tag 0x100 = DW_TAG_auto_variable
dw_tag 0x101 = DW_TAG_arg_variable
dw_tag 0x102 = DW_TAG_return_variable
dw_tag n | 0x4080 <= n && n <= 0xffff = error $ "User DW_TAG data requires extension of parser for code " ++ show n
dw_tag n = error $ "Unrecognized DW_TAG " ++ show n
