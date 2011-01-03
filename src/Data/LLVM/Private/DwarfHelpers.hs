module Data.LLVM.Private.DwarfHelpers ( mkDwarfVirtuality
                                      , mkDwarfLang
                                      , mkDwarfEncoding
                                      , mkDwarfTag
                                      , DW_VAR_TAG(..)
                                      ) where

import Data.Dwarf

mkDwarfVirtuality :: Integer -> DW_VIRTUALITY
mkDwarfVirtuality i = case i of
  1 -> DW_VIRTUALITY_none
  2 -> DW_VIRTUALITY_virtual
  3 -> DW_VIRTUALITY_pure_virtual
  _ -> error "Invalid virtuality"

-- TODO: Add DWARF4 languages (python, probably go)
mkDwarfLang :: Integer -> DW_LANG
mkDwarfLang i = case i of
  1 -> DW_LANG_C89
  2 -> DW_LANG_C
  3 -> DW_LANG_Ada83
  4 -> DW_LANG_C_plus_plus
  5 -> DW_LANG_Cobol74
  6 -> DW_LANG_Cobol85
  7 -> DW_LANG_Fortran77
  8 -> DW_LANG_Fortran90
  9 -> DW_LANG_Pascal83
  10 -> DW_LANG_Modula2
  11 -> DW_LANG_Java
  12 -> DW_LANG_C99
  13 -> DW_LANG_Ada95
  14 -> DW_LANG_Fortran95
  15 -> DW_LANG_PLI
  16 -> DW_LANG_ObjC
  17 -> DW_LANG_ObjC_plus_plus
  18 -> DW_LANG_UPC
  0x8765 -> DW_LANG_UPC
  19 -> DW_LANG_D
  -- 20 -> DW_LANG_Python
  -- _ -> DW_LANG_Other i
  _ -> error "Invalid virtuality"

mkDwarfEncoding :: Integer -> DW_ATE
mkDwarfEncoding i = case i of
  1 -> DW_ATE_address
  2 -> DW_ATE_boolean
  3 -> DW_ATE_complex_float
  4 -> DW_ATE_float
  5 -> DW_ATE_signed
  6 -> DW_ATE_signed_char
  7 -> DW_ATE_unsigned
  8 -> DW_ATE_unsigned_char
  9 -> DW_ATE_imaginary_float
  10 -> DW_ATE_packed_decimal
  11 -> DW_ATE_numeric_string
  12 -> DW_ATE_edited
  13 -> DW_ATE_signed_fixed
  14 -> DW_ATE_unsigned_fixed
  15 -> DW_ATE_decimal_float

mkDwarfTag :: Integer -> DW_TAG
mkDwarfTag i = case i of
  1 -> DW_TAG_array_type
  2 -> DW_TAG_class_type
  3 -> DW_TAG_entry_point
  4 -> DW_TAG_enumeration_type
  5 -> DW_TAG_formal_parameter
  6 -> DW_TAG_imported_declaration
  7 -> DW_TAG_label
  8 -> DW_TAG_lexical_block
  9 -> DW_TAG_member
  10 -> DW_TAG_pointer_type
  11 -> DW_TAG_reference_type
  12 -> DW_TAG_compile_unit
  13 -> DW_TAG_string_type
  14 -> DW_TAG_structure_type
  15 -> DW_TAG_subroutine_type
  16 -> DW_TAG_typedef
  17 -> DW_TAG_union_type
  18 -> DW_TAG_unspecified_parameters
  19 -> DW_TAG_variant
  20 -> DW_TAG_common_block
  21 -> DW_TAG_common_inclusion
  22 -> DW_TAG_inheritance
  23 -> DW_TAG_inlined_subroutine
  24 -> DW_TAG_module
  25 -> DW_TAG_ptr_to_member_type
  26 -> DW_TAG_set_type
  27 -> DW_TAG_subrange_type
  28 -> DW_TAG_with_stmt
  29 -> DW_TAG_access_declaration
  30 -> DW_TAG_base_type
  31 -> DW_TAG_catch_block
  32 -> DW_TAG_const_type
  33 -> DW_TAG_constant
  34 -> DW_TAG_enumerator
  35 -> DW_TAG_file_type
  36 -> DW_TAG_friend
  37 -> DW_TAG_namelist
  38 -> DW_TAG_namelist_item
  39 -> DW_TAG_packed_type
  40 -> DW_TAG_subprogram
  41 -> DW_TAG_template_type_parameter
  42 -> DW_TAG_template_value_parameter
  43 -> DW_TAG_thrown_type
  44 -> DW_TAG_try_block
  45 -> DW_TAG_variant_part
  46 -> DW_TAG_variable
  47 -> DW_TAG_volatile_type
  48 -> DW_TAG_dwarf_procedure
  49 -> DW_TAG_restrict_type
  50 -> DW_TAG_interface_type
  51 -> DW_TAG_namespace
  52 -> DW_TAG_imported_module
  53 -> DW_TAG_unspecified_type
  54 -> DW_TAG_partial_unit
  55 -> DW_TAG_imported_unit
  56 -> DW_TAG_condition
  57 -> DW_TAG_shared_type

-- This is a temporary type until the dwarf package supports DWARF4
-- extensions
data DW_VAR_TAG = DW_TAG_auto_variable
                | DW_TAG_arg_variable
                | DW_TAG_return_variable
                deriving (Show, Eq)