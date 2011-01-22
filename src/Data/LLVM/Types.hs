{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.LLVM.Types ( module Data.LLVM.Private.AttributeTypes
                       , module Data.LLVM.Private.ReferentialTypes
                       ) where

import Data.LLVM.Private.AttributeTypes
import Data.LLVM.Private.Printers
import Data.LLVM.Private.ReferentialTypes

-- Technically, these are orphan instances.  But really, if you are
-- using the public interface to this library they are not.  They are
-- only defined outside of the module containing the data type
-- declarations (Data.LLVM.Private.ReferentialTypes) to keep that
-- module's compile time down.
--
-- I disable the warning since this seems reasonable.
--
-- This also would allow users to define their own show instances for
-- these types if desired.

instance Show Metadata where
  show = printMetadata

instance Show Type where
  show = printType

instance Show Value where
  show = printValue

instance Show Module where
  show = printModule

