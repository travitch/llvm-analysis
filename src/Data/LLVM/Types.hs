module Data.LLVM.Types ( module Data.LLVM.Private.AttributeTypes
                       , module Data.LLVM.Private.ReferentialTypes
                       ) where

import Data.LLVM.Private.AttributeTypes
import Data.LLVM.Private.Printers
import Data.LLVM.Private.ReferentialTypes

instance Show Metadata where
  show = printMetadata

instance Show Type where
  show = printType

instance Show Value where
  show = printValue

instance Show Module where
  show = printModule



