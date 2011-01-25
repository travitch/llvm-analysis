{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.LLVM.Types ( module Data.LLVM.Private.AttributeTypes
                       , module Data.LLVM.Private.ReferentialTypes
                       , Module(..)
                       , moduleFunctions
                       ) where

import Data.List (intercalate)
import Data.Map (Map)

import Data.LLVM.CFG

import Data.LLVM.Private.AttributeTypes
import Data.LLVM.Private.Printers
import Data.LLVM.Private.ReferentialTypes

data Module = Module { moduleDataLayout :: DataLayout
                     , moduleTarget :: TargetTriple
                     , moduleAssembly :: [Assembly]
                     , moduleGlobals :: [Value]
                     , moduleCFGs :: Map Value CFG
                     }

moduleFunctions :: Module -> [Value]
moduleFunctions Module { moduleGlobals = globals } =
  filter valueIsFunction globals

printModule :: Module -> String
printModule Module { moduleDataLayout = layout
                   , moduleTarget = triple
                   , moduleAssembly = asm
                   , moduleGlobals = vals
                   } =
  concat [ layoutS, "\n", tripleS, "\n", asmS, "\n", valS, "\n" ]
  where layoutS = concat [ "target datalayout = \"", show layout, "\"" ]
        tripleS = concat [ "target triple = \"", show triple, "\"" ]
        asmS = printAsm asm
        valS = intercalate "\n\n" $ map printValue vals


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

instance Show Module where
  show = printModule
