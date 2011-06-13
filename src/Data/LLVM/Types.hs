{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.LLVM.Types (
  module Data.LLVM.Private.Types.Attributes,
  module Data.LLVM.Private.Types.Identifiers,
  module Data.LLVM.Private.Types.Referential,
  Module(..),
  moduleFunctions,
  ) where

import Control.DeepSeq
import Control.Monad.State.Strict
import qualified Data.HashSet as S
import Data.List ( intercalate )

import Data.LLVM.Private.ForceModule
import Data.LLVM.Private.Printers
import Data.LLVM.Private.Types.Attributes
import Data.LLVM.Private.Types.Identifiers
import Data.LLVM.Private.Types.Referential

data Module = Module { moduleDataLayout :: DataLayout
                     , moduleTarget :: TargetTriple
                     , moduleAssembly :: [Assembly]
                     , moduleGlobals :: [Value]
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
  where
    layoutS = concat [ "target datalayout = \"", show layout, "\"" ]
    tripleS = concat [ "target triple = \"", show triple, "\"" ]
    asmS = printAsm asm
    valS = intercalate "\n\n" $ map printValue vals

instance Show Module where
  show = printModule

instance NFData Module where
  rnf m = evalState (forceModule m) (S.empty, S.empty) `seq` ()

-- | Force the module to be fully evaluated to rnf form from the
-- top-down.  There are cycles, so we have to be careful to avoid
-- traversing them infinitely.
forceModule :: Module -> ForceMonad Module
forceModule m = do
  mapM_ forceGlobal (moduleGlobals m)
  return $ moduleDataLayout m `deepseq` moduleTarget m `deepseq`
            moduleAssembly m `deepseq` m `seq` m
