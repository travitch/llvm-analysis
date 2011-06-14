{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.LLVM.Types (
  Module(..),
  module Data.LLVM.Private.Types.Referential,
  module Data.LLVM.Private.Types.Attributes,
  module Data.LLVM.Private.Types.Identifiers,
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

-- | This is the top-level representation of a program in LLVM.  This
-- is the type returned from all of the parsers, and all analysis
-- begins at the Module level.
data Module = Module { moduleDataLayout :: DataLayout
                       -- ^ The layout of the primitive datatypes on
                       -- the architecture this module was generated
                       -- for
                     , moduleTarget :: TargetTriple
                       -- ^ The architecture that this module was
                       -- generated for
                     , moduleAssembly :: [Assembly]
                       -- ^ Module-level assembly declarations
                     , moduleGlobals :: [Value]
                       -- ^ Global values (functions, constants,
                       -- variables, and external references)
                     }

-- | This helper extracts just the function definitions from the
-- 'Module'
moduleFunctions :: Module -> [Value]
moduleFunctions Module { moduleGlobals = globals } =
  filter valueIsFunction globals

-- | Implementation of the Show instance
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
