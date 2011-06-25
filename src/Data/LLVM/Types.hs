{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.LLVM.Types (
  Module(..),
  moduleGlobals,
  module Data.LLVM.Private.Types.Referential,
  module Data.LLVM.Private.Types.Attributes,
  module Data.LLVM.Private.Types.CAttributes,
  module Data.LLVM.Private.Types.Identifiers,
  ) where

import Control.DeepSeq
import Control.Monad.State.Strict
import qualified Data.HashSet as S
import Data.List ( intercalate )
import Data.ByteString.Char8 ( ByteString )

import Data.LLVM.Private.ForceModule
import Data.LLVM.Private.Printers
import Data.LLVM.Private.Types.Attributes
import Data.LLVM.Private.Types.CAttributes
import Data.LLVM.Private.Types.Identifiers
import Data.LLVM.Private.Types.Referential

-- | This is the top-level representation of a program in LLVM.  This
-- is the type returned from all of the parsers, and all analysis
-- begins at the Module level.
data Module = Module { moduleIdentifier :: ByteString
                     , moduleDataLayout :: DataLayout
                       -- ^ The layout of the primitive datatypes on
                       -- the architecture this module was generated
                       -- for
                     , moduleTarget :: TargetTriple
                       -- ^ The architecture that this module was
                       -- generated for
                     , moduleAssembly :: Assembly
                       -- ^ Module-level assembly declarations
                     , moduleAliases :: [Value]
                     , moduleGlobalVariables :: [Value]
                     , moduleFunctions :: [Value]
--                     , moduleGlobals :: [Value]
                       -- ^ Global values (functions, constants,
                       -- variables, and external references)
                     }

-- | This helper extracts just the function definitions from the
-- 'Module'
-- moduleFunctions :: Module -> [Value]
-- moduleFunctions Module { moduleGlobals = globals } =
--   filter valueIsFunction globals

-- | Implementation of the Show instance
printModule :: Module -> String
printModule Module { moduleIdentifier = ident
                   , moduleDataLayout = layout
                   , moduleTarget = triple
                   , moduleAssembly = asm
                   , moduleAliases = aliases
                   , moduleGlobalVariables = vars
                   , moduleFunctions = funcs
                   } =
  concat [ layoutS, "\n", tripleS, "\n", asmS, "\n"
         , aliasesS, "\n", varS, "\n", funcS, "\n"
         ]
  where
    layoutS = concat [ "target datalayout = \"", show layout, "\"" ]
    tripleS = concat [ "target triple = \"", show triple, "\"" ]
    asmS = printAsm asm
    aliasesS = intercalate "\n\n" $ map printValue aliases
    varS = intercalate "\n\n" $ map printValue vars
    funcS = intercalate "\n\n" $ map printValue funcs

moduleGlobals :: Module -> [Value]
moduleGlobals m = concat [ moduleAliases m
                         , moduleGlobalVariables m
                         , moduleFunctions m
                         ]

instance Show Module where
  show = printModule

instance NFData Module where
  rnf m = evalState (forceModule m) (S.empty, S.empty) `seq` ()

-- | Force the module to be fully evaluated to rnf form from the
-- top-down.  There are cycles, so we have to be careful to avoid
-- traversing them infinitely.
forceModule :: Module -> ForceMonad Module
forceModule m = do
  mapM_ forceGlobal (moduleAliases m)
  mapM_ forceGlobal (moduleGlobalVariables m)
  mapM_ forceGlobal (moduleFunctions m)
  return $ moduleDataLayout m `deepseq` moduleTarget m `deepseq`
            moduleAssembly m `deepseq` m `seq` m
