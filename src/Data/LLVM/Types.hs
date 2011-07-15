{-# OPTIONS_HADDOCK hide #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.LLVM.Types (
  Module(..),
  moduleDefinedFunctions,
  moduleGlobals,
  module ReEx
  ) where

import Control.DeepSeq
import Control.Monad.State.Strict
import qualified Data.HashSet as S
import Data.List ( intercalate )
import Data.ByteString.Char8 ( ByteString )

import Data.LLVM.Private.ForceModule
import Data.LLVM.Private.Printers
import Data.LLVM.Attributes as ReEx
import Data.LLVM.Identifiers as ReEx
import Data.LLVM.Private.Types.Referential as ReEx

-- | This is the top-level representation of a program in LLVM.  This
-- is the type returned from all of the parsers, and all analysis
-- begins at the Module level.
data Module = Module { moduleIdentifier :: ByteString
                     , moduleDataLayout :: ByteString -- DataLayout
                       -- ^ The layout of the primitive datatypes on
                       -- the architecture this module was generated
                       -- for
                     , moduleTarget :: ByteString -- TargetTriple
                       -- ^ The architecture that this module was
                       -- generated for
                     , moduleAssembly :: Assembly
                       -- ^ Module-level assembly declarations
                     , moduleAliases :: [Value]
                     , moduleGlobalVariables :: [Value]
                     , moduleFunctions :: [Value]
                     , moduleNextId :: Int
                     }

-- | Extract a list of only those functions that are *defined* in the
-- module (not external).
moduleDefinedFunctions :: Module -> [Value]
moduleDefinedFunctions = filter isNotExternal . moduleFunctions

isNotExternal :: Value -> Bool
isNotExternal Value { valueContent = Function {} } = True
isNotExternal Value { valueContent = GlobalDeclaration {} } = True
isNotExternal _ = False

-- | Implementation of the Show instance
printModule :: Module -> String
printModule Module { moduleIdentifier = _
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

-- | Get a list of all types of globals in the Module (functions,
-- aliases, and global variables)
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
  return $ moduleAssembly m `deepseq` m `seq` m
