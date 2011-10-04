{-# OPTIONS_HADDOCK hide #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.LLVM.Types (
  Module(..),
  moduleGlobals,
  module Data.LLVM.Attributes,
  module Data.LLVM.Identifiers,
  module Data.LLVM.Private.Types.Referential,
  ) where

import Control.DeepSeq
import Control.Monad.State.Strict
import qualified Data.HashSet as S
import Data.List ( intercalate )
import Data.ByteString.Char8 ( ByteString )

import Data.LLVM.Private.ForceModule
import Data.LLVM.Private.Printers
import Data.LLVM.Attributes
import Data.LLVM.Identifiers
import Data.LLVM.Private.Types.Referential

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
                     , moduleDefinedFunctions :: [Function]
                     , moduleGlobalVariables :: [GlobalVariable]
                     , moduleExternalValues :: [ExternalValue]
                     , moduleExternalFunctions :: [ExternalFunction]
                     , moduleAliases :: [GlobalAlias]
                     , moduleNextId :: UniqueId
                     }

-- isNotExternal :: Value -> Bool
-- isNotExternal Value { valueContent = Function {} } = True
-- isNotExternal Value { valueContent = GlobalDeclaration {} } = True
-- isNotExternal _ = False

-- | Implementation of the Show instance
printModule :: Module -> String
printModule Module { moduleIdentifier = _
                   , moduleDataLayout = layout
                   , moduleTarget = triple
                   , moduleAssembly = asm
                   , moduleAliases = aliases
                   , moduleGlobalVariables = vars
                   , moduleDefinedFunctions = funcs
                   , moduleExternalValues = evars
                   , moduleExternalFunctions = efuncs
                   } =
  concat [ layoutS, "\n", tripleS, "\n", asmS, "\n"
         , aliasesS, "\n", varS, "\n", funcS, "\n"
         ]
  where
    layoutS = concat [ "target datalayout = \"", show layout, "\"" ]
    tripleS = concat [ "target triple = \"", show triple, "\"" ]
    asmS = printAsm asm
    aliasesS = intercalate "\n\n" $ map (printValue . Value) aliases
    varS = intercalate "\n\n" $ map (printValue . Value) vars
    funcS = intercalate "\n\n" $ map (printValue . Value) funcs

-- | Get a list of all types of globals in the Module (functions,
-- aliases, and global variables)
moduleGlobals :: Module -> [Value]
moduleGlobals m = concat [ map Value $ moduleAliases m
                         , map Value $ moduleGlobalVariables m
                         , map Value $ moduleDefinedFunctions m
                         , map Value $ moduleExternalValues m
                         , map Value $ moduleExternalFunctions m
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
  mapM_ forceGlobalAlias (moduleAliases m)
  mapM_ forceGlobalVariable (moduleGlobalVariables m)
  mapM_ forceFunction (moduleDefinedFunctions m)
  mapM_ forceExternalValue (moduleExternalValues m)
  mapM_ forceExternalFunction (moduleExternalFunctions m)
  return $ moduleAssembly m `deepseq` m `seq` m
