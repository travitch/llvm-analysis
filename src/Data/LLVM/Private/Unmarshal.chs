{-# LANGUAGE ForeignFunctionInterface, RankNTypes #-}
module Data.LLVM.Private.Unmarshal where

#include "c++/marshal.h"

import Control.Applicative
import Control.Monad.State
import Data.Array.Storable
import Data.HashMap.Strict ( HashMap )
import qualified Data.HashMap.Strict as M
import Foreign.C
import Foreign.C.String
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Storable
import Data.LLVM.Private.C2HS

{#enum CmpPredicate {underscoreToCase} deriving (Show, Eq) #}
{#enum CallingConvention {} deriving (Show, Eq) #}
{#enum TypeTag {} deriving (Show, Eq) #}
{#enum ValueTag {underscoreToCase} deriving (Show, Eq) #}
{#enum LinkageType {} deriving (Show, Eq) #}
{#enum VisibilityType {} deriving (Show, Eq) #}

data CModule
{#pointer *CModule as ModulePtr -> CModule #}

cModuleIdentifier :: ModulePtr -> IO String
cModuleIdentifier m = ({#get CModule->moduleIdentifier#} m) >>= peekCString

cModuleDataLayout :: ModulePtr -> IO String
cModuleDataLayout m = ({#get CModule->moduleDataLayout#} m) >>= peekCString

cModuleTargetTriple :: ModulePtr -> IO String
cModuleTargetTriple m = ({#get CModule->targetTriple#} m) >>= peekCString

cModuleInlineAsm :: ModulePtr -> IO String
cModuleInlineAsm m = ({#get CModule->moduleInlineAsm#} m) >>= peekCString

cModuleHasError :: ModulePtr -> IO Bool
cModuleHasError m = cToBool <$> ({#get CModule->hasError#} m)

cModuleErrorMessage :: ModulePtr -> IO String
cModuleErrorMessage m = ({#get CModule->errMsg#} m) >>= peekCString

cModuleLittleEndian :: ModulePtr -> IO Bool
cModuleLittleEndian m = cToBool <$> ({#get CModule->littleEndian#} m)

cModulePointerSize :: ModulePtr -> IO Int
cModulePointerSize m = cIntConv <$> ({#get CModule->pointerSize#} m)

cModuleGlobalVariables :: ModulePtr -> IO (StorableArray Int ValuePtr)
cModuleGlobalVariables m =
  peekArray m {#get CModule->globalVariables#} {#get CModule->numGlobalVariables#}

cModuleGlobalAliases :: ModulePtr -> IO (StorableArray Int ValuePtr)
cModuleGlobalAliases m =
  peekArray m ({#get CModule->globalAliases#}) ({#get CModule->numGlobalAliases#})

cModuleFunctions :: ModulePtr -> IO (StorableArray Int ValuePtr)
cModuleFunctions m =
  peekArray m ({#get CModule->functions#}) ({#get CModule->numFunctions#})

peekArray :: forall a b c i e . (Ix i, Integral i, Integral c) =>
             a -> (a -> IO (Ptr b)) -> (a -> IO c) -> IO (StorableArray i e)
peekArray obj arrAccessor sizeAccessor = do
  nElts <- sizeAccessor obj
  arrPtr <- arrAccessor obj
  fArrPtr <- newForeignPtr_ (castPtr arrPtr)
  unsafeForeignPtrToStorableArray fArrPtr (1, cIntConv nElts)

data CType = CType TypeTag Int Bool Bool (Ptr TypePtr) Int TypePtr String
{#pointer *CType as TypePtr -> CType #}

data CValue = CValue ValueTag TypePtr String (Ptr ()) (Ptr ())
{#pointer *CValue as ValuePtr -> CValue #}

{#fun marshalLLVM { `String' } -> `ModulePtr' id #}
{#fun disposeCModule { id `ModulePtr' } -> `()' #}

data KnotState = KnotState { valueMap :: HashMap Int Int }

translate bitcodefile = do
  m <- marshalLLVM bitcodefile

  let initialState = KnotState { valueMap = M.empty }
  (ir, finalState) <- evalStateT (mfix (tieKnot m)) initialState

  disposeCModule m
  return ir

tieKnot m (_, finalState) = return (undefined, finalState)