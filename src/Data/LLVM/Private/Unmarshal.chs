{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}
module Data.LLVM.Private.Unmarshal where

#include "c++/marshal.h"

import Foreign.C
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
import Data.LLVM.Private.C2HS

{#enum CmpPredicate {underscoreToCase} deriving (Show, Eq) #}
{#enum CallingConvention {} deriving (Show, Eq) #}
{#enum TypeTag {} deriving (Show, Eq) #}
{#enum ValueTag {underscoreToCase} deriving (Show, Eq) #}
{#enum LinkageType {} deriving (Show, Eq) #}
{#enum VisibilityType {} deriving (Show, Eq) #}

data CModule
{#pointer *CModule as ModulePtr -> CModule #}

{#fun marshalLLVM { `String' } -> `ModulePtr' id #}
{#fun disposeCModule { id `ModulePtr' } -> `()' #}
{#fun cmoduleErrMsg as ^ { id `ModulePtr' } -> `String' #}
{#fun cmoduleDataLayout as ^ { id `ModulePtr' } -> `String' #}
{#fun cmoduleIdentifier as ^ { id `ModulePtr' } -> `String' #}
{#fun cmoduleTargetTriple as ^ { id `ModulePtr' } -> `String' #}
{#fun cmoduleInlineAsm as ^ { id `ModulePtr' } -> `String' #}
{#fun cmoduleIsLittleEndian as ^ { id `ModulePtr' } -> `Int' #}
{#fun cmodulePointerSize as ^ { id `ModulePtr' } -> `Int' #}
{#fun cmoduleIsError as ^ { id `ModulePtr' } -> `Int' #}

