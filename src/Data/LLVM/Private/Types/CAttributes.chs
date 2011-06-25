module Data.LLVM.Private.Types.CAttributes (
  -- * Types
  ArithFlags(..),
  CmpPredicate(..),
  CallingConvention(..),
  LinkageType(..),
  VisibilityStyle(..)
  ) where

#include "c++/marshal.h"

import Control.DeepSeq

{#enum ArithFlags {} deriving (Show, Eq) #}
{#enum CmpPredicate {underscoreToCase} deriving (Show, Eq) #}
{#enum CallingConvention {} deriving (Show, Eq) #}
{#enum LinkageType {} deriving (Show, Eq) #}
{#enum VisibilityStyle {} deriving (Show, Eq) #}

instance NFData ArithFlags
instance NFData CmpPredicate
instance NFData CallingConvention
instance NFData LinkageType
instance NFData VisibilityStyle
