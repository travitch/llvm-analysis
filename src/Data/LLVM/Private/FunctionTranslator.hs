module Data.LLVM.Private.FunctionTranslator ( transFuncDef ) where

import qualified Data.Map as M

import Data.LLVM.Types
import Data.LLVM.Private.AttributeTypes
import qualified Data.LLVM.Private.PlaceholderTypes as O

mkFuncType typeMapper O.FunctionDefinition { O.funcRetType = fret
                                           , O.funcParams = params
                                           , O.funcIsVararg = isVararg
                                           , O.funcAttrs = attrs
                                           } =
  TypeFunction rtype argTypes isVararg attrs
  where rtype = typeMapper fret
        argTypes = map (typeMapper . xtype) params
        xtype (O.FormalParameter t _ _) = t

mkFuncType _ _ = error "Non-func decl in mkFuncType"

getFuncIdent O.FunctionDefinition { O.funcName = ident } = ident

transFuncDef typeMapper transValOrConst getMetadata vals decl =
  M.insert (O.funcName decl) v vals
  where v = Value { valueType = mkFuncType typeMapper decl
                  , valueName = Just ident
                  , valueMetadata = getMetadata ident
                  , valueContent = ExternalValue -- changeme
                  }
        ident = getFuncIdent decl