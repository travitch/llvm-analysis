module Data.LLVM.Private.Translators.Constants ( translateConstant ) where

import qualified Data.Map as M
import Data.Map (Map, (!))

import Data.LLVM.Private.AttributeTypes
import qualified Data.LLVM.Private.PlaceholderTypes as O
import Data.LLVM.Types

mkCVal ty c = Value { valueName = Nothing
                    , valueMetadata = Nothing
                    , valueType = ty
                    , valueContent = c
                    }

-- translateConstant :: Map Identifier Value -> Map Identifier Value -> O.Constant -> Value
translateConstant typeMapper globalDecls localDecls v = case v of
  O.ConstValue c ty -> trConstVal ty c
  O.ValueRef ident -> globalDecls ! ident
  where trConst = translateConstant typeMapper globalDecls localDecls
        trConstVal ty c = x
          where t = typeMapper ty
                x = case c of
                  O.BlockAddress fIdent bIdent ->
                    mkCVal t $ BlockAddress (globalDecls ! fIdent) (localDecls ! bIdent)
                  O.ConstantAggregateZero -> mkCVal t ConstantAggregateZero
                  O.ConstantArray cs ->
                    mkCVal t $ ConstantArray $ map trConst cs
                  -- O.ConstantExpr c ->
                  O.ConstantFP fp -> mkCVal t $ ConstantFP fp
                  O.ConstantInt i -> mkCVal t $ ConstantInt i
                  O.ConstantPointerNull -> mkCVal t ConstantPointerNull
                  O.ConstantVector cs -> mkCVal t $ ConstantVector $ map trConst cs
                  O.InlineAsm asm constraints ->
                    mkCVal t $ InlineAsm asm constraints
