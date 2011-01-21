module Data.LLVM.Private.Translators.Constants ( translateConstant ) where

import Data.Map (Map, (!))

import Data.LLVM.Private.AttributeTypes
import qualified Data.LLVM.Private.PlaceholderTypes as O
import Data.LLVM.Private.Translators.Instructions
import Data.LLVM.Types

mkCVal :: Type -> ValueT -> Value
mkCVal ty c = Value { valueName = Nothing
                    , valueMetadata = Nothing
                    , valueType = ty
                    , valueContent = c
                    }

translateConstant :: (O.Type -> Type) -> Map Identifier Value ->
                     Map Identifier Value -> O.Constant -> Value
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
                  O.ConstantStruct cs ->
                    mkCVal t $ ConstantStruct $ map trConst cs
                  O.ConstantExpr inst ->
                    mkCVal t $ translateInstruction typeMapper trConst inst
                  O.ConstantFP fp -> mkCVal t $ ConstantFP fp
                  O.ConstantInt i -> mkCVal t $ ConstantInt i
                  O.ConstantString txt -> mkCVal t $ ConstantString txt
                  O.ConstantPointerNull -> mkCVal t ConstantPointerNull
                  O.ConstantVector cs -> mkCVal t $ ConstantVector $ map trConst cs
                  O.InlineAsm asm constraints ->
                    mkCVal t $ InlineAsm asm constraints
                  O.UndefValue -> mkCVal t UndefValue
                  O.MDNode _ ->
                    error "translateConstant should never recieve an MDNode"
                  O.MDString _ ->
                    error "translateConstant should never receive an MDString"
                  O.GlobalVariable _ _ _ ->
                    error "translateConstant should never receive a GlobalVariable"

