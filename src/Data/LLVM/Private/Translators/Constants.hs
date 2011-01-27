module Data.LLVM.Private.Translators.Constants ( translateConstant
                                               , transMap
                                               ) where

import Data.Map (Map, (!))

import Data.LLVM.Private.KnotHelpers
import qualified Data.LLVM.Private.PlaceholderTypes as O
import Data.LLVM.Private.Translators.Instructions
import Data.LLVM.Types

import Debug.Trace
debug = flip trace

transMap trConst dict vals =
  foldr f ([], dict) vals
  where f v (acc,d) =
          let (c, d') = trConst v d
          in (c : acc, d')

mkCVal :: Type -> ValueT -> IdentDict -> (Value, IdentDict)
mkCVal ty c d = (v, d')
  where v = Value { valueName = Nothing
                  , valueMetadata = Nothing
                  , valueType = ty
                  , valueContent = c
                  , valueUniqueId = uid
                  }
        (uid, d') = nextSequenceNumber d

translateConstant :: (O.Type -> Type) -> Map Identifier Value ->
                     Map Identifier Value -> O.Constant -> IdentDict ->
                     (Value, IdentDict)
translateConstant typeMapper globalDecls localDecls v d = case v of
  O.ConstValue c ty -> trConstVal ty c
  O.ValueRef ident -> case ident of
    GlobalIdentifier _ -> (globalDecls ! ident, d)
    LocalIdentifier _ -> (localDecls ! ident `debug` ("Symbol lookup: " ++ show ident), d)
    _ -> error $ "Metadata identifiers cannot be translated with trConst " ++ show ident
  where trConst = translateConstant typeMapper globalDecls localDecls
        trConstVal ty c = x
          where t = typeMapper ty
                x = case c of
                  O.BlockAddress fIdent bIdent ->
                    let ba = BlockAddress (globalDecls ! fIdent) (localDecls ! bIdent)
                    in mkCVal t ba d
                  O.ConstantAggregateZero -> mkCVal t ConstantAggregateZero d
                  O.ConstantArray cs ->
                    let (vs, d') = transMap trConst d cs
                    in mkCVal t (ConstantArray vs ) d'
                  O.ConstantStruct cs ->
                    let (vs, d') = transMap trConst d cs
                    in mkCVal t (ConstantStruct vs) d'
                  O.ConstantExpr inst ->
                    let (i, d') = translateInstruction typeMapper trConst inst d
                    in mkCVal t i d'
                  O.ConstantFP fp -> mkCVal t (ConstantFP fp) d
                  O.ConstantInt i -> mkCVal t (ConstantInt i) d
                  O.ConstantString txt -> mkCVal t (ConstantString txt) d
                  O.ConstantPointerNull -> mkCVal t ConstantPointerNull d
                  O.ConstantVector cs ->
                    let (vs, d') = transMap trConst d cs
                    in mkCVal t (ConstantVector vs) d'
                  O.InlineAsm asm constraints ->
                    mkCVal t (InlineAsm asm constraints) d
                  O.UndefValue -> mkCVal t UndefValue d
                  O.MDNode _ ->
                    error "translateConstant should never recieve an MDNode"
                  O.MDString _ ->
                    error "translateConstant should never receive an MDString"
                  O.GlobalVariable _ _ _ ->
                    error "translateConstant should never receive a GlobalVariable"

