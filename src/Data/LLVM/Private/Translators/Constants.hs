module Data.LLVM.Private.Translators.Constants ( translateConstant
                                               ) where

import Data.List ( mapAccumR )
import qualified Data.HashMap.Strict as M
import Text.Printf

import Data.LLVM.Private.UniqueId
import Data.LLVM.Private.Translators.Instructions
import Data.LLVM.Private.Types.Identifiers
import Data.LLVM.Private.Types.InternalMap
import qualified Data.LLVM.Private.Types.Placeholder as O
import Data.LLVM.Types

mkCVal :: UniqueId -> Type -> ValueT -> Value
mkCVal uid ty c = Value { valueName = Nothing
                        , valueMetadata = Nothing
                        , valueType = ty
                        , valueContent = c
                        , valueUniqueId = uid
                        }

translateConstant :: (O.Type -> Type) -> Map Identifier Value ->
                     Map Identifier Value -> O.Constant -> IdStream ->
                     Value
translateConstant typeMapper globalDecls localDecls v initstream =
  case v of
    O.ConstValue c ty -> trConstVal ty c
    O.ValueRef ident ->
      case ident of
        LocalIdentifier {} -> case M.lookup ident localDecls of
          Just i -> i
          Nothing -> error $ printf "Identifier %s not found in the localDecl map" (show ident)
        GlobalIdentifier {} -> case M.lookup ident globalDecls of
          Just i -> i
          Nothing -> error $ printf "Identifier %s not found in the globalDecl map" (show ident)
        _ -> error $ printf "Metadata identifiers cannot be translated with trConst (%s)" (show ident)

  where trConst = translateConstant typeMapper globalDecls localDecls
        transMap idstream vals =
          snd $ mapAccumR f idstream vals
          where f s val = let (thisStream, otherStream) = split2 s
                              c = trConst val thisStream
                          in (otherStream, c)
        thisId = extract initstream
        restIds = split initstream
        trConstVal ty c = x
          where t = typeMapper ty
                x = case c of
                  O.BlockAddress fIdent bIdent ->
                    let funcConstant = case M.lookup fIdent globalDecls of
                          Just f -> f
                          Nothing -> error $ printf "Function %s not found in the globalDecl map" (show fIdent)
                        labelConstant = case M.lookup bIdent localDecls of
                          Just b -> b
                          Nothing -> error $ printf "Local block %s not found in function %s" (show bIdent) (show fIdent)
                        ba = BlockAddress funcConstant labelConstant
                    in mkCVal thisId t ba
                  O.ConstantAggregateZero -> mkCVal thisId t ConstantAggregateZero
                  O.ConstantArray cs ->
                    let vs = transMap restIds cs
                    in mkCVal thisId t (ConstantArray vs )
                  O.ConstantStruct cs ->
                    let vs = transMap restIds cs
                    in mkCVal thisId t (ConstantStruct vs)
                  O.ConstantExpr inst ->
                    let i = translateInstruction typeMapper trConst inst restIds
                    in mkCVal thisId t i
                  O.ConstantFP fp -> mkCVal thisId t (ConstantFP fp)
                  O.ConstantInt i -> mkCVal thisId t (ConstantInt i)
                  O.ConstantString txt -> mkCVal thisId t (ConstantString txt)
                  O.ConstantPointerNull -> mkCVal thisId t ConstantPointerNull
                  O.ConstantVector cs ->
                    let vs = transMap restIds cs
                    in mkCVal thisId t (ConstantVector vs)
                  O.InlineAsm asm constraints ->
                    mkCVal thisId t (InlineAsm asm constraints)
                  O.UndefValue -> mkCVal thisId t UndefValue
                  O.MDNode _ ->
                    error "translateConstant should never recieve an MDNode"
                  O.MDString _ ->
                    error "translateConstant should never receive an MDString"
                  O.GlobalVariable _ _ _ ->
                    error "translateConstant should never receive a GlobalVariable"

