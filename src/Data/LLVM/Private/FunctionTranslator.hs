module Data.LLVM.Private.FunctionTranslator ( transFuncDef ) where

import Data.Map (Map, (!))
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

transFuncDef :: (O.Type -> Type) ->
                (O.Constant -> Value) ->
                (Identifier -> Maybe Metadata) ->
                (Map Identifier Value) ->
                O.GlobalDeclaration ->
                Map Identifier Value
transFuncDef typeMapper transValOrConst getMetadata vals decl =
  M.insert (O.funcName decl) v vals
  where v = Value { valueType = mkFuncType typeMapper decl
                  , valueName = Just ident
                  , valueMetadata = getMetadata ident
                  , valueContent = ExternalValue -- changeme
                  }
        ident = getFuncIdent decl
        (localVals, body) = translateBody (O.funcBody decl)
        translateConstant (O.ValueRef ident@(LocalIdentifier _)) = localVals ! ident
        translateConstant c = transValOrConst c
        translateBody = foldr translateBlock (M.empty, [])
        translateBlock (O.BasicBlock ident placeholderInsts) (locals, blocks) =
          (M.insert ident bb blocksWithLocals, bb : blocks)
          where bb = Value { valueType = TypeVoid
                           , valueName = Just ident
                           , valueMetadata = Nothing -- can BBs have metadata?
                           , valueContent = BasicBlock insts
                           }
                (blocksWithLocals, insts) = translateInsts locals placeholderInsts
        translateInsts locals = foldr translateInstruction (locals, [])
        translateInstruction O.Instruction { O.instName = Nothing
                                           , O.instType = itype
                                           , O.instContent = O.UnwindInst
                                           , O.instMetadata = md
                                           } (locals, insts) =
          (locals, v : insts)
          where v = Value { valueType = typeMapper itype
                          , valueName = Nothing
                          , valueMetadata = maybe Nothing getMetadata md
                          , valueContent = UnwindInst
                          }

