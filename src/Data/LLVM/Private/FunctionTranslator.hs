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
        trConst (O.ValueRef ident@(LocalIdentifier _)) = localVals ! ident
        trConst c = transValOrConst c
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

        -- This helper converts the non-content fields of an instruction
        -- to the equivalent fields in a Value.  It performs the necessary
        -- translations to fetch metadata and handle type differences
        repackInst O.Instruction { O.instType = itype
                                 , O.instName = iname
                                 , O.instMetadata = md
                                 } ni = v
          where v = Value { valueType = typeMapper itype
                          , valueName = iname
                          , valueMetadata = maybe Nothing getMetadata md
                          , valueContent = ni
                          }

        -- This handles updating the accumulator in the big
        -- translateInstruction fold.  Always add the new value to the
        -- instruction list.  Add it to the local variable map only if
        -- it has a name.
        foldResult (locals, insts) val = case valueName v of
          Just i -> (M.insert i val locals, val : insts)
          Nothing -> (locals, val : insts)

        translateInstruction i@O.Instruction { O.instContent = c } acc =
          foldResult acc v
          where v = repackInst i content
                trPair (v, t) = (trConst v, trConst t)
                content = case c of
                  O.RetInst mc -> RetInst $ maybe Nothing (Just . trConst) mc
                  O.UnconditionalBranchInst target ->
                    UnconditionalBranchInst $ trConst target
                  O.BranchInst cond tTarget fTarget ->
                    BranchInst { branchCondition = trConst cond
                               , branchTrueTarget = trConst tTarget
                               , branchFalseTarget = trConst fTarget
                               }
                  O.SwitchInst val defTarget cases ->
                    SwitchInst { switchValue = trConst val
                               , switchDefaultTarget = trConst defTarget
                               , switchCases = map trPair cases
                               }
                  O.IndirectBranchInst val dests ->
                    IndirectBranchInst { indirectBranchAddress = trConst val
                                       , indirectBranchTargets = map trConst dests
                                       }
                  O.UnwindInst -> UnwindInst
                  O.UnreachableInst -> UnreachableInst
                  O.AddInst flags lhs rhs -> AddInst flags (trConst lhs) (trConst rhs)
                  O.SubInst flags lhs rhs -> SubInst flags (trConst lhs) (trConst rhs)
                  O.MulInst flags lhs rhs -> MulInst flags (trConst lhs) (trConst rhs)
                  O.DivInst lhs rhs -> DivInst (trConst lhs) (trConst rhs)
                  O.RemInst lhs rhs -> RemInst (trConst lhs) (trConst rhs)
                  O.ShlInst lhs rhs -> ShlInst (trConst lhs) (trConst rhs)
                  O.LshrInst lhs rhs -> LshrInst (trConst lhs) (trConst rhs)
                  O.AshrInst lhs rhs -> AshrInst (trConst lhs) (trConst rhs)
                  O.AndInst lhs rhs -> AndInst (trConst lhs) (trConst rhs)
                  O.OrInst lhs rhs -> OrInst (trConst lhs) (trConst rhs)
                  O.XorInst lhs rhs -> XorInst (trConst lhs) (trConst rhs)
                  O.ExtractElementInst vec idx ->
                    ExtractElementInst { extractElementVector = trConst vec
                                       , extractElementIndex = trConst idx
                                       }
                  O.InsertElementInst vec elt idx ->
                    InsertElementInst { insertElementVector = trConst vec
                                      , insertElementValue = trConst elt
                                      , insertElementIndex = trConst idx
                                      }
                  O.ShuffleVectorInst vec1 vec2 mask ->
                    ShuffleVectorInst { shuffleVectorV1 = trConst vec1
                                      , shuffleVectorV2 = trConst vec2
                                      , shuffleVectorMask = trConst mask
                                      }
                  O.ExtractValueInst agg indices ->
                    ExtractValueInst { extractValueAggregate = trConst agg
                                     , extractValueIndices = indices
                                     }
                  O.InsertValueInst agg val idx ->
                    InsertValueInst { insertValueAggregate = trConst agg
                                    , insertValueValue = trConst val
                                    , insertValueIndex = idx
                                    }
                  O.AllocaInst ty val align ->
                    AllocaInst (typeMapper ty) (trConst val) align
                  O.LoadInst volatile dest align ->
                    LoadInst volatile (trConst dest) align
                  O.StoreInst volatile value dest align ->
                    StoreInst volatile (trConst value) (trConst dest) align
                  O.TruncInst val ty -> TruncInst (trConst val) (typeMapper ty)
                  O.ZExtInst val ty -> ZExtInst (trConst val) (typeMapper ty)
                  O.SExtInst val ty -> SExtInst (trConst val) (typeMapper ty)
                  O.FPTruncInst val ty -> FPTruncInst (trConst val) (typeMapper ty)
                  O.FPExtInst val ty -> FPExtInst (trConst val) (typeMapper ty)
                  O.FPToUIInst val ty -> FPToUIInst (trConst val) (typeMapper ty)
                  O.FPToSIInst val ty -> FPToSIInst (trConst val) (typeMapper ty)
                  O.UIToFPInst val ty -> UIToFPInst (trConst val) (typeMapper ty)
                  O.SIToFPInst val ty -> SIToFPInst (trConst val) (typeMapper ty)
                  O.PtrToIntInst val ty -> PtrToIntInst (trConst val) (typeMapper ty)
                  O.IntToPtrInst val ty -> IntToPtrInst (trConst val) (typeMapper ty)
                  O.BitcastInst val ty -> BitcastInst (trConst val) (typeMapper ty)
                  O.ICmpInst cond val1 val2 -> ICmpInst cond (trConst val1) (trConst val2)
                  O.FCmpInst cond val1 val2 -> FCmpInst cond (trConst val1) (trConst val2)
                  O.PhiNode vals -> PhiNode $ map trPair vals
                  O.SelectInst cond val1 val2 ->
                    SelectInst (trConst cond) (trConst val1) (trConst val2)
                  O.GetElementPtrInst inBounds val indices ->
                    GetElementPtrInst { getElementPtrInBounds = inBounds
                                      , getElementPtrValue = trConst val
                                      , getElementPtrIndices = map trConst indices
                                      }