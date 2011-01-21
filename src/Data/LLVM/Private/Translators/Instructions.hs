module Data.LLVM.Private.Translators.Instructions ( translateInstruction ) where

import Data.LLVM.Types
import qualified Data.LLVM.Private.PlaceholderTypes as O

translateInstruction :: (O.Type -> Type) -> (O.Constant -> Value) ->
                        O.InstructionT -> ValueT
translateInstruction typeMapper trConst oldContent = newContent
  where trPair (v, t) = (trConst v, trConst t)
        newContent = case oldContent of
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
          O.InsertValueInst agg val indices ->
            InsertValueInst { insertValueAggregate = trConst agg
                            , insertValueValue = trConst val
                            , insertValueIndices = indices
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
          O.CallInst { O.callIsTail = isTail
                     , O.callConvention = cc
                     , O.callParamAttrs = paramAttrs
                     , O.callRetType = rtype
                     , O.callFunction = func
                     , O.callArguments = args
                     , O.callAttrs = cAttrs
                     , O.callHasSRet = hasSRet
                     } ->
            CallInst { callIsTail = isTail
                     , callConvention = cc
                     , callParamAttrs = paramAttrs
                     , callRetType = typeMapper rtype
                     , callFunction = trConst func
                     , callArguments = map trConst args
                     , callAttrs = cAttrs
                     , callHasSRet = hasSRet
                     }
          O.InvokeInst { O.invokeConvention = cc
                       , O.invokeParamAttrs = paramAttrs
                       , O.invokeRetType = rtype
                       , O.invokeFunction = func
                       , O.invokeArguments = args
                       , O.invokeAttrs = funcAttrs
                       , O.invokeNormalLabel = normLabl
                       , O.invokeUnwindLabel = unwindLabl
                       , O.invokeHasSRet = hasSRet
                       } ->
            InvokeInst { invokeConvention = cc
                       , invokeParamAttrs = paramAttrs
                       , invokeRetType = typeMapper rtype
                       , invokeFunction = trConst func
                       , invokeArguments = map trConst args
                       , invokeAttrs = funcAttrs
                       , invokeNormalLabel = trConst normLabl
                       , invokeUnwindLabel = trConst unwindLabl
                       , invokeHasSRet = hasSRet
                       }
          O.VaArgInst val ty -> VaArgInst (trConst val) (typeMapper ty)
