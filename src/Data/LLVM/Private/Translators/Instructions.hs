module Data.LLVM.Private.Translators.Instructions ( translateInstruction ) where

import Data.LLVM.Types
import Data.LLVM.Private.KnotHelpers
import qualified Data.LLVM.Private.PlaceholderTypes as O


translateInstruction :: (O.Type -> Type) -> (O.Constant -> IdentDict -> (Value, IdentDict)) ->
                        O.InstructionT -> IdentDict -> (ValueT, IdentDict)
translateInstruction typeMapper trConst oldContent dict = newContent
  where trPair (v, t) = (trConst v, trConst t)
        trArg (v, atts) = (trConst v, atts)

        transMap dct vals =
          foldr f ([], dct) vals
          where f v (acc,d) =
                  let (c, d') = trConst v d
                  in (c : acc, d')

        trFlaggedBinop cons flags lhs rhs d =
          let (l, d1) = trConst lhs d
              (r, d2) = trConst rhs d1
          in (cons flags l r, d2)

        trBinop cons lhs rhs d =
          let (l, d1) = trConst lhs d
              (r, d2) = trConst rhs d1
          in (cons l r, d2)

        trCast cons val ty d =
          let (v, d1) = trConst val d
          in (cons v (typeMapper ty), d1)

        newContent = case oldContent of
          O.RetInst Nothing -> (RetInst Nothing, dict)
          O.RetInst (Just c) ->
            let (c', d1) = trConst c dict
            in (RetInst (Just c'), d1)
          O.UnconditionalBranchInst target ->
            let (t', d1) = trConst target dict
            in (UnconditionalBranchInst t', d1)
          O.BranchInst cond tTarget fTarget ->
            let (c', d1) = trConst cond dict
                (t', d2) = trConst tTarget d1
                (f', d3) = trConst fTarget d2
            in (BranchInst { branchCondition = c'
                           , branchTrueTarget = t'
                           , branchFalseTarget = f'
                           }, d3)
          O.SwitchInst val defTarget cases ->
            let (vs, dests) = unzip cases
                (v', d1) = trConst val dict
                (dt', d2) = trConst defTarget d1
                (vs', d3) = transMap d2 vs
                (dests', d4) = transMap d3 dests
            in (SwitchInst { switchValue = v'
                           , switchDefaultTarget = dt'
                           , switchCases = zip vs' dests'
                           }, d4)
          O.IndirectBranchInst val dests ->
            let (v', d1) = trConst val dict
                (ds', d2) = transMap d1 dests
            in (IndirectBranchInst { indirectBranchAddress = v'
                                   , indirectBranchTargets = ds'
                                   }, d2)
          O.UnwindInst -> (UnwindInst, dict)
          O.UnreachableInst -> (UnreachableInst, dict)
          O.AddInst flags lhs rhs -> trFlaggedBinop AddInst flags lhs rhs dict
          O.SubInst flags lhs rhs -> trFlaggedBinop SubInst flags lhs rhs dict
          O.MulInst flags lhs rhs -> trFlaggedBinop MulInst flags lhs rhs dict
          O.DivInst lhs rhs -> trBinop DivInst lhs rhs dict
          O.RemInst lhs rhs -> trBinop RemInst lhs rhs dict
          O.ShlInst lhs rhs -> trBinop ShlInst lhs rhs dict
          O.LshrInst lhs rhs -> trBinop LshrInst lhs rhs dict
          O.AshrInst lhs rhs -> trBinop AshrInst lhs rhs dict
          O.AndInst lhs rhs -> trBinop AndInst lhs rhs dict
          O.OrInst lhs rhs -> trBinop OrInst lhs rhs dict
          O.XorInst lhs rhs -> trBinop XorInst lhs rhs dict
          O.ExtractElementInst vec idx ->
            let (v', d1) = trConst vec dict
                (i', d2) = trConst idx d1
            in (ExtractElementInst { extractElementVector = v'
                                   , extractElementIndex = i'
                                   }, d2)
          O.InsertElementInst vec elt idx ->
            let (v', d1) = trConst vec dict
                (e', d2) = trConst elt d1
                (i', d3) = trConst idx d2
            in (InsertElementInst { insertElementVector = v'
                                  , insertElementValue = e'
                                  , insertElementIndex = i'
                                  }, d3)
          O.ShuffleVectorInst vec1 vec2 mask ->
            let (v1, d1) = trConst vec1 dict
                (v2, d2) = trConst vec2 d1
                (m, d3) = trConst mask d2
            in (ShuffleVectorInst { shuffleVectorV1 = v1
                                  , shuffleVectorV2 = v2
                                  , shuffleVectorMask = m
                                  }, d3)
          O.ExtractValueInst agg indices ->
            let (a, d1) = trConst agg dict
            in (ExtractValueInst { extractValueAggregate = a
                                 , extractValueIndices = indices
                                 }, d1)
          O.InsertValueInst agg val indices ->
            let (a, d1) = trConst agg dict
                (v, d2) = trConst val d1
            in (InsertValueInst { insertValueAggregate = a
                                , insertValueValue = v
                                , insertValueIndices = indices
                                }, d2)
          O.AllocaInst ty val align ->
            let (v, d1) = trConst val dict
            in (AllocaInst (typeMapper ty) v align, d1)
          O.LoadInst volatile dest align ->
            let (v, d1) = trConst dest dict
            in (LoadInst volatile v align, d1)
          O.StoreInst volatile value dest align ->
            let (v, d1) = trConst value dict
                (d', d2) = trConst dest d1
            in (StoreInst volatile v d' align, d2)
          O.TruncInst val ty -> trCast TruncInst val ty dict
          O.ZExtInst val ty -> trCast ZExtInst val ty dict
          O.SExtInst val ty -> trCast SExtInst val ty dict
          O.FPTruncInst val ty -> trCast FPTruncInst val ty dict
          O.FPExtInst val ty -> trCast FPExtInst val ty dict
          O.FPToUIInst val ty -> trCast FPToUIInst val ty dict
          O.FPToSIInst val ty -> trCast FPToSIInst val ty dict
          O.UIToFPInst val ty -> trCast UIToFPInst val ty dict
          O.SIToFPInst val ty -> trCast SIToFPInst val ty dict
          O.PtrToIntInst val ty -> trCast PtrToIntInst val ty dict
          O.IntToPtrInst val ty -> trCast IntToPtrInst val ty dict
          O.BitcastInst val ty -> trCast BitcastInst val ty dict
          O.ICmpInst cond val1 val2 ->
            let (v1, d1) = trConst val1 dict
                (v2, d2) = trConst val2 d1
            in (ICmpInst cond v1 v2, d2)
          O.FCmpInst cond val1 val2 ->
            let (v1, d1) = trConst val1 dict
                (v2, d2) = trConst val2 d1
            in (FCmpInst cond v1 v2, d2)
          O.PhiNode vals ->
            let (consts, lbls) = unzip vals
                (cs', d1) = transMap dict consts
                (lbls', d2) = transMap d1 lbls
            in (PhiNode $ zip cs' lbls', d2)
          O.SelectInst cond val1 val2 ->
            let (c', d1) = trConst cond dict
                (v1', d2) = trConst val1 d1
                (v2', d3) = trConst val2 d2
            in (SelectInst c' v1' v2', d3)
          O.GetElementPtrInst inBounds val indices ->
            let (v', d1) = trConst val dict
                (idxs, d2) = transMap d1 indices
            in (GetElementPtrInst { getElementPtrInBounds = inBounds
                                  , getElementPtrValue = v'
                                  , getElementPtrIndices = idxs
                                  }, d2)
          O.CallInst { O.callIsTail = isTail
                     , O.callConvention = cc
                     , O.callParamAttrs = paramAttrs
                     , O.callRetType = rtype
                     , O.callFunction = func
                     , O.callArguments = args
                     , O.callAttrs = cAttrs
                     , O.callHasSRet = hasSRet
                     } ->
            let (f, d1) = trConst func dict
                (consts, attrs) = unzip args
                (as, d2) = transMap d1 consts
            in (CallInst { callIsTail = isTail
                         , callConvention = cc
                         , callParamAttrs = paramAttrs
                         , callRetType = typeMapper rtype
                         , callFunction = f
                         , callArguments = zip as attrs
                         , callAttrs = cAttrs
                         , callHasSRet = hasSRet
                         }, d2)
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
            let (f, d1) = trConst func dict
                (consts, attrs) = unzip args
                (as, d2) = transMap d1 consts
                (nl, d3) = trConst normLabl d2
                (ul, d4) = trConst unwindLabl d3
            in (InvokeInst { invokeConvention = cc
                           , invokeParamAttrs = paramAttrs
                           , invokeRetType = typeMapper rtype
                           , invokeFunction = f
                           , invokeArguments = zip as attrs
                           , invokeAttrs = funcAttrs
                           , invokeNormalLabel = nl
                           , invokeUnwindLabel = ul
                           , invokeHasSRet = hasSRet
                           }, d4)
          O.VaArgInst val ty ->
            let (v', d1) = trConst val dict
            in (VaArgInst v' (typeMapper ty), d1)
