module Data.LLVM.Private.Translators.Instructions ( translateInstruction ) where

import Data.List (mapAccumR)

import Data.LLVM.Types
import Data.LLVM.Private.KnotHelpers
import qualified Data.LLVM.Private.PlaceholderTypes as O

translateInstruction :: (O.Type -> Type) -> (O.Constant -> IdStream -> Value) ->
                        O.InstructionT -> IdStream -> ValueT
translateInstruction typeMapper trConst oldContent idStream = newContent
  where -- trPair (v, t) = (trConst v, trConst t)
        -- trArg (v, atts) = (trConst v, atts)

        transMap aStream vals =
          snd $ mapAccumR f aStream vals
          where f s v = let (thisStream, otherStream) = splitStream s
                            c = trConst v thisStream
                        in (otherStream, c)

        trFlaggedBinop cons flags lhs rhs s =
          let (lstream, rstream) = splitStream s
              l = trConst lhs lstream
              r = trConst rhs rstream
          in cons flags l r

        trBinop cons lhs rhs s =
          let (lstream, rstream) = splitStream s
              l = trConst lhs lstream
              r = trConst rhs rstream
          in cons l r

        trCast cons val ty s =
          let v = trConst val s
          in cons v (typeMapper ty)

        newContent = case oldContent of
          O.RetInst Nothing -> RetInst Nothing
          O.RetInst (Just c) ->
            let c' = trConst c idStream
            in RetInst (Just c')
          O.UnconditionalBranchInst target ->
            let t' = trConst target idStream
            in UnconditionalBranchInst t'
          O.BranchInst cond tTarget fTarget ->
            let (condstream, bstream) = splitStream idStream
                (tstream, fstream) = splitStream bstream
                c' = trConst cond condstream
                t' = trConst tTarget tstream
                f' = trConst fTarget fstream
            in BranchInst { branchCondition = c'
                          , branchTrueTarget = t'
                          , branchFalseTarget = f'
                          }
          O.SwitchInst val defTarget cases ->
            let (valStream, ostream1) = splitStream idStream
                (defStream, ostream2) = splitStream ostream1
                (caseStream, targetStream) = splitStream ostream2
                (vs, dests) = unzip cases
                v' = trConst val valStream
                dt' = trConst defTarget defStream
                vs' = transMap caseStream vs
                dests' = transMap targetStream dests
            in SwitchInst { switchValue = v'
                          , switchDefaultTarget = dt'
                          , switchCases = zip vs' dests'
                          }
          O.IndirectBranchInst val dests ->
            let (valStream, destStream) = splitStream idStream
                v' = trConst val valStream
                ds' = transMap destStream dests
            in IndirectBranchInst { indirectBranchAddress = v'
                                  , indirectBranchTargets = ds'
                                  }
          O.UnwindInst -> UnwindInst
          O.UnreachableInst -> UnreachableInst
          O.AddInst flags lhs rhs -> trFlaggedBinop AddInst flags lhs rhs idStream
          O.SubInst flags lhs rhs -> trFlaggedBinop SubInst flags lhs rhs idStream
          O.MulInst flags lhs rhs -> trFlaggedBinop MulInst flags lhs rhs idStream
          O.DivInst lhs rhs -> trBinop DivInst lhs rhs idStream
          O.RemInst lhs rhs -> trBinop RemInst lhs rhs idStream
          O.ShlInst lhs rhs -> trBinop ShlInst lhs rhs idStream
          O.LshrInst lhs rhs -> trBinop LshrInst lhs rhs idStream
          O.AshrInst lhs rhs -> trBinop AshrInst lhs rhs idStream
          O.AndInst lhs rhs -> trBinop AndInst lhs rhs idStream
          O.OrInst lhs rhs -> trBinop OrInst lhs rhs idStream
          O.XorInst lhs rhs -> trBinop XorInst lhs rhs idStream
          O.ExtractElementInst vec idx ->
            let (vStream, iStream) = splitStream idStream
                v' = trConst vec vStream
                i' = trConst idx iStream
            in ExtractElementInst { extractElementVector = v'
                                  , extractElementIndex = i'
                                  }
          O.InsertElementInst vec elt idx ->
            let (vStream, ostream) = splitStream idStream
                (eStream, iStream) = splitStream ostream
                v' = trConst vec vStream
                e' = trConst elt eStream
                i' = trConst idx iStream
            in InsertElementInst { insertElementVector = v'
                                 , insertElementValue = e'
                                 , insertElementIndex = i'
                                 }
          O.ShuffleVectorInst vec1 vec2 mask ->
            let (v1Stream, ostream) = splitStream idStream
                (v2Stream, maskStream) = splitStream ostream
                v1 = trConst vec1 v1Stream
                v2 = trConst vec2 v2Stream
                m = trConst mask maskStream
            in ShuffleVectorInst { shuffleVectorV1 = v1
                                 , shuffleVectorV2 = v2
                                 , shuffleVectorMask = m
                                 }
          O.ExtractValueInst agg indices ->
            let a = trConst agg idStream
            in ExtractValueInst { extractValueAggregate = a
                                , extractValueIndices = indices
                                }
          O.InsertValueInst agg val indices ->
            let (aStream, vStream) = splitStream idStream
                a = trConst agg aStream
                v = trConst val vStream
            in InsertValueInst { insertValueAggregate = a
                               , insertValueValue = v
                               , insertValueIndices = indices
                               }
          O.AllocaInst ty val align ->
            let v = trConst val idStream
            in AllocaInst (typeMapper ty) v align
          O.LoadInst volatile dest align ->
            let v = trConst dest idStream
            in LoadInst volatile v align
          O.StoreInst volatile value dest align ->
            let (vStream, dStream) = splitStream idStream
                v = trConst value vStream
                d' = trConst dest dStream
            in StoreInst volatile v d' align
          O.TruncInst val ty -> trCast TruncInst val ty idStream
          O.ZExtInst val ty -> trCast ZExtInst val ty idStream
          O.SExtInst val ty -> trCast SExtInst val ty idStream
          O.FPTruncInst val ty -> trCast FPTruncInst val ty idStream
          O.FPExtInst val ty -> trCast FPExtInst val ty idStream
          O.FPToUIInst val ty -> trCast FPToUIInst val ty idStream
          O.FPToSIInst val ty -> trCast FPToSIInst val ty idStream
          O.UIToFPInst val ty -> trCast UIToFPInst val ty idStream
          O.SIToFPInst val ty -> trCast SIToFPInst val ty idStream
          O.PtrToIntInst val ty -> trCast PtrToIntInst val ty idStream
          O.IntToPtrInst val ty -> trCast IntToPtrInst val ty idStream
          O.BitcastInst val ty -> trCast BitcastInst val ty idStream
          O.ICmpInst cond val1 val2 ->
            let (s1, s2) = splitStream idStream
                v1 = trConst val1 s1
                v2 = trConst val2 s2
            in ICmpInst cond v1 v2
          O.FCmpInst cond val1 val2 ->
            let (s1, s2) = splitStream idStream
                v1 = trConst val1 s1
                v2 = trConst val2 s2
            in FCmpInst cond v1 v2
          O.PhiNode vals ->
            let (consts, lbls) = unzip vals
                (s1, s2) = splitStream idStream
                cs' = transMap s1 consts
                lbls' = transMap s2 lbls
            in PhiNode $ zip cs' lbls'
          O.SelectInst cond val1 val2 ->
            let (cStream, ostream) = splitStream idStream
                (s1, s2) = splitStream ostream
                c' = trConst cond cStream
                v1' = trConst val1 s1
                v2' = trConst val2 s2
            in SelectInst c' v1' v2'
          O.GetElementPtrInst inBounds val indices ->
            let (s1, s2) = splitStream idStream
                v' = trConst val s1
                idxs = transMap s2 indices
            in GetElementPtrInst { getElementPtrInBounds = inBounds
                                 , getElementPtrValue = v'
                                 , getElementPtrIndices = idxs
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
            let (s1, s2) = splitStream idStream
                f = trConst func s1
                (consts, attrs) = unzip args
                as = transMap s2 consts
            in CallInst { callIsTail = isTail
                         , callConvention = cc
                         , callParamAttrs = paramAttrs
                         , callRetType = typeMapper rtype
                         , callFunction = f
                         , callArguments = zip as attrs
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
            let (consts, attrs) = unzip args
                (fStream, r1) = splitStream idStream
                (aStream, r2) = splitStream r1
                (ns, us) = splitStream r2
                f = trConst func fStream
                as = transMap aStream consts
                nl = trConst normLabl ns
                ul = trConst unwindLabl us
            in InvokeInst { invokeConvention = cc
                          , invokeParamAttrs = paramAttrs
                          , invokeRetType = typeMapper rtype
                          , invokeFunction = f
                          , invokeArguments = zip as attrs
                          , invokeAttrs = funcAttrs
                          , invokeNormalLabel = nl
                          , invokeUnwindLabel = ul
                          , invokeHasSRet = hasSRet
                          }
          O.VaArgInst val ty ->
            let v' = trConst val idStream
            in VaArgInst v' (typeMapper ty)
