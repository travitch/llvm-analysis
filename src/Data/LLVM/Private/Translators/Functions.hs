module Data.LLVM.Private.Translators.Functions ( translateFunctionDefinition ) where

import Data.Map (Map, (!))
import qualified Data.Map as M

import Data.LLVM.Types
import Data.LLVM.Private.AttributeTypes
import qualified Data.LLVM.Private.PlaceholderTypes as O

mkFuncType typeMapper d@O.FunctionDefinition { O.funcRetType = fret
                                             , O.funcParams = params
                                             , O.funcIsVararg = isVararg
                                             , O.funcAttrs = attrs
                                             } = llvmType
  where rtype = typeMapper fret
        argTypes = map (typeMapper . xtype) params
        xtype (O.FormalParameter t _ _) = t
        llvmType = TypeFunction rtype argTypes isVararg attrs
mkFuncType _ _ = error "Non-func decl in mkFuncType"

getFuncIdent O.FunctionDefinition { O.funcName = ident } = ident

translateFunctionDefinition :: (O.Type -> Type) ->
                ((Map Identifier Value) -> O.Constant -> Value) ->
                (Map Identifier Metadata) ->
                (Map Identifier Value) ->
                O.GlobalDeclaration ->
                Map Identifier Value
translateFunctionDefinition typeMapper pTransValOrConst globalMetadata vals decl =
  M.insert (O.funcName decl) v vals
  where v = Value { valueType = ftype
                  , valueName = Just ident
                  , valueMetadata = getMetadata ident
                  , valueContent =
                    Function { functionType = ftype
                             , functionParameters = map translateParameter $ O.funcParams decl
                             , functionBody = translatedBody
                             , functionLinkage = O.funcLinkage decl
                             , functionVisibility = O.funcVisibility decl
                             , functionCC = O.funcCC decl
                             , functionRetAttrs = O.funcRetAttrs decl
                             , functionName = O.funcName decl
                             , functionSection = O.funcSection decl
                             , functionAlign = O.funcAlign decl
                             , functionGCName = O.funcGCName decl
                             , functionIsVararg = O.funcIsVararg decl
                             }
                  }
        transValOrConst = pTransValOrConst localVals

        -- Trivial metadata lookup function
        getMetadata i = M.lookup i globalMetadata

        -- Helper to translate the placeholder constants from the parser
        -- into real values
        trConst (O.ValueRef ident@(LocalIdentifier _)) = localVals ! ident
        trConst c = transValOrConst c

        -- Translated type of the function
        ftype = mkFuncType typeMapper decl
        -- Name of the function
        ident = getFuncIdent decl
        -- Remove @llvm.dbg.* calls from the body after extracting the
        -- information they provide.  Some of the debug information
        -- (e.g. changes in variable values) are ignored.  That
        -- information is implicit in phi nodes.  There is actually
        -- some information in this metadata map for local variables,
        -- too.  I'm not attaching that for now since it doesn't seem
        -- all that useful and is somewhat inconvenient.  It could be
        -- fixed if necessary.
        (localMetadata, noDebugBody) = stripDebugCalls (O.funcBody decl)
        -- Tie the knot on the function body, converting all
        -- instructions to values
        (localVals, translatedBody) = translateBody noDebugBody
        translateParameter (O.FormalParameter ty attrs ident) =
          Value { valueType = typeMapper ty
                , valueName = Just ident
                -- Note: Parameter metadata is mapped in an odd way -
                -- via pseudo-calls to llvm.dbg.declare (or .value).
                -- We construct this map by preprocessing the function
                -- body so that we can map metadata to parameters here.
                , valueMetadata = M.lookup ident localMetadata
                , valueContent = Argument attrs
                }
        translateBody = foldr translateBlock (M.empty, [])
        translateBlock (O.BasicBlock ident placeholderInsts) (locals, blocks) =
          (M.insert ident bb blocksWithLocals, bb : blocks)
          where bb = Value { valueType = TypeVoid
                           , valueName = Just ident
                           , valueMetadata = Nothing -- can BBs have metadata?
                           , valueContent = BasicBlock insts
                           }
                (blocksWithLocals, insts) = translateInsts locals placeholderInsts
        translateInsts locals = foldr trInst (locals, [])
        trInst = translateInstruction typeMapper trConst getMetadata
        -- Returns a new body and a map of identifiers (vals) to
        -- metadata identifiers
        stripDebugCalls = foldr undebugBlock (M.empty, [])
        undebugBlock (O.BasicBlock ident is) (md, blocks) = (md', newBlock:blocks)
          where (md', is') = foldr undebugInst (md, []) is
                newBlock = O.BasicBlock ident is'
        undebugInst :: O.Instruction -> (Map Identifier Metadata, [O.Instruction]) -> (Map Identifier Metadata, [O.Instruction])
        undebugInst i acc@(md, insts) = case O.instContent i of
          O.CallInst { O.callFunction =
                          O.ValueRef (GlobalIdentifier "llvm.dbg.declare")
                     , O.callArguments = args
                     } -> destructureDebugCall args acc
          O.CallInst { O.callFunction =
                          O.ValueRef (GlobalIdentifier "llvm.dbg.value")
                     , O.callArguments = args
                     } -> destructureDebugCall args acc
          -- Not a debug call - just include it
          _ -> (md, i : insts)

        -- Always discard the instruction, but update the metadata map
        -- when possible
        destructureDebugCall [ (O.ConstValue (O.MDNode [Just (O.ValueRef varRef)]) _)
                             , (O.ValueRef i@(MetaIdentifier _)) ] acc@(md, insts) =
          case getMetadata i of
            Nothing -> acc
            Just metadata -> (M.insert varRef metadata md, insts)
        destructureDebugCall [ (O.ConstValue (O.MDNode [Just (O.ValueRef varRef)]) _)
                             , _
                             , (O.ValueRef i@(MetaIdentifier _)) ] acc@(md, insts) =
          case getMetadata i of
            Nothing -> acc
            Just metadata -> (M.insert varRef metadata md, insts)
        destructureDebugCall _ acc = acc

-- This helper converts the non-content fields of an instruction to
-- the equivalent fields in a Value.  It performs the necessary
-- translations to fetch metadata and handle type differences If we
-- want to attach metadata to alloca nodes, we could pass in the local
-- metadata map here and search for iname - if there is an entry in
-- that map then it is actually a local and we have extra metadata
repackInst typeMapper getMetadata O.Instruction { O.instType = itype
                                                , O.instName = iname
                                                , O.instMetadata = md
                                                } newContent = v
  where v = Value { valueType = typeMapper itype
                  , valueName = iname
                  , valueMetadata = maybe Nothing getMetadata md
                  , valueContent = newContent
                  }

-- This handles updating the accumulator in the big
-- translateInstruction fold.  Always add the new value to the
-- instruction list.  Add it to the local variable map only if
-- it has a name.
foldResult (locals, insts) val = case valueName val of
  Just i -> (M.insert i val locals, val : insts)
  Nothing -> (locals, val : insts)

translateInstruction typeMapper trConst getMetadata i acc =
  foldResult acc v
  where v = repackInst typeMapper getMetadata i content
        trPair (v, t) = (trConst v, trConst t)
        content = case O.instContent i of
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
          O.CallInst { O.callIsTail = isTail
                     , O.callConvention = cc
                     , O.callParamAttrs = paramAttrs
                     , O.callRetType = rtype
                     , O.callFunction = func
                     , O.callArguments = args
                     , O.callAttrs = cAttrs
                     } ->
            CallInst { callIsTail = isTail
                     , callConvention = cc
                     , callParamAttrs = paramAttrs
                     , callRetType = typeMapper rtype
                     , callFunction = trConst func
                     , callArguments = map trConst args
                     , callAttrs = cAttrs
                     }
          O.InvokeInst { O.invokeConvention = cc
                       , O.invokeParamAttrs = paramAttrs
                       , O.invokeRetType = rtype
                       , O.invokeFunction = func
                       , O.invokeArguments = args
                       , O.invokeAttrs = funcAttrs
                       , O.invokeNormalLabel = normLabl
                       , O.invokeUnwindLabel = unwindLabl
                       } ->
            InvokeInst { invokeConvention = cc
                       , invokeParamAttrs = paramAttrs
                       , invokeRetType = typeMapper rtype
                       , invokeFunction = trConst func
                       , invokeArguments = map trConst args
                       , invokeAttrs = funcAttrs
                       , invokeNormalLabel = trConst normLabl
                       , invokeUnwindLabel = trConst unwindLabl
                       }
          O.VaArgInst val ty -> VaArgInst (trConst val) (typeMapper ty)
