module Data.LLVM.Private.ForceModule (
  -- * Types
  ForceMonad,
  -- * Functions
  forceFunction,
  forceGlobalVariable,
  forceGlobalAlias,
  forceExternalValue,
  forceExternalFunction,
  ) where

import Control.DeepSeq
import Control.Monad.State.Strict
import Data.HashSet ( HashSet )
import qualified Data.HashSet as S

import Data.LLVM.Private.Types.Referential

type ForceMonad = State (HashSet Value, HashSet Metadata)

forceInstruction :: Instruction -> ForceMonad ()
forceInstruction i = do
  instructionType i `seq` i `seq` return ()
  mapM_ metaForceIfNeeded (instructionMetadata i)
  case i of
    RetInst { retInstValue = rv } -> maybe (return ()) forceValueIfConstant rv
    UnconditionalBranchInst { unconditionalBranchTarget = t } ->
      t `seq` return ()
    BranchInst { branchCondition = c
               , branchTrueTarget = tt
               , branchFalseTarget = ft
               } -> do
      forceValueIfConstant c
      tt `seq` ft `seq` i `seq` return ()
    SwitchInst { switchValue = sv
               , switchDefaultTarget = dt
               , switchCases = cs
               } -> do
      dt `seq` i `seq` return ()
      forceValueIfConstant sv
      let forceValPair (v1, v2) = v2 `seq` forceValueIfConstant v1
      mapM_ forceValPair cs
    IndirectBranchInst { indirectBranchAddress = addr
                       , indirectBranchTargets = targets
                       } -> do
      foldr seq (return ()) targets
      forceValueIfConstant addr
    UnwindInst { } -> return ()
    UnreachableInst { } -> return ()
    ExtractElementInst { extractElementVector = vec
                       , extractElementIndex = idx
                       } ->
      mapM_ forceValueIfConstant [ vec, idx ]
    InsertElementInst { insertElementVector = vec
                      , insertElementValue = val
                      , insertElementIndex = idx
                      } ->
      mapM_ forceValueIfConstant [ vec, val, idx ]
    ShuffleVectorInst { shuffleVectorV1 = v1
                      , shuffleVectorV2 = v2
                      , shuffleVectorMask = mask
                      } ->
      mapM_ forceValueIfConstant [ v1, v2, mask ]
    ExtractValueInst { extractValueAggregate = agg
                     , extractValueIndices = idxs
                     } -> do
      forceValueIfConstant agg
      idxs `deepseq` return ()
    InsertValueInst { insertValueAggregate = agg
                    , insertValueValue = val
                    , insertValueIndices = idxs
                    } -> do
      mapM_ forceValueIfConstant [ agg, val ]
      idxs `deepseq` return ()
    AllocaInst { allocaNumElements = elems } -> forceValueIfConstant elems
    LoadInst { loadAddress = addr } -> forceValueIfConstant addr
    StoreInst { storeValue = val
              , storeAddress = addr } ->
      mapM_ forceValueIfConstant [ val, addr ]
    AddInst { binaryLhs = v1
            , binaryRhs = v2 } -> mapM_ forceValueIfConstant [ v1, v2 ]
    SubInst { binaryLhs = v1
            , binaryRhs = v2 } -> mapM_ forceValueIfConstant [ v1, v2 ]
    MulInst { binaryLhs = v1
            , binaryRhs = v2 } -> mapM_ forceValueIfConstant [ v1, v2 ]
    DivInst { binaryLhs = v1
            , binaryRhs = v2 } -> mapM_ forceValueIfConstant [ v1, v2 ]
    RemInst { binaryLhs = v1
            , binaryRhs = v2 } -> mapM_ forceValueIfConstant [ v1, v2 ]
    ShlInst { binaryLhs = v1
            , binaryRhs = v2 } -> mapM_ forceValueIfConstant [ v1, v2 ]
    LshrInst { binaryLhs = v1
             , binaryRhs = v2 } -> mapM_ forceValueIfConstant [ v1, v2 ]
    AshrInst { binaryLhs = v1
             , binaryRhs = v2 } -> mapM_ forceValueIfConstant [ v1, v2 ]
    AndInst { binaryLhs = v1
            , binaryRhs = v2 } -> mapM_ forceValueIfConstant [ v1, v2 ]
    OrInst { binaryLhs = v1
           , binaryRhs = v2 } -> mapM_ forceValueIfConstant [ v1, v2 ]
    XorInst { binaryLhs = v1
            , binaryRhs = v2 } -> mapM_ forceValueIfConstant [ v1, v2 ]
    TruncInst { castedValue = cv } -> forceValueIfConstant cv
    ZExtInst { castedValue = cv } -> forceValueIfConstant cv
    SExtInst { castedValue = cv } -> forceValueIfConstant cv
    FPTruncInst { castedValue = cv } -> forceValueIfConstant cv
    FPExtInst { castedValue = cv } -> forceValueIfConstant cv
    FPToSIInst { castedValue = cv } -> forceValueIfConstant cv
    FPToUIInst { castedValue = cv } -> forceValueIfConstant cv
    SIToFPInst { castedValue = cv } -> forceValueIfConstant cv
    UIToFPInst { castedValue = cv } -> forceValueIfConstant cv
    PtrToIntInst { castedValue = cv } -> forceValueIfConstant cv
    IntToPtrInst { castedValue = cv } -> forceValueIfConstant cv
    BitcastInst { castedValue = cv } -> forceValueIfConstant cv
    ICmpInst { cmpV1 = v1
             , cmpV2 = v2
             } -> mapM_ forceValueIfConstant [ v1, v2 ]
    FCmpInst { cmpV1 = v1
             , cmpV2 = v2
             } -> mapM_ forceValueIfConstant [ v1, v2 ]
    SelectInst { selectCondition = c
               , selectTrueValue = tv
               , selectFalseValue = fv
               } -> mapM_ forceValueIfConstant [ c, tv, fv ]
    CallInst { callFunction = f
             , callAttrs = fattrs
             , callArguments = args
             , callParamAttrs = paramAttrs
             } -> do
      paramAttrs `deepseq` fattrs `deepseq` return ()
      forceValueIfConstant f
      let forceArg (v, ps) = forceValueIfConstant v >> ps `deepseq` return ()
      mapM_ forceArg args
    GetElementPtrInst { getElementPtrValue = v
                      , getElementPtrIndices = idxs
                      } -> mapM_ forceValueIfConstant (v:idxs)
    InvokeInst { invokeFunction = f
               , invokeParamAttrs = paramAttrs
               , invokeArguments = args
               , invokeAttrs = attrs
               , invokeNormalLabel = normal
               , invokeUnwindLabel = unwind
               } -> do
      paramAttrs `deepseq` attrs `deepseq` normal `seq` unwind `seq` return ()
      forceValueIfConstant f
      let forceArg (v, ps) = forceValueIfConstant v >> ps `deepseq` return ()
      mapM_ forceArg args
    VaArgInst { vaArgValue = v } -> forceValueIfConstant v
    PhiNode { phiIncomingValues = vs } -> do
      let forcePair (v1, v2) = forceValueIfConstant v1 >> forceValueIfConstant v2
      mapM_ forcePair vs



forceValueIfConstant :: Value -> ForceMonad ()
forceValueIfConstant v = do
  valueName v `deepseq` valueUniqueId v `deepseq` v `seq` return ()
  mapM_ metaForceIfNeeded (valueMetadata v)
  case valueContent v of
    ConstantC c -> forceConstant c
    _ -> valueContent v `seq` return ()

forceConstant :: Constant -> ForceMonad ()
forceConstant c = case constantType c `seq` c of
  UndefValue { } -> return ()
  ConstantAggregateZero { } -> return ()
  ConstantPointerNull { } -> return ()
  BlockAddress { } -> blockAddressFunction c `seq` blockAddressBlock c `seq` return ()
  ConstantArray { } -> mapM_ forceValueIfConstant (constantArrayValues c)
  ConstantFP { } -> return ()
  ConstantInt { } -> return ()
  ConstantString { } -> return ()
  ConstantStruct { } -> mapM_ forceValueIfConstant (constantStructValues c)
  ConstantVector { } -> mapM_ forceValueIfConstant (constantVectorValues c)
  ConstantValue { } -> forceInstruction (constantInstruction c)
  InlineAsm { } -> return ()

forceFunction :: Function -> ForceMonad ()
forceFunction f = do
  functionRetAttrs f `deepseq` functionAttrs f `deepseq`
    functionSection f `seq` f `seq` return ()
  mapM_ forceBasicBlock (functionBody f)
  mapM_ forceArgument (functionParameters f)
  mapM_ metaForceIfNeeded (functionMetadata f)

forceArgument :: Argument -> ForceMonad ()
forceArgument a = do
  argumentParamAttrs a `deepseq` argumentType a `seq` a `seq` return ()
  mapM_ metaForceIfNeeded (argumentMetadata a)

forceGlobalVariable :: GlobalVariable -> ForceMonad ()
forceGlobalVariable gv = do
  globalVariableType gv `seq` gv `seq` return ()
  mapM_ metaForceIfNeeded (globalVariableMetadata gv)
  maybe (return ()) forceValueIfConstant (globalVariableInitializer gv)

forceGlobalAlias :: GlobalAlias -> ForceMonad ()
forceGlobalAlias ga = do
  ga `seq` return ()
  forceValueIfConstant (globalAliasTarget ga)
  mapM_ metaForceIfNeeded (globalAliasMetadata ga)

forceExternalValue :: ExternalValue -> ForceMonad ()
forceExternalValue ev = do
  externalValueType ev `seq` ev `seq` return ()
  mapM_ metaForceIfNeeded (externalValueMetadata ev)

forceExternalFunction :: ExternalFunction -> ForceMonad ()
forceExternalFunction ef = do
  externalFunctionAttrs ef `deepseq` externalFunctionType ef `seq` ef `seq` return ()
  mapM_ metaForceIfNeeded (externalFunctionMetadata ef)

forceBasicBlock :: BasicBlock -> ForceMonad ()
forceBasicBlock b = do
  valueName b `deepseq` valueType b `seq` b `seq` return ()
  mapM_ metaForceIfNeeded (valueMetadata b)
  mapM_ forceInstruction (basicBlockInstructions b)

metaForceIfNeeded :: Metadata -> ForceMonad ()
metaForceIfNeeded m = do
  (vset, mset) <- get
  case S.member m mset of
    True -> return ()
    False -> do
      put (vset, S.insert m mset)
      forceMetadata m
  where
    forceMetadata :: Metadata -> ForceMonad ()
    forceMetadata md = do
      md `seq` return ()
      forceMetadataT (metaValueContent md)

forceMetadataT :: MetadataContent -> ForceMonad ()
forceMetadataT m@(MetaSourceLocation {}) = do
  m `seq` return ()
  metaForceIfNeeded (metaSourceScope m)
forceMetadataT m@(MetaDWLexicalBlock {}) = do
  m `seq` return ()
  mapM_ metaForceIfNeeded [ metaLexicalBlockContext m ]
forceMetadataT m@(MetaDWCompileUnit {}) =
  metaCompileUnitSourceFile m `seq` metaCompileUnitCompileDir m `seq`
    metaCompileUnitProducer m `seq` metaCompileUnitFlags m `seq` m `seq` return ()
forceMetadataT m@(MetaDWFile {}) = do
  metaFileSourceFile m `seq` metaFileSourceDir m `seq` m `seq` return ()
  metaForceIfNeeded (metaFileCompileUnit m)
forceMetadataT m@(MetaDWVariable {}) = do
  metaGlobalVarName m `seq` metaGlobalVarDisplayName m `seq`
   metaGlobalVarLinkageName m `seq` m `seq` return ()
  mapM_ metaForceIfNeeded [ metaGlobalVarContext m
                          , metaGlobalVarFile m
                          , metaGlobalVarType m
                          ]
forceMetadataT m@(MetaDWSubprogram {}) = do
  metaSubprogramName m `seq` metaSubprogramDisplayName m `seq`
    metaSubprogramLinkageName m `seq` m `seq` return ()
  mapM_ metaForceIfNeeded [ metaSubprogramContext m
                          , metaSubprogramFile m
                          , metaSubprogramType m
                          ]
  maybe (return ()) metaForceIfNeeded (metaSubprogramBaseType m)
forceMetadataT m@(MetaDWBaseType {}) = do
  metaBaseTypeName m `seq` m `seq` return ()
  metaForceIfNeeded (metaBaseTypeContext m)
  maybe (return ()) metaForceIfNeeded (metaBaseTypeFile m)
forceMetadataT m@(MetaDWDerivedType {}) = do
  metaDerivedTypeName m `seq` m `seq` return ()
  metaForceIfNeeded (metaDerivedTypeContext m)
  mapM_ (maybe (return ()) metaForceIfNeeded) [ metaDerivedTypeFile m
                                              , metaDerivedTypeParent m
                                              , metaDerivedTypeCompileUnit m
                                              ]
forceMetadataT m@(MetaDWCompositeType {}) = do
  metaCompositeTypeName m `seq` m `seq` return ()
  metaForceIfNeeded (metaCompositeTypeContext m)
  mapM_ (maybe (return ()) metaForceIfNeeded) [ metaCompositeTypeFile m
                                              , metaCompositeTypeParent m
                                              , metaCompositeTypeMembers m
                                              , metaCompositeTypeCompileUnit m
                                              , metaCompositeTypeContainer m
                                              , metaCompositeTypeTemplateParams m
                                              ]
forceMetadataT m@(MetaDWSubrange {}) = m `seq` return ()
forceMetadataT m@(MetaDWEnumerator {}) =
  metaEnumeratorName m `seq` m `seq` return ()
forceMetadataT m@(MetaDWLocal {}) = do
  metaLocalName m `seq` m `seq` return ()
  mapM_ metaForceIfNeeded [ metaLocalContext m
                          , metaLocalFile m
                          , metaLocalType m
                          ]
forceMetadataT m@(MetadataList ms) = do
  m `seq` return ()
  mapM_ metaForceIfNeeded ms
forceMetadataT m@(MetaDWNamespace {}) = do
  m `seq` return ()
  mapM_ metaForceIfNeeded [ metaNamespaceContext m
                          , metaNamespaceCompileUnit m
                          ]
forceMetadataT m@(MetaDWTemplateTypeParameter {}) = do
  m `seq` return ()
  mapM_ metaForceIfNeeded [ metaTemplateTypeParameterContext m
                          , metaTemplateTypeParameterType m
                          ]
forceMetadataT m@(MetaDWTemplateValueParameter {}) = do
  m `seq` return ()
  mapM_ metaForceIfNeeded [ metaTemplateValueParameterContext m
                          , metaTemplateValueParameterType m
                          ]
