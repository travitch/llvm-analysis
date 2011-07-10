module Data.LLVM.Private.ForceModule ( ForceMonad, forceGlobal ) where

import Control.DeepSeq
import Control.Monad.State.Strict
import Data.HashSet ( HashSet )
import qualified Data.HashSet as S

import Data.LLVM.Private.Types.Referential

type ForceMonad = State (HashSet Value, HashSet Metadata)


-- | Force each type of global as much as is safe.
forceGlobal :: Value -> ForceMonad ()
forceGlobal v = do
  -- The unique id field is strict and will be forced when we force this constructor.
  valueName v `deepseq` v `seq` return ()
  -- Expand the metadata, if there is any
  mapM_ metaForceIfNeeded (valueMetadata v)
  forceGlobalValueT (valueContent v)
  return ()

isConstant :: Value -> Bool
isConstant v = case valueContent v of
  Argument _ -> True
  ConstantAggregateZero -> True
  ConstantArray _ -> True
  ConstantFP _ -> True
  ConstantInt _ -> True
  ConstantString _ -> True
  ConstantPointerNull -> True
  ConstantStruct _ -> True
  ConstantVector _ -> True
  ConstantValue _ -> True
  _ -> False

forceInstruction :: Value -> ForceMonad ()
forceInstruction v = do
  valueName v `deepseq` valueUniqueId v `deepseq` v `seq` return ()
  mapM_ metaForceIfNeeded (valueMetadata v)
  forceValueT (valueContent v)

forceValueIfConstant :: Value -> ForceMonad ()
forceValueIfConstant v = do
  valueName v `deepseq` valueUniqueId v `deepseq` v `seq` return ()
  mapM_ metaForceIfNeeded (valueMetadata v)
  case isConstant v of
    True -> forceValueT (valueContent v)
    False -> valueContent v `seq` return ()

forceValueT :: ValueT -> ForceMonad ()
forceValueT c =
  case c of
    Function {} -> return () -- Forced separately
    GlobalDeclaration {} -> return ()
    GlobalAlias {} -> return ()
    ExternalValue -> return ()
    BasicBlock _ -> return () -- Nothing to do and forced separately
    ExternalFunction _ -> return ()
    Argument atts -> atts `deepseq` c `seq` return ()
    UndefValue -> c `seq` return ()
    ConstantAggregateZero -> c `seq` return ()
    ConstantArray vs -> do
      mapM_ forceValueIfConstant vs
      c `seq` return ()
    ConstantFP d -> d `seq` c `seq` return ()
    ConstantInt i -> i `seq` c `seq` return ()
    ConstantString s -> s `seq` c `seq` return ()
    ConstantPointerNull -> c `seq` return ()
    ConstantStruct vs -> do
      mapM_ forceValueIfConstant vs
      c `seq` return ()
    ConstantVector vs -> do
      mapM_ forceValueIfConstant vs
      c `seq` return ()
    ConstantValue v' -> do
      forceValueT v'
      c `seq` return ()
    InlineAsm s1 s2 -> s1 `seq` s2 `seq` c `seq` return ()
    RetInst mv -> case mv of
      Nothing -> return ()
      Just r -> r `seq` c `seq` return ()
    UnconditionalBranchInst i -> do
      forceValueIfConstant i
      c `seq` return ()
    BranchInst {} -> do
      mapM_ forceValueIfConstant [ branchCondition c
                                 , branchTrueTarget c
                                 , branchFalseTarget c
                                 ]
      c `seq` return ()
    SwitchInst {} -> do
      mapM_ forceValueIfConstant [ switchValue c
                                 , switchDefaultTarget c
                                 ]
      c `seq` return ()
      let forceValPair (v1, v2) = mapM_ forceValueIfConstant [ v1, v2 ]
      mapM_ forceValPair (switchCases c)
    IndirectBranchInst {} -> do
      forceValueIfConstant (indirectBranchAddress c)
      c `seq` return ()
      mapM_ forceValueIfConstant (indirectBranchTargets c)
    UnwindInst -> c `seq` return ()
    UnreachableInst -> c `seq` return ()
    AddInst flags v1 v2 -> do
      flags `deepseq` c `seq` return ()
      mapM_ forceValueIfConstant [ v1, v2 ]
    SubInst flags v1 v2 -> do
      flags `deepseq` c `seq` return ()
      mapM_ forceValueIfConstant [ v1, v2 ]
    MulInst flags v1 v2 -> do
      flags `deepseq` c `seq` return ()
      mapM_ forceValueIfConstant [ v1, v2 ]
    DivInst v1 v2 -> do
      c `seq` return ()
      mapM_ forceValueIfConstant [ v1, v2 ]
    RemInst v1 v2 -> do
      c `seq` return ()
      mapM_ forceValueIfConstant [ v1, v2 ]
    ShlInst v1 v2 -> do
      c `seq` return ()
      mapM_ forceValueIfConstant [ v1, v2 ]
    LshrInst v1 v2 -> do
      c `seq` return ()
      mapM_ forceValueIfConstant [ v1, v2 ]
    AshrInst v1 v2 -> do
      c `seq` return ()
      mapM_ forceValueIfConstant [ v1, v2 ]
    AndInst v1 v2 -> do
      c `seq` return ()
      mapM_ forceValueIfConstant [ v1, v2 ]
    OrInst v1 v2 -> do
      c `seq` return ()
      mapM_ forceValueIfConstant [ v1, v2 ]
    XorInst v1 v2 -> do
      c `seq` return ()
      mapM_ forceValueIfConstant [ v1, v2 ]
    ExtractElementInst {} -> do
      mapM_ forceValueIfConstant [ extractElementVector c
                                 , extractElementIndex c
                                 ]
      c `seq` return ()
    InsertElementInst {} -> do
      mapM_ forceValueIfConstant [ insertElementVector c
                                 , insertElementValue c
                                 , insertElementIndex c
                                 ]
      c `seq` return ()
    ShuffleVectorInst {} -> do
      mapM_ forceValueIfConstant [ shuffleVectorV1 c
                                 , shuffleVectorV2 c
                                 , shuffleVectorMask c
                                 ]
      c `seq` return ()
    ExtractValueInst {} -> do
      forceValueIfConstant (extractValueAggregate c)
      mapM_ (`seq` return ()) (extractValueIndices c)
      c `seq` return ()
    InsertValueInst {} -> do
      mapM_ forceValueIfConstant [ insertValueAggregate c
                                 , insertValueValue c
                                 ]
      insertValueIndices c `deepseq` c `seq` return ()
    AllocaInst v i -> do
      forceValueIfConstant v
      i `seq` c `seq` return ()
    LoadInst {} -> do
      forceValueIfConstant (loadAddress c)
      c `seq` return ()
    StoreInst {} -> do
      mapM_ forceValueIfConstant [ storeValue c, storeAddress c ]
      c `seq` return ()
    TruncInst v -> do
      forceValueIfConstant v
      c `seq` return ()
    ZExtInst v -> do
      forceValueIfConstant v
      c `seq` return ()
    SExtInst v -> do
      forceValueIfConstant v
      c `seq` return ()
    FPTruncInst v -> do
      forceValueIfConstant v
      c `seq` return ()
    FPExtInst v -> do
      forceValueIfConstant v
      c `seq` return ()
    FPToUIInst v -> do
      forceValueIfConstant v
      c `seq` return ()
    FPToSIInst v -> do
      forceValueIfConstant v
      c `seq` return ()
    UIToFPInst v -> do
      forceValueIfConstant v
      c `seq` return ()
    SIToFPInst v -> do
      forceValueIfConstant v
      c `seq` return ()
    PtrToIntInst v -> do
      forceValueIfConstant v
      c `seq` return ()
    IntToPtrInst v -> do
      forceValueIfConstant v
      c `seq` return ()
    BitcastInst v -> do
      forceValueIfConstant v
      c `seq` return ()
    ICmpInst cond v1 v2 -> do
      mapM_ forceValueIfConstant [ v1, v2 ]
      cond `seq` c `seq` return ()
    FCmpInst cond v1 v2 -> do
      mapM_ forceValueIfConstant [ v1, v2 ]
      cond `seq` c `seq` return ()
    PhiNode vs -> do
      let seqp (v1, v2) = mapM_ forceValueIfConstant [ v1, v2 ]
      mapM_ seqp vs
      c `seq` return ()
    SelectInst v1 v2 v3 -> do
      mapM_ forceValueIfConstant [ v1, v2, v3 ]
      c `seq` return ()
    GetElementPtrInst {} -> do
      mapM_ forceValueIfConstant $ getElementPtrValue c : getElementPtrIndices c
      getElementPtrInBounds c `seq` c `seq` return ()
    CallInst {} -> do
      mapM_ forceValueIfConstant $ callFunction c : map fst (callArguments c)
      callParamAttrs c `deepseq` c `seq` return ()
    InvokeInst {} -> do
      let vs = invokeNormalLabel c : invokeUnwindLabel c :
                 invokeFunction c : map fst (invokeArguments c)
      mapM_ forceValueIfConstant vs
      invokeParamAttrs c `deepseq` c `seq` return ()
    VaArgInst v -> do
      forceValueIfConstant v
      c `seq` return ()
    BlockAddress v1 v2 -> do
      mapM_ forceValueIfConstant [ v1, v2 ]
      c `seq` return ()

forceGlobalValueT :: ValueT -> ForceMonad ()
forceGlobalValueT f@(Function {})= do
  -- It is safe to deepseq the parameters since they can't contain
  -- cyclic stuff, and the Function owns them.
  functionRetAttrs f `deepseq` functionAttrs f `deepseq`
    functionSection f `seq` f `seq` return ()
  mapM_ forceBasicBlock (functionBody f)
  mapM_ forceValueIfConstant (functionParameters f)
forceGlobalValueT g@(GlobalDeclaration {}) = do
  globalVariableSection g `seq` g `seq` return ()
  maybe (return ()) forceValueIfConstant (globalVariableInitializer g)
forceGlobalValueT g@(GlobalAlias {}) = do
  globalAliasLinkage g `seq` globalAliasVisibility g `seq` g `seq` return ()
  forceValueIfConstant (globalAliasValue g)
forceGlobalValueT e@ExternalValue = do
  e `seq` return ()
forceGlobalValueT e@(ExternalFunction atts) = do
  atts `deepseq` e `seq` return ()

forceBasicBlock :: Value -> ForceMonad ()
forceBasicBlock v = do
  valueName v `deepseq` v `seq` return ()
  mapM_ metaForceIfNeeded (valueMetadata v)
  let c = valueContent v
  case c of
    BasicBlock is -> mapM_ forceInstruction is


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

forceMetadataT :: MetadataT -> ForceMonad ()
forceMetadataT m@(MetaSourceLocation {}) = do
  m `seq` return ()
  metaForceIfNeeded (metaSourceScope m)
forceMetadataT m@(MetaDWLexicalBlock {}) = do
  m `seq` return ()
  mapM_ metaForceIfNeeded [ metaLexicalBlockContext m ]
forceMetadataT m@(MetaDWCompileUnit {}) = do
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
                                              ]
forceMetadataT m@(MetaDWCompositeType {}) = do
  metaCompositeTypeName m `seq` m `seq` return ()
  metaForceIfNeeded (metaCompositeTypeContext m)
  mapM_ (maybe (return ()) metaForceIfNeeded) [ metaCompositeTypeFile m
                                              , metaCompositeTypeParent m
                                              , metaCompositeTypeMembers m
                                              ]
forceMetadataT m@(MetaDWSubrange {}) = do
  m `seq` return ()
forceMetadataT m@(MetaDWEnumerator {}) = do
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
forceMetadataT m@(MetadataValueConstant v) = do
  -- v will actually be forced by the value expander, so we can just
  -- force the constructor for it here.
  v `seq` m `seq` return ()
forceMetadataT m@MetadataDiscarded = do
  m `seq` return ()
forceMetadataT m@MetadataUnknown = do
  m `seq` return ()