{-# LANGUAGE ForeignFunctionInterface, DeriveDataTypeable, RankNTypes #-}
module Data.LLVM.Private.Parser.Unmarshal ( parseBitcode ) where

#include "c++/marshal.h"

import Control.Applicative
import Control.DeepSeq
import Control.Exception
import Control.Monad.State
import Data.Array.Storable
import Data.ByteString.Char8 ( ByteString )
import qualified Data.ByteString.Char8 as BS
import Data.Int
import Data.IORef
import Data.Map ( Map )
import qualified Data.Map as M
import Data.Maybe ( catMaybes )
import Data.Typeable
import Foreign.C
import Foreign.C.String
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Storable

import Data.LLVM.Private.C2HS
import Data.LLVM.Private.Parser.Options
import Data.LLVM.Types

data TranslationException = TooManyReturnValues
                          | InvalidBranchInst
                          | InvalidSwitchLayout
                          | InvalidIndirectBranchOperands
                          | KnotTyingFailure
                          | InvalidSelectArgs !Int
                          | InvalidExtractElementInst !Int
                          | InvalidInsertElementInst !Int
                          | InvalidShuffleVectorInst !Int
                          | InvalidFunctionInTranslateValue
                          | InvalidAliasInTranslateValue
                          | InvalidGlobalVarInTranslateValue
                          deriving (Show, Typeable)
instance Exception TranslationException


{#enum TypeTag {} deriving (Show, Eq) #}
{#enum ValueTag {underscoreToCase} deriving (Show, Eq) #}

data CModule
{#pointer *CModule as ModulePtr -> CModule #}

cModuleIdentifier :: ModulePtr -> IO ByteString
cModuleIdentifier m = ({#get CModule->moduleIdentifier#} m) >>= BS.packCString

cModuleDataLayout :: ModulePtr -> IO ByteString
cModuleDataLayout m = ({#get CModule->moduleDataLayout#} m) >>= BS.packCString

cModuleTargetTriple :: ModulePtr -> IO ByteString
cModuleTargetTriple m = ({#get CModule->targetTriple#} m) >>= BS.packCString

cModuleInlineAsm :: ModulePtr -> IO ByteString
cModuleInlineAsm m = ({#get CModule->moduleInlineAsm#} m) >>= BS.packCString

cModuleHasError :: ModulePtr -> IO Bool
cModuleHasError m = cToBool <$> ({#get CModule->hasError#} m)

cModuleErrorMessage :: ModulePtr -> IO String
cModuleErrorMessage m = ({#get CModule->errMsg#} m) >>= peekCString

cModuleLittleEndian :: ModulePtr -> IO Bool
cModuleLittleEndian m = cToBool <$> ({#get CModule->littleEndian#} m)

cModulePointerSize :: ModulePtr -> IO Int
cModulePointerSize m = cIntConv <$> ({#get CModule->pointerSize#} m)

cModuleGlobalVariables :: ModulePtr -> IO [ValuePtr]
cModuleGlobalVariables m =
  peekArray m {#get CModule->globalVariables#} {#get CModule->numGlobalVariables#}

cModuleGlobalAliases :: ModulePtr -> IO [ValuePtr]
cModuleGlobalAliases m =
  peekArray m ({#get CModule->globalAliases#}) ({#get CModule->numGlobalAliases#})

cModuleFunctions :: ModulePtr -> IO [ValuePtr]
cModuleFunctions m =
  peekArray m ({#get CModule->functions#}) ({#get CModule->numFunctions#})

peekArray :: forall a b c e . (Integral c, Storable e) =>
             a -> (a -> IO (Ptr b)) -> (a -> IO c) -> IO [e]
peekArray obj arrAccessor sizeAccessor = do
  nElts <- sizeAccessor obj
  arrPtr <- arrAccessor obj
  fArrPtr <- newForeignPtr_ (castPtr arrPtr)
  arr <- unsafeForeignPtrToStorableArray fArrPtr (1, cIntConv nElts)
  getElems arr

data CType -- = CType TypeTag Int Bool Bool (Ptr TypePtr) Int TypePtr String
{#pointer *CType as TypePtr -> CType #}

data CValue -- = CValue ValueTag TypePtr String (Ptr ()) (Ptr ())
{#pointer *CValue as ValuePtr -> CValue #}

cValueTag :: ValuePtr -> IO ValueTag
cValueTag v = cToEnum <$> ({#get CValue->valueTag#} v)

cValueType :: ValuePtr -> IO TypePtr
cValueType = {#get CValue->valueType#}

cValueName :: ValuePtr -> IO (Maybe Identifier)
cValueName v = do
  namePtr <- ({#get CValue->name#}) v
  case namePtr == nullPtr of
    True -> return Nothing
    False -> do
      name <- BS.packCString namePtr
      return $! (Just . makeIdentifier) name

cValueData :: ValuePtr -> IO (Ptr ())
cValueData = {#get CValue->data#}

data CGlobalInfo
{#pointer *CGlobalInfo as GlobalInfoPtr -> CGlobalInfo #}
cGlobalIsExternal :: GlobalInfoPtr -> IO Bool
cGlobalIsExternal g = cToBool <$> ({#get CGlobalInfo->isExternal#} g)
cGlobalAlignment :: GlobalInfoPtr -> IO Int64
cGlobalAlignment g = cIntConv <$> ({#get CGlobalInfo->alignment#} g)
cGlobalVisibility :: GlobalInfoPtr -> IO VisibilityStyle
cGlobalVisibility g = cToEnum <$> ({#get CGlobalInfo->visibility#} g)
cGlobalLinkage :: GlobalInfoPtr -> IO LinkageType
cGlobalLinkage g = cToEnum <$> ({#get CGlobalInfo->linkage#} g)
cGlobalSection :: GlobalInfoPtr -> IO (Maybe ByteString)
cGlobalSection g = do
  s <- {#get CGlobalInfo->section#} g
  case s == nullPtr of
    True -> return Nothing
    False -> do
      bs <- BS.packCString s
      return $! Just bs
cGlobalInitializer :: GlobalInfoPtr -> IO ValuePtr
cGlobalInitializer = {#get CGlobalInfo->initializer#}
cGlobalIsThreadLocal :: GlobalInfoPtr -> IO Bool
cGlobalIsThreadLocal g = cToBool <$> ({#get CGlobalInfo->isThreadLocal#} g)
cGlobalAliasee :: GlobalInfoPtr -> IO ValuePtr
cGlobalAliasee = {#get CGlobalInfo->aliasee#}

data CFunctionInfo
{#pointer *CFunctionInfo as FunctionInfoPtr -> CFunctionInfo #}
cFunctionIsExternal :: FunctionInfoPtr -> IO Bool
cFunctionIsExternal f = cToBool <$> {#get CFunctionInfo->isExternal#} f
cFunctionAlignment :: FunctionInfoPtr -> IO Int64
cFunctionAlignment f = cIntConv <$> {#get CFunctionInfo->alignment#} f
cFunctionVisibility :: FunctionInfoPtr -> IO VisibilityStyle
cFunctionVisibility f = cToEnum <$> {#get CFunctionInfo->visibility#} f
cFunctionLinkage :: FunctionInfoPtr -> IO LinkageType
cFunctionLinkage f = cToEnum <$> {#get CFunctionInfo->linkage#} f
cFunctionSection :: FunctionInfoPtr -> IO (Maybe ByteString)
cFunctionSection f = do
  s <- {#get CFunctionInfo->section#} f
  case s == nullPtr of
    True -> return Nothing
    False -> do
      bs <- BS.packCString s
      return $! Just bs
cFunctionIsVarArg :: FunctionInfoPtr -> IO Bool
cFunctionIsVarArg f = cToBool <$> {#get CFunctionInfo->isVarArg#} f
cFunctionCallingConvention :: FunctionInfoPtr -> IO CallingConvention
cFunctionCallingConvention f = cToEnum <$> {#get CFunctionInfo->callingConvention#} f
cFunctionGCName :: FunctionInfoPtr -> IO (Maybe GCName)
cFunctionGCName f = do
  s <- {#get CFunctionInfo->gcName#} f
  case s == nullPtr of
    True -> return Nothing
    False -> do
      bs <- BS.packCString s
      return $! Just (GCName bs)
cFunctionArguments :: FunctionInfoPtr -> IO [ValuePtr]
cFunctionArguments f =
  peekArray f {#get CFunctionInfo->arguments#} {#get CFunctionInfo->argListLen#}
cFunctionBlocks :: FunctionInfoPtr -> IO [ValuePtr]
cFunctionBlocks f =
  peekArray f {#get CFunctionInfo->body#} {#get CFunctionInfo->blockListLen#}

data CArgInfo
{#pointer *CArgumentInfo as ArgInfoPtr -> CArgInfo #}

cArgInfoHasSRet :: ArgInfoPtr -> IO Bool
cArgInfoHasSRet a = cToBool <$> ({#get CArgumentInfo->hasSRet#} a)
cArgInfoHasByVal :: ArgInfoPtr -> IO Bool
cArgInfoHasByVal a = cToBool <$> ({#get CArgumentInfo->hasByVal#} a)
cArgInfoHasNest :: ArgInfoPtr -> IO Bool
cArgInfoHasNest a = cToBool <$> ({#get CArgumentInfo->hasNest#} a)
cArgInfoHasNoAlias :: ArgInfoPtr -> IO Bool
cArgInfoHasNoAlias a = cToBool <$> ({#get CArgumentInfo->hasNoAlias#} a)
cArgInfoHasNoCapture :: ArgInfoPtr -> IO Bool
cArgInfoHasNoCapture a = cToBool <$> ({#get CArgumentInfo->hasNoCapture#} a)

data CBasicBlockInfo
{#pointer *CBasicBlockInfo as BasicBlockPtr -> CBasicBlockInfo #}

cBasicBlockInstructions :: BasicBlockPtr -> IO [ValuePtr]
cBasicBlockInstructions b =
  peekArray b {#get CBasicBlockInfo->instructions#} {#get CBasicBlockInfo->blockLen#}

data CInlineAsmInfo
{#pointer *CInlineAsmInfo as InlineAsmInfoPtr -> CInlineAsmInfo #}

cInlineAsmString :: InlineAsmInfoPtr -> IO ByteString
cInlineAsmString a =
  ({#get CInlineAsmInfo->asmString#} a) >>= BS.packCString
cInlineAsmConstraints :: InlineAsmInfoPtr -> IO ByteString
cInlineAsmConstraints a =
  ({#get CInlineAsmInfo->constraintString#} a) >>= BS.packCString

data CBlockAddrInfo
{#pointer *CBlockAddrInfo as BlockAddrInfoPtr -> CBlockAddrInfo #}

cBlockAddrFunc :: BlockAddrInfoPtr -> IO ValuePtr
cBlockAddrFunc = {#get CBlockAddrInfo->func #}
cBlockAddrBlock :: BlockAddrInfoPtr -> IO ValuePtr
cBlockAddrBlock = {#get CBlockAddrInfo->block #}

data CAggregateInfo
{#pointer *CConstAggregate as AggregateInfoPtr -> CAggregateInfo #}

cAggregateValues :: AggregateInfoPtr -> IO [ValuePtr]
cAggregateValues a =
  peekArray a {#get CConstAggregate->constants#} {#get CConstAggregate->numElements#}

data CConstFP
{#pointer *CConstFP as FPInfoPtr -> CConstFP #}
cFPVal :: FPInfoPtr -> IO Double
cFPVal f = cFloatConv <$> ({#get CConstFP->val#} f)

data CConstInt
{#pointer *CConstInt as IntInfoPtr -> CConstInt #}
cIntVal :: IntInfoPtr -> IO Integer
cIntVal i = cIntConv <$> ({#get CConstInt->val#} i)

data CInstructionInfo
{#pointer *CInstructionInfo as InstInfoPtr -> CInstructionInfo #}
cInstructionOperands :: InstInfoPtr -> IO [ValuePtr]
cInstructionOperands i =
  peekArray i {#get CInstructionInfo->operands#} {#get CInstructionInfo->numOperands#}

data CBinaryOpInfo
{#pointer *CBinaryOpInfo as BinaryInfoPtr -> CBinaryOpInfo #}
cBinaryOpLHS :: BinaryInfoPtr -> IO ValuePtr
cBinaryOpLHS = {#get CBinaryOpInfo->lhs #}
cBinaryOpRHS :: BinaryInfoPtr -> IO ValuePtr
cBinaryOpRHS = {#get CBinaryOpInfo->rhs #}
cBinaryOpFlags :: BinaryInfoPtr -> IO ArithFlags
cBinaryOpFlags o = cToEnum <$> {#get CBinaryOpInfo->flags#} o

data CUnaryOpInfo
{#pointer *CUnaryOpInfo as UnaryInfoPtr -> CUnaryOpInfo #}
cUnaryOpOperand :: UnaryInfoPtr -> IO ValuePtr
cUnaryOpOperand = {#get CUnaryOpInfo->val#}
cUnaryOpAlign :: UnaryInfoPtr -> IO Int64
cUnaryOpAlign u = cIntConv <$> {#get CUnaryOpInfo->align#} u
cUnaryOpIsVolatile :: UnaryInfoPtr -> IO Bool
cUnaryOpIsVolatile u = cToBool <$> {#get CUnaryOpInfo->isVolatile#} u
cUnaryOpAddrSpace :: UnaryInfoPtr -> IO Int
cUnaryOpAddrSpace u = cIntConv <$> {#get CUnaryOpInfo->addrSpace#} u

data CCmpInfo
{#pointer *CCmpInfo as CmpInfoPtr -> CCmpInfo #}
cCmpOp1 :: CmpInfoPtr -> IO ValuePtr
cCmpOp1 = {#get CCmpInfo->op1 #}
cCmpOp2 :: CmpInfoPtr -> IO ValuePtr
cCmpOp2 = {#get CCmpInfo->op2 #}
cCmpPred :: CmpInfoPtr -> IO CmpPredicate
cCmpPred c = cToEnum <$> {#get CCmpInfo->pred#} c


data CStoreInfo
{#pointer *CStoreInfo as StoreInfoPtr -> CStoreInfo #}
cStoreValue :: StoreInfoPtr -> IO ValuePtr
cStoreValue = {#get CStoreInfo->value#}
cStorePointer :: StoreInfoPtr -> IO ValuePtr
cStorePointer = {#get CStoreInfo->pointer#}
cStoreAddrSpace :: StoreInfoPtr -> IO Int
cStoreAddrSpace s = cIntConv <$> {#get CStoreInfo->addrSpace#} s
cStoreAlign :: StoreInfoPtr -> IO Int64
cStoreAlign s = cIntConv <$> {#get CStoreInfo->align#} s
cStoreIsVolatile :: StoreInfoPtr -> IO Bool
cStoreIsVolatile s = cToBool <$> {#get CStoreInfo->isVolatile#} s

data CGEPInfo
{#pointer *CGEPInfo as GEPInfoPtr -> CGEPInfo #}
cGEPOperand :: GEPInfoPtr -> IO ValuePtr
cGEPOperand = {#get CGEPInfo->operand#}
cGEPIndices :: GEPInfoPtr -> IO [ValuePtr]
cGEPIndices g =
  peekArray g {#get CGEPInfo->indices#} {#get CGEPInfo->indexListLen#}
cGEPInBounds :: GEPInfoPtr -> IO Bool
cGEPInBounds g = cToBool <$> {#get CGEPInfo->inBounds#} g
cGEPAddrSpace :: GEPInfoPtr -> IO Int
cGEPAddrSpace g = cIntConv <$> {#get CGEPInfo->addrSpace#} g

data CInsExtValInfo
{#pointer *CInsExtValInfo as InsExtPtr -> CInsExtValInfo #}
cInsExtAggregate :: InsExtPtr -> IO ValuePtr
cInsExtAggregate = {#get CInsExtValInfo->aggregate#}
cInsExtValue :: InsExtPtr -> IO ValuePtr
cInsExtValue = {#get CInsExtValInfo->val#}
cInsExtIndices :: InsExtPtr -> IO [Int]
cInsExtIndices ie =
  peekArray ie {#get CInsExtValInfo->indices#} {#get CInsExtValInfo->numIndices#}

data CPHIInfo
{#pointer *CPHIInfo as PHIInfoPtr -> CPHIInfo #}
cPHIValues :: PHIInfoPtr -> IO [ValuePtr]
cPHIValues p =
  peekArray p {#get CPHIInfo->incomingValues#} {#get CPHIInfo->numIncomingValues#}
cPHIBlocks :: PHIInfoPtr -> IO [ValuePtr]
cPHIBlocks p =
  peekArray p {#get CPHIInfo->valueBlocks#} {#get CPHIInfo->numIncomingValues#}

data CCallInfo
{#pointer *CCallInfo as CallInfoPtr -> CCallInfo #}
cCallValue :: CallInfoPtr -> IO ValuePtr
cCallValue = {#get CCallInfo->calledValue #}
cCallArguments :: CallInfoPtr -> IO [ValuePtr]
cCallArguments c =
  peekArray c {#get CCallInfo->arguments#} {#get CCallInfo->argListLen#}
cCallConvention :: CallInfoPtr -> IO CallingConvention
cCallConvention c = cToEnum <$> {#get CCallInfo->callingConvention#} c
cCallHasSRet :: CallInfoPtr -> IO Bool
cCallHasSRet c = cToBool <$> {#get CCallInfo->hasSRet#} c
cCallIsTail :: CallInfoPtr -> IO Bool
cCallIsTail c = cToBool <$> {#get CCallInfo->isTail#} c
cCallUnwindDest :: CallInfoPtr -> IO ValuePtr
cCallUnwindDest = {#get CCallInfo->unwindDest#}
cCallNormalDest :: CallInfoPtr -> IO ValuePtr
cCallNormalDest = {#get CCallInfo->normalDest#}

-- | Parse the named file into an FFI-friendly representation of an
-- LLVM module.
{#fun marshalLLVM { `String' } -> `ModulePtr' id #}

-- | Free all of the resources allocated by 'marshalLLVM'
{#fun disposeCModule { id `ModulePtr' } -> `()' #}

-- FIXME: add accessors for value metadata

type KnotMonad = StateT KnotState IO
data KnotState = KnotState { valueMap :: Map IntPtr Value
                           , typeMap :: Map IntPtr Type
                           , idSrc :: IORef Int
                           }
emptyState :: IORef Int -> KnotState
emptyState r = KnotState { valueMap = M.empty
                         , typeMap = M.empty
                         , idSrc = r
                         }

nextId :: KnotMonad Int
nextId = do
  s <- get
  let r = idSrc s
  thisId <- liftIO $ readIORef r
  liftIO $ modifyIORef r (+1)

  return thisId

parseBitcode :: ParserOptions -> FilePath -> IO (Either String Module)
parseBitcode _ bitcodefile = do
  m <- marshalLLVM bitcodefile

  hasError <- cModuleHasError m
  case hasError of
    True -> do
      err <- cModuleErrorMessage m
      disposeCModule m
      return $! Left err
    False -> do
      ref <- newIORef 0
      (ir, _) <- evalStateT (mfix (tieKnot m)) (emptyState ref)

      disposeCModule m
      return $! Right (ir `deepseq` ir)


tieKnot :: ModulePtr -> (Module, KnotState) -> KnotMonad (Module, KnotState)
tieKnot m (_, finalState) = do
  modIdent <- liftIO $ cModuleIdentifier m
  dataLayout <- liftIO $ cModuleDataLayout m
  triple <- liftIO $ cModuleTargetTriple m
  inlineAsm <- liftIO $ cModuleInlineAsm m

  vars <- liftIO $ cModuleGlobalVariables m
  aliases <- liftIO $ cModuleGlobalAliases m
  funcs <- liftIO $ cModuleFunctions m

  vars' <- mapM (translateGlobalVariable finalState) vars
  aliases' <- mapM (translateAlias finalState) aliases
  funcs' <- mapM (translateFunction finalState) funcs

  let ir = Module { moduleIdentifier = modIdent
                  , moduleDataLayout = undefined
                  , moduleTarget = undefined
                  , moduleAssembly = Assembly inlineAsm
                  , moduleAliases = aliases'
                  , moduleGlobalVariables = vars'
                  , moduleFunctions = funcs'
                  }
  s <- get
  return (ir, s)

translateType :: TypePtr -> KnotMonad Type
translateType tp = undefined

recordValue :: ValuePtr -> Value -> KnotMonad ()
recordValue vp v = do
  s <- get
  let key = ptrToIntPtr vp
      oldMap = valueMap s
  put s { valueMap = M.insert key v oldMap }

translateAlias :: KnotState -> ValuePtr -> KnotMonad Value
translateAlias finalState vp = do
  name <- liftIO $ cValueName vp
  typePtr <- liftIO $ cValueType vp
  dataPtr <- liftIO $ cValueData vp
  let dataPtr' = castPtr dataPtr

  isExtern <- liftIO $ cGlobalIsExternal dataPtr'
  vis <- liftIO $ cGlobalVisibility dataPtr'
  link <- liftIO $ cGlobalLinkage dataPtr'
  aliasee <- liftIO $ cGlobalAliasee dataPtr'

  ta <- translateConstOrRef finalState aliasee
  tt <- translateType typePtr

  uid <- nextId

  let ga = GlobalAlias { globalAliasLinkage = link
                       , globalAliasVisibility = vis
                       , globalAliasValue = ta
                       }
      v = Value { valueType = tt
                , valueName = name
                , valueMetadata = Nothing
                , valueContent = ga
                , valueUniqueId = uid
                }

  recordValue vp v

  return v

translateGlobalVariable :: KnotState -> ValuePtr -> KnotMonad Value
translateGlobalVariable finalState vp = do
  name <- liftIO $ cValueName vp
  typePtr <- liftIO $ cValueType vp
  dataPtr <- liftIO $ cValueData vp
  tt <- translateType typePtr

  uid <- nextId

  let dataPtr' = castPtr dataPtr
      basicVal = Value { valueName = name
                       , valueType = tt
                       , valueMetadata = Nothing
                       , valueContent = ExternalValue
                       , valueUniqueId = uid
                       }
  isExtern <- liftIO $ cGlobalIsExternal dataPtr'

  case isExtern of
    True -> do
      recordValue vp basicVal
      return basicVal
    False -> do
      align <- liftIO $ cGlobalAlignment dataPtr'
      vis <- liftIO $ cGlobalVisibility dataPtr'
      link <- liftIO $ cGlobalLinkage dataPtr'
      section <- liftIO $ cGlobalSection dataPtr'
      isThreadLocal <- liftIO $ cGlobalIsThreadLocal dataPtr'
      initializer <- liftIO $ cGlobalInitializer dataPtr'

      ti <- case initializer == nullPtr of
        True -> return Nothing
        False -> do
          tv <- translateConstOrRef finalState initializer
          return $ Just tv

      let gv = GlobalDeclaration { globalVariableLinkage = link
                                 , globalVariableVisibility = vis
                                 , globalVariableInitializer = ti
                                 , globalVariableAlignment = align
                                 , globalVariableSection = section
                                 , globalVariableIsThreadLocal = isThreadLocal
                                 , globalVariableAddressSpace = undefined
                                 , globalVariableAnnotation = undefined
                                 }
          v = basicVal { valueContent = gv }
      recordValue vp v
      return v

translateFunction :: KnotState -> ValuePtr -> KnotMonad Value
translateFunction finalState vp = do
  name <- liftIO $ cValueName vp
  typePtr <- liftIO $ cValueType vp
  dataPtr <- liftIO $ cValueData vp
  tt <- translateType typePtr

  uid <- nextId

  let dataPtr' = castPtr dataPtr
      basicVal = Value { valueName = name
                       , valueType = tt
                       , valueMetadata = Nothing
                       , valueContent = ExternalFunction [] -- FIXME: there are attributes here
                       , valueUniqueId = uid
                       }
  isExtern <- liftIO $ cFunctionIsExternal dataPtr'

  case isExtern of
    True -> do
      recordValue vp basicVal
      return basicVal
    False -> do
      align <- liftIO $ cFunctionAlignment dataPtr'
      vis <- liftIO $ cFunctionVisibility dataPtr'
      link <- liftIO $ cFunctionLinkage dataPtr'
      section <- liftIO $ cFunctionSection dataPtr'
      cc <- liftIO $ cFunctionCallingConvention dataPtr'
      gcname <- liftIO $ cFunctionGCName dataPtr'
      args <- liftIO $ cFunctionArguments dataPtr'
      blocks <- liftIO $ cFunctionBlocks dataPtr'
      isVarArg <- liftIO $ cFunctionIsVarArg dataPtr'

      args' <- mapM (translateValue finalState) args
      blocks' <- mapM (translateValue finalState) blocks

      let f = Function { functionParameters = args'
                       , functionBody = blocks'
                       , functionLinkage = link
                       , functionVisibility = vis
                       , functionCC = cc
                       , functionRetAttrs = [] -- FIXME
                       , functionAttrs = [] -- FIXME
                       , functionSection = section
                       , functionAlign = align
                       , functionGCName = gcname
                       , functionIsVararg = isVarArg
                       }
          v = basicVal { valueContent = f }
      recordValue vp v
      return v

-- | Only the top-level translators should call this: Globals and BasicBlocks
-- (Or translateConstOrRef when translating constants)
translateValue :: KnotState -> ValuePtr -> KnotMonad Value
translateValue finalState vp = do
  tag <- liftIO $ cValueTag vp
  name <- liftIO $ cValueName vp
  typePtr <- liftIO $ cValueType vp
  dataPtr <- liftIO $ cValueData vp

  tt <- translateType typePtr

  content <- case tag of
    ValArgument -> translateArgument finalState (castPtr dataPtr)
    ValBasicblock -> translateBasicBlock finalState (castPtr dataPtr)
    ValInlineasm -> translateInlineAsm finalState (castPtr dataPtr)
    ValBlockaddress -> translateBlockAddress finalState (castPtr dataPtr)
    ValConstantaggregatezero -> return ConstantAggregateZero
    ValConstantpointernull -> return ConstantPointerNull
    ValUndefvalue -> return UndefValue
    ValConstantarray -> translateConstantAggregate finalState ConstantArray (castPtr dataPtr)
    ValConstantstruct -> translateConstantAggregate finalState ConstantStruct (castPtr dataPtr)
    ValConstantvector -> translateConstantAggregate finalState ConstantVector (castPtr dataPtr)
    ValConstantfp -> translateConstantFP finalState (castPtr dataPtr)
    ValConstantint -> translateConstantInt finalState (castPtr dataPtr)
    -- ValCnstantexpr
    ValRetinst -> translateRetInst finalState (castPtr dataPtr)
    ValBranchinst -> translateBranchInst finalState (castPtr dataPtr)
    ValSwitchinst -> translateSwitchInst finalState (castPtr dataPtr)
    ValIndirectbrinst -> translateIndirectBrInst finalState (castPtr dataPtr)
    ValInvokeinst -> translateInvokeInst finalState (castPtr dataPtr)
    ValUnwindinst -> return UnwindInst
    ValUnreachableinst -> return UnreachableInst
    ValAddinst -> translateFlaggedBinaryOp finalState AddInst (castPtr dataPtr)
    ValFaddinst -> translateFlaggedBinaryOp finalState AddInst (castPtr dataPtr)
    ValSubinst -> translateFlaggedBinaryOp finalState SubInst (castPtr dataPtr)
    ValFsubinst -> translateFlaggedBinaryOp finalState SubInst (castPtr dataPtr)
    ValMulinst ->  translateFlaggedBinaryOp finalState MulInst (castPtr dataPtr)
    ValFmulinst ->  translateFlaggedBinaryOp finalState MulInst (castPtr dataPtr)
    ValUdivinst -> translateBinaryOp finalState DivInst (castPtr dataPtr)
    ValSdivinst -> translateBinaryOp finalState DivInst (castPtr dataPtr)
    ValFdivinst -> translateBinaryOp finalState DivInst (castPtr dataPtr)
    ValUreminst -> translateBinaryOp finalState RemInst (castPtr dataPtr)
    ValSreminst -> translateBinaryOp finalState RemInst (castPtr dataPtr)
    ValFreminst -> translateBinaryOp finalState RemInst (castPtr dataPtr)
    ValShlinst -> translateBinaryOp finalState ShlInst (castPtr dataPtr)
    ValLshrinst -> translateBinaryOp finalState LshrInst (castPtr dataPtr)
    ValAshrinst -> translateBinaryOp finalState AshrInst (castPtr dataPtr)
    ValAndinst -> translateBinaryOp finalState AndInst (castPtr dataPtr)
    ValOrinst -> translateBinaryOp finalState OrInst (castPtr dataPtr)
    ValXorinst -> translateBinaryOp finalState XorInst (castPtr dataPtr)
    ValAllocainst -> translateAllocaInst finalState (castPtr dataPtr)
    ValLoadinst -> translateLoadInst finalState (castPtr dataPtr)
    ValStoreinst -> translateStoreInst finalState (castPtr dataPtr)
    ValGetelementptrinst -> translateGEPInst finalState (castPtr dataPtr)
    ValTruncinst -> translateCastInst finalState TruncInst (castPtr dataPtr)
    ValZextinst -> translateCastInst finalState ZExtInst (castPtr dataPtr)
    ValSextinst -> translateCastInst finalState SExtInst (castPtr dataPtr)
    ValFptruncinst -> translateCastInst finalState FPTruncInst (castPtr dataPtr)
    ValFpextinst -> translateCastInst finalState FPExtInst (castPtr dataPtr)
    ValFptouiinst -> translateCastInst finalState FPToUIInst (castPtr dataPtr)
    ValFptosiinst -> translateCastInst finalState FPToSIInst (castPtr dataPtr)
    ValUitofpinst -> translateCastInst finalState UIToFPInst (castPtr dataPtr)
    ValSitofpinst -> translateCastInst finalState SIToFPInst (castPtr dataPtr)
    ValPtrtointinst -> translateCastInst finalState PtrToIntInst (castPtr dataPtr)
    ValInttoptrinst -> translateCastInst finalState IntToPtrInst (castPtr dataPtr)
    ValBitcastinst -> translateCastInst finalState BitcastInst (castPtr dataPtr)
    ValIcmpinst -> translateCmpInst finalState ICmpInst (castPtr dataPtr)
    ValFcmpinst -> translateCmpInst finalState FCmpInst (castPtr dataPtr)
    ValPhinode -> translatePhiNode finalState (castPtr dataPtr)
    ValCallinst -> translateCallInst finalState (castPtr dataPtr)
    ValSelectinst -> translateSelectInst finalState (castPtr dataPtr)
    ValVaarginst -> translateVarArgInst finalState (castPtr dataPtr)
    ValExtractelementinst -> translateExtractElementInst finalState (castPtr dataPtr)
    ValInsertelementinst -> translateInsertElementInst finalState (castPtr dataPtr)
    ValShufflevectorinst -> translateShuffleVectorInst finalState (castPtr dataPtr)
    ValExtractvalueinst -> translateExtractValueInst finalState (castPtr dataPtr)
    ValInsertvalueinst -> translateInsertValueInst finalState (castPtr dataPtr)
    ValFunction -> throw InvalidFunctionInTranslateValue
    ValAlias -> throw InvalidAliasInTranslateValue
    ValGlobalvariable -> throw InvalidGlobalVarInTranslateValue

  uid <- nextId

  let tv = Value { valueType = tt
                 , valueName = name
                 , valueMetadata = undefined
                 , valueContent = content
                 , valueUniqueId = uid
                 }

  recordValue vp tv

  return tv

isConstant :: ValueTag -> Bool
isConstant vt = case vt of
  ValConstantaggregatezero -> True
  ValConstantarray -> True
  ValConstantfp -> True
  ValConstantint -> True
  ValConstantpointernull -> True
  ValConstantstruct -> True
  ValConstantvector -> True
  ValUndefvalue -> True
  ValConstantexpr -> True
  ValBlockaddress -> True
  _ -> False

translateConstOrRef :: KnotState -> ValuePtr -> KnotMonad Value
translateConstOrRef finalState vp = do
  tag <- liftIO $ cValueTag vp
  if isConstant tag
    then translateValue finalState vp
    else case M.lookup (ptrToIntPtr vp) (valueMap finalState) of
      Just v -> return v
      Nothing -> throw KnotTyingFailure


translateArgument :: KnotState -> ArgInfoPtr -> KnotMonad ValueT
translateArgument _ dataPtr = do
  hasSRet <- liftIO $ cArgInfoHasSRet dataPtr
  hasByVal <- liftIO $ cArgInfoHasByVal dataPtr
  hasNest <- liftIO $ cArgInfoHasNest dataPtr
  hasNoAlias <- liftIO $ cArgInfoHasNoAlias dataPtr
  hasNoCapture <- liftIO $ cArgInfoHasNoCapture dataPtr
  let attrOrNothing b att = if b then Just att else Nothing
      atts = [ attrOrNothing hasSRet PASRet
             , attrOrNothing hasByVal PAByVal
             , attrOrNothing hasNest PANest
             , attrOrNothing hasNoAlias PANoAlias
             , attrOrNothing hasNoCapture PANoCapture
             ]
  return $! Argument (catMaybes atts)

translateBasicBlock :: KnotState -> BasicBlockPtr -> KnotMonad ValueT
translateBasicBlock finalState dataPtr = do
  insts <- liftIO $ cBasicBlockInstructions dataPtr
  tinsts <- mapM (translateValue finalState) insts
  return $! BasicBlock tinsts

translateInlineAsm :: KnotState -> InlineAsmInfoPtr -> KnotMonad ValueT
translateInlineAsm _ dataPtr = do
  asmString <- liftIO $ cInlineAsmString dataPtr
  constraints <- liftIO $ cInlineAsmConstraints dataPtr
  return $! InlineAsm asmString constraints

translateBlockAddress :: KnotState -> BlockAddrInfoPtr -> KnotMonad ValueT
translateBlockAddress finalState dataPtr = do
  fval <- liftIO $ cBlockAddrFunc dataPtr
  bval <- liftIO $ cBlockAddrBlock dataPtr
  f' <- translateConstOrRef finalState fval
  b' <- translateConstOrRef finalState bval
  return $! BlockAddress f' b'

translateConstantAggregate :: KnotState -> ([Value] -> ValueT) -> AggregateInfoPtr -> KnotMonad ValueT
translateConstantAggregate finalState constructor dataPtr = do
  vals <- liftIO $ cAggregateValues dataPtr
  vals' <- mapM (translateConstOrRef finalState) vals
  return $! constructor vals'

translateConstantFP :: KnotState -> FPInfoPtr -> KnotMonad ValueT
translateConstantFP _ dataPtr = do
  fpval <- liftIO $ cFPVal dataPtr
  return $! ConstantFP fpval

translateConstantInt :: KnotState -> IntInfoPtr -> KnotMonad ValueT
translateConstantInt _ dataPtr = do
  intval <- liftIO $ cIntVal dataPtr
  return $! ConstantInt intval

translateRetInst :: KnotState -> InstInfoPtr -> KnotMonad ValueT
translateRetInst finalState dataPtr = do
  opPtrs <- liftIO $ cInstructionOperands dataPtr
  case opPtrs of
    [] -> return $! RetInst Nothing
    [val] -> do
      val' <- translateConstOrRef finalState val
      return $! RetInst (Just val')
    _ -> throw TooManyReturnValues

translateBranchInst :: KnotState -> InstInfoPtr -> KnotMonad ValueT
translateBranchInst finalState dataPtr = do
  opPtrs <- liftIO $ cInstructionOperands dataPtr
  case opPtrs of
    [dst] -> do
      dst' <- translateConstOrRef finalState dst
      return $! UnconditionalBranchInst dst'
    [val, t, f] -> do
      val' <- translateConstOrRef finalState val
      tbranch <- translateConstOrRef finalState t
      fbranch <- translateConstOrRef finalState f
      return $! BranchInst { branchCondition = val'
                           , branchTrueTarget = tbranch
                           , branchFalseTarget = fbranch
                           }
    _ -> throw InvalidBranchInst

translateSwitchInst :: KnotState -> InstInfoPtr -> KnotMonad ValueT
translateSwitchInst finalState dataPtr = do
  opPtrs <- liftIO $ cInstructionOperands dataPtr
  case opPtrs of
    (swVal:defTarget:cases) -> do
      val' <- translateConstOrRef finalState swVal
      def' <- translateConstOrRef finalState defTarget
      -- Process the rest of the list in pairs since that is how LLVM
      -- stores them, but transform it into a nice list of actual
      -- pairs
      let tpairs acc (v1:dest:rest) = do
            v1' <- translateConstOrRef finalState v1
            dest' <- translateConstOrRef finalState dest
            tpairs ((v1', dest'):acc) rest
          tpairs acc [] = return $ reverse acc
          tpairs _ _ = throw InvalidSwitchLayout
      cases' <- tpairs [] cases
      return $! SwitchInst { switchValue = val'
                           , switchDefaultTarget = def'
                           , switchCases = cases'
                           }
    _ -> throw InvalidSwitchLayout

translateIndirectBrInst :: KnotState -> InstInfoPtr -> KnotMonad ValueT
translateIndirectBrInst finalState dataPtr = do
  opPtrs <- liftIO $ cInstructionOperands dataPtr
  case opPtrs of
    (addr:targets) -> do
      addr' <- translateConstOrRef finalState addr
      targets' <- mapM (translateConstOrRef finalState) targets
      return $! IndirectBranchInst { indirectBranchAddress = addr'
                                   , indirectBranchTargets = targets'
                                   }
    _ -> throw InvalidIndirectBranchOperands

translateInvokeInst :: KnotState -> CallInfoPtr -> KnotMonad ValueT
translateInvokeInst finalState dataPtr = do
  func <- liftIO $ cCallValue dataPtr
  args <- liftIO $ cCallArguments dataPtr
  cc <- liftIO $ cCallConvention dataPtr
  hasSRet <- liftIO $ cCallHasSRet dataPtr
  ndest <- liftIO $ cCallNormalDest dataPtr
  udest <- liftIO $ cCallUnwindDest dataPtr

  f' <- translateConstOrRef finalState func
  args' <- mapM (translateConstOrRef finalState) args
  n' <- translateConstOrRef finalState ndest
  u' <- translateConstOrRef finalState udest

  return $! InvokeInst { invokeConvention = cc
                       , invokeParamAttrs = [] -- FIXME
                       , invokeFunction = f'
                       , invokeArguments = zip args' (repeat []) -- FIXME
                       , invokeAttrs = [] -- FIXME
                       , invokeNormalLabel = n'
                       , invokeUnwindLabel = u'
                       , invokeHasSRet = hasSRet
                       }

translateFlaggedBinaryOp :: KnotState -> (ArithFlags -> Value -> Value -> ValueT) ->
                            BinaryInfoPtr -> KnotMonad ValueT
translateFlaggedBinaryOp finalState constructor dataPtr = do
  lhs <- liftIO $ cBinaryOpLHS dataPtr
  rhs <- liftIO $ cBinaryOpRHS dataPtr
  flags <- liftIO $ cBinaryOpFlags dataPtr

  lhs' <- translateConstOrRef finalState lhs
  rhs' <- translateConstOrRef finalState rhs

  return $! constructor flags lhs' rhs'

translateBinaryOp :: KnotState -> (Value -> Value -> ValueT) ->
                     BinaryInfoPtr -> KnotMonad ValueT
translateBinaryOp finalState constructor dataPtr = do
  lhs <- liftIO $ cBinaryOpLHS dataPtr
  rhs <- liftIO $ cBinaryOpRHS dataPtr

  lhs' <- translateConstOrRef finalState lhs
  rhs' <- translateConstOrRef finalState rhs

  return $! constructor lhs' rhs'

translateAllocaInst :: KnotState -> UnaryInfoPtr -> KnotMonad ValueT
translateAllocaInst finalState dataPtr = do
  vptr <- liftIO $ cUnaryOpOperand dataPtr
  align <- liftIO $ cUnaryOpAlign dataPtr

  val <- translateConstOrRef finalState vptr

  return $! AllocaInst val align

translateLoadInst :: KnotState -> UnaryInfoPtr -> KnotMonad ValueT
translateLoadInst finalState dataPtr = do
  addrptr <- liftIO $ cUnaryOpOperand dataPtr
  align <- liftIO $ cUnaryOpAlign dataPtr
  volFlag <- liftIO $ cUnaryOpIsVolatile dataPtr

  addr <- translateConstOrRef finalState addrptr

  return $! LoadInst volFlag addr align

translateStoreInst :: KnotState -> StoreInfoPtr -> KnotMonad ValueT
translateStoreInst finalState dataPtr = do
  vptr <- liftIO $ cStoreValue dataPtr
  pptr <- liftIO $ cStorePointer dataPtr
  addrSpace <- liftIO $ cStoreAddrSpace dataPtr
  align <- liftIO $ cStoreAlign dataPtr
  isVol <- liftIO $ cStoreIsVolatile dataPtr

  val' <- translateConstOrRef finalState vptr
  ptr' <- translateConstOrRef finalState pptr

  return $! StoreInst isVol val' ptr' align

translateGEPInst :: KnotState -> GEPInfoPtr -> KnotMonad ValueT
translateGEPInst finalState dataPtr = do
  vptr <- liftIO $ cGEPOperand dataPtr
  iptrs <- liftIO $ cGEPIndices dataPtr
  inBounds <- liftIO $ cGEPInBounds dataPtr
  addrSpace <- liftIO $ cGEPAddrSpace dataPtr

  oper <- translateConstOrRef finalState vptr
  is <- mapM (translateConstOrRef finalState) iptrs

  return $! GetElementPtrInst { getElementPtrInBounds = inBounds
                              , getElementPtrValue = oper
                              , getElementPtrIndices = is
                              }

translateCastInst :: KnotState -> (Value -> ValueT) -> UnaryInfoPtr -> KnotMonad ValueT
translateCastInst finalState constructor dataPtr = do
  vptr <- liftIO $ cUnaryOpOperand dataPtr
  v <- translateConstOrRef finalState vptr
  return $! constructor v

translateCmpInst :: KnotState -> (CmpPredicate -> Value -> Value -> ValueT) ->
                    CmpInfoPtr -> KnotMonad ValueT
translateCmpInst finalState constructor dataPtr = do
  op1ptr <- liftIO $ cCmpOp1 dataPtr
  op2ptr <- liftIO $ cCmpOp2 dataPtr
  predicate <- liftIO $ cCmpPred dataPtr

  op1 <- translateConstOrRef finalState op1ptr
  op2 <- translateConstOrRef finalState op2ptr

  return $! constructor predicate op1 op2

translatePhiNode :: KnotState -> PHIInfoPtr -> KnotMonad ValueT
translatePhiNode finalState dataPtr = do
  vptrs <- liftIO $ cPHIValues dataPtr
  bptrs <- liftIO $ cPHIBlocks dataPtr

  vals <- mapM (translateConstOrRef finalState) vptrs
  blocks <- mapM (translateConstOrRef finalState) bptrs

  return $! PhiNode $ zip vals blocks

translateCallInst :: KnotState -> CallInfoPtr -> KnotMonad ValueT
translateCallInst finalState dataPtr = do
  vptr <- liftIO $ cCallValue dataPtr
  aptrs <- liftIO $ cCallArguments dataPtr
  cc <- liftIO $ cCallConvention dataPtr
  hasSRet <- liftIO $ cCallHasSRet dataPtr
  isTail <- liftIO $ cCallIsTail dataPtr

  val <- translateConstOrRef finalState vptr
  args <- mapM (translateConstOrRef finalState) aptrs

  return $! CallInst { callIsTail = isTail
                     , callConvention = cc
                     , callParamAttrs = [] -- FIXME
                     , callFunction = val
                     , callArguments = zip args (repeat []) -- FIXME
                     , callAttrs = [] -- FIXME
                     , callHasSRet = hasSRet
                     }

translateSelectInst :: KnotState -> InstInfoPtr -> KnotMonad ValueT
translateSelectInst finalState dataPtr = do
  opPtrs <- liftIO $ cInstructionOperands dataPtr
  ops <- mapM (translateConstOrRef finalState) opPtrs
  case ops of
    [cond, trueval, falseval] -> do
      return $! SelectInst cond trueval falseval
    _ -> throw $ InvalidSelectArgs (length ops)

translateVarArgInst :: KnotState -> UnaryInfoPtr -> KnotMonad ValueT
translateVarArgInst finalState dataPtr = do
  opptr <- liftIO $ cUnaryOpOperand dataPtr
  op <- translateConstOrRef finalState opptr
  return $! VaArgInst op

translateExtractElementInst :: KnotState -> InstInfoPtr -> KnotMonad ValueT
translateExtractElementInst finalState dataPtr = do
  opPtrs <- liftIO $ cInstructionOperands dataPtr
  ops <- mapM (translateConstOrRef finalState) opPtrs
  case ops of
    [vec, idx] -> do
      return $! ExtractElementInst { extractElementVector = vec
                                   , extractElementIndex = idx
                                   }
    _ -> throw $ InvalidExtractElementInst (length ops)

translateInsertElementInst :: KnotState -> InstInfoPtr -> KnotMonad ValueT
translateInsertElementInst finalState dataPtr = do
  opPtrs <- liftIO $ cInstructionOperands dataPtr
  ops <- mapM (translateConstOrRef finalState) opPtrs
  case ops of
    [vec, val, idx] -> do
      return $! InsertElementInst { insertElementVector = vec
                                  , insertElementValue = val
                                  , insertElementIndex = idx
                                  }
    _ -> throw $ InvalidInsertElementInst (length ops)

translateShuffleVectorInst :: KnotState -> InstInfoPtr -> KnotMonad ValueT
translateShuffleVectorInst finalState dataPtr = do
  opPtrs <- liftIO $ cInstructionOperands dataPtr
  ops <- mapM (translateConstOrRef finalState) opPtrs
  case ops of
    [v1, v2, vecMask] -> do
      return $! ShuffleVectorInst { shuffleVectorV1 = v1
                                  , shuffleVectorV2 = v2
                                  , shuffleVectorMask = vecMask
                                  }
    _ -> throw $ InvalidShuffleVectorInst (length ops)
{-
cInsExtAggregate :: InsExtPtr -> IO ValuePtr
cInsExtValue :: InsExtPtr -> IO ValuePtr
cInsExtIndices :: InsExtPtr -> IO [Int]
-}
translateExtractValueInst :: KnotState -> InsExtPtr -> KnotMonad ValueT
translateExtractValueInst finalState dataPtr = do
  aggPtr <- liftIO $ cInsExtAggregate dataPtr
  indices <- liftIO $ cInsExtIndices dataPtr

  agg <- translateConstOrRef finalState aggPtr

  return $! ExtractValueInst { extractValueAggregate = agg
                             , extractValueIndices = indices
                             }

translateInsertValueInst :: KnotState -> InsExtPtr -> KnotMonad ValueT
translateInsertValueInst finalState dataPtr = do
  aggPtr <- liftIO $ cInsExtAggregate dataPtr
  valPtr <- liftIO $ cInsExtValue dataPtr
  indices <- liftIO $ cInsExtIndices dataPtr

  agg <- translateConstOrRef finalState aggPtr
  val <- translateConstOrRef finalState valPtr

  return $! InsertValueInst { insertValueAggregate = agg
                            , insertValueValue = val
                            , insertValueIndices = indices
                            }
