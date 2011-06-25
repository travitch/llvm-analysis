{-# LANGUAGE ForeignFunctionInterface, DeriveDataTypeable, RankNTypes #-}
module Data.LLVM.Private.Unmarshal where

#include "c++/marshal.h"

import Control.Applicative
import Control.Exception
import Control.Monad.State
import Data.Array.Storable
import Data.ByteString.Char8 ( ByteString )
import qualified Data.ByteString.Char8 as BS
import Data.Int
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
import Data.LLVM.Types hiding ( LinkageType, CallingConvention, VisibilityStyle )

data TranslationException = TooManyReturnValues
                          | InvalidBranchInst
                          | InvalidSwitchLayout
                          | KnotTyingFailure
                          deriving (Show, Typeable)
instance Exception TranslationException

{#enum ArithFlags {} deriving (Show, Eq) #}
{#enum CmpPredicate {underscoreToCase} deriving (Show, Eq) #}
{#enum CallingConvention {} deriving (Show, Eq) #}
{#enum TypeTag {} deriving (Show, Eq) #}
{#enum ValueTag {underscoreToCase} deriving (Show, Eq) #}
{#enum LinkageType {} deriving (Show, Eq) #}
{#enum VisibilityType {} deriving (Show, Eq) #}

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
cGlobalVisibility :: GlobalInfoPtr -> IO VisibilityType
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
cFunctionVisibility :: FunctionInfoPtr -> IO VisibilityType
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
cFunctionGCName :: FunctionInfoPtr -> IO (Maybe ByteString)
cFunctionGCName f = do
  s <- {#get CFunctionInfo->gcName#} f
  case s == nullPtr of
    True -> return Nothing
    False -> do
      bs <- BS.packCString s
      return $! Just bs
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

-- | Parse the named file into an FFI-friendly representation of an
-- LLVM module.
{#fun marshalLLVM { `String' } -> `ModulePtr' id #}

-- | Free all of the resources allocated by 'marshalLLVM'
{#fun disposeCModule { id `ModulePtr' } -> `()' #}

-- FIXME: add accessors for value metadata

type KnotMonad = StateT KnotState IO
data KnotState = KnotState { valueMap :: Map IntPtr Value
                           , typeMap :: Map IntPtr Type
                           }
emptyState :: KnotState
emptyState = KnotState { valueMap = M.empty
                       , typeMap = M.empty
                       }

translate :: FilePath -> IO (Either String Module)
translate bitcodefile = do
  m <- marshalLLVM bitcodefile

  hasError <- cModuleHasError m
  case hasError of
    True -> do
      err <- cModuleErrorMessage m
      disposeCModule m
      return $! Left err
    False -> do
      (ir, _) <- evalStateT (mfix (tieKnot m)) emptyState

      disposeCModule m
      return $! Right ir


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

  let ga = GlobalAlias { globalAliasLinkage = link
                       , globalAliasVisibility = vis
                       , globalAliasValue = ta
                       }
      v = Value { valueType = tt
                , valueName = name
                , valueMetadata = Nothing
                , valueContent = ga
                , valueUniqueId = ptrToIntPtr vp
                }

  recordValue vp v

  return v

translateGlobalVariable :: KnotState -> ValuePtr -> KnotMonad Value
translateGlobalVariable finalState vp = do
  name <- liftIO $ cValueName vp
  typePtr <- liftIO $ cValueType vp
  dataPtr <- liftIO $ cValueData vp
  tt <- translateType typePtr

  let dataPtr' = castPtr dataPtr
      basicVal = Value { valueName = name
                       , valueType = tt
                       , valueMetadata = Nothing
                       , valueContent = ExternalValue
                       , valueUniqueId = ptrToIntPtr vp
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

      ti <- translateConstOrRef finalState initializer

      let gv = GlobalDeclaration { globalVariableLinkage = link
                                 , globalVariableVisibility = vis
                                 , globalVariableInitializer = ti
                                 , globalVariableAlignment = align
                                 , globalVariableSection = section
                                 , globalVariableIsThreadLocal = isThreadLocal
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

  let dataPtr' = castPtr dataPtr
      basicVal = Value { valueName = name
                       , valueType = tt
                       , valueMetadata = Nothing
                       , valueContent = ExternalFunction [] -- FIXME: there are attributes here
                       , valueUniqueId = ptrToIntPtr vp
                       }
  isExtern <- liftIO $ cFunctionIsExternal dataPtr'

  case isExtern of
    True -> do
      recordValue vp basicVal
      return basicVal
    False -> do
      align <- liftIO $ cFunctionAlignment vp
      vis <- liftIO $ cFunctionVisibility vp
      link <- liftIO $ cFunctionLinkage vp
      section <- liftIO $ cFunctionSection vp
      cc <- liftIO $ cFunctionCallingConvention vp
      gcname <- liftIO $ cFunctionGCName vp
      args <- liftIO $ cFunctionArguments vp
      blocks <- liftIO $ cFunctionBlocks vp
      isVarArg <- liftIO $ cFunctionIsVarArg vp

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

  curState <- get
  let key = ptrToIntPtr vp
      curVals = valueMap curState
      tv = Value { valueType = tt
                 , valueName = name
                 , valueMetadata = undefined
                 , valueContent = content
                 , valueUniqueId = fromIntegral key
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
    then translateValue vp
    else case M.lookup (ptrToIntPtr vp) finalState of
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