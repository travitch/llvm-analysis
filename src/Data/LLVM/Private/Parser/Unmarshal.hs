-- | This module converts the C form of the LLVM IR into a fully
-- referential Haskell version of the IR.  The translation is slightly
-- lossy around integral types in some cases, as Haskell Ints do not
-- have the same range as C ints.  In the vast majority of cases this
-- should not really be an issue, but it is possible to lose
-- information.  If it is an issue it can be changed.
{-# LANGUAGE DeriveDataTypeable, RankNTypes, FlexibleInstances #-}
module Data.LLVM.Private.Parser.Unmarshal ( parseLLVMBitcodeFile ) where

import Prelude hiding ( catch )

import Control.Applicative
import Control.DeepSeq
import Control.Exception
import Control.Monad.State
import Data.ByteString.Char8 ( ByteString )
import qualified Data.ByteString.Char8 as BS
import Data.IORef
import Data.Map ( Map )
import Data.Set ( Set )
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe ( catMaybes, isJust )
import Data.Typeable
import Foreign.Ptr

import Data.LLVM.Private.Parser.Options
import Data.LLVM.Types
import Data.LLVM.Interop

data TranslationException = TooManyReturnValues
                          | InvalidBranchInst
                          | InvalidSwitchLayout
                          | InvalidIndirectBranchOperands
                          | KnotTyingFailure ValueTag
                          | TypeKnotTyingFailure TypeTag
                          | MetaKnotFailure
                          | InvalidSelectArgs !Int
                          | InvalidExtractElementInst !Int
                          | InvalidInsertElementInst !Int
                          | InvalidShuffleVectorInst !Int
                          | InvalidFunctionInTranslateValue
                          | InvalidAliasInTranslateValue
                          | InvalidGlobalVarInTranslateValue
                          | InvalidBinaryOp !Int
                          | InvalidUnaryOp !Int
                          | InvalidGEPInst !Int
                          | InvalidExtractValueInst !Int
                          | InvalidInsertValueInst !Int
                          deriving (Show, Typeable)
instance Exception TranslationException

type KnotMonad = StateT KnotState IO
data KnotState = KnotState { valueMap :: Map IntPtr Value
                           , typeMap :: Map IntPtr Type
                           , metaMap :: Map IntPtr Metadata
                           , idSrc :: IORef Int
                           , typeIdSrc :: IORef Int
                           , metaIdSrc :: IORef Int
                           , result :: Maybe Module
                           , visitedTypes :: Set IntPtr
                           , visitedMetadata :: Set IntPtr
                           , localId :: Int
                           , constantTranslationDepth :: Int
                           , stringCache :: Map ByteString ByteString
                           }

instance InternString (StateT KnotState IO) where
  internString str = do
    s <- get
    let cache = stringCache s
    case M.lookup str cache of
      Just cval -> return cval
      Nothing -> do
        put s { stringCache = M.insert str str cache }
        return str


emptyState :: IORef Int -> IORef Int -> IORef Int -> KnotState
emptyState r1 r2 r3 =
  KnotState { valueMap = M.empty
            , typeMap = M.empty
            , metaMap = M.empty
            , idSrc = r1
            , typeIdSrc = r2
            , metaIdSrc = r3
            , result = Nothing
            , visitedTypes = S.empty
            , visitedMetadata = S.empty
            , localId = 0
            , constantTranslationDepth = 0
            , stringCache = M.empty
            }

nextId :: KnotMonad Int
nextId = do
  s <- get
  let r = idSrc s
  thisId <- liftIO $ readIORef r
  liftIO $ modifyIORef r (+1)

  return thisId

nextTypeId :: KnotMonad Int
nextTypeId = do
  s <- get
  let r = typeIdSrc s
  thisId <- liftIO $ readIORef r
  liftIO $ modifyIORef r (+1)

  return thisId

nextMetaId :: KnotMonad Int
nextMetaId = do
  s <- get
  let r = metaIdSrc s
  thisId <- liftIO $ readIORef r
  liftIO $ modifyIORef r (+1)

  return thisId

-- | Parse the named LLVM bitcode file into the LLVM form of the IR (a
-- 'Module').  In the case of an error, a descriptive string will be
-- returned.
parseLLVMBitcodeFile :: ParserOptions -> FilePath -> IO (Either String Module)
parseLLVMBitcodeFile opts bitcodefile = do
  let includeLineNumbers = metaPositionPrecision opts == PositionPrecise
  m <- marshalLLVM bitcodefile includeLineNumbers

  hasError <- cModuleHasError m
  case hasError of
    True -> do
      Just err <- cModuleErrorMessage m
      disposeCModule m
      return $ Left err
    False -> catch (doParse m) exHandler
  where
    exHandler :: TranslationException -> IO (Either String Module)
    exHandler ex = return $ Left (show ex)
    doParse m = do
      idref <- newIORef 1
      tref <- newIORef 1
      mref <- newIORef 1
      res <- evalStateT (mfix (tieKnot m)) (emptyState idref tref mref)

      disposeCModule m
      case result res of
        Just r -> return $ Right (r `deepseq` r)
        Nothing -> return $ Left "No module in result"


tieKnot :: ModulePtr -> KnotState -> KnotMonad KnotState
tieKnot m finalState = do
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
                  , moduleDataLayout = dataLayout
                  , moduleTarget = triple
                  , moduleAssembly = Assembly inlineAsm
                  , moduleAliases = aliases'
                  , moduleGlobalVariables = vars'
                  , moduleFunctions = funcs'
                  }
  s <- get
  return s { result = Just ir }

translateType :: KnotState -> TypePtr -> KnotMonad Type
translateType finalState tp = do
  s <- get
  let ip = ptrToIntPtr tp
  -- This top-level translateType function is never called
  -- recursively, so the set it introduces here will be valid for the
  -- entire duration of the translateType call.  It will be
  -- overwritten on the next call.
  put s { visitedTypes = S.singleton ip }
  case M.lookup ip (typeMap s) of
    Just t -> return t
    Nothing -> do
      t <- translateType' finalState tp
      st <- get
      put st { typeMap = M.insert ip t (typeMap st) }
      return t

translateTypeRec :: KnotState -> TypePtr -> KnotMonad Type
translateTypeRec finalState tp = do
  s <- get
  let ip = ptrToIntPtr tp
  -- Mark this type as visited in the state - the pattern match below
  -- refers to the version of the map *before* this insertion.
  put s { visitedTypes = S.insert ip (visitedTypes s) }
  case M.lookup ip (typeMap s) of
    -- If we already translated, just do the simple thing.
    Just t -> return t
    Nothing -> do
      tag <- liftIO $ cTypeTag tp
      case (S.member ip (visitedTypes s), tag) of
        -- This is a cyclic reference - look it up in the final result
        (False, TYPE_NAMED) -> do
          -- If we have never seen a reference to this named type
          -- before, we need to create it.
          name <- liftIO $ cTypeName tp
          itp <- liftIO $ cTypeInner tp
          innerType <- translateTypeRec finalState itp

          let t = TypeNamed name innerType

          st <- get
          let m = typeMap st
              m' = M.insert (ptrToIntPtr itp) innerType m
              m'' = M.insert ip t m'
          put st { typeMap = m'' }

          return t

        (True, TYPE_NAMED) -> do
          -- Otherwise, if we *have* seen it before, we can just look
          -- it up.  This handles the case of seeing the same named
          -- type for the first time within e.g., a TypeStruct
          return $ M.findWithDefault (throw (TypeKnotTyingFailure tag)) ip (typeMap finalState)
        (True, _) -> do
          -- Here we have detected a cycle in a type that isn't broken
          -- up by a NamedType.  We introduce an artificial name (a
          -- type upref) to break the cycle.  This makes it a lot
          -- easier to print out types later on, as we don't have to
          -- do on-the-fly cycle detection everywhere we want to work
          -- with types.
          let innerType = M.findWithDefault (throw (TypeKnotTyingFailure tag)) ip (typeMap finalState)
          uprefName <- nextTypeId
          let t = TypeNamed (show uprefName) innerType
          st <- get
          put st { typeMap = M.insert ip t (typeMap st) }
          return t
        _ -> translateType' finalState tp

translateType' :: KnotState -> TypePtr -> KnotMonad Type
translateType' finalState tp = do
  s <- get
  tag <- liftIO $ cTypeTag tp
  t <- case tag of
    TYPE_VOID -> return TypeVoid
    TYPE_FLOAT -> return TypeFloat
    TYPE_DOUBLE -> return TypeDouble
    TYPE_X86_FP80 -> return TypeX86FP80
    TYPE_FP128 -> return TypeFP128
    TYPE_PPC_FP128 -> return TypePPCFP128
    TYPE_LABEL -> return TypeLabel
    TYPE_METADATA -> return TypeMetadata
    TYPE_X86_MMX -> return TypeX86MMX
    TYPE_OPAQUE -> return TypeOpaque
    TYPE_INTEGER -> do
      sz <- liftIO $ cTypeSize tp
      return $ TypeInteger sz
    TYPE_FUNCTION -> do
      isVa <- liftIO $ cTypeIsVarArg tp
      rtp <- liftIO $ cTypeInner tp
      argTypePtrs <- liftIO $ cTypeList tp

      rType <- translateTypeRec finalState rtp
      argTypes <- mapM (translateTypeRec finalState) argTypePtrs

      return $ TypeFunction rType argTypes isVa
    TYPE_STRUCT -> do
      isPacked <- liftIO $ cTypeIsPacked tp
      ptrs <- liftIO $ cTypeList tp

      types <- mapM (translateTypeRec finalState) ptrs

      return $ TypeStruct types isPacked
    TYPE_ARRAY -> do
      sz <- liftIO $ cTypeSize tp
      itp <- liftIO $ cTypeInner tp
      innerType <- translateTypeRec finalState itp

      return $ TypeArray sz innerType
    TYPE_POINTER -> do
      itp <- liftIO $ cTypeInner tp
      addrSpc <- liftIO $ cTypeAddrSpace tp
      innerType <- translateTypeRec finalState itp

      return $ TypePointer innerType addrSpc
    TYPE_VECTOR -> do
      sz <- liftIO $ cTypeSize tp
      itp <- liftIO $ cTypeInner tp
      innerType <- translateTypeRec finalState itp

      return $ TypeVector sz innerType
    TYPE_NAMED -> do
      name <- liftIO $ cTypeName tp
      itp <- liftIO $ cTypeInner tp
      innerType <- translateTypeRec finalState itp

      return $ TypeNamed name innerType
  -- Need to get the latest state that exists after processing all
  -- inner types above, otherwise we'll erase their updates from the
  -- map.
  s' <- get
  put s' { typeMap = M.insert (ptrToIntPtr tp) t (typeMap s') }
  return t

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
  metaPtr <- liftIO $ cValueMetadata vp
  let dataPtr' = castPtr dataPtr

  mds <- mapM (translateMetadata finalState) metaPtr

  vis <- liftIO $ cGlobalVisibility dataPtr'
  link <- liftIO $ cGlobalLinkage dataPtr'
  aliasee <- liftIO $ cGlobalAliasee dataPtr'

  ta <- translateConstOrRef finalState aliasee
  tt <- translateType finalState typePtr

  uid <- nextId

  let ga = GlobalAlias { globalAliasLinkage = link
                       , globalAliasVisibility = vis
                       , globalAliasValue = ta
                       }
      v = Value { valueType = tt
                , valueName = name
                , valueMetadata = mds
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
  metaPtr <- liftIO $ cValueMetadata vp
  tt <- translateType finalState typePtr

  mds <- mapM (translateMetadata finalState) metaPtr
  uid <- nextId

  let dataPtr' = castPtr dataPtr
      basicVal = Value { valueName = name
                       , valueType = tt
                       , valueMetadata = mds
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
      isConst <- liftIO $ cGlobalIsConstant dataPtr'

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
                                 , globalVariableIsConstant = isConst
                                 }
          v = basicVal { valueContent = gv }
      recordValue vp v
      return v

resetLocalIdCounter :: KnotMonad ()
resetLocalIdCounter = do
  s <- get
  put s { localId = 0 }

translateFunction :: KnotState -> ValuePtr -> KnotMonad Value
translateFunction finalState vp = do
  name <- liftIO $ cValueName vp
  typePtr <- liftIO $ cValueType vp
  dataPtr <- liftIO $ cValueData vp
  metaPtr <- liftIO $ cValueMetadata vp
  tt <- translateType finalState typePtr

  mds <- mapM (translateMetadata finalState) metaPtr

  uid <- nextId

  resetLocalIdCounter

  let dataPtr' = castPtr dataPtr
      basicVal = Value { valueName = name
                       , valueType = tt
                       , valueMetadata = mds
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

constantBracket :: KnotMonad ValueT -> KnotMonad ValueT
constantBracket c = do
  s <- get
  let tdepth = constantTranslationDepth s
  put s { constantTranslationDepth = tdepth + 1 }
  ret <- c
  s' <- get
  let tdepth' = constantTranslationDepth s'
  put s' { constantTranslationDepth = tdepth' - 1 }
  return ret

-- | This wrapper checks to see if we have translated the value yet
-- (but not against the final state - only the internal running
-- state).  This way we really translate it if it hasn't been seen
-- yet, but get the translated value if we have touched it before.
translateValue :: KnotState -> ValuePtr -> KnotMonad Value
translateValue finalState vp = do
  s <- get
  case M.lookup (ptrToIntPtr vp) (valueMap s) of
    Nothing -> translateValue' finalState vp
    Just v -> return v

-- | Only the top-level translators should call this: Globals and BasicBlocks
-- (Or translateConstOrRef when translating constants)
translateValue' :: KnotState -> ValuePtr -> KnotMonad Value
translateValue' finalState vp = do
  tag <- liftIO $ cValueTag vp
  name <- liftIO $ cValueName vp
  typePtr <- liftIO $ cValueType vp
  dataPtr <- liftIO $ cValueData vp
  metaPtr <- liftIO $ cValueMetadata vp

  mds <- mapM (translateMetadata finalState) metaPtr

  s <- get
  let cdepth = constantTranslationDepth s
      idCtr = localId s
  realName <- case isJust name || isGlobal tag || isConstant tag || cdepth > 0 of
    True -> return name
    False -> do
      put s { localId = idCtr + 1 }
      return $ Just $ makeLocalIdentifier $ BS.pack (show idCtr)

  tt <- translateType finalState typePtr

  content <- case tag of
    ValArgument -> translateArgument finalState (castPtr dataPtr)
    ValBasicblock -> translateBasicBlock finalState (castPtr dataPtr)
    ValInlineasm -> constantBracket $ translateInlineAsm finalState (castPtr dataPtr)
    ValBlockaddress -> constantBracket $ translateBlockAddress finalState (castPtr dataPtr)
    ValConstantaggregatezero -> return ConstantAggregateZero
    ValConstantpointernull -> return ConstantPointerNull
    ValUndefvalue -> return UndefValue
    ValConstantarray -> constantBracket $ translateConstantAggregate finalState ConstantArray (castPtr dataPtr)
    ValConstantstruct -> constantBracket $ translateConstantAggregate finalState ConstantStruct (castPtr dataPtr)
    ValConstantvector -> constantBracket $ translateConstantAggregate finalState ConstantVector (castPtr dataPtr)
    ValConstantfp -> translateConstantFP finalState (castPtr dataPtr)
    ValConstantint -> translateConstantInt finalState (castPtr dataPtr)
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
    ValConstantexpr -> constantBracket $ translateConstantExpr finalState (castPtr dataPtr)

  uid <- nextId

  let tv = Value { valueType = tt
                 , valueName = realName
                 , valueMetadata = mds
                 , valueContent = content
                 , valueUniqueId = uid
                 }

  recordValue vp tv

  return tv

isGlobal :: ValueTag -> Bool
isGlobal vt = case vt of
  ValFunction -> True
  ValAlias -> True
  ValGlobalvariable -> True
  _ -> False

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
  ValInlineasm -> True
  _ -> False

translateConstOrRef :: KnotState -> ValuePtr -> KnotMonad Value
translateConstOrRef finalState vp = do
  tag <- liftIO $ cValueTag vp
  let ip = ptrToIntPtr vp
  case isConstant tag of
    True -> translateValue finalState vp
    False ->
      return $ M.findWithDefault (throw (KnotTyingFailure tag)) ip (valueMap finalState)


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
  return $ Argument (catMaybes atts)

translateBasicBlock :: KnotState -> BasicBlockPtr -> KnotMonad ValueT
translateBasicBlock finalState dataPtr = do
  insts <- liftIO $ cBasicBlockInstructions dataPtr
  tinsts <- mapM (translateValue finalState) insts
  return $ BasicBlock tinsts

translateInlineAsm :: KnotState -> InlineAsmInfoPtr -> KnotMonad ValueT
translateInlineAsm _ dataPtr = do
  asmString <- liftIO $ cInlineAsmString dataPtr
  constraints <- liftIO $ cInlineAsmConstraints dataPtr
  return $ InlineAsm asmString constraints

translateBlockAddress :: KnotState -> BlockAddrInfoPtr -> KnotMonad ValueT
translateBlockAddress finalState dataPtr = do
  fval <- liftIO $ cBlockAddrFunc dataPtr
  bval <- liftIO $ cBlockAddrBlock dataPtr
  f' <- translateConstOrRef finalState fval
  b' <- translateConstOrRef finalState bval
  return $ BlockAddress f' b'

translateConstantAggregate :: KnotState -> ([Value] -> ValueT) -> AggregateInfoPtr -> KnotMonad ValueT
translateConstantAggregate finalState constructor dataPtr = do
  vals <- liftIO $ cAggregateValues dataPtr
  vals' <- mapM (translateConstOrRef finalState) vals
  return $ constructor vals'

translateConstantFP :: KnotState -> FPInfoPtr -> KnotMonad ValueT
translateConstantFP _ dataPtr = do
  fpval <- liftIO $ cFPVal dataPtr
  return $ ConstantFP fpval

translateConstantInt :: KnotState -> IntInfoPtr -> KnotMonad ValueT
translateConstantInt _ dataPtr = do
  intval <- liftIO $ cIntVal dataPtr
  return $ ConstantInt intval

translateRetInst :: KnotState -> InstInfoPtr -> KnotMonad ValueT
translateRetInst finalState dataPtr = do
  opPtrs <- liftIO $ cInstructionOperands dataPtr
  case opPtrs of
    [] -> return $ RetInst Nothing
    [val] -> do
      val' <- translateConstOrRef finalState val
      return $ RetInst (Just val')
    _ -> throw TooManyReturnValues

-- | Note, in LLVM the operands of the Branch instruction are ordered as
--
-- [Condition, FalseTarget,] TrueTarget
--
-- This is not exactly as expected.
translateBranchInst :: KnotState -> InstInfoPtr -> KnotMonad ValueT
translateBranchInst finalState dataPtr = do
  opPtrs <- liftIO $ cInstructionOperands dataPtr
  case opPtrs of
    [dst] -> do
      dst' <- translateConstOrRef finalState dst
      return $ UnconditionalBranchInst dst'
    [val, f, t] -> do
      val' <- translateConstOrRef finalState val
      fbranch <- translateConstOrRef finalState f
      tbranch <- translateConstOrRef finalState t
      return $ BranchInst { branchCondition = val'
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
      return $ SwitchInst { switchValue = val'
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
      return $ IndirectBranchInst { indirectBranchAddress = addr'
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

  return $ InvokeInst { invokeConvention = cc
                      , invokeParamAttrs = [] -- FIXME
                      , invokeFunction = f'
                      , invokeArguments = zip args' (repeat []) -- FIXME
                      , invokeAttrs = [] -- FIXME
                      , invokeNormalLabel = n'
                      , invokeUnwindLabel = u'
                      , invokeHasSRet = hasSRet
                      }

translateFlaggedBinaryOp :: KnotState -> (ArithFlags -> Value -> Value -> ValueT) ->
                            InstInfoPtr -> KnotMonad ValueT
translateFlaggedBinaryOp finalState constructor dataPtr = do
  opPtrs <- liftIO $ cInstructionOperands dataPtr
  flags <- liftIO $ cInstructionArithFlags dataPtr

  ops <- mapM (translateConstOrRef finalState) opPtrs

  case ops of
    [lhs, rhs] -> return $ constructor flags lhs rhs
    _ -> throw $ InvalidBinaryOp (length ops)

translateBinaryOp :: KnotState -> (Value -> Value -> ValueT) ->
                     InstInfoPtr -> KnotMonad ValueT
translateBinaryOp finalState constructor dataPtr = do
  opPtrs <- liftIO $ cInstructionOperands dataPtr
  ops <- mapM (translateConstOrRef finalState) opPtrs

  case ops of
    [lhs, rhs] -> return $ constructor lhs rhs
    _ -> throw $ InvalidBinaryOp (length ops)

translateAllocaInst :: KnotState -> InstInfoPtr -> KnotMonad ValueT
translateAllocaInst finalState dataPtr = do
  opPtrs <- liftIO $ cInstructionOperands dataPtr
  align <- liftIO $ cInstructionAlign dataPtr
  ops <- mapM (translateConstOrRef finalState) opPtrs

  case ops of
    [val] -> return $ AllocaInst val align
    _ -> throw $ InvalidUnaryOp (length ops)


translateLoadInst :: KnotState -> InstInfoPtr -> KnotMonad ValueT
translateLoadInst finalState dataPtr = do
  opPtrs <- liftIO $ cInstructionOperands dataPtr
  align <- liftIO $ cInstructionAlign dataPtr
  vol <- liftIO $ cInstructionIsVolatile dataPtr

  ops <- mapM (translateConstOrRef finalState) opPtrs

  case ops of
    [addr] -> return $ LoadInst { loadIsVolatile = vol
                                , loadAddress = addr
                                , loadAlignment = align
                                }
    _ -> throw $ InvalidUnaryOp (length ops)

translateStoreInst :: KnotState -> InstInfoPtr -> KnotMonad ValueT
translateStoreInst finalState dataPtr = do
  opPtrs <- liftIO $ cInstructionOperands dataPtr
  addrSpace <- liftIO $ cInstructionAddrSpace dataPtr
  align <- liftIO $ cInstructionAlign dataPtr
  isVol <- liftIO $ cInstructionIsVolatile dataPtr

  ops <- mapM (translateConstOrRef finalState) opPtrs

  case ops of
    [val, ptr] -> return $ StoreInst { storeIsVolatile = isVol
                                     , storeValue = val
                                     , storeAddress = ptr
                                     , storeAlignment = align
                                     , storeAddrSpace = addrSpace
                                     }
    _ -> throw $ InvalidBinaryOp (length ops)

translateGEPInst :: KnotState -> InstInfoPtr -> KnotMonad ValueT
translateGEPInst finalState dataPtr = do
  opPtrs <- liftIO $ cInstructionOperands dataPtr
  inBounds <- liftIO $ cInstructionInBounds dataPtr
  addrSpace <- liftIO $ cInstructionAddrSpace dataPtr

  ops <- mapM (translateConstOrRef finalState) opPtrs

  case ops of
    (val:indices) -> return $ GetElementPtrInst { getElementPtrInBounds = inBounds
                                                , getElementPtrValue = val
                                                , getElementPtrIndices = indices
                                                , getElementPtrAddrSpace = addrSpace
                                                }
    _ -> throw $ InvalidGEPInst (length ops)

translateCastInst :: KnotState -> (Value -> ValueT) -> InstInfoPtr -> KnotMonad ValueT
translateCastInst finalState constructor dataPtr = do
  opPtrs <- liftIO $ cInstructionOperands dataPtr
  ops <- mapM (translateConstOrRef finalState) opPtrs

  case ops of
    [v] -> return $ constructor v
    _ -> throw $ InvalidUnaryOp (length ops)

translateCmpInst :: KnotState -> (CmpPredicate -> Value -> Value -> ValueT) ->
                    InstInfoPtr -> KnotMonad ValueT
translateCmpInst finalState constructor dataPtr = do
  opPtrs <- liftIO $ cInstructionOperands dataPtr
  predicate <- liftIO $ cInstructionCmpPred dataPtr

  ops <- mapM (translateConstOrRef finalState) opPtrs

  case ops of
    [op1, op2] -> return $ constructor predicate op1 op2
    _ -> throw $ InvalidBinaryOp (length ops)

translatePhiNode :: KnotState -> PHIInfoPtr -> KnotMonad ValueT
translatePhiNode finalState dataPtr = do
  vptrs <- liftIO $ cPHIValues dataPtr
  bptrs <- liftIO $ cPHIBlocks dataPtr

  vals <- mapM (translateConstOrRef finalState) vptrs
  blocks <- mapM (translateConstOrRef finalState) bptrs

  return $ PhiNode $ zip vals blocks

translateCallInst :: KnotState -> CallInfoPtr -> KnotMonad ValueT
translateCallInst finalState dataPtr = do
  vptr <- liftIO $ cCallValue dataPtr
  aptrs <- liftIO $ cCallArguments dataPtr
  cc <- liftIO $ cCallConvention dataPtr
  hasSRet <- liftIO $ cCallHasSRet dataPtr
  isTail <- liftIO $ cCallIsTail dataPtr

  val <- translateConstOrRef finalState vptr
  args <- mapM (translateConstOrRef finalState) aptrs

  return $ CallInst { callIsTail = isTail
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
      return $ SelectInst cond trueval falseval
    _ -> throw $ InvalidSelectArgs (length ops)

translateVarArgInst :: KnotState -> InstInfoPtr -> KnotMonad ValueT
translateVarArgInst finalState dataPtr = do
  opPtrs <- liftIO $ cInstructionOperands dataPtr
  ops <- mapM (translateConstOrRef finalState) opPtrs
  case ops of
    [op] -> return $ VaArgInst op
    _ -> throw $ InvalidUnaryOp (length ops)

translateExtractElementInst :: KnotState -> InstInfoPtr -> KnotMonad ValueT
translateExtractElementInst finalState dataPtr = do
  opPtrs <- liftIO $ cInstructionOperands dataPtr
  ops <- mapM (translateConstOrRef finalState) opPtrs
  case ops of
    [vec, idx] -> do
      return $ ExtractElementInst { extractElementVector = vec
                                  , extractElementIndex = idx
                                  }
    _ -> throw $ InvalidExtractElementInst (length ops)

translateInsertElementInst :: KnotState -> InstInfoPtr -> KnotMonad ValueT
translateInsertElementInst finalState dataPtr = do
  opPtrs <- liftIO $ cInstructionOperands dataPtr
  ops <- mapM (translateConstOrRef finalState) opPtrs
  case ops of
    [vec, val, idx] -> do
      return $ InsertElementInst { insertElementVector = vec
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
      return $ ShuffleVectorInst { shuffleVectorV1 = v1
                                 , shuffleVectorV2 = v2
                                 , shuffleVectorMask = vecMask
                                 }
    _ -> throw $ InvalidShuffleVectorInst (length ops)

translateExtractValueInst :: KnotState -> InstInfoPtr -> KnotMonad ValueT
translateExtractValueInst finalState dataPtr = do
  opPtrs <- liftIO $ cInstructionOperands dataPtr
  indices <- liftIO $ cInstructionIndices dataPtr
  ops <- mapM (translateConstOrRef finalState) opPtrs
  case ops of
    [agg] -> return $ ExtractValueInst { extractValueAggregate = agg
                                       , extractValueIndices = indices
                                       }
    _ -> throw $ InvalidExtractValueInst (length ops)

translateInsertValueInst :: KnotState -> InstInfoPtr -> KnotMonad ValueT
translateInsertValueInst finalState dataPtr = do
  opPtrs <- liftIO $ cInstructionOperands dataPtr
  indices <- liftIO $ cInstructionIndices dataPtr
  ops <- mapM (translateConstOrRef finalState) opPtrs
  case ops of
    [agg, val] ->
      return $ InsertValueInst { insertValueAggregate = agg
                               , insertValueValue = val
                               , insertValueIndices = indices
                               }
    _ -> throw $ InvalidInsertValueInst (length ops)

translateConstantExpr :: KnotState -> ConstExprPtr -> KnotMonad ValueT
translateConstantExpr finalState dataPtr = do
  ii <- liftIO $ cConstExprInstInfo dataPtr
  tag <- liftIO $ cConstExprTag dataPtr
  vt <- case tag of
    ValAddinst -> translateFlaggedBinaryOp finalState AddInst ii
    ValFaddinst -> translateFlaggedBinaryOp finalState AddInst ii
    ValSubinst -> translateFlaggedBinaryOp finalState SubInst ii
    ValFsubinst -> translateFlaggedBinaryOp finalState SubInst ii
    ValMulinst ->  translateFlaggedBinaryOp finalState MulInst ii
    ValFmulinst ->  translateFlaggedBinaryOp finalState MulInst ii
    ValUdivinst -> translateBinaryOp finalState DivInst ii
    ValSdivinst -> translateBinaryOp finalState DivInst ii
    ValFdivinst -> translateBinaryOp finalState DivInst ii
    ValUreminst -> translateBinaryOp finalState RemInst ii
    ValSreminst -> translateBinaryOp finalState RemInst ii
    ValFreminst -> translateBinaryOp finalState RemInst ii
    ValShlinst -> translateBinaryOp finalState ShlInst ii
    ValLshrinst -> translateBinaryOp finalState LshrInst ii
    ValAshrinst -> translateBinaryOp finalState AshrInst ii
    ValAndinst -> translateBinaryOp finalState AndInst ii
    ValOrinst -> translateBinaryOp finalState OrInst ii
    ValXorinst -> translateBinaryOp finalState XorInst ii
    ValGetelementptrinst -> translateGEPInst finalState ii
    ValTruncinst -> translateCastInst finalState TruncInst ii
    ValZextinst -> translateCastInst finalState ZExtInst ii
    ValSextinst -> translateCastInst finalState SExtInst ii
    ValFptruncinst -> translateCastInst finalState FPTruncInst ii
    ValFpextinst -> translateCastInst finalState FPExtInst ii
    ValFptouiinst -> translateCastInst finalState FPToUIInst ii
    ValFptosiinst -> translateCastInst finalState FPToSIInst ii
    ValUitofpinst -> translateCastInst finalState UIToFPInst ii
    ValSitofpinst -> translateCastInst finalState SIToFPInst ii
    ValPtrtointinst -> translateCastInst finalState PtrToIntInst ii
    ValInttoptrinst -> translateCastInst finalState IntToPtrInst ii
    ValBitcastinst -> translateCastInst finalState BitcastInst ii
    ValIcmpinst -> translateCmpInst finalState ICmpInst ii
    ValFcmpinst -> translateCmpInst finalState FCmpInst ii
    ValSelectinst -> translateSelectInst finalState ii
    ValVaarginst -> translateVarArgInst finalState ii
    ValExtractelementinst -> translateExtractElementInst finalState ii
    ValInsertelementinst -> translateInsertElementInst finalState ii
    ValShufflevectorinst -> translateShuffleVectorInst finalState ii
    ValExtractvalueinst -> translateExtractValueInst finalState ii
    ValInsertvalueinst -> translateInsertValueInst finalState ii
  return $ ConstantValue vt

translateMetadata :: KnotState -> MetaPtr -> KnotMonad Metadata
translateMetadata finalState mp = do
  s <- get
  let ip = ptrToIntPtr mp
  put s { visitedMetadata = S.insert ip (visitedMetadata s) }
  case M.lookup ip (metaMap s) of
    Just m -> return m
    Nothing -> translateMetadata' finalState mp

translateMetadataRec :: KnotState -> MetaPtr -> KnotMonad Metadata
translateMetadataRec finalState mp = do
  s <- get
  let ip = ptrToIntPtr mp
  -- If we have already visited this metadata object, look it up in
  -- the final state.  We record visits *before* making recursive
  -- calls, allowing us to tie the knot by looking already-visited
  -- nodes up in the final state.
  --
  -- If we haven't seen this node before, we can safely call the
  -- outermost 'translateMetadata', which will make an entry in the
  -- visited set and then do the translation.
  case S.member ip (visitedMetadata s) of
    False -> translateMetadata finalState mp
    True -> return $ M.findWithDefault (throw MetaKnotFailure) ip (metaMap finalState)

maybeTranslateMetadataRec :: KnotState -> Maybe MetaPtr -> KnotMonad (Maybe Metadata)
maybeTranslateMetadataRec _ Nothing = return Nothing
maybeTranslateMetadataRec finalState (Just mp) =
  Just <$> translateMetadataRec finalState mp

translateMetadata' :: KnotState -> MetaPtr -> KnotMonad Metadata
translateMetadata' finalState mp = do
  let ip = ptrToIntPtr mp
  s <- get
  put s { visitedMetadata = S.insert ip (visitedMetadata s) }
  metaTag <- liftIO $ cMetaTypeTag mp
  tag <- liftIO $ cMetaTag mp
  content <- case metaTag of
    MetaLocation -> do
      line <- liftIO $ cMetaLocationLine mp
      col <- liftIO $ cMetaLocationColumn mp
      scope <- liftIO $ cMetaLocationScope mp

      scope' <- translateMetadataRec finalState scope
      return MetaSourceLocation { metaSourceRow = line
                                , metaSourceCol = col
                                , metaSourceScope = scope'
                                }
    MetaDerivedtype -> do
      ctxt <- liftIO $ cMetaTypeContext mp
      name <- cMetaTypeName mp
      f <- liftIO $ cMetaTypeFile mp
      line <- liftIO $ cMetaTypeLine mp
      size <- liftIO $ cMetaTypeSize mp
      align <- liftIO $ cMetaTypeAlign mp
      off <- liftIO $ cMetaTypeOffset mp
      parent <- liftIO $ cMetaTypeDerivedFrom mp

      cu <- liftIO $ cMetaTypeCompileUnit mp
      isArtif <- liftIO $ cMetaTypeIsArtificial mp
      isVirt <- liftIO $ cMetaTypeIsVirtual mp
      isForward <- liftIO $ cMetaTypeIsForward mp
      isProt <- liftIO $ cMetaTypeIsProtected mp
      isPriv <- liftIO $ cMetaTypeIsPrivate mp

      f' <- maybeTranslateMetadataRec finalState f
      ctxt' <- translateMetadataRec finalState ctxt
      parent' <- maybeTranslateMetadataRec finalState parent
      cu' <- maybeTranslateMetadataRec finalState cu

      return MetaDWDerivedType { metaDerivedTypeContext = ctxt'
                               , metaDerivedTypeName = name
                               , metaDerivedTypeFile = f'
                               , metaDerivedTypeLine = line
                               , metaDerivedTypeSize = size
                               , metaDerivedTypeAlign = align
                               , metaDerivedTypeOffset = off
                               , metaDerivedTypeParent = parent'
                               , metaDerivedTypeTag = tag
                               , metaDerivedTypeCompileUnit = cu'
                               , metaDerivedTypeIsArtificial = isArtif
                               , metaDerivedTypeIsVirtual = isVirt
                               , metaDerivedTypeIsForward = isForward
                               , metaDerivedTypeIsProtected = isProt
                               , metaDerivedTypeIsPrivate = isPriv
                               }
    MetaCompositetype -> do
      ctxt <- liftIO $ cMetaTypeContext mp
      name <- cMetaTypeName mp
      f <- liftIO $ cMetaTypeFile mp
      line <- liftIO $ cMetaTypeLine mp
      size <- liftIO $ cMetaTypeSize mp
      align <- liftIO $ cMetaTypeAlign mp
      off <- liftIO $ cMetaTypeOffset mp
      parent <- liftIO $ cMetaTypeDerivedFrom mp
      flags <- liftIO $ cMetaTypeFlags mp
      members <- liftIO $ cMetaTypeCompositeComponents mp
      rlang <- liftIO $ cMetaTypeRuntimeLanguage mp
      ctype <- liftIO $ cMetaTypeContainingType mp
      tparams <- liftIO $ cMetaTypeTemplateParams mp
      cu <- liftIO $ cMetaTypeCompileUnit mp
      isArtif <- liftIO $ cMetaTypeIsArtificial mp
      isVirtual <- liftIO $ cMetaTypeIsVirtual mp
      isForward <- liftIO $ cMetaTypeIsForward mp
      isProt <- liftIO $ cMetaTypeIsProtected mp
      isPriv <- liftIO $ cMetaTypeIsPrivate mp
      isByRef <- liftIO $ cMetaTypeIsByRefStruct mp

      ctxt' <- translateMetadataRec finalState ctxt
      f' <- maybeTranslateMetadataRec finalState f
      parent' <- maybeTranslateMetadataRec finalState parent
      members' <- maybeTranslateMetadataRec finalState members
      ctype' <- maybeTranslateMetadataRec finalState ctype
      tparams' <- maybeTranslateMetadataRec finalState tparams
      cu' <- maybeTranslateMetadataRec finalState cu

      return MetaDWCompositeType { metaCompositeTypeTag = tag
                                 , metaCompositeTypeContext = ctxt'
                                 , metaCompositeTypeName = name
                                 , metaCompositeTypeFile = f'
                                 , metaCompositeTypeLine = line
                                 , metaCompositeTypeSize = size
                                 , metaCompositeTypeAlign = align
                                 , metaCompositeTypeOffset = off
                                 , metaCompositeTypeFlags = flags
                                 , metaCompositeTypeParent = parent'
                                 , metaCompositeTypeMembers = members'
                                 , metaCompositeTypeRuntime = rlang
                                 , metaCompositeTypeContainer = ctype'
                                 , metaCompositeTypeTemplateParams = tparams'
                                 , metaCompositeTypeCompileUnit = cu'
                                 , metaCompositeTypeIsArtificial = isArtif
                                 , metaCompositeTypeIsVirtual = isVirtual
                                 , metaCompositeTypeIsForward = isForward
                                 , metaCompositeTypeIsProtected = isProt
                                 , metaCompositeTypeIsPrivate = isPriv
                                 , metaCompositeTypeIsByRefStruct = isByRef
                                 }
    MetaBasictype -> do
      ctxt <- liftIO $ cMetaTypeContext mp
      name <- cMetaTypeName mp
      f <- liftIO $ cMetaTypeFile mp
      line <- liftIO $ cMetaTypeLine mp
      size <- liftIO $ cMetaTypeSize mp
      align <- liftIO $ cMetaTypeAlign mp
      off <- liftIO $ cMetaTypeOffset mp
      flags <- liftIO $ cMetaTypeFlags mp
      encoding <- liftIO $ cMetaTypeEncoding mp

      ctxt' <- translateMetadataRec finalState ctxt
      f' <- maybeTranslateMetadataRec finalState f

      return MetaDWBaseType { metaBaseTypeContext = ctxt'
                            , metaBaseTypeName = name
                            , metaBaseTypeFile = f'
                            , metaBaseTypeLine = line
                            , metaBaseTypeSize = size
                            , metaBaseTypeAlign = align
                            , metaBaseTypeOffset = off
                            , metaBaseTypeFlags = flags
                            , metaBaseTypeEncoding = encoding
                            }
    MetaVariable -> do
      ctxt <- liftIO $ cMetaVariableContext mp
      name <- cMetaVariableName mp
      file <- liftIO $ cMetaVariableCompileUnit mp
      line <- liftIO $ cMetaVariableLine mp
      argNo <- liftIO $ cMetaVariableArgNumber mp
      ty <- liftIO $ cMetaVariableType mp
      isArtif <- liftIO $ cMetaVariableIsArtificial mp
      cplxAddr <- liftIO $ cMetaVariableAddrElements mp
      byRef <- liftIO $ cMetaVariableIsBlockByRefVar mp

      ctxt' <- translateMetadataRec finalState ctxt
      file' <- translateMetadataRec finalState file
      ty' <- translateMetadataRec finalState ty

      return MetaDWLocal { metaLocalTag = tag
                         , metaLocalContext = ctxt'
                         , metaLocalName = name
                         , metaLocalFile = file'
                         , metaLocalLine = line
                         , metaLocalArgNo = argNo
                         , metaLocalType = ty'
                         , metaLocalIsArtificial = isArtif
                         , metaLocalIsBlockByRefVar = byRef
                         , metaLocalAddrElements = cplxAddr
                         }
    MetaSubprogram -> do
      ctxt <- liftIO $ cMetaSubprogramContext mp
      name <- cMetaSubprogramName mp
      displayName <- cMetaSubprogramDisplayName mp
      linkageName <- cMetaSubprogramLinkageName mp
      compUnit <- liftIO $ cMetaSubprogramCompileUnit mp
      line <- liftIO $ cMetaSubprogramLine mp
      ty <- liftIO $ cMetaSubprogramType mp
      isLocal <- liftIO $ cMetaSubprogramIsLocal mp
      isDef <- liftIO $ cMetaSubprogramIsDefinition mp
      virt <- liftIO $ cMetaSubprogramVirtuality mp
      virtIdx <- liftIO $ cMetaSubprogramVirtualIndex mp
      baseType <- liftIO $ cMetaSubprogramContainingType mp
      isArtif <- liftIO $ cMetaSubprogramIsArtificial mp
      isOpt <- liftIO $ cMetaSubprogramIsOptimized mp
      isPrivate <- liftIO $ cMetaSubprogramIsPrivate mp
      isProtected <- liftIO $ cMetaSubprogramIsProtected mp
      isExplicit <- liftIO $ cMetaSubprogramIsExplicit mp
      isPrototyped <- liftIO $ cMetaSubprogramIsPrototyped mp

      ctxt' <- translateMetadataRec finalState ctxt
      compUnit' <- translateMetadataRec finalState compUnit
      ty' <- translateMetadataRec finalState ty
      baseType' <- maybeTranslateMetadataRec finalState baseType

      return MetaDWSubprogram { metaSubprogramContext = ctxt'
                              , metaSubprogramName = name
                              , metaSubprogramDisplayName = displayName
                              , metaSubprogramLinkageName = linkageName
                              , metaSubprogramFile = compUnit'
                              , metaSubprogramLine = line
                              , metaSubprogramType = ty'
                              , metaSubprogramStatic = isLocal
                              , metaSubprogramNotExtern = not isPrivate && not isProtected
                              , metaSubprogramVirtuality = virt
                              , metaSubprogramVirtIndex = virtIdx
                              , metaSubprogramBaseType = baseType'
                              , metaSubprogramArtificial = isArtif
                              , metaSubprogramOptimized = isOpt
                              , metaSubprogramIsExplicit = isExplicit
                              , metaSubprogramIsPrototyped = isPrototyped
                              }
    MetaGlobalvariable -> do
      ctxt <- liftIO $ cMetaGlobalContext mp
      name <- cMetaGlobalName mp
      displayName <- cMetaGlobalDisplayName mp
      linkageName <- cMetaGlobalLinkageName mp
      file <- liftIO $ cMetaGlobalCompileUnit mp
      line <- liftIO $ cMetaGlobalLine mp
      ty <- liftIO $ cMetaGlobalType mp
      isLocal <- liftIO $ cMetaGlobalIsLocal mp
      def <- liftIO $ cMetaGlobalIsDefinition mp

      ctxt' <- translateMetadataRec finalState ctxt
      file' <- translateMetadataRec finalState file
      ty' <- translateMetadataRec finalState ty

      return MetaDWVariable { metaGlobalVarContext = ctxt'
                            , metaGlobalVarName = name
                            , metaGlobalVarDisplayName = displayName
                            , metaGlobalVarLinkageName = linkageName
                            , metaGlobalVarFile = file'
                            , metaGlobalVarLine = line
                            , metaGlobalVarType = ty'
                            , metaGlobalVarStatic = isLocal
                            , metaGlobalVarNotExtern = not def
                            }
    MetaFile -> do
      file <- cMetaFileFilename mp
      dir <- cMetaFileDirectory mp
      cu <- liftIO $ cMetaFileCompileUnit mp

      cu' <- translateMetadataRec finalState cu

      return MetaDWFile { metaFileSourceFile = file
                        , metaFileSourceDir = dir
                        , metaFileCompileUnit = cu'
                        }
    MetaCompileunit -> do
      lang <- liftIO $ cMetaCompileUnitLanguage mp
      fname <- cMetaCompileUnitFilename mp
      dir <- cMetaCompileUnitDirectory mp
      producer <- cMetaCompileUnitProducer mp
      isMain <- liftIO $ cMetaCompileUnitIsMain mp
      isOpt <- liftIO $ cMetaCompileUnitIsOptimized mp
      flags <- cMetaCompileUnitFlags mp
      rv <- liftIO $ cMetaCompileUnitRuntimeVersion mp

      return MetaDWCompileUnit { metaCompileUnitLanguage = lang
                               , metaCompileUnitSourceFile = fname
                               , metaCompileUnitCompileDir = dir
                               , metaCompileUnitProducer = producer
                               , metaCompileUnitIsMain = isMain
                               , metaCompileUnitIsOpt = isOpt
                               , metaCompileUnitFlags = flags
                               , metaCompileUnitVersion = rv
                               }
    MetaNamespace -> do
      ctxt <- liftIO $ cMetaNamespaceContext mp
      name <- cMetaNamespaceName mp
      cu <- liftIO $ cMetaNamespaceCompileUnit mp
      line <- liftIO $ cMetaNamespaceLine mp

      ctxt' <- translateMetadataRec finalState ctxt
      cu' <- translateMetadataRec finalState cu

      return MetaDWNamespace { metaNamespaceContext = ctxt'
                             , metaNamespaceName = name
                             , metaNamespaceCompileUnit = cu'
                             , metaNamespaceLine = line
                             }
    MetaLexicalblock -> do
      ctxt <- liftIO $ cMetaLexicalBlockContext mp
      line <- liftIO $ cMetaLexicalBlockLine mp
      col <- liftIO $ cMetaLexicalBlockColumn mp

      ctxt' <- translateMetadataRec finalState ctxt

      return MetaDWLexicalBlock { metaLexicalBlockRow = line
                                , metaLexicalBlockCol = col
                                , metaLexicalBlockContext = ctxt'
                                }
    MetaSubrange -> do
      lo <- liftIO $ cMetaSubrangeLo mp
      hi <- liftIO $ cMetaSubrangeHi mp
      return MetaDWSubrange { metaSubrangeLow = lo
                            , metaSubrangeHigh = hi
                            }
    MetaEnumerator -> do
      name <- cMetaEnumeratorName mp
      val <- liftIO $ cMetaEnumeratorValue mp
      return MetaDWEnumerator { metaEnumeratorName = name
                              , metaEnumeratorValue = val
                              }
    MetaArray -> do
      elts <- liftIO $ cMetaArrayElts mp
      elts' <- mapM (translateMetadataRec finalState) elts
      return $ MetadataList elts'
    MetaTemplatetypeparameter -> do
      ctxt <- liftIO $ cMetaTemplateTypeContext mp
      name <- cMetaTemplateTypeName mp
      ty <- liftIO $ cMetaTemplateTypeType mp
      line <- liftIO $ cMetaTemplateTypeLine mp
      col <- liftIO $ cMetaTemplateTypeColumn mp

      ctxt' <- translateMetadataRec finalState ctxt
      ty' <- translateMetadataRec finalState ty

      return MetaDWTemplateTypeParameter { metaTemplateTypeParameterContext = ctxt'
                                         , metaTemplateTypeParameterType = ty'
                                         , metaTemplateTypeParameterLine = line
                                         , metaTemplateTypeParameterCol = col
                                         , metaTemplateTypeParameterName = name
                                         }
    MetaTemplatevalueparameter -> do
      ctxt <- liftIO $ cMetaTemplateValueContext mp
      name <- cMetaTemplateValueName mp
      ty <- liftIO $ cMetaTemplateValueType mp
      val <- liftIO $ cMetaTemplateValueValue mp
      line <- liftIO $ cMetaTemplateValueLine mp
      col <- liftIO $ cMetaTemplateValueColumn mp

      ctxt' <- translateMetadataRec finalState ctxt
      ty' <- translateMetadataRec finalState ty

      return MetaDWTemplateValueParameter { metaTemplateValueParameterContext = ctxt'
                                          , metaTemplateValueParameterType = ty'
                                          , metaTemplateValueParameterLine = line
                                          , metaTemplateValueParameterCol = col
                                          , metaTemplateValueParameterValue = val
                                          , metaTemplateValueParameterName = name
                                          }

  uid <- nextMetaId
  let md = Metadata { metaValueContent = content
                    , metaValueUniqueId = uid
                    }
  st <- get
  put st { metaMap = M.insert ip md (metaMap st) }
  return md
