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
import Data.Maybe ( catMaybes )
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
                          | InvalidTag String ValueTag
                          | InvalidBlockAddressFunction Value
                          | InvalidBlockAddressBlock Value
                          | InvalidUnconditionalBranchTarget Value
                          | NonConstantTag ValueTag
                          | NonInstructionTag ValueTag
                          | InvalidBranchTarget Value
                          | InvalidSwitchTarget Value
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
            , stringCache = M.empty
            }

genId :: (KnotState -> IORef Int) -> KnotMonad Int
genId accessor = do
  s <- get
  let r = accessor s
  thisId <- liftIO $ readIORef r
  liftIO $ modifyIORef r (+1)

  return thisId

nextId :: KnotMonad Int
nextId = genId idSrc

nextTypeId :: KnotMonad Int
nextTypeId = genId typeIdSrc

nextMetaId :: KnotMonad Int
nextMetaId = genId metaIdSrc

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

isExternVar :: ValuePtr -> KnotMonad Bool
isExternVar vp = do
  dataPtr <- liftIO $ cValueData vp
  let dataPtr' = castPtr dataPtr
  liftIO $ cGlobalIsExternal dataPtr'



isExternFunc :: ValuePtr -> KnotMonad Bool
isExternFunc vp = do
  dataPtr <- liftIO $ cValueData vp
  let dataPtr' = castPtr dataPtr
  liftIO $ cFunctionIsExternal dataPtr'

-- swiped from http://www.haskell.org/pipermail/beginners/2009-December/002882.html
partitionM :: Monad m => (a -> m Bool) -> [a] -> m ([a],[a])
partitionM p xs = do
  (f,g) <- pMHelper p xs
  return (f [], g [])

pMHelper :: Monad m => (a -> m Bool) -> [a] -> m ([a] -> [a],[a] -> [a])
pMHelper p xs = foldM help (id,id) xs
  where
    help (f,g) x = do
      b <- p x
      return (if b then (f . (x:),g) else (f,g . (x:)))

tieKnot :: ModulePtr -> KnotState -> KnotMonad KnotState
tieKnot m finalState = do
  modIdent <- liftIO $ cModuleIdentifier m
  dataLayout <- liftIO $ cModuleDataLayout m
  triple <- liftIO $ cModuleTargetTriple m
  inlineAsm <- liftIO $ cModuleInlineAsm m

  vars <- liftIO $ cModuleGlobalVariables m
  aliases <- liftIO $ cModuleGlobalAliases m
  funcs <- liftIO $ cModuleFunctions m

  (externVs, globalVs) <- partitionM isExternVar vars
  (externFs, globalFs) <- partitionM isExternFunc funcs

  globalVars <- mapM (translateGlobalVariable finalState) globalVs
  externVars <- mapM (translateExternalVariable finalState) externVs
  globalAliases <- mapM (translateAlias finalState) aliases
  definedFuncs <- mapM (translateFunction finalState) globalFs
  externFuncs <- mapM (translateExternalFunction finalState) externFs

  s <- get
  lastId <- liftIO $ readIORef (idSrc s)
  let ir = Module { moduleIdentifier = modIdent
                  , moduleDataLayout = dataLayout
                  , moduleTarget = triple
                  , moduleAssembly = Assembly inlineAsm
                  , moduleAliases = globalAliases
                  , moduleGlobalVariables = globalVars
                  , moduleDefinedFunctions = definedFuncs
                  , moduleExternalValues = externVars
                  , moduleExternalFunctions = externFuncs
                  , moduleNextId = lastId + 1
                  }
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

        (True, TYPE_NAMED) ->
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

translateAlias :: KnotState -> ValuePtr -> KnotMonad GlobalAlias
translateAlias finalState vp = do
  Just name <- liftIO $ cValueName vp
  dataPtr <- liftIO $ cValueData vp
  metaPtr <- liftIO $ cValueMetadata vp
  let dataPtr' = castPtr dataPtr

  mds <- mapM (translateMetadata finalState) metaPtr

  vis <- liftIO $ cGlobalVisibility dataPtr'
  link <- liftIO $ cGlobalLinkage dataPtr'
  aliasee <- liftIO $ cGlobalAliasee dataPtr'

  ta <- translateConstOrRef finalState aliasee

  uid <- nextId

  let ga = GlobalAlias { globalAliasLinkage = link
                       , globalAliasVisibility = vis
                       , globalAliasTarget = ta
                       , globalAliasName = name
                       , globalAliasMetadata = mds
                       , globalAliasUniqueId = uid
                       }

  recordValue vp (Value ga)

  return ga

translateExternalVariable :: KnotState -> ValuePtr -> KnotMonad ExternalValue
translateExternalVariable finalState vp = do
  Just name <- liftIO $ cValueName vp
  typePtr <- liftIO $ cValueType vp
  metaPtr <- liftIO $ cValueMetadata vp
  tt <- translateType finalState typePtr

  mds <- mapM (translateMetadata finalState) metaPtr
  uid <- nextId

  let ev = ExternalValue { externalValueType = tt
                         , externalValueName = name
                         , externalValueMetadata = mds
                         , externalValueUniqueId = uid
                         }
  recordValue vp (Value ev)
  return ev


translateGlobalVariable :: KnotState -> ValuePtr -> KnotMonad GlobalVariable
translateGlobalVariable finalState vp = do
  Just name <- liftIO $ cValueName vp
  typePtr <- liftIO $ cValueType vp
  dataPtr <- liftIO $ cValueData vp
  metaPtr <- liftIO $ cValueMetadata vp
  tt <- translateType finalState typePtr

  mds <- mapM (translateMetadata finalState) metaPtr
  uid <- nextId

  let dataPtr' = castPtr dataPtr
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

  let gv = GlobalVariable { globalVariableLinkage = link
                          , globalVariableVisibility = vis
                          , globalVariableInitializer = ti
                          , globalVariableAlignment = align
                          , globalVariableSection = section
                          , globalVariableIsThreadLocal = isThreadLocal
                          , globalVariableIsConstant = isConst
                          , globalVariableMetadata = mds
                          , globalVariableType = tt
                          , globalVariableName = name
                          , globalVariableUniqueId = uid
                          }
  recordValue vp (Value gv)
  return gv

translateExternalFunction :: KnotState -> ValuePtr -> KnotMonad ExternalFunction
translateExternalFunction finalState vp = do
  Just name <- liftIO $ cValueName vp
  typePtr <- liftIO $ cValueType vp
  metaPtr <- liftIO $ cValueMetadata vp
  tt <- translateType finalState typePtr

  mds <- mapM (translateMetadata finalState) metaPtr

  uid <- nextId

  let ef = ExternalFunction { externalFunctionType = tt
                            , externalFunctionName = name
                            , externalFunctionMetadata = mds
                            , externalFunctionUniqueId = uid
                            , externalFunctionAttrs = [] -- FIXME: Need to figure out how to find attrs
                            }
  recordValue vp (Value ef)
  return ef


resetLocalIdCounter :: KnotMonad ()
resetLocalIdCounter = do
  s <- get
  put s { localId = 0 }

translateFunction :: KnotState -> ValuePtr -> KnotMonad Function
translateFunction finalState vp = do
  Just name <- liftIO $ cValueName vp
  typePtr <- liftIO $ cValueType vp
  dataPtr <- liftIO $ cValueData vp
  metaPtr <- liftIO $ cValueMetadata vp
  tt <- translateType finalState typePtr

  mds <- mapM (translateMetadata finalState) metaPtr

  uid <- nextId

  resetLocalIdCounter

  let dataPtr' = castPtr dataPtr
  align <- liftIO $ cFunctionAlignment dataPtr'
  vis <- liftIO $ cFunctionVisibility dataPtr'
  link <- liftIO $ cFunctionLinkage dataPtr'
  section <- liftIO $ cFunctionSection dataPtr'
  cc <- liftIO $ cFunctionCallingConvention dataPtr'
  gcname <- liftIO $ cFunctionGCName dataPtr'
  args <- liftIO $ cFunctionArguments dataPtr'
  blocks <- liftIO $ cFunctionBlocks dataPtr'

  f <- mfix (\finalF -> do
                args' <- mapM (translateArgument finalState finalF) args
                blocks' <- mapM (translateBasicBlock finalState finalF) blocks
                let f' = Function { functionParameters = args'
                                  , functionBody = blocks'
                                  , functionLinkage = link
                                  , functionVisibility = vis
                                  , functionCC = cc
                                  , functionRetAttrs = [] -- FIXME
                                  , functionAttrs = [] -- FIXME
                                  , functionSection = section
                                  , functionAlign = align
                                  , functionGCName = gcname
                                  , functionType = tt
                                  , functionName = name
                                  , functionMetadata = mds
                                  , functionUniqueId = uid
                                  }
                return f')
  recordValue vp (Value f)
  return f

translateConstant :: KnotState -> ValuePtr -> KnotMonad Constant
translateConstant finalState vp = do
  tag <- liftIO $ cValueTag vp
  typePtr <- liftIO $ cValueType vp
  dataPtr <- liftIO $ cValueData vp
  tt <- translateType finalState typePtr

  constant <- case tag of
    ValInlineasm -> translateInlineAsm finalState (castPtr dataPtr) tt
    ValBlockaddress -> translateBlockAddress finalState (castPtr dataPtr) tt
    ValConstantaggregatezero -> do
      uid <- nextId
      return ConstantAggregateZero { constantType = tt
                                   , constantUniqueId = uid
                                   }
    ValConstantpointernull -> do
      uid <- nextId
      return ConstantPointerNull { constantType = tt
                                 , constantUniqueId = uid
                                 }
    ValUndefvalue -> do
      uid <- nextId
      return UndefValue { constantType = tt
                        , constantUniqueId = uid
                        }
    ValConstantarray -> translateConstantAggregate finalState ConstantArray (castPtr dataPtr) tt
    ValConstantstruct -> translateConstantAggregate finalState ConstantStruct (castPtr dataPtr) tt
    ValConstantvector -> translateConstantAggregate finalState ConstantVector (castPtr dataPtr) tt
    ValConstantfp -> translateConstantFP finalState (castPtr dataPtr) tt
    ValConstantint -> translateConstantInt finalState (castPtr dataPtr) tt
    ValConstantexpr -> do
      uid <- nextId
      i <- translateConstantExpr finalState (castPtr dataPtr) tt
      return ConstantValue { constantType = tt
                           , constantUniqueId = uid
                           , constantInstruction = i
                           }
    _ -> throw $ NonConstantTag tag

  recordValue vp (Value constant)

  return constant


-- | Most instructions don't have explicit names in LLVM - when they
-- are printed the LLVM libraries just generate numeric names and they
-- are never stored.  This function takes the stated name of an
-- instruction and, if it should have a temporary name like that, we
-- generate one using a local counter.
computeRealName :: Maybe Identifier -> KnotMonad (Maybe Identifier)
computeRealName name = do
  s <- get
  let idCtr = localId s
  case name of
    Just n -> return (Just n)
    Nothing -> do
      put s { localId = idCtr + 1 }
      return $ Just $ makeLocalIdentifier $ BS.pack (show idCtr)

translateInstruction :: KnotState -> Maybe BasicBlock -> ValuePtr -> KnotMonad Instruction
translateInstruction finalState bb vp = do
  tag <- liftIO $ cValueTag vp
  name <- liftIO $ cValueName vp
  typePtr <- liftIO $ cValueType vp
  dataPtr <- liftIO $ cValueData vp
  metaPtr <- liftIO $ cValueMetadata vp

  mds <- mapM (translateMetadata finalState) metaPtr

  realName <- computeRealName name
  tt <- translateType finalState typePtr

  inst <- case tag of
    ValRetinst -> translateRetInst finalState (castPtr dataPtr) tt mds bb
    ValBranchinst -> translateBranchInst finalState (castPtr dataPtr) tt mds bb
    ValSwitchinst -> translateSwitchInst finalState (castPtr dataPtr) tt mds bb
    ValIndirectbrinst -> translateIndirectBrInst finalState (castPtr dataPtr) tt mds bb
    ValUnwindinst -> do
      uid <- nextId
      return UnwindInst { instructionName = Nothing
                        , instructionType = tt
                        , instructionMetadata = mds
                        , instructionUniqueId = uid
                        , instructionBasicBlock = bb
                        }
    ValUnreachableinst -> do
      uid <- nextId
      return UnreachableInst { instructionName = Nothing
                             , instructionType = tt
                             , instructionMetadata = mds
                             , instructionUniqueId = uid
                             , instructionBasicBlock = bb
                             }
    ValInvokeinst -> translateInvokeInst finalState (castPtr dataPtr) realName tt mds bb
    ValAddinst -> translateFlaggedBinaryOp finalState AddInst (castPtr dataPtr) realName tt mds bb
    ValFaddinst -> translateFlaggedBinaryOp finalState AddInst (castPtr dataPtr) realName tt mds bb
    ValSubinst -> translateFlaggedBinaryOp finalState SubInst (castPtr dataPtr) realName tt mds bb
    ValFsubinst -> translateFlaggedBinaryOp finalState SubInst (castPtr dataPtr) realName tt mds bb
    ValMulinst ->  translateFlaggedBinaryOp finalState MulInst (castPtr dataPtr) realName tt mds bb
    ValFmulinst ->  translateFlaggedBinaryOp finalState MulInst (castPtr dataPtr) realName tt mds bb
    ValUdivinst -> translateBinaryOp finalState DivInst (castPtr dataPtr) realName tt mds bb
    ValSdivinst -> translateBinaryOp finalState DivInst (castPtr dataPtr) realName tt mds bb
    ValFdivinst -> translateBinaryOp finalState DivInst (castPtr dataPtr) realName tt mds bb
    ValUreminst -> translateBinaryOp finalState RemInst (castPtr dataPtr) realName tt mds bb
    ValSreminst -> translateBinaryOp finalState RemInst (castPtr dataPtr) realName tt mds bb
    ValFreminst -> translateBinaryOp finalState RemInst (castPtr dataPtr) realName tt mds bb
    ValShlinst -> translateBinaryOp finalState ShlInst (castPtr dataPtr) realName tt mds bb
    ValLshrinst -> translateBinaryOp finalState LshrInst (castPtr dataPtr) realName tt mds bb
    ValAshrinst -> translateBinaryOp finalState AshrInst (castPtr dataPtr) realName tt mds bb
    ValAndinst -> translateBinaryOp finalState AndInst (castPtr dataPtr) realName tt mds bb
    ValOrinst -> translateBinaryOp finalState OrInst (castPtr dataPtr) realName tt mds bb
    ValXorinst -> translateBinaryOp finalState XorInst (castPtr dataPtr) realName tt mds bb
    ValAllocainst -> translateAllocaInst finalState (castPtr dataPtr) realName tt mds bb
    ValLoadinst -> translateLoadInst finalState (castPtr dataPtr) realName tt mds bb
    ValStoreinst -> translateStoreInst finalState (castPtr dataPtr) tt mds bb
    ValGetelementptrinst -> translateGEPInst finalState (castPtr dataPtr) realName tt mds bb
    ValTruncinst -> translateCastInst finalState TruncInst (castPtr dataPtr) realName tt mds bb
    ValZextinst -> translateCastInst finalState ZExtInst (castPtr dataPtr) realName tt mds bb
    ValSextinst -> translateCastInst finalState SExtInst (castPtr dataPtr) realName tt mds bb
    ValFptruncinst -> translateCastInst finalState FPTruncInst (castPtr dataPtr) realName tt mds bb
    ValFpextinst -> translateCastInst finalState FPExtInst (castPtr dataPtr) realName tt mds bb
    ValFptouiinst -> translateCastInst finalState FPToUIInst (castPtr dataPtr) realName tt mds bb
    ValFptosiinst -> translateCastInst finalState FPToSIInst (castPtr dataPtr) realName tt mds bb
    ValUitofpinst -> translateCastInst finalState UIToFPInst (castPtr dataPtr) realName tt mds bb
    ValSitofpinst -> translateCastInst finalState SIToFPInst (castPtr dataPtr) realName tt mds bb
    ValPtrtointinst -> translateCastInst finalState PtrToIntInst (castPtr dataPtr) realName tt mds bb
    ValInttoptrinst -> translateCastInst finalState IntToPtrInst (castPtr dataPtr) realName tt mds bb
    ValBitcastinst -> translateCastInst finalState BitcastInst (castPtr dataPtr) realName tt mds bb
    ValIcmpinst -> translateCmpInst finalState ICmpInst (castPtr dataPtr) realName tt mds bb
    ValFcmpinst -> translateCmpInst finalState FCmpInst (castPtr dataPtr) realName tt mds bb
    ValPhinode -> translatePhiNode finalState (castPtr dataPtr) realName tt mds bb
    ValCallinst -> translateCallInst finalState (castPtr dataPtr) realName tt mds bb
    ValSelectinst -> translateSelectInst finalState (castPtr dataPtr) realName tt mds bb
    ValVaarginst -> translateVarArgInst finalState (castPtr dataPtr) realName tt mds bb
    ValExtractelementinst -> translateExtractElementInst finalState (castPtr dataPtr) realName tt mds bb
    ValInsertelementinst -> translateInsertElementInst finalState (castPtr dataPtr) realName tt mds bb
    ValShufflevectorinst -> translateShuffleVectorInst finalState (castPtr dataPtr) realName tt mds bb
    ValExtractvalueinst -> translateExtractValueInst finalState (castPtr dataPtr) realName tt mds bb
    ValInsertvalueinst -> translateInsertValueInst finalState (castPtr dataPtr) realName tt mds bb
    _ -> throw $ NonInstructionTag tag

  recordValue vp (Value inst)

  return inst

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
  s <- get
  case M.lookup (ptrToIntPtr vp) (valueMap s) of
    Just v -> return v
    Nothing -> do
      tag <- liftIO $ cValueTag vp
      let ip = ptrToIntPtr vp
      case isConstant tag of
        True -> Value <$> translateConstant finalState vp
        False ->
          return $ M.findWithDefault (throw (KnotTyingFailure tag)) ip (valueMap finalState)

translateArgument :: KnotState -> Function -> ValuePtr -> KnotMonad Argument
translateArgument finalState finalF vp = do
  tag <- liftIO $ cValueTag vp
  Just name <- liftIO $ cValueName vp
  typePtr <- liftIO $ cValueType vp
  dataPtr <- liftIO $ cValueData vp
  metaPtr <- liftIO $ cValueMetadata vp

  mds <- mapM (translateMetadata finalState) metaPtr
  uid <- nextId

  when (tag /= ValArgument) (throw $ InvalidTag "Argument" tag)

  tt <- translateType finalState typePtr

  let dataPtr' = castPtr dataPtr

  hasSRet <- liftIO $ cArgInfoHasSRet dataPtr'
  hasByVal <- liftIO $ cArgInfoHasByVal dataPtr'
  hasNest <- liftIO $ cArgInfoHasNest dataPtr'
  hasNoAlias <- liftIO $ cArgInfoHasNoAlias dataPtr'
  hasNoCapture <- liftIO $ cArgInfoHasNoCapture dataPtr'
  let attrOrNothing b att = if b then Just att else Nothing
      atts = [ attrOrNothing hasSRet PASRet
             , attrOrNothing hasByVal PAByVal
             , attrOrNothing hasNest PANest
             , attrOrNothing hasNoAlias PANoAlias
             , attrOrNothing hasNoCapture PANoCapture
             ]
  let a = Argument { argumentType = tt
                   , argumentName = name
                   , argumentMetadata = mds
                   , argumentUniqueId = uid
                   , argumentParamAttrs = catMaybes atts
                   , argumentFunction = finalF
                   }
  recordValue vp (Value a)
  return a


translateBasicBlock :: KnotState -> Function -> ValuePtr -> KnotMonad BasicBlock
translateBasicBlock finalState f vp = do
  tag <- liftIO $ cValueTag vp
  name <- liftIO $ cValueName vp
  typePtr <- liftIO $ cValueType vp
  dataPtr <- liftIO $ cValueData vp
  metaPtr <- liftIO $ cValueMetadata vp

  mds <- mapM (translateMetadata finalState) metaPtr

  when (tag /= ValBasicblock) (throw $ InvalidTag "BasicBlock" tag)


  uid <- nextId
  tt <- translateType finalState typePtr

  let dataPtr' = castPtr dataPtr
  Just realName <- computeRealName name

  insts <- liftIO $ cBasicBlockInstructions dataPtr'
  -- Use mfix here to let instructions have a reference to their
  -- enclosing basic block.  mfix is needed since the block doesn't
  -- exist until after the instructions are translated
  bb <- mfix (\finalBB -> do
                 tinsts <- mapM (translateInstruction finalState (Just finalBB)) insts
                 let block' = BasicBlock { basicBlockType = tt
                                        , basicBlockName = realName
                                        , basicBlockMetadata = mds
                                        , basicBlockUniqueId = uid
                                        , basicBlockInstructions = tinsts
                                        , basicBlockFunction = f
                                        }
                 return block')

  recordValue vp (Value bb)
  return bb

translateInlineAsm :: KnotState -> InlineAsmInfoPtr -> Type -> KnotMonad Constant
translateInlineAsm _ dataPtr tt = do
  uid <- nextId
  asmString <- liftIO $ cInlineAsmString dataPtr
  constraints <- liftIO $ cInlineAsmConstraints dataPtr
  return InlineAsm { constantType = tt
                   , constantUniqueId = uid
                   , inlineAsmString = asmString
                   , inlineAsmConstraints = constraints
                   }

translateBlockAddress :: KnotState -> BlockAddrInfoPtr -> Type -> KnotMonad Constant
translateBlockAddress finalState dataPtr tt = do
  uid <- nextId
  fval <- liftIO $ cBlockAddrFunc dataPtr
  bval <- liftIO $ cBlockAddrBlock dataPtr
  f' <- translateConstOrRef finalState fval
  b' <- translateConstOrRef finalState bval
  let f'' = case valueContent f' of
        FunctionC f -> f
        _ -> throw (InvalidBlockAddressFunction f')
      b'' = case valueContent b' of
        BasicBlockC b -> b
        _ -> throw (InvalidBlockAddressBlock b')
  return BlockAddress { constantType = tt
                      , constantUniqueId = uid
                      , blockAddressFunction = f''
                      , blockAddressBlock = b''
                      }

translateConstantAggregate :: KnotState -> (Type -> UniqueId -> [Value] -> Constant)
                              -> AggregateInfoPtr -> Type -> KnotMonad Constant
translateConstantAggregate finalState constructor dataPtr tt = do
  uid <- nextId
  vals <- liftIO $ cAggregateValues dataPtr
  vals' <- mapM (translateConstOrRef finalState) vals
  return $ constructor tt uid vals'

translateConstantFP :: KnotState -> FPInfoPtr -> Type -> KnotMonad Constant
translateConstantFP _ dataPtr tt = do
  uid <- nextId
  fpval <- liftIO $ cFPVal dataPtr
  return ConstantFP { constantType = tt
                    , constantUniqueId = uid
                    , constantFPValue = fpval
                    }

translateConstantInt :: KnotState -> IntInfoPtr -> Type -> KnotMonad Constant
translateConstantInt _ dataPtr tt = do
  uid <- nextId
  intval <- liftIO $ cIntVal dataPtr
  return $ ConstantInt { constantType = tt
                       , constantUniqueId = uid
                       , constantIntValue = intval
                       }

translateRetInst :: KnotState -> InstInfoPtr -> Type -> [Metadata] -> Maybe BasicBlock -> KnotMonad Instruction
translateRetInst finalState dataPtr tt mds bb = do
  uid <- nextId
  opPtrs <- liftIO $ cInstructionOperands dataPtr
  rv <- case opPtrs of
    [] -> return Nothing
    [val] -> do
      val' <- translateConstOrRef finalState val
      return (Just val')
    _ -> throw TooManyReturnValues
  return RetInst { instructionType = tt
                 , instructionName = Nothing
                 , instructionMetadata = mds
                 , instructionUniqueId = uid
                 , instructionBasicBlock = bb
                 , retInstValue = rv
                 }

-- | Note, in LLVM the operands of the Branch instruction are ordered as
--
-- [Condition, FalseTarget,] TrueTarget
--
-- This is not exactly as expected.
translateBranchInst :: KnotState -> InstInfoPtr -> Type -> [Metadata] -> Maybe BasicBlock -> KnotMonad Instruction
translateBranchInst finalState dataPtr tt mds bb = do
  uid <- nextId
  opPtrs <- liftIO $ cInstructionOperands dataPtr
  case opPtrs of
    [dst] -> do
      dst' <- translateConstOrRef finalState dst
      let dst'' = case valueContent dst' of
            BasicBlockC b -> b
            _ -> throw (InvalidUnconditionalBranchTarget dst')
      return UnconditionalBranchInst { instructionType = tt
                                     , instructionName = Nothing
                                     , instructionMetadata = mds
                                     , instructionUniqueId = uid
                                     , instructionBasicBlock = bb
                                     , unconditionalBranchTarget = dst''
                                     }
    [val, f, t] -> do
      val' <- translateConstOrRef finalState val
      fbranch <- translateConstOrRef finalState f
      tbranch <- translateConstOrRef finalState t
      let tbr' = case valueContent tbranch of
            BasicBlockC b -> b
            _ -> throw (InvalidBranchTarget tbranch)
          fbr' = case valueContent fbranch of
            BasicBlockC b -> b
            _ -> throw (InvalidBranchTarget fbranch)
      return BranchInst { instructionType = tt
                        , instructionName = Nothing
                        , instructionMetadata = mds
                        , instructionUniqueId = uid
                        , instructionBasicBlock = bb
                        , branchCondition = val'
                        , branchTrueTarget = tbr'
                        , branchFalseTarget = fbr'
                        }
    _ -> throw InvalidBranchInst

translateSwitchInst :: KnotState -> InstInfoPtr -> Type -> [Metadata] -> Maybe BasicBlock -> KnotMonad Instruction
translateSwitchInst finalState dataPtr tt mds bb = do
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
            let dest'' = case valueContent dest' of
                  BasicBlockC b -> b
                  _ -> throw (InvalidSwitchTarget dest')
            tpairs ((v1', dest''):acc) rest
          tpairs acc [] = return $ reverse acc
          tpairs _ _ = throw InvalidSwitchLayout
          def'' = case valueContent def' of
            BasicBlockC b -> b
            _ -> throw (InvalidSwitchTarget def')
      cases' <- tpairs [] cases
      uid <- nextId
      return SwitchInst { instructionType = tt
                        , instructionName = Nothing
                        , instructionMetadata = mds
                        , instructionUniqueId = uid
                        , instructionBasicBlock = bb
                        , switchValue = val'
                        , switchDefaultTarget = def''
                        , switchCases = cases'
                        }
    _ -> throw InvalidSwitchLayout

translateIndirectBrInst :: KnotState -> InstInfoPtr -> Type -> [Metadata] -> Maybe BasicBlock -> KnotMonad Instruction
translateIndirectBrInst finalState dataPtr tt mds bb = do
  opPtrs <- liftIO $ cInstructionOperands dataPtr
  uid <- nextId
  case opPtrs of
    (addr:targets) -> do
      addr' <- translateConstOrRef finalState addr
      targets' <- mapM (translateConstOrRef finalState) targets
      return IndirectBranchInst { instructionType = tt
                                , instructionName = Nothing
                                , instructionMetadata = mds
                                , instructionUniqueId = uid
                                , instructionBasicBlock = bb
                                , indirectBranchAddress = addr'
                                , indirectBranchTargets = map toBasicBlock targets'
                                }
    _ -> throw InvalidIndirectBranchOperands
  where
    toBasicBlock b = case valueContent b of
      BasicBlockC b' -> b'
      _ -> throw (InvalidBranchTarget b)

translateInvokeInst :: KnotState -> CallInfoPtr -> Maybe Identifier -> Type
                       -> [Metadata] -> Maybe BasicBlock -> KnotMonad Instruction
translateInvokeInst finalState dataPtr name tt mds bb = do
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

  uid <- nextId

  let n'' = case valueContent n' of
        BasicBlockC b -> b
      u'' = case valueContent u' of
        BasicBlockC b -> b

  return InvokeInst { instructionName = name
                    , instructionType = tt
                    , instructionMetadata = mds
                    , instructionUniqueId = uid
                    , instructionBasicBlock = bb
                    , invokeConvention = cc
                    , invokeParamAttrs = [] -- FIXME
                    , invokeFunction = f'
                    , invokeArguments = zip args' (repeat []) -- FIXME
                    , invokeAttrs = [] -- FIXME
                    , invokeNormalLabel = n''
                    , invokeUnwindLabel = u''
                    , invokeHasSRet = hasSRet
                    }

translateFlaggedBinaryOp :: KnotState
                            -> (Type -> Maybe Identifier -> [Metadata] -> UniqueId -> Maybe BasicBlock -> ArithFlags -> Value -> Value -> Instruction)
                            -> InstInfoPtr -> Maybe Identifier -> Type
                            -> [Metadata] -> Maybe BasicBlock -> KnotMonad Instruction
translateFlaggedBinaryOp finalState constructor dataPtr name tt mds bb = do
  opPtrs <- liftIO $ cInstructionOperands dataPtr
  flags <- liftIO $ cInstructionArithFlags dataPtr

  ops <- mapM (translateConstOrRef finalState) opPtrs
  uid <- nextId

  case ops of
    [lhs, rhs] -> return $ constructor tt name mds uid bb flags lhs rhs
    _ -> throw $ InvalidBinaryOp (length ops)

translateBinaryOp :: KnotState
                     -> (Type -> Maybe Identifier -> [Metadata] -> UniqueId -> Maybe BasicBlock -> Value -> Value -> Instruction)
                     -> InstInfoPtr -> Maybe Identifier -> Type
                     -> [Metadata] -> Maybe BasicBlock -> KnotMonad Instruction
translateBinaryOp finalState constructor dataPtr name tt mds bb = do
  opPtrs <- liftIO $ cInstructionOperands dataPtr
  ops <- mapM (translateConstOrRef finalState) opPtrs
  uid <- nextId

  case ops of
    [lhs, rhs] -> return $ constructor tt name mds uid bb lhs rhs
    _ -> throw $ InvalidBinaryOp (length ops)

translateAllocaInst :: KnotState -> InstInfoPtr -> Maybe Identifier
                       -> Type -> [Metadata] -> Maybe BasicBlock -> KnotMonad Instruction
translateAllocaInst finalState dataPtr name tt mds bb = do
  opPtrs <- liftIO $ cInstructionOperands dataPtr
  align <- liftIO $ cInstructionAlign dataPtr
  ops <- mapM (translateConstOrRef finalState) opPtrs
  uid <- nextId

  case ops of
    [val] -> return AllocaInst { instructionType = tt
                               , instructionName = name
                               , instructionMetadata = mds
                               , instructionUniqueId = uid
                               , instructionBasicBlock = bb
                               , allocaNumElements = val
                               , allocaAlign = align
                               }
    _ -> throw $ InvalidUnaryOp (length ops)


translateLoadInst :: KnotState -> InstInfoPtr -> Maybe Identifier
                     -> Type -> [Metadata] -> Maybe BasicBlock -> KnotMonad Instruction
translateLoadInst finalState dataPtr name tt mds bb = do
  opPtrs <- liftIO $ cInstructionOperands dataPtr
  align <- liftIO $ cInstructionAlign dataPtr
  vol <- liftIO $ cInstructionIsVolatile dataPtr
  uid <- nextId

  ops <- mapM (translateConstOrRef finalState) opPtrs

  case ops of
    [addr] -> return LoadInst { instructionType = tt
                              , instructionName = name
                              , instructionMetadata = mds
                              , instructionUniqueId = uid
                              , instructionBasicBlock = bb
                              , loadIsVolatile = vol
                              , loadAddress = addr
                              , loadAlignment = align
                              }
    _ -> throw $ InvalidUnaryOp (length ops)

translateStoreInst :: KnotState -> InstInfoPtr -> Type -> [Metadata]
                      -> Maybe BasicBlock -> KnotMonad Instruction
translateStoreInst finalState dataPtr tt mds bb = do
  opPtrs <- liftIO $ cInstructionOperands dataPtr
  addrSpace <- liftIO $ cInstructionAddrSpace dataPtr
  align <- liftIO $ cInstructionAlign dataPtr
  isVol <- liftIO $ cInstructionIsVolatile dataPtr

  ops <- mapM (translateConstOrRef finalState) opPtrs
  uid <- nextId

  case ops of
    [val, ptr] -> return StoreInst { instructionType = tt
                                   , instructionName = Nothing
                                   , instructionMetadata = mds
                                   , instructionUniqueId = uid
                                   , instructionBasicBlock = bb
                                   , storeIsVolatile = isVol
                                   , storeValue = val
                                   , storeAddress = ptr
                                   , storeAlignment = align
                                   , storeAddressSpace = addrSpace
                                   }
    _ -> throw $ InvalidBinaryOp (length ops)

translateGEPInst :: KnotState -> InstInfoPtr -> Maybe Identifier
                    -> Type -> [Metadata] -> Maybe BasicBlock -> KnotMonad Instruction
translateGEPInst finalState dataPtr name tt mds bb = do
  opPtrs <- liftIO $ cInstructionOperands dataPtr
  inBounds <- liftIO $ cInstructionInBounds dataPtr
  addrSpace <- liftIO $ cInstructionAddrSpace dataPtr
  uid <- nextId

  ops <- mapM (translateConstOrRef finalState) opPtrs

  case ops of
    (val:indices) -> return GetElementPtrInst { instructionName = name
                                              , instructionType = tt
                                              , instructionMetadata = mds
                                              , instructionUniqueId = uid
                                              , instructionBasicBlock = bb
                                              , getElementPtrInBounds = inBounds
                                              , getElementPtrValue = val
                                              , getElementPtrIndices = indices
                                              , getElementPtrAddrSpace = addrSpace
                                              }
    _ -> throw $ InvalidGEPInst (length ops)

translateCastInst :: KnotState
                     -> (Type -> Maybe Identifier -> [Metadata] -> UniqueId -> Maybe BasicBlock -> Value -> Instruction)
                     -> InstInfoPtr -> Maybe Identifier -> Type
                     -> [Metadata] -> Maybe BasicBlock -> KnotMonad Instruction
translateCastInst finalState constructor dataPtr name tt mds bb = do
  uid <- nextId
  opPtrs <- liftIO $ cInstructionOperands dataPtr
  ops <- mapM (translateConstOrRef finalState) opPtrs

  case ops of
    [v] -> return $ constructor tt name mds uid bb v
    _ -> throw $ InvalidUnaryOp (length ops)

translateCmpInst :: KnotState
                    -> (Type -> Maybe Identifier -> [Metadata] -> UniqueId -> Maybe BasicBlock -> CmpPredicate -> Value -> Value -> Instruction)
                    -> InstInfoPtr -> Maybe Identifier -> Type -> [Metadata]
                    -> Maybe BasicBlock -> KnotMonad Instruction
translateCmpInst finalState constructor dataPtr name tt mds bb = do
  opPtrs <- liftIO $ cInstructionOperands dataPtr
  predicate <- liftIO $ cInstructionCmpPred dataPtr
  uid <- nextId

  ops <- mapM (translateConstOrRef finalState) opPtrs

  case ops of
    [op1, op2] -> return $ constructor tt name mds uid bb predicate op1 op2
    _ -> throw $ InvalidBinaryOp (length ops)

translatePhiNode :: KnotState -> PHIInfoPtr -> Maybe Identifier
                    -> Type -> [Metadata] -> Maybe BasicBlock -> KnotMonad Instruction
translatePhiNode finalState dataPtr name tt mds bb = do
  vptrs <- liftIO $ cPHIValues dataPtr
  bptrs <- liftIO $ cPHIBlocks dataPtr
  uid <- nextId

  vals <- mapM (translateConstOrRef finalState) vptrs
  blocks <- mapM (translateConstOrRef finalState) bptrs

  return PhiNode { instructionType = tt
                 , instructionName = name
                 , instructionMetadata = mds
                 , instructionUniqueId = uid
                 , instructionBasicBlock = bb
                 , phiIncomingValues = zip vals blocks
                 }

translateCallInst :: KnotState -> CallInfoPtr -> Maybe Identifier
                     -> Type -> [Metadata] -> Maybe BasicBlock -> KnotMonad Instruction
translateCallInst finalState dataPtr name tt mds bb = do
  vptr <- liftIO $ cCallValue dataPtr
  aptrs <- liftIO $ cCallArguments dataPtr
  cc <- liftIO $ cCallConvention dataPtr
  hasSRet <- liftIO $ cCallHasSRet dataPtr
  isTail <- liftIO $ cCallIsTail dataPtr
  uid <- nextId

  val <- translateConstOrRef finalState vptr
  args <- mapM (translateConstOrRef finalState) aptrs

  return CallInst { instructionType = tt
                  , instructionName = name
                  , instructionMetadata = mds
                  , instructionUniqueId = uid
                  , instructionBasicBlock = bb
                  , callIsTail = isTail
                  , callConvention = cc
                  , callParamAttrs = [] -- FIXME
                  , callFunction = val
                  , callArguments = zip args (repeat []) -- FIXME
                  , callAttrs = [] -- FIXME
                  , callHasSRet = hasSRet
                  }

translateSelectInst :: KnotState -> InstInfoPtr -> Maybe Identifier
                       -> Type -> [Metadata] -> Maybe BasicBlock -> KnotMonad Instruction
translateSelectInst finalState dataPtr name tt mds bb = do
  opPtrs <- liftIO $ cInstructionOperands dataPtr
  ops <- mapM (translateConstOrRef finalState) opPtrs
  uid <- nextId
  case ops of
    [cond, trueval, falseval] ->
      return SelectInst { instructionType = tt
                        , instructionName = name
                        , instructionMetadata = mds
                        , instructionUniqueId = uid
                        , instructionBasicBlock = bb
                        , selectCondition = cond
                        , selectTrueValue = trueval
                        , selectFalseValue = falseval
                        }
    _ -> throw $ InvalidSelectArgs (length ops)

translateVarArgInst :: KnotState -> InstInfoPtr -> Maybe Identifier
                       -> Type -> [Metadata] -> Maybe BasicBlock -> KnotMonad Instruction
translateVarArgInst finalState dataPtr name tt mds bb = do
  opPtrs <- liftIO $ cInstructionOperands dataPtr
  ops <- mapM (translateConstOrRef finalState) opPtrs
  uid <- nextId
  case ops of
    [op] -> return VaArgInst { instructionType = tt
                             , instructionName = name
                             , instructionMetadata = mds
                             , instructionUniqueId = uid
                             , instructionBasicBlock = bb
                             , vaArgValue = op
                             }
    _ -> throw $ InvalidUnaryOp (length ops)

translateExtractElementInst :: KnotState -> InstInfoPtr -> Maybe Identifier
                               -> Type -> [Metadata] -> Maybe BasicBlock -> KnotMonad Instruction
translateExtractElementInst finalState dataPtr name tt mds bb = do
  opPtrs <- liftIO $ cInstructionOperands dataPtr
  ops <- mapM (translateConstOrRef finalState) opPtrs
  uid <- nextId
  case ops of
    [vec, idx] ->
      return ExtractElementInst { instructionType = tt
                                , instructionName = name
                                , instructionMetadata = mds
                                , instructionUniqueId = uid
                                , instructionBasicBlock = bb
                                , extractElementVector = vec
                                , extractElementIndex = idx
                                }
    _ -> throw $ InvalidExtractElementInst (length ops)

translateInsertElementInst :: KnotState -> InstInfoPtr -> Maybe Identifier
                              -> Type -> [Metadata] -> Maybe BasicBlock -> KnotMonad Instruction
translateInsertElementInst finalState dataPtr name tt mds bb = do
  opPtrs <- liftIO $ cInstructionOperands dataPtr
  ops <- mapM (translateConstOrRef finalState) opPtrs
  uid <- nextId
  case ops of
    [vec, val, idx] ->
      return InsertElementInst { instructionType = tt
                               , instructionName = name
                               , instructionMetadata = mds
                               , instructionUniqueId = uid
                               , instructionBasicBlock = bb
                               , insertElementVector = vec
                               , insertElementValue = val
                               , insertElementIndex = idx
                               }
    _ -> throw $ InvalidInsertElementInst (length ops)

translateShuffleVectorInst :: KnotState -> InstInfoPtr -> Maybe Identifier
                              -> Type -> [Metadata] -> Maybe BasicBlock -> KnotMonad Instruction
translateShuffleVectorInst finalState dataPtr name tt mds bb = do
  opPtrs <- liftIO $ cInstructionOperands dataPtr
  ops <- mapM (translateConstOrRef finalState) opPtrs
  uid <- nextId
  case ops of
    [v1, v2, vecMask] ->
      return ShuffleVectorInst { instructionType = tt
                               , instructionName = name
                               , instructionMetadata = mds
                               , instructionUniqueId = uid
                               , instructionBasicBlock = bb
                               , shuffleVectorV1 = v1
                               , shuffleVectorV2 = v2
                               , shuffleVectorMask = vecMask
                               }
    _ -> throw $ InvalidShuffleVectorInst (length ops)

translateExtractValueInst :: KnotState -> InstInfoPtr -> Maybe Identifier
                             -> Type -> [Metadata] -> Maybe BasicBlock -> KnotMonad Instruction
translateExtractValueInst finalState dataPtr name tt mds bb = do
  opPtrs <- liftIO $ cInstructionOperands dataPtr
  indices <- liftIO $ cInstructionIndices dataPtr
  uid <- nextId
  ops <- mapM (translateConstOrRef finalState) opPtrs
  case ops of
    [agg] -> return ExtractValueInst { instructionType = tt
                                     , instructionName = name
                                     , instructionMetadata = mds
                                     , instructionUniqueId = uid
                                     , instructionBasicBlock = bb
                                     , extractValueAggregate = agg
                                     , extractValueIndices = indices
                                     }
    _ -> throw $ InvalidExtractValueInst (length ops)

translateInsertValueInst :: KnotState -> InstInfoPtr -> Maybe Identifier
                            -> Type -> [Metadata] -> Maybe BasicBlock -> KnotMonad Instruction
translateInsertValueInst finalState dataPtr name tt mds bb = do
  opPtrs <- liftIO $ cInstructionOperands dataPtr
  indices <- liftIO $ cInstructionIndices dataPtr
  uid <- nextId
  ops <- mapM (translateConstOrRef finalState) opPtrs
  case ops of
    [agg, val] ->
      return InsertValueInst { instructionType = tt
                             , instructionName = name
                             , instructionMetadata = mds
                             , instructionUniqueId = uid
                             , instructionBasicBlock = bb
                             , insertValueAggregate = agg
                             , insertValueValue = val
                             , insertValueIndices = indices
                             }
    _ -> throw $ InvalidInsertValueInst (length ops)

translateConstantExpr :: KnotState -> ConstExprPtr -> Type -> KnotMonad Instruction
translateConstantExpr finalState dataPtr tt = do
  let mds = []
      bb = Nothing
  ii <- liftIO $ cConstExprInstInfo dataPtr
  tag <- liftIO $ cConstExprTag dataPtr
  case tag of
    ValAddinst -> translateFlaggedBinaryOp finalState AddInst ii Nothing tt mds bb
    ValFaddinst -> translateFlaggedBinaryOp finalState AddInst ii Nothing tt mds bb
    ValSubinst -> translateFlaggedBinaryOp finalState SubInst ii Nothing tt mds bb
    ValFsubinst -> translateFlaggedBinaryOp finalState SubInst ii Nothing tt mds bb
    ValMulinst ->  translateFlaggedBinaryOp finalState MulInst ii Nothing tt mds bb
    ValFmulinst ->  translateFlaggedBinaryOp finalState MulInst ii Nothing tt mds bb
    ValUdivinst -> translateBinaryOp finalState DivInst ii Nothing tt mds bb
    ValSdivinst -> translateBinaryOp finalState DivInst ii Nothing tt mds bb
    ValFdivinst -> translateBinaryOp finalState DivInst ii Nothing tt mds bb
    ValUreminst -> translateBinaryOp finalState RemInst ii Nothing tt mds bb
    ValSreminst -> translateBinaryOp finalState RemInst ii Nothing tt mds bb
    ValFreminst -> translateBinaryOp finalState RemInst ii Nothing tt mds bb
    ValShlinst -> translateBinaryOp finalState ShlInst ii Nothing tt mds bb
    ValLshrinst -> translateBinaryOp finalState LshrInst ii Nothing tt mds bb
    ValAshrinst -> translateBinaryOp finalState AshrInst ii Nothing tt mds bb
    ValAndinst -> translateBinaryOp finalState AndInst ii Nothing tt mds bb
    ValOrinst -> translateBinaryOp finalState OrInst ii Nothing tt mds bb
    ValXorinst -> translateBinaryOp finalState XorInst ii Nothing tt mds bb
    ValGetelementptrinst -> translateGEPInst finalState ii Nothing tt mds bb
    ValTruncinst -> translateCastInst finalState TruncInst ii Nothing tt mds bb
    ValZextinst -> translateCastInst finalState ZExtInst ii Nothing tt mds bb
    ValSextinst -> translateCastInst finalState SExtInst ii Nothing tt mds bb
    ValFptruncinst -> translateCastInst finalState FPTruncInst ii Nothing tt mds bb
    ValFpextinst -> translateCastInst finalState FPExtInst ii Nothing tt mds bb
    ValFptouiinst -> translateCastInst finalState FPToUIInst ii Nothing tt mds bb
    ValFptosiinst -> translateCastInst finalState FPToSIInst ii Nothing tt mds bb
    ValUitofpinst -> translateCastInst finalState UIToFPInst ii Nothing tt mds bb
    ValSitofpinst -> translateCastInst finalState SIToFPInst ii Nothing tt mds bb
    ValPtrtointinst -> translateCastInst finalState PtrToIntInst ii Nothing tt mds bb
    ValInttoptrinst -> translateCastInst finalState IntToPtrInst ii Nothing tt mds bb
    ValBitcastinst -> translateCastInst finalState BitcastInst ii Nothing tt mds bb
    ValIcmpinst -> translateCmpInst finalState ICmpInst ii Nothing tt mds bb
    ValFcmpinst -> translateCmpInst finalState FCmpInst ii Nothing tt mds bb
    ValSelectinst -> translateSelectInst finalState ii Nothing tt mds bb
    ValVaarginst -> translateVarArgInst finalState ii Nothing tt mds bb
    ValExtractelementinst -> translateExtractElementInst finalState ii Nothing tt mds bb
    ValInsertelementinst -> translateInsertElementInst finalState ii Nothing tt mds bb
    ValShufflevectorinst -> translateShuffleVectorInst finalState ii Nothing tt mds bb
    ValExtractvalueinst -> translateExtractValueInst finalState ii Nothing tt mds bb
    ValInsertvalueinst -> translateInsertValueInst finalState ii Nothing tt mds bb
    _ -> throw (NonInstructionTag tag)

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
      --isDef <- liftIO $ cMetaSubprogramIsDefinition mp
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
