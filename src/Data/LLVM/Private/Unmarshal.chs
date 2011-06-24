{-# LANGUAGE ForeignFunctionInterface, RankNTypes #-}
module Data.LLVM.Private.Unmarshal where

#include "c++/marshal.h"

import Control.Applicative
import Control.Monad.State
import Data.Array.Storable
import qualified Data.ByteString.Char8 as BS
import Data.Map ( Map )
import qualified Data.Map as M
import Data.Maybe ( catMaybes )
import Foreign.C
import Foreign.C.String
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Storable

import Data.LLVM.Private.C2HS
import Data.LLVM.Types

{#enum CmpPredicate {underscoreToCase} deriving (Show, Eq) #}
{#enum CallingConvention {} deriving (Show, Eq) #}
{#enum TypeTag {} deriving (Show, Eq) #}
{#enum ValueTag {underscoreToCase} deriving (Show, Eq) #}
{#enum LinkageType {} deriving (Show, Eq) #}
{#enum VisibilityType {} deriving (Show, Eq) #}

data CModule
{#pointer *CModule as ModulePtr -> CModule #}

cModuleIdentifier :: ModulePtr -> IO String
cModuleIdentifier m = ({#get CModule->moduleIdentifier#} m) >>= peekCString

cModuleDataLayout :: ModulePtr -> IO String
cModuleDataLayout m = ({#get CModule->moduleDataLayout#} m) >>= peekCString

cModuleTargetTriple :: ModulePtr -> IO String
cModuleTargetTriple m = ({#get CModule->targetTriple#} m) >>= peekCString

cModuleInlineAsm :: ModulePtr -> IO String
cModuleInlineAsm m = ({#get CModule->moduleInlineAsm#} m) >>= peekCString

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

data CType = CType TypeTag Int Bool Bool (Ptr TypePtr) Int TypePtr String
{#pointer *CType as TypePtr -> CType #}

data CValue = CValue ValueTag TypePtr String (Ptr ()) (Ptr ())
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
      name <- peekCString namePtr
      return $! (Just . makeIdentifier . BS.pack) name

cValueData :: ValuePtr -> IO (Ptr ())
cValueData = {#get CValue->data#}


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

cInlineAsmString :: InlineAsmInfoPtr -> IO String
cInlineAsmString a = ({#get CInlineAsmInfo->asmString#} a) >>= peekCString
cInlineAsmConstraints :: InlineAsmInfoPtr -> IO String
cInlineAsmConstraints a = ({#get CInlineAsmInfo->constraintString#} a) >>= peekCString

{#fun marshalLLVM { `String' } -> `ModulePtr' id #}
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

  vars' <- mapM (translateValue finalState) vars
  aliases' <- mapM (translateValue finalState) aliases
  funcs' <- mapM (translateValue finalState) funcs

  let ir = Module { moduleIdentifier = BS.pack modIdent
                  , moduleDataLayout = undefined
                  , moduleTarget = undefined
                  , moduleAssembly = undefined
                  , moduleAliases = aliases'
                  , moduleGlobalVariables = vars'
                  , moduleFunctions = funcs'
                  }
  s <- get
  return (ir, s)

{-
cValueTag :: ValuePtr -> IO ValueTag
cValueType :: ValuePtr -> IO TypePtr
cValueName :: ValuePtr -> IO (Maybe Identifier)
-}

translateType :: KnotState -> TypePtr -> KnotMonad Type
translateType finalState tp = undefined

translateValue :: KnotState -> ValuePtr -> KnotMonad Value
translateValue finalState vp = do
  tag <- liftIO $ cValueTag vp
  name <- liftIO $ cValueName vp
  typePtr <- liftIO $ cValueType vp
  dataPtr <- liftIO $ cValueData vp

  tt <- translateType finalState typePtr

  content <- case tag of
    ValArgument -> translateArgument finalState (castPtr dataPtr)
    ValBasicblock -> translateBasicBlock finalState (castPtr dataPtr)
    ValInlineasm -> translateInlineAsm finalState (castPtr dataPtr)

  curState <- get
  let key = ptrToIntPtr vp
      curVals = valueMap curState
      tv = Value { valueType = tt
                 , valueName = name
                 , valueMetadata = undefined
                 , valueContent = content
                 , valueUniqueId = fromIntegral key
                 }

  put curState { valueMap = M.insert key tv curVals }

  return tv

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
  return $! InlineAsm (BS.pack asmString) (BS.pack constraints)
