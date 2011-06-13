{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.LLVM.Types
       ( module Data.LLVM.Private.AttributeTypes
       , module Data.LLVM.Private.ReferentialTypes
       , Map
       , Module(..)
       , moduleFunctions
       ) where

import Control.DeepSeq
import Control.Monad.State
import Data.HashMap.Strict ( HashMap )
import Data.HashSet ( HashSet )
import qualified Data.HashSet as S
import Data.List ( intercalate )

-- import Data.LLVM.CFG

import Data.LLVM.Private.AttributeTypes
import Data.LLVM.Private.Printers
import Data.LLVM.Private.ReferentialTypes

type Map = HashMap

data Module = Module { moduleDataLayout :: DataLayout
                     , moduleTarget :: TargetTriple
                     , moduleAssembly :: [Assembly]
                     , moduleGlobals :: [Value]
--                      , moduleCFGs :: Map Value CFG
                     }



moduleFunctions :: Module -> [Value]
moduleFunctions Module { moduleGlobals = globals } =
  filter valueIsFunction globals

printModule :: Module -> String
printModule Module { moduleDataLayout = layout
                   , moduleTarget = triple
                   , moduleAssembly = asm
                   , moduleGlobals = vals
                   } =
  concat [ layoutS, "\n", tripleS, "\n", asmS, "\n", valS, "\n" ]
  where layoutS = concat [ "target datalayout = \"", show layout, "\"" ]
        tripleS = concat [ "target triple = \"", show triple, "\"" ]
        asmS = printAsm asm
        valS = intercalate "\n\n" $ map printValue vals


-- Technically, these are orphan instances.  But really, if you are
-- using the public interface to this library they are not.  They are
-- only defined outside of the module containing the data type
-- declarations (Data.LLVM.Private.ReferentialTypes) to keep that
-- module's compile time down.
--
-- I disable the warning since this seems reasonable.
--
-- This also would allow users to define their own show instances for
-- these types if desired.

instance Show Module where
  show = printModule

instance NFData Module where
  rnf m = evalState (forceModule m) (S.empty, S.empty) `seq` ()

type ForceMonad = State (HashSet Value, HashSet Metadata)

-- | Force the module to be fully evaluated to rnf form from the
-- top-down.  There are cycles, so we have to be careful to avoid
-- traversing them infinitely.
forceModule :: Module -> ForceMonad Module
forceModule m = do
  mapM_ forceGlobal (moduleGlobals m)
  return $ moduleDataLayout m `deepseq` moduleTarget m `deepseq`
            moduleAssembly m `deepseq` m `seq` m

-- | Force each type of global as much as is safe.
forceGlobal :: Value -> ForceMonad ()
forceGlobal v = do
  -- The unique id field is strict and will be forced when we force this constructor.
  valueName v `deepseq` valueType v `deepseq` v `seq` return ()
  -- Expand the metadata, if there is any
  maybe (return ()) metaForceIfNeeded (valueMetadata v)
  forceGlobalValueT (valueContent v)
  return ()

forceGlobalValueT :: ValueT -> ForceMonad ()
forceGlobalValueT = undefined

metaForceIfNeeded :: Metadata -> ForceMonad ()
metaForceIfNeeded m = do
  s <- get
  let (vset, mset) = s
  case S.member m mset of
    True -> return ()
    False -> do
      forceMetadata m
      let mset' = S.insert m mset
      put (vset, mset')

forceMetadata :: Metadata -> ForceMonad ()
forceMetadata m = do
  metaValueName m `deepseq` m `seq` return ()
  forceMetadataT (metaValueContent m)

forceMetadataT :: MetadataT -> ForceMonad ()
forceMetadataT m@(MetaSourceLocation {}) = do
  m `seq` return ()
  metaForceIfNeeded (metaSourceScope m)
forceMetadataT m@(MetaDWLexicalBlock {}) = do
  m `seq` return ()
  mapM_ metaForceIfNeeded [ metaLexicalBlockContext m
                          , metaLexicalBlockFile m
                          ]
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