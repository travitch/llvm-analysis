module Data.LLVM.Private.TieKnot ( tieKnot ) where

import Control.DeepSeq

import Data.List ( foldl' )
import qualified Data.HashMap.Strict as M
import Data.ByteString.Char8 ( ByteString )

import Data.LLVM.Private.UniqueId
import Data.LLVM.Private.Translators.Constants
import Data.LLVM.Private.Translators.Functions
import Data.LLVM.Private.Translators.Metadata
import Data.LLVM.Private.Translators.Types
import Data.LLVM.Private.Types.Identifiers
import qualified Data.LLVM.Private.Types.Placeholder as O
import Data.LLVM.Private.ParserOptions

import Data.LLVM.Types

-- Idea:
-- 1) Extract module assembly since it stands alone.
-- 2) Build a function that eliminates named types and can map a
--    placeholder type to a real type.  This will be used everywhere else
-- 3) Finally, everything else can refer to everything else, so fix up all
--    references in one go using the previously defined maps in completeGraph

-- | Convert the placeholder Module type into the user-visible Module
-- with all references resolved.  This uses the knot-tying technique
-- in which references are set up and resolved lazily using an
-- intermediate Map structure.
--
-- To allow these Maps (and all of the placeholder data) to be
-- collected, this function runs 'deepseq' over the resulting 'Module'
-- to ensure all thunks are fully evaluated.
tieKnot :: ParserOptions -> O.Module -> Module
tieKnot opts (O.Module layout triple decls) = m `deepseq` m
  where
    m = Module { moduleDataLayout = layout
               , moduleTarget = triple
               , moduleAssembly = moduleAsm
               , moduleGlobals = globalValues
               }
    typeMapper = translateType decls
    moduleAsm = extractModuleAssembly decls
    globalValues :: [Value]
    globalValues = completeGraph opts typeMapper decls

-- FIXME: Could do something with the named metadata.  There seem to
-- be two entries that don't really give much information: the lists
-- of defined globals.
completeGraph :: ParserOptions -> (O.Type -> Type) ->
                 [O.GlobalDeclaration] ->
                 [Value]
completeGraph opts typeMapper decls = M.elems globalValues
  where
    (valStream, mdStream) = split2 initialStream
    globalValues = go decls M.empty valStream
    (boundMD, mdForGlobals) = mdGo decls (M.empty, M.empty) mdStream
    metadata = boundMD `M.union` mdForGlobals

    mdGo [] (md, mv) _ = (md, mv)
    mdGo (O.UnnamedMetadata name refs : rest) (md, mv) idstream =
      let (s1, s2) = split2 idstream
      in mdGo rest (transMetadata s1 md mv name refs) s2
    mdGo (_:rest) vs idstream = mdGo rest vs idstream

    go [] vals _ = vals
    go (decl:rest) vals idstream =
      let (s1, s2) = split2 idstream
          thisId = extract idstream
      in case decl of
        O.GlobalDeclaration name addrspace linkage vis annot ty initializer align section ->
          let g = transGlobalVar typeMapper trGlobal getMetadata s1 name addrspace linkage vis annot ty initializer align section
              updatedVals = M.insert name g vals
          in go rest updatedVals s2
        O.FunctionDefinition { O.funcName = fname } ->
          -- Use the complex stream split since the func def trans
          -- can use an unbounded number of ids
          let (locals, global) = translateFunctionDefinition typeMapper (trConst locals) metadata s1 decl
              updatedVals = M.insert fname global vals
          in go rest updatedVals s2
        O.GlobalAlias name linkage vis ty constant ->
          -- This requires a full split of the id stream since
          -- transAlias could use several ids
          let g = transAlias typeMapper trGlobal getMetadata s1 name linkage vis ty constant
              updatedVals = M.insert name g vals
          in go rest updatedVals s2
        O.ExternalValueDecl ty ident ->
          -- This branch only uses one id for transExternal, so use
          -- the simple split stream for the next recursive call
          let g = transExternal typeMapper getMetadata thisId ty ident
              updatedVals = M.insert ident g vals
          in go rest updatedVals (split idstream)
        O.ExternalFuncDecl ty ident attrs ->
          -- In this branch, we only use one id for
          -- transExternalFunc, so use the simple split stream for
          -- the next recursive call
          let g = transExternalFunc typeMapper getMetadata thisId ty ident attrs
              updatedVals = M.insert ident g vals
          in go rest updatedVals (split idstream)
        _ -> go rest vals idstream

    -- Return the updated metadata graph - but needs to refer to
    -- the "completed" version in 'metadata'
    getMetadata ident = M.lookup ident metadata
    transMetadata = translateMetadata opts (trConst M.empty) metadata
    trConst :: Map Identifier Value -> O.Constant -> IdStream -> Value
    trConst = translateConstant typeMapper globalValues
    trGlobal = trConst M.empty


mkValue :: UniqueId -> Type -> Maybe Identifier -> Maybe Metadata -> ValueT ->
           Value
mkValue uid ty ident md val =
  Value { valueType = ty
        , valueName = ident
        , valueMetadata = md
        , valueContent = val
        , valueUniqueId = uid
        }

transExternal :: (O.Type -> Type) -> (Identifier -> Maybe Metadata) ->
                 UniqueId -> O.Type -> Identifier ->
                 Value
transExternal typeMapper getGlobalMD thisId ty ident =
  mkValue thisId (typeMapper ty) (Just ident) (getGlobalMD ident) ExternalValue

transExternalFunc :: (O.Type -> Type) -> (Identifier -> Maybe Metadata) ->
                     UniqueId -> O.Type -> Identifier ->
                     [FunctionAttribute] -> Value
transExternalFunc typeMapper getGlobalMD thisId ty ident attrs =
  mkValue thisId (typeMapper ty) (Just ident) (getGlobalMD ident) (ExternalFunction attrs)


transAlias :: (O.Type -> Type) -> (O.Constant -> IdStream -> Value) ->
              (Identifier -> Maybe Metadata) -> IdStream ->
              Identifier -> LinkageType -> VisibilityStyle ->
              O.Type -> O.Constant -> Value
transAlias typeMapper trConst getGlobalMD idstream name linkage vis ty constant =
  val
  where
    aliasVal = trConst constant (split idstream)
    val = mkValue (extract idstream) (typeMapper ty) (Just name) (getGlobalMD name) val'
    val' = GlobalAlias { globalAliasLinkage = linkage
                       , globalAliasVisibility = vis
                       , globalAliasValue = aliasVal
                       }

transGlobalVar :: (O.Type -> Type) ->
                  (O.Constant -> IdStream -> Value) ->
                  (Identifier -> Maybe Metadata) ->
                  IdStream -> Identifier -> Int -> LinkageType -> VisibilityStyle ->
                  GlobalAnnotation -> O.Type -> Maybe O.Constant -> Integer ->
                  Maybe ByteString ->
                  Value
transGlobalVar typeMapper trConst getGlobalMD idstream name addrspace linkage vis annot ty initializer align section =
  val
  where
    i = maybe Nothing (Just . ((flip trConst) (split idstream))) initializer
    val = mkValue (extract idstream) (typeMapper ty) (Just name) (getGlobalMD name) decl
    decl = GlobalDeclaration { globalVariableAddressSpace = addrspace
                             , globalVariableLinkage = linkage
                             , globalVariableVisibility = vis
                             , globalVariableAnnotation = annot
                             , globalVariableInitializer = i
                             , globalVariableAlignment = fromIntegral $ align
                             , globalVariableSection = section
                             }

extractModuleAssembly :: [O.GlobalDeclaration] -> [Assembly]
extractModuleAssembly decls = foldl' xtract [] decls
  where
    xtract acc decl = case decl of
      O.ModuleAssembly asm -> asm : acc
      _ -> acc


