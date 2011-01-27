module Data.LLVM.Private.TieKnot ( tieKnot ) where

import qualified Data.Map as M
import Data.Map (Map)
import Data.Text (Text)

import Data.LLVM.Private.KnotHelpers
import Data.LLVM.Private.Translators.Constants
import Data.LLVM.Private.Translators.Functions
import Data.LLVM.Private.Translators.Metadata
import Data.LLVM.Private.Translators.Types
import qualified Data.LLVM.Private.PlaceholderTypes as O

import Data.LLVM.CFG
import Data.LLVM.Types

import Debug.Trace
debug = flip trace

-- Idea:
-- 1) Extract module assembly since it stands alone.
-- 2) Build a function that eliminates named types and can map a
--    placeholder type to a real type.  This will be used everywhere else
-- 3) Finally, everything else can refer to everything else, so fix up all
--    references in one go using the previously defined maps in completeGraph
tieKnot :: O.Module -> Module
tieKnot (O.Module layout triple decls) =
  Module { moduleDataLayout = layout
         , moduleTarget = triple
         , moduleAssembly = moduleAsm
         , moduleGlobals = globalValues
         , moduleCFGs = M.fromList $ zip funcs $ map makeCFG funcs
         }
  where typeMapper = translateType decls
        moduleAsm = extractModuleAssembly decls
        globalValues :: [Value]
        globalValues = completeGraph typeMapper decls
        funcs = filter valueIsFunction globalValues


-- FIXME: Could do something with the named metadata.  There seem to
-- be two entries that don't really give much information: the lists
-- of defined globals.
completeGraph :: (O.Type -> Type) ->
                 [O.GlobalDeclaration] ->
                 [Value]
completeGraph typeMapper decls = M.elems $ getGlobals identDict
  where identDict = go decls emptyDict
        (boundMD, mdForGlobals) = mdGo decls (M.empty, M.empty)
        metadata = M.union boundMD mdForGlobals
        mdGo [] (md, mv) = (md, mv)
        mdGo ((O.UnnamedMetadata name refs) : rest) (md, mv) =
          mdGo rest (transMetadata md mv name refs)
        mdGo (_:rest) vs = mdGo rest vs
        go [] vals = vals
        go (decl:rest) vals = case decl of
          O.GlobalDeclaration name addrspace annots ty initializer align section ->
            go rest (transGlobalVar typeMapper (trConst M.empty) getMetadata vals name addrspace annots ty initializer align section)
          O.FunctionDefinition { O.funcName = fname } ->
            let locals = getFunctionLocals fname identDict
            in go rest (translateFunctionDefinition typeMapper (trConst locals) metadata vals decl)
          O.GlobalAlias name linkage vis ty constant ->
            go rest (transAlias typeMapper (trConst M.empty) getMetadata vals name linkage vis ty constant)
          O.ExternalValueDecl ty ident ->
            go rest (transExternal typeMapper getMetadata vals ty ident)
          O.ExternalFuncDecl ty ident attrs ->
            go rest (transExternalFunc typeMapper getMetadata vals ty ident attrs)
          _ -> go rest vals

        -- Return the updated metadata graph - but needs to refer to
        -- the "completed" version in 'metadata'
        getMetadata ident = M.lookup ident metadata
        transMetadata = translateMetadata (trConst M.empty) metadata
        trConst :: Map Identifier Value -> O.Constant -> IdentDict -> (Value, IdentDict)
        trConst = translateConstant typeMapper (getGlobals identDict)


mkValue :: Type -> Maybe Identifier -> Maybe Metadata -> ValueT ->
           IdentDict -> (Value, IdentDict)
mkValue ty ident md val dict =
  (Value { valueType = ty
         , valueName = ident
         , valueMetadata = md
         , valueContent = val
         , valueUniqueId = uid
         }, s)
  where (uid, s) = nextSequenceNumber dict

transExternal :: (O.Type -> Type) -> (Identifier -> Maybe Metadata) ->
                 IdentDict -> O.Type -> Identifier ->
                 IdentDict
transExternal typeMapper getGlobalMD vals ty ident =
  addGlobal ident val vals'
  where (val, vals') = mkValue (typeMapper ty) (Just ident) (getGlobalMD ident) ExternalValue vals

transExternalFunc :: (O.Type -> Type) -> (Identifier -> Maybe Metadata) ->
                 IdentDict -> O.Type -> Identifier -> [FunctionAttribute] ->
                 IdentDict
transExternalFunc typeMapper getGlobalMD vals ty ident attrs =
  addGlobal ident val vals'
  where (val, vals') = mkValue (typeMapper ty) (Just ident) (getGlobalMD ident) (ExternalFunction attrs) vals


transAlias :: (O.Type -> Type) -> (O.Constant -> IdentDict -> (Value, IdentDict)) ->
              (Identifier -> Maybe Metadata) -> IdentDict ->
              Identifier -> LinkageType -> VisibilityStyle ->
              O.Type -> O.Constant -> IdentDict
transAlias typeMapper trConst getGlobalMD dict name linkage vis ty constant =
  addGlobal name val d2
  where (aliasVal, d1) = trConst constant dict
        (val, d2) = mkValue (typeMapper ty) (Just name) (getGlobalMD name) val' d1
        val' = GlobalAlias { globalAliasLinkage = linkage
                           , globalAliasVisibility = vis
                           , globalAliasValue = aliasVal
                           }

transGlobalVar :: (O.Type -> Type) ->
                  (O.Constant -> IdentDict -> (Value, IdentDict)) ->
                  (Identifier -> Maybe Metadata) ->
                  IdentDict -> Identifier -> Int ->
                  [GlobalAnnotation] -> O.Type -> O.Constant -> Integer ->
                  Maybe Text ->
                  IdentDict
transGlobalVar typeMapper trConst getGlobalMD dict name addrspace annots ty initializer align section =
  addGlobal name val d2
  where (i, d1) = trConst initializer dict
        (val, d2) = mkValue (typeMapper ty) (Just name) (getGlobalMD name) decl d1
        decl = GlobalDeclaration { globalVariableAddressSpace = addrspace
                                 , globalVariableAnnotations = annots
                                 , globalVariableInitializer = i
                                 , globalVariableAlignment = align
                                 , globalVariableSection = section
                                 }

extractModuleAssembly :: [O.GlobalDeclaration] -> [Assembly]
extractModuleAssembly decls = reverse $ foldr xtract [] decls
  where xtract decl acc = case decl of
          O.ModuleAssembly asm -> asm : acc
          _ -> acc


