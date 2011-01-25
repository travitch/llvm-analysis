module Data.LLVM.Private.TieKnot ( tieKnot ) where

import qualified Data.Map as M
import Data.Map (Map)
import Data.Text (Text)

import Data.LLVM.Private.AttributeTypes
import Data.LLVM.Private.Translators.Constants
import Data.LLVM.Private.Translators.Functions
import Data.LLVM.Private.Translators.Metadata
import Data.LLVM.Private.Translators.Types
import qualified Data.LLVM.Private.PlaceholderTypes as O

import Data.LLVM.CFG
import Data.LLVM.Types

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
completeGraph typeMapper decls = M.elems globalDecls
  where globalDecls = go decls M.empty
        (boundMD, mdForGlobals) = mdGo decls (M.empty, M.empty)
        metadata = M.union boundMD mdForGlobals
        mdGo [] (md, mv) = (md, mv)
        mdGo ((O.UnnamedMetadata name refs) : rest) (md, mv) =
          mdGo rest (transMetadata md mv name refs)
        mdGo (_:rest) vs = mdGo rest vs
        go [] vals = vals
        go (decl:rest) vals = case decl of
          O.GlobalDeclaration name addrspace annots ty initializer align section ->
            go rest (transGlobalVar typeMapper (transValOrConst M.empty) getMetadata vals name addrspace annots ty initializer align section)
          O.FunctionDefinition {} ->
            go rest (translateFunctionDefinition typeMapper transValOrConst metadata vals decl)
          O.GlobalAlias name linkage vis ty constant ->
            go rest (transAlias typeMapper (transValOrConst M.empty) getMetadata vals name linkage vis ty constant)
          O.ExternalValueDecl ty ident ->
            go rest (transExternal typeMapper getMetadata vals ty ident)
          O.ExternalFuncDecl ty ident attrs ->
            go rest (transExternalFunc typeMapper getMetadata vals ty ident attrs)
          _ -> go rest vals

        -- Return the updated metadata graph - but needs to refer to
        -- the "completed" version in 'metadata'
        getMetadata ident = M.lookup ident metadata
        transMetadata = translateMetadata (transValOrConst M.empty) metadata
        transValOrConst = translateConstant typeMapper globalDecls


mkValue :: Type -> Maybe Identifier -> Maybe Metadata -> ValueT -> Value
mkValue ty ident md val =
  Value { valueType = ty
          , valueName = ident
          , valueMetadata = md
          , valueContent = val
          }

transExternal :: (O.Type -> Type) -> (Identifier -> Maybe Metadata) ->
                 Map Identifier Value -> O.Type -> Identifier ->
                 Map Identifier Value
transExternal typeMapper getGlobalMD vals ty ident =
  M.insert ident val vals
  where val = mkValue (typeMapper ty) (Just ident) (getGlobalMD ident) ExternalValue

transExternalFunc :: (O.Type -> Type) -> (Identifier -> Maybe Metadata) ->
                 Map Identifier Value -> O.Type -> Identifier -> [FunctionAttribute] ->
                 Map Identifier Value
transExternalFunc typeMapper getGlobalMD vals ty ident attrs =
  M.insert ident val vals
  where val = mkValue (typeMapper ty) (Just ident) (getGlobalMD ident) (ExternalFunction attrs)


transAlias :: (O.Type -> Type) -> (O.Constant -> Value) ->
              (Identifier -> Maybe Metadata) -> Map Identifier Value ->
              Identifier -> LinkageType -> VisibilityStyle ->
              O.Type -> O.Constant -> Map Identifier Value
transAlias typeMapper transValOrConst getGlobalMD vals name linkage vis ty constant =
  M.insert name val vals
  where val = mkValue (typeMapper ty) (Just name) (getGlobalMD name) val'
        val' = GlobalAlias { globalAliasLinkage = linkage
                             , globalAliasVisibility = vis
                             , globalAliasValue = transValOrConst constant
                             }

transGlobalVar :: (O.Type -> Type) -> (O.Constant -> Value) ->
                  (Identifier -> Maybe Metadata) ->
                  Map Identifier Value -> Identifier -> Int ->
                  [GlobalAnnotation] -> O.Type -> O.Constant -> Integer ->
                  Maybe Text ->
                  Map Identifier Value
transGlobalVar typeMapper transValOrConst getGlobalMD vals name addrspace annots ty initializer align section =
  M.insert name val vals
  where val = mkValue (typeMapper ty) (Just name) (getGlobalMD name) val'
        val' = GlobalDeclaration { globalVariableAddressSpace = addrspace
                                 , globalVariableAnnotations = annots
                                 , globalVariableInitializer = transValOrConst initializer
                                 , globalVariableAlignment = align
                                 , globalVariableSection = section
                                 }

extractModuleAssembly :: [O.GlobalDeclaration] -> [Assembly]
extractModuleAssembly decls = reverse $ foldr xtract [] decls
  where xtract decl acc = case decl of
          O.ModuleAssembly asm -> asm : acc
          _ -> acc


