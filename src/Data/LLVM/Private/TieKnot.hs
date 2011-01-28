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
completeGraph typeMapper decls = M.elems globalValues
  where globalValues = go decls M.empty initialStream
        (boundMD, mdForGlobals) = mdGo decls (M.empty, M.empty)
        metadata = boundMD `M.union` mdForGlobals
        mdGo [] (md, mv) = (md, mv)
        mdGo (O.UnnamedMetadata name refs : rest) (md, mv) =
          mdGo rest (transMetadata md mv name refs)
        mdGo (_:rest) vs = mdGo rest vs

        go [] vals _ = vals
        go (decl:rest) vals idstream@(thisId:restIds) =
          let (s1, s2) = splitStream idstream
          in case decl of
          O.GlobalDeclaration name addrspace annots ty initializer align section ->
            let g = transGlobalVar typeMapper trGlobal getMetadata s1 name addrspace annots ty initializer align section
                updatedVals = M.insert name g vals
            in go rest updatedVals s2
          O.FunctionDefinition { O.funcName = fname } ->
            let (locals, global) = translateFunctionDefinition typeMapper (trConst locals) metadata s1 decl
                updatedVals = M.insert fname global vals
            in go rest updatedVals s2
          O.GlobalAlias name linkage vis ty constant ->
            let g = transAlias typeMapper trGlobal getMetadata s1 name linkage vis ty constant
                updatedVals = M.insert name g vals
            in go rest updatedVals s2
          O.ExternalValueDecl ty ident ->
            let g = transExternal typeMapper getMetadata thisId ty ident
                updatedVals = M.insert ident g vals
            in go rest updatedVals restIds
          O.ExternalFuncDecl ty ident attrs ->
            let g = transExternalFunc typeMapper getMetadata thisId ty ident attrs
                updatedVals = M.insert ident g vals
            in go rest updatedVals restIds
          _ -> go rest vals idstream

        -- Return the updated metadata graph - but needs to refer to
        -- the "completed" version in 'metadata'
        getMetadata ident = M.lookup ident metadata
        transMetadata = translateMetadata (trConst M.empty) metadata
        trConst :: Map Identifier Value -> O.Constant -> IdStream -> Value
        trConst = translateConstant typeMapper globalValues
        trGlobal = trConst M.empty


mkValue :: Integer -> Type -> Maybe Identifier -> Maybe Metadata -> ValueT ->
           Value
mkValue uid ty ident md val =
  Value { valueType = ty
        , valueName = ident
        , valueMetadata = md
        , valueContent = val
        , valueUniqueId = uid
        }

transExternal :: (O.Type -> Type) -> (Identifier -> Maybe Metadata) ->
                 Integer -> O.Type -> Identifier ->
                 Value
transExternal typeMapper getGlobalMD thisId ty ident =
  mkValue thisId (typeMapper ty) (Just ident) (getGlobalMD ident) ExternalValue

transExternalFunc :: (O.Type -> Type) -> (Identifier -> Maybe Metadata) ->
                     Integer -> O.Type -> Identifier ->
                     [FunctionAttribute] -> Value
transExternalFunc typeMapper getGlobalMD thisId ty ident attrs =
  mkValue thisId (typeMapper ty) (Just ident) (getGlobalMD ident) (ExternalFunction attrs)


transAlias :: (O.Type -> Type) -> (O.Constant -> IdStream -> Value) ->
              (Identifier -> Maybe Metadata) -> IdStream ->
              Identifier -> LinkageType -> VisibilityStyle ->
              O.Type -> O.Constant -> Value
transAlias typeMapper trConst getGlobalMD (thisId:restIds) name linkage vis ty constant =
  val
  where aliasVal = trConst constant restIds
        val = mkValue thisId (typeMapper ty) (Just name) (getGlobalMD name) val'
        val' = GlobalAlias { globalAliasLinkage = linkage
                           , globalAliasVisibility = vis
                           , globalAliasValue = aliasVal
                           }

transGlobalVar :: (O.Type -> Type) ->
                  (O.Constant -> IdStream -> Value) ->
                  (Identifier -> Maybe Metadata) ->
                  IdStream -> Identifier -> Int ->
                  [GlobalAnnotation] -> O.Type -> O.Constant -> Integer ->
                  Maybe Text ->
                  Value
transGlobalVar typeMapper trConst getGlobalMD (thisId:restIds) name addrspace annots ty initializer align section =
  val
  where i = trConst initializer restIds
        val = mkValue thisId (typeMapper ty) (Just name) (getGlobalMD name) decl
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


