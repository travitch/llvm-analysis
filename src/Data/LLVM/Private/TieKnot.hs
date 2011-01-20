module Data.LLVM.Private.TieKnot ( tieKnot ) where

import qualified Data.Map as M
import Data.Map (Map, (!))
import qualified Data.Text as T

import Data.LLVM.Private.AttributeTypes
import Data.LLVM.Private.PlaceholderTypeExtractors
import Data.LLVM.Private.FunctionTranslator
import Data.LLVM.Private.MetadataTranslator
import qualified Data.LLVM.Private.PlaceholderTypes as O
import qualified Data.LLVM.Types as N

-- Idea:
-- 1) Extract module assembly since it stands alone.
-- 2) Build a function that eliminates named types and can map a
--    placeholder type to a real type.  This will be used everywhere else
-- 3) Finally, everything else can refer to everything else, so fix up all
--    references in one go using the previously defined maps in completeGraph
tieKnot :: O.Module -> N.Module
tieKnot (O.Module layout triple decls) =
  N.Module { N.moduleDataLayout = layout
           , N.moduleTarget = triple
           , N.moduleAssembly = moduleAsm
           , N.moduleGlobals = globalValues
           }
  where translateType = makeTypeTranslator decls
        moduleAsm = extractModuleAssembly decls
        globalValues :: [N.Value]
        globalValues = completeGraph translateType decls

-- FIXME: Could do something with the named metadata.  There seem to
-- be two entries that don't really give much information: the lists
-- of defined globals.
completeGraph :: (O.Type -> N.Type) ->
                 [O.GlobalDeclaration] ->
                 [N.Value]
completeGraph typeMapper decls = M.elems globalDecls
  where globalDecls = go decls M.empty
        (boundMD, mdForGlobals) = mdGo decls (M.empty, M.empty)
        metadata = M.union boundMD mdForGlobals
        mdGo [] (md, mv) = (md, mv)
        mdGo ((O.UnnamedMetadata name refs) : rest) (md, mv) =
          mdGo rest (transMetadata md mv name refs)
        mdGo (_:rest) vs = mdGo rest vs
        metaRef (O.ValueRef name) = metadata ! name
        metaRef c = error ("Constant is not a metadata reference: " ++ show c)
        go [] vals = vals
        go (decl:rest) vals = case decl of
          O.GlobalDeclaration name addrspace annots ty init align ->
            go rest (transGlobalVar typeMapper transValOrConst getMetadata vals name addrspace annots ty init align)
          O.FunctionDefinition {} ->
            go rest (transFuncDef typeMapper transValOrConst metadata vals decl)
          O.GlobalAlias name linkage vis ty const ->
            go rest (transAlias typeMapper transValOrConst getMetadata vals name linkage vis ty const)
          O.ExternalDecl ty ident ->
            go rest (transExternal typeMapper getMetadata vals ty ident)
          _ -> go rest vals

        -- Return the updated metadata graph - but needs to refer to
        -- the "completed" version in 'metadata'
        getMetadata ident = M.lookup ident metadata
        -- getLocalMD ident = M.lookup ident metadata
        transMetadata = translateMetadata metadata
        transValOrConst :: O.Constant -> N.Value
        transValOrConst v = case v of
          -- O.ConstValue c ty ->
          O.ValueRef ident -> globalDecls ! ident


mkValue :: N.Type -> Maybe Identifier -> Maybe N.Metadata -> N.ValueT -> N.Value
mkValue ty ident md val =
  N.Value { N.valueType = ty
          , N.valueName = ident
          , N.valueMetadata = md
          , N.valueContent = val
          }

transExternal typeMapper getGlobalMD vals ty ident =
  M.insert ident val vals
  where val = mkValue (typeMapper ty) (Just ident) (getGlobalMD ident) N.ExternalValue

transAlias typeMapper transValOrConst getGlobalMD vals name linkage vis ty const =
  M.insert name val vals
  where val = mkValue (typeMapper ty) (Just name) (getGlobalMD name) val'
        val' = N.GlobalAlias { N.globalAliasLinkage = linkage
                             , N.globalAliasVisibility = vis
                             , N.globalAliasValue = transValOrConst const
                             }

transGlobalVar typeMapper transValOrConst getGlobalMD vals name addrspace annots ty init align =
  M.insert name val vals
  where val = mkValue (typeMapper ty) (Just name) (getGlobalMD name) val'
        val' = N.GlobalDeclaration { N.globalVariableAddressSpace = addrspace
                                   , N.globalVariableAnnotations = annots
                                   , N.globalVariableInitializer = transValOrConst init
                                   , N.globalVariableAlignment = align
                                   }

extractModuleAssembly :: [O.GlobalDeclaration] -> [Assembly]
extractModuleAssembly decls = reverse $ foldr xtract [] decls
  where xtract decl acc = case decl of
          O.ModuleAssembly asm -> asm : acc
          _ -> acc

-- Create a map from the placeholder types to the final types.  FIXME:
-- Add support for type uprefs.  The code generator always seems to
-- just use named types, so it doesn't seem crucial.
makeTypeTranslator :: [O.GlobalDeclaration] -> (O.Type -> N.Type)
makeTypeTranslator decls = trans'
  where mapping = namedTrans' decls M.empty
        namedTrans' [] m = m
        namedTrans' (d:rest) m = namedTrans' rest $ case d of
          O.NamedType ident ty@(O.TypeNamed name) -> M.insert name (trans' ty) m
          O.NamedType _ _ -> error "NamedType has non TypeNamed as content"
          _ -> m
        trans' :: O.Type -> N.Type
        trans' t = case t of
          O.TypeInteger i -> N.TypeInteger i
          O.TypeFloat -> N.TypeFloat
          O.TypeDouble -> N.TypeDouble
          O.TypeFP128 -> N.TypeFP128
          O.TypeX86FP80 -> N.TypeX86FP80
          O.TypePPCFP128 -> N.TypePPCFP128
          O.TypeX86MMX -> N.TypeX86MMX
          O.TypeVoid -> N.TypeVoid
          O.TypeLabel -> N.TypeLabel
          O.TypeMetadata -> N.TypeMetadata
          O.TypeArray i t' -> N.TypeArray i (trans' t')
          O.TypeVector i t' -> N.TypeVector i (trans' t')
          O.TypeFunction t' ts' v at -> N.TypeFunction (trans' t') (map trans' ts') v at
          O.TypeOpaque -> N.TypeOpaque
          O.TypePointer t' -> N.TypePointer (trans' t')
          O.TypeStruct ts' -> N.TypeStruct (map trans' ts')
          O.TypePackedStruct ts' -> N.TypePackedStruct (map trans' ts')
          O.TypeUpref i -> error "Type uprefs not supported yet"
          O.TypeNamed name -> mapping ! name


