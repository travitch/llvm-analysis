module Data.LLVM.Private.TieKnot ( tieKnot ) where

import Data.LLVM.Private.AttributeTypes
import qualified Data.LLVM.Private.PlaceholderTypes as O
import qualified Data.LLVM.Types as T
import qualified Data.Map as M
import Data.Map (Map, (!))

-- Idea:
-- 1) Extract module assembly since it stands alone.
-- 2) Build a function that eliminates named types and can map a
--    placeholder type to a real type.  This will be used everywhere else
-- 3) Build the list of externally referenced functions.  Their types need
--    to be translated, but they do not reference any other values (from the
--    IR perspective).
-- 4) Finally, everything else can refer to everything else, so fix up all
--    references in one go using the previously defined maps in completeGraph
tieKnot :: O.Module -> T.Module
tieKnot (O.Module layout triple decls) =
  T.Module { T.moduleDataLayout = layout
           , T.moduleTarget = triple
           , T.moduleAssembly = moduleAsm
           , T.moduleGlobals = globalValues ++ externs
           }
  where translateType = makeTypeTranslator decls
        externFuncMap = makeExternFunctionMap decls translateType
        mapExternFunc = flip M.lookup externFuncMap
        externs :: [T.Value]
        externs = M.elems externFuncMap
        moduleAsm = extractModuleAssembly decls
        globalValues :: [T.Value]
        globalValues = completeGraph translateType mapExternFunc decls

-- FIXME: Could do something with the named metadata.  There seem to
-- be two entries that don't really give much information: the lists
-- of defined globals.
completeGraph :: (O.Type -> T.Type) ->
                 (Identifier -> Maybe T.Value) ->
                 [O.GlobalDeclaration] ->
                 [T.Value]
completeGraph typeMapper externMapper decls = M.elems globalDecls
  where (metadata, globalDecls) = go decls M.empty M.empty
        metaRef (O.ValueRef name) = metadata ! name
        metaRef c = error ("Constant is not a metadata reference: " ++ show c)
        go [] md gd = (md, gd)
        go (decl:rest) md gd = case decl of
          O.UnnamedMetadata name refs isSLoc ->
            go rest (transMetadata md name refs isSLoc) gd
          -- O.GlobalDeclaration name addrspace annots ty init align ->
          --   go rest md (transGlobalVar gd name addrspace annots ty init align)
          -- O.FunctionDefinition {} ->
          --   go rest md (transFuncDef gd decl)
          -- O.GlobalAlias name linkage vis type const ->
          --   go rest md (transAlias gd name linkage vis type const)
          _ -> go rest md gd
        -- Return the updated metadata graph - but needs to refer to
        -- the "completed" version in 'metadata'
        transMetadata md name reflist isSLoc = M.insert name mdval md
          where mdval = if isSLoc then mkMetaSourceLocation else decodeRefs
                mkMetaSourceLocation =
                  T.MetaSourceLocation { T.metaSourceRow = unconstInt reflist 0
                                       , T.metaSourceCol = unconstInt reflist 1
                                       , T.metaSourceScope = metaRef (reflist !! 2)
                                       }
                decodeRefs = T.MetaDWLexicalBlock

unconstInt :: [O.Constant] -> Int -> Integer
unconstInt consts idx = getInt (consts !! idx)
  where getInt (O.ConstValue (O.ConstantInt i) (O.TypeInteger 32)) = i
        getInt c = error ("Constant is not an int: " ++ show c)


extractModuleAssembly :: [O.GlobalDeclaration] -> [Assembly]
extractModuleAssembly decls = reverse $ foldr xtract [] decls
  where xtract decl acc = case decl of
          O.ModuleAssembly asm -> asm : acc
          _ -> acc

-- Returns a function that maps the identifiers of extern functions to
-- actual Function Values (if the identifier is indeed an external
-- function).
makeExternFunctionMap :: [O.GlobalDeclaration] -> (O.Type -> T.Type) -> Map Identifier T.Value
makeExternFunctionMap decls typeMapper = mapping -- flip M.lookup mapping
  where mapping = funcMapper decls M.empty
        funcMapper :: [O.GlobalDeclaration] -> Map Identifier T.Value -> Map Identifier T.Value
        funcMapper [] m = m
        funcMapper (d:rest) m = funcMapper rest $ case d of
          O.ExternalDecl t name ->
            M.insert name (mkFuncVal t name) m
          _ -> funcMapper rest m
          where mkFuncVal t name =
                  T.Value { T.valueType = typeMapper t
                          , T.valueName = Just name
                          , T.valueMetadata = Nothing
                          , T.valueContent = T.Function { T.functionType = typeMapper t
                                                        , T.functionParameters = Nothing
                                                        , T.functionBody = Nothing
                                                        }
                          }

-- Create a map from the placeholder types to the final types.  FIXME:
-- Add support for type uprefs.  The code generator always seems to
-- just use named types, so it doesn't seem crucial.
makeTypeTranslator :: [O.GlobalDeclaration] -> (O.Type -> T.Type)
makeTypeTranslator decls = trans'
  where mapping = namedTrans' decls M.empty
        namedTrans' [] m = m
        namedTrans' (d:rest) m = namedTrans' rest $ case d of
          O.NamedType ident ty@(O.TypeNamed name) -> M.insert name (trans' ty) m
          O.NamedType _ _ -> error "NamedType has non TypeNamed as content"
          _ -> m
        trans' :: O.Type -> T.Type
        trans' t = case t of
          O.TypeInteger i -> T.TypeInteger i
          O.TypeFloat -> T.TypeFloat
          O.TypeDouble -> T.TypeDouble
          O.TypeFP128 -> T.TypeFP128
          O.TypeX86FP80 -> T.TypeX86FP80
          O.TypePPCFP128 -> T.TypePPCFP128
          O.TypeX86MMX -> T.TypeX86MMX
          O.TypeVoid -> T.TypeVoid
          O.TypeLabel -> T.TypeLabel
          O.TypeMetadata -> T.TypeMetadata
          O.TypeArray i t' -> T.TypeArray i (trans' t')
          O.TypeVector i t' -> T.TypeVector i (trans' t')
          O.TypeFunction t' ts' v at -> T.TypeFunction (trans' t') (map trans' ts') v at
          O.TypeOpaque -> T.TypeOpaque
          O.TypePointer t' -> T.TypePointer (trans' t')
          O.TypeStruct ts' -> T.TypeStruct (map trans' ts')
          O.TypePackedStruct ts' -> T.TypePackedStruct (map trans' ts')
          O.TypeUpref i -> error "Type uprefs not supported yet"
          O.TypeNamed name -> mapping M.! name


