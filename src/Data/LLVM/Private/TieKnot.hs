module Data.LLVM.Private.TieKnot ( tieKnot ) where

import qualified Data.Map as M
import Data.Map (Map, (!))
import qualified Data.Text as T

import Data.LLVM.Private.AttributeTypes
import Data.LLVM.Private.DwarfHelpers
import qualified Data.LLVM.Private.PlaceholderTypes as O
import qualified Data.LLVM.Types as N

-- Constant defined by LLVM to version tags
llvmDebugVersion = 524288

-- Idea:
-- 1) Extract module assembly since it stands alone.
-- 2) Build a function that eliminates named types and can map a
--    placeholder type to a real type.  This will be used everywhere else
-- 3) Build the list of externally referenced functions.  Their types need
--    to be translated, but they do not reference any other values (from the
--    IR perspective).
-- 4) Finally, everything else can refer to everything else, so fix up all
--    references in one go using the previously defined maps in completeGraph
tieKnot :: O.Module -> N.Module
tieKnot (O.Module layout triple decls) =
  N.Module { N.moduleDataLayout = layout
           , N.moduleTarget = triple
           , N.moduleAssembly = moduleAsm
           , N.moduleGlobals = globalValues ++ externs
           }
  where translateType = makeTypeTranslator decls
        externFuncMap = makeExternFunctionMap decls translateType
        mapExternFunc = flip M.lookup externFuncMap
        externs :: [N.Value]
        externs = M.elems externFuncMap
        moduleAsm = extractModuleAssembly decls
        globalValues :: [N.Value]
        globalValues = completeGraph translateType mapExternFunc decls

-- FIXME: Could do something with the named metadata.  There seem to
-- be two entries that don't really give much information: the lists
-- of defined globals.
completeGraph :: (O.Type -> N.Type) ->
                 (Identifier -> Maybe N.Value) ->
                 [O.GlobalDeclaration] ->
                 [N.Value]
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
                  N.MetaSourceLocation { N.metaSourceRow = getInt (reflist !! 0)
                                       , N.metaSourceCol = getInt (reflist !! 1)
                                       , N.metaSourceScope = metaRef (reflist !! 2)
                                       }
                decodeRefs = case reflist of
                  [] -> error "Empty metadata not allowed"
                  [elt] -> mkMDAliasOrValue elt
                  tag:rest -> mkMetadata tag rest
                mkMDAliasOrValue vref@(O.ValueRef name) = metaRef vref
                -- FIXME: Uncomment after implementing generic value translation
                -- mkMDAliasOrValue val = MetaNewValue (translate val)

                -- Here, subtract out the version information from the
                -- tag.
                mkMetadata tag components = case (getInt tag) - llvmDebugVersion of
                  11 -> N.MetaDWLexicalBlock { N.metaLexicalBlockRow = getInt (components !! 1)
                                             , N.metaLexicalBlockCol = getInt (components !! 2)
                                             , N.metaLexicalBlockContext = metaRef (components !! 0)
                                             }
                  17 -> N.MetaDWCompileUnit { N.metaCompileUnitLanguage = mkDwarfLang $ getInt (components !! 1)
                                            , N.metaCompileUnitSourceFile = getMDString (components !! 2)
                                            , N.metaCompileUnitCompileDir = getMDString (components !! 3)
                                            , N.metaCompileUnitProducer = getMDString (components !! 4)
                                            , N.metaCompileUnitIsMain = getBool (components !! 5)
                                            , N.metaCompileUnitIsOpt = getBool (components !! 6)
                                            , N.metaCompileUnitFlags = getMDString (components !! 7)
                                            , N.metaCompileUnitVersion = getInt (components !! 8)
                                            }
                  41 -> N.MetaDWFile { N.metaFileSourceFile = getMDString (components !! 0)
                                     , N.metaFileSourceDir = getMDString (components !! 1)
                                     , N.metaFileCompileUnit = metaRef (components !! 2)
                                     }
-- Notes on metadata blocks.  An MDNode containing just a reference to
-- other metadata can probably just be collapsed.  An MDNode
-- containing any other single value or reference is an argument to
-- llvm.dbg.value noting the new value of a variable.  Any other piece
-- of metadata (besides the source locations handled above) should
-- have an i32 tag as the first argument


getInt (O.ConstValue (O.ConstantInt i) (O.TypeInteger 32)) = i
getInt c = error ("Constant is not an int: " ++ show c)

getBool (O.ConstValue (O.ConstantInt i) (O.TypeInteger 1)) = i == 1
getBool c = error ("Constant is not a bool: " ++ show c)

getMDString (O.ConstValue (O.MDString txt) O.TypeMetadata) = txt
getMDString c = error ("Not a constant metadata string: " ++ show c)

extractModuleAssembly :: [O.GlobalDeclaration] -> [Assembly]
extractModuleAssembly decls = reverse $ foldr xtract [] decls
  where xtract decl acc = case decl of
          O.ModuleAssembly asm -> asm : acc
          _ -> acc

-- Returns a function that maps the identifiers of extern functions to
-- actual Function Values (if the identifier is indeed an external
-- function).
makeExternFunctionMap :: [O.GlobalDeclaration] -> (O.Type -> N.Type) -> Map Identifier N.Value
makeExternFunctionMap decls typeMapper = mapping -- flip M.lookup mapping
  where mapping = funcMapper decls M.empty
        funcMapper :: [O.GlobalDeclaration] -> Map Identifier N.Value -> Map Identifier N.Value
        funcMapper [] m = m
        funcMapper (d:rest) m = funcMapper rest $ case d of
          O.ExternalDecl t name ->
            M.insert name (mkFuncVal t name) m
          _ -> funcMapper rest m
          where mkFuncVal t name =
                  N.Value { N.valueType = typeMapper t
                          , N.valueName = Just name
                          , N.valueMetadata = Nothing
                          , N.valueContent = N.Function { N.functionType = typeMapper t
                                                        , N.functionParameters = Nothing
                                                        , N.functionBody = Nothing
                                                        }
                          }

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
          O.TypeNamed name -> mapping M.! name


