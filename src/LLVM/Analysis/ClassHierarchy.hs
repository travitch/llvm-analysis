{-# LANGUAGE ViewPatterns, TemplateHaskell #-}
-- | This module defines a class hierarchy analysis for C++.
--
-- This analysis operates entirely at the bitcode level and does not
-- rely on metadata.
--
-- The hierarchy analysis result only includes class instantiations in
-- the bitcode provided (i.e., it is most useful for whole-program
-- bitcodes).  Results for single compilation units will be
-- incomplete.
--
-- Also note that this analysis requires the input bitcode to be built
-- with C++ run-time type information.
module LLVM.Analysis.ClassHierarchy (
  -- * Types
  CHA,
  VTable,
  -- * Functions
  classSubtypes,
  classParents,
  classAncestors,
  classVTable,
  runCHA,
  -- * Testing
  classHierarchyToTestFormat
  ) where

import ABI.Itanium
import Data.Foldable ( foldMap, toList )
import Data.Generics.Uniplate.Data
import Data.List ( stripPrefix )
import Data.Map ( Map )
import qualified Data.Map as M
import Data.Maybe ( mapMaybe )
import Data.Monoid
import Data.Set ( Set )
import qualified Data.Set as S
import Data.Vector ( Vector )
import qualified Data.Vector as V
import Debug.Trace.LocationTH

import LLVM.Analysis
import LLVM.Analysis.Util.Names

-- | The result of the class hierarchy analysis
data CHA = CHA { childrenMap :: Map Name (Set Name)
                 -- ^ All classes derived from the class used as the map key
               , parentMap :: Map Name (Set Name)
                 -- ^ The parent classes of the map key
               , vtblMap :: Map Name VTable
                 -- ^ The virtual function pointer table for the map key
               , typeMapping :: Map Name Type
                 -- ^ A relation between ABI names and LLVM Types
               }

-- Note that all keys are by Name here.  The name is the name of the
-- class, with conversions done between LLVM types and Names
-- as-needed.  This is simplest because it isn't possible to get an
-- LLVM type at all stages of the analysis - those can only be
-- reconstructed after the entire module is analyzed.  Since LLVM
-- records namespace information in class type names, this process is
-- robust enough.

instance Monoid CHA where
  mempty = CHA mempty mempty mempty mempty
  mappend c1 c2 = CHA { childrenMap = M.unionWith S.union (childrenMap c1) (childrenMap c2)
                      , parentMap = M.unionWith S.union (parentMap c1) (parentMap c2)
                      , typeMapping = typeMapping c1 `mappend` typeMapping c2
                      , vtblMap = vtblMap c1 `mappend` vtblMap c2
                      }

-- | An interface for inspecting virtual function tables
data VTable = ExternalVTable
            | VTable (Vector Function)

-- | List of all types derived from the given 'Type'.
classSubtypes :: CHA -> Type -> [Type]
classSubtypes cha t =
  namesToTypes cha (M.findWithDefault mempty (typeToName t) (childrenMap cha))

-- | List of all types *transitively* drived from the given 'Type'
classTransitiveSubtypes :: CHA -> Type -> [Type]
classTransitiveSubtypes = transitiveTypes childrenMap

-- | List of the immediate parent types of the given 'Type'.  The list
-- is only empty for the root of a class hierarchy.
classParents :: CHA -> Type -> [Type]
classParents cha t =
  namesToTypes cha (M.findWithDefault mempty (typeToName t) (parentMap cha))

-- | List of all (transitive) parent types of the given 'Type'.
classAncestors :: CHA -> Type -> [Type]
classAncestors = transitiveTypes parentMap

transitiveTypes :: (CHA -> Map Name (Set Name)) -> CHA -> Type -> [Type]
transitiveTypes selector cha t0 =
  namesToTypes cha (go (S.singleton (typeToName t0)))
  where
    go ts = ts `mappend` foldMap getParents ts
    getParents t = M.findWithDefault mempty t (selector cha)

-- | Retrieve the vtbl for a given type.  Will return Nothing if the
-- type is not a class or if the class has no virtual methods.
classVTable :: CHA -> Type -> Maybe VTable
classVTable cha t = M.lookup (typeToName t) (vtblMap cha)

-- | The analysis reconstructs the class hierarchy by looking at
-- typeinfo structures (which are probably only generated when
-- compiling with run-time type information enabled).  It also finds
-- vtables by demangling the names of the vtables in the module.
runCHA :: Module -> CHA
runCHA m = foldr buildTypeMap cha0 ctors
  where
    gvs = moduleGlobalVariables m
    ctors = moduleConstructors m
    cha0 = foldr recordParents mempty gvs

moduleConstructors :: Module -> [Function]
moduleConstructors = filter isC2Constructor . moduleDefinedFunctions

-- | Fill in the mapping from Names to LLVM Types in the class
-- hierarchy analysis by examining the first argument of each
-- constructor.  This argument indicates the LLVM type of the type
-- being constructed; parsing the LLVM type name into a Name yields
-- the map key.
buildTypeMap :: Function -> CHA -> CHA
buildTypeMap f cha =
  case parseTypeName fname of
    Left e -> $failure e
    Right n ->
      cha { typeMapping = M.insert n t (typeMapping cha) }
  where
    t = constructedType f
    Just fname = case t of
      TypeStruct (Just tn) _ _ -> stripPrefix "class." tn
      _ -> $failure ("Expected class type: " ++ show t)

-- | Determine the parent classes for each class type (if any) and
-- record them in the class hierarchy analysis summary.  This
-- information is derived from the typeinfo structures.  Additionally,
-- record the vtable for each type.
recordParents :: GlobalVariable -> CHA -> CHA
recordParents gv acc =
  case dname of
    Left _ -> acc
    Right structuredName ->
      case structuredName of
        VirtualTable (ClassEnumType typeName) ->
          recordVTable acc typeName (globalVariableInitializer gv)
        VirtualTable tn -> $failure ("Expected a class name for virtual table: " ++ show tn)
        TypeInfo (ClassEnumType typeName) ->
          recordTypeInfo acc typeName (globalVariableInitializer gv)
        TypeInfo tn -> $failure ("Expected a class name for typeinfo: " ++ show tn)
        _ -> acc
  where
    n = identifierAsString (globalVariableName gv)
    dname = demangleName n

-- | Record the vtable by storing only the function pointers from the
recordVTable :: CHA -> Name -> Maybe Value -> CHA
recordVTable cha typeName Nothing =
  cha { vtblMap = M.insert typeName ExternalVTable (vtblMap cha) }
recordVTable cha typeName (Just v) =
  case valueContent' v of
    ConstantC (ConstantArray _ _ vs) ->
      cha { vtblMap = M.insert typeName (makeVTable vs) (vtblMap cha) }
    _ -> recordVTable cha typeName Nothing

-- | Build a VTable given the list of values in the vtable array.  The
-- actual vtable (as indexed) doesn't begin at index zero, so we drop
-- all of the values that are not functions, then take everything that
-- is.
makeVTable :: [Value] -> VTable
makeVTable =
  VTable . V.fromList . map unsafeToFunction . takeWhile isVTableFunctionType . dropWhile isVTableFunctionType

unsafeToFunction :: Value -> Function
unsafeToFunction v =
  case valueContent v of
    ConstantC ConstantValue {
      constantInstruction = BitcastInst {
         castedValue = (valueContent -> FunctionC f)}} -> f
    _ -> $failure ("Expected vtable function entry: " ++ show v)


isVTableFunctionType :: Value -> Bool
isVTableFunctionType v =
  case valueContent v of
    ConstantC ConstantValue {
      constantInstruction = BitcastInst {
         castedValue = (valueContent -> FunctionC _)}} -> True
    _ -> False

recordTypeInfo :: CHA -> Name -> Maybe Value -> CHA
recordTypeInfo cha _ Nothing = cha
recordTypeInfo cha name (Just tbl) =
  case valueContent tbl of
    ConstantC (ConstantStruct _ _ vs) ->
      let parentClassNames = mapMaybe toParentClassName vs
      in cha { parentMap = M.insertWith' S.union name (S.fromList parentClassNames) (parentMap cha)
             , childrenMap = foldr (addChild name) (childrenMap cha) parentClassNames
             }
    _ -> $failure ("Expected typeinfo literal " ++ show tbl)

toParentClassName :: Value -> Maybe Name
toParentClassName v =
  case valueContent v of
    ConstantC ConstantValue {
      constantInstruction = BitcastInst {
         castedValue = (valueContent -> GlobalVariableC GlobalVariable {
                           globalVariableName = gvn })}} ->
      case demangleName (identifierAsString gvn) of
        Left _ -> Nothing
        Right (TypeInfo (ClassEnumType n)) -> Just n
        _ -> Nothing
    _ -> Nothing

addChild :: Name -> Name -> Map Name (Set Name) -> Map Name (Set Name)
addChild thisType parentType =
  M.insertWith' S.union parentType (S.singleton thisType)

constructedType :: Function -> Type
constructedType f =
  case map argumentType $ functionParameters f of
    (TypePointer t@(TypeStruct (Just _) _ _) _):_ -> t
    t -> $failure ("Expected pointer to struct type: " ++ show t)

-- Helpers

-- | Determine if the given function is a C2 constructor or not.  C1
-- and C3 don't give us the information we want, so ignore them
isC2Constructor :: Function -> Bool
isC2Constructor f =
  case dname of
    Left _ -> False
    Right structuredName ->
      case universeBi structuredName of
        [C2] -> True
        _ -> False
  where
    n = identifierAsString (functionName f)
    dname = demangleName n

typeToName :: Type -> Name
typeToName (TypeStruct (Just n) _ _) =
  case parseTypeName n of
    Right tn -> tn
    Left e -> $failure e
typeToName t = $failure ("Expected named struct type: " ++ show t)

nameToString :: Name -> String
nameToString n = maybe errMsg id (unparseTypeName n)
  where
    errMsg = error ("Could not encode name as string: " ++ show n)

nameToType :: CHA -> Name -> Type
nameToType cha n = M.findWithDefault errMsg n (typeMapping cha)
  where
    errMsg = error ("Expected name in typeMapping for CHA: " ++ show n)

namesToTypes :: CHA -> Set Name -> [Type]
namesToTypes cha = map (nameToType cha) . toList


-- Testing

classHierarchyToTestFormat :: CHA -> Map String (Set String)
classHierarchyToTestFormat cha =
  foldr mapify mempty (M.toList (childrenMap cha))
  where
    mapify (ty, subtypes) =
      let ss = S.map nameToString subtypes
      in M.insertWith S.union (nameToString ty) ss
