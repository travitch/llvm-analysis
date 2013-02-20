{-# LANGUAGE ViewPatterns #-}
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
  resolveVirtualCallee,
  classSubtypes,
  classTransitiveSubtypes,
  classParents,
  classAncestors,
  classVTable,
  functionAtSlot,
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
import Data.Maybe ( fromMaybe, mapMaybe )
import Data.Monoid
import Data.Set ( Set )
import qualified Data.Set as S
import Data.Vector ( Vector, (!?) )
import qualified Data.Vector as V

import LLVM.Analysis hiding ( (!?) )
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
               , chaModule :: Module
                 -- ^ A saved reference to the module
               }

-- Note that all keys are by Name here.  The name is the name of the
-- class, with conversions done between LLVM types and Names
-- as-needed.  This is simplest because it isn't possible to get an
-- LLVM type at all stages of the analysis - those can only be
-- reconstructed after the entire module is analyzed.  Since LLVM
-- records namespace information in class type names, this process is
-- robust enough.

-- | An interface for inspecting virtual function tables
data VTable = ExternalVTable
            | VTable (Vector Function)
            deriving (Show)

resolveVirtualCallee :: CHA -> Instruction -> Maybe [Function]
resolveVirtualCallee cha i =
  case i of
    -- Resolve direct calls (note, this does not cover calls to
    -- external functions, unfortunately).
    CallInst { callFunction = (valueContent' -> FunctionC f) } -> Just [f]
    -- Resolve actual virtual dispatches.  Note that the first
    -- argument is always the @this@ pointer.
    CallInst { callFunction = (valueContent' -> InstructionC LoadInst { loadAddress = la })
             , callArguments = (thisVal, _) : _
             } ->
      virtualDispatch cha la thisVal
    InvokeInst { invokeFunction = (valueContent' -> FunctionC f) } -> Just [f]
    InvokeInst { invokeFunction = (valueContent' -> InstructionC LoadInst { loadAddress = la })
               , invokeArguments = (thisVal, _) : _
               } ->
      virtualDispatch cha la thisVal
    _ -> Nothing

-- | Dispatch to one of the vtable lookup strategies based on the
-- value that was loaded from the vtable.
virtualDispatch :: CHA -> Value -> Value -> Maybe [Function]
virtualDispatch cha loadAddr thisVal = do
  slotNumber <- getVFuncSlot cha loadAddr thisVal
  return $! mapMaybe (functionAtSlot slotNumber) vtbls
  where
    TypePointer thisType _ = valueType thisVal
    derivedTypes = classTransitiveSubtypes cha thisType
    vtbls = mapMaybe (classVTable cha) derivedTypes

-- | Identify the slot number of a virtual function call.  Basically,
-- work backwards from the starred instructions in the virtual
-- function call dispatch patterns:
--
-- clang:
--
--   %2 = bitcast %struct.Base* %0 to void (%struct.Base*)***
--   %vtable = load void (%struct.Base*)*** %2
--   %vfn = getelementptr inbounds void (%struct.Base*)** %vtable, i64 1
-- * %3 = load void (%struct.Base*)** %vfn
--   call void %3(%struct.Base* %0)
--
-- clang0:
--
--   %0 = bitcast %struct.Base* %b to void (%struct.Base*)***
--   %vtable = load void (%struct.Base*)*** %0
-- * %1 = load void (%struct.Base*)** %vtable
--   call void %1(%struct.Base* %b)
--
-- dragonegg:
--
--   %2 = getelementptr inbounds %struct.Base* %1, i32 0, i32 0
--   %3 = load i32 (...)*** %2, align 4
--   %4 = bitcast i32 (...)** %3 to i8*
--   %5 = getelementptr i8* %4, i32 4
--   %6 = bitcast i8* %5 to i32 (...)**
-- * %7 = load i32 (...)** %6, align 4
--   %8 = bitcast i32 (...)* %7 to void (%struct.Base*)*
--   call void %8(%struct.Base* %1)
--
-- dragonegg0 (first method slot):
--
--   %2 = getelementptr inbounds %struct.Base* %1, i32 0, i32 0
--   %3 = load i32 (...)*** %2, align 4
-- * %4 = load i32 (...)** %3, align 4
--   %5 = bitcast i32 (...)* %4 to void (%struct.Base*)*
--   call void %5(%struct.Base* %1)
getVFuncSlot :: CHA -> Value -> Value -> Maybe Int
getVFuncSlot cha loadAddr thisArg =
  case valueContent loadAddr of
    -- Clang style
    InstructionC GetElementPtrInst {
      getElementPtrIndices = [valueContent -> ConstantC ConstantInt { constantIntValue = slotNo }],
      getElementPtrValue =
        (valueContent -> InstructionC LoadInst {
            loadAddress =
               (valueContent -> InstructionC BitcastInst {
                   castedValue = thisPtr
                      })})} ->
      case thisArg == thisPtr of
        True -> return $! fromIntegral slotNo
        False -> Nothing
    InstructionC LoadInst {
      loadAddress = (valueContent -> InstructionC BitcastInst {
                        castedValue = base})} ->
      case thisArg == base of
        True -> return 0
        False -> Nothing
    -- Dragonegg0 style (slot 0 call)
    InstructionC LoadInst {
      loadAddress =
         (valueContent -> InstructionC GetElementPtrInst {
             getElementPtrIndices = [ valueContent -> ConstantC ConstantInt { constantIntValue = 0 }
                                    , valueContent -> ConstantC ConstantInt { constantIntValue = 0 }
                                    ],
             getElementPtrValue = thisPtr})} ->
      case thisArg == thisPtr of
        True -> return 0
        False -> Nothing
    -- Dragonegg general case
    InstructionC BitcastInst {
      castedValue =
         (valueContent -> InstructionC GetElementPtrInst {
             getElementPtrIndices = [valueContent -> ConstantC ConstantInt { constantIntValue = offset }],
             getElementPtrValue =
               (valueContent -> InstructionC BitcastInst {
                   castedValue =
                      (valueContent -> InstructionC LoadInst {
                          loadAddress =
                             (valueContent -> InstructionC GetElementPtrInst {
                                 getElementPtrIndices = [ valueContent -> ConstantC ConstantInt { constantIntValue = 0 }
                                                        , valueContent -> ConstantC ConstantInt { constantIntValue = 0 }
                                                        ],
                                 getElementPtrValue = thisPtr})})})})} ->
      case thisArg == thisPtr of
        True -> Just $! indexFromOffset cha (fromIntegral offset)
        False -> Nothing
    _ -> Nothing

indexFromOffset :: CHA -> Int -> Int
indexFromOffset cha bytes = (bytes * 8) `div` pointerBits
  where
    m = chaModule cha
    targetData = moduleDataLayout m
    pointerBits = alignmentPrefSize (targetPointerPrefs targetData)

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
    go ts =
      let nextLevel = foldMap getParents ts
      in case mempty == nextLevel of
        True -> ts
        False -> go nextLevel `mappend` ts
    getParents t = M.findWithDefault mempty t (selector cha)

-- | Retrieve the vtbl for a given type.  Will return Nothing if the
-- type is not a class or if the class has no virtual methods.
classVTable :: CHA -> Type -> Maybe VTable
classVTable cha t = M.lookup (typeToName t) (vtblMap cha)

-- | Get the function at the named slot in a vtable.  Returns Nothing
-- for external vtables.
functionAtSlot :: Int -> VTable -> Maybe Function
functionAtSlot _ ExternalVTable = Nothing
functionAtSlot slot (VTable v) = v !? slot

-- | The analysis reconstructs the class hierarchy by looking at
-- typeinfo structures (which are probably only generated when
-- compiling with run-time type information enabled).  It also finds
-- vtables by demangling the names of the vtables in the module.
runCHA :: Module -> CHA
runCHA m = foldr buildTypeMap cha1 ctors
  where
    gvs = moduleGlobalVariables m
    ctors = moduleConstructors m
    cha0 = CHA mempty mempty mempty mempty m
    cha1 = foldr recordParents cha0 gvs

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
    Left e -> error ("LLVM.Analysis.ClassHierarchy.buildTypeMap: " ++ e)
    Right n ->
      cha { typeMapping = M.insert n t (typeMapping cha) }
  where
    t = constructedType f
    fname = case t of
      TypeStruct (Right tn) _ _ -> stripNamePrefix tn
      _ -> error ("LLVM.Analysis.ClassHierarchy.buildTypeMap: Expected class type: " ++ show t)

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
        VirtualTable tn -> error ("LLVM.Analysis.ClassHierarchy.recordParents: Expected a class name for virtual table: " ++ show tn)
        TypeInfo (ClassEnumType typeName) ->
          recordTypeInfo acc typeName (globalVariableInitializer gv)
        TypeInfo tn -> error ("LLVM.Analysis.ClassHierarchy.recordParents: Expected a class name for typeinfo: " ++ show tn)
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
  VTable . V.fromList . map unsafeToFunction . takeWhile isVTableFunctionType . dropWhile (not . isVTableFunctionType)

unsafeToFunction :: Value -> Function
unsafeToFunction v =
  case valueContent' v of
    FunctionC f -> f
    _ -> error ("LLVM.Analysis.ClassHierarchy.unsafeToFunction: Expected vtable function entry: " ++ show v)


isVTableFunctionType :: Value -> Bool
isVTableFunctionType v =
  case valueContent' v of
    FunctionC _ -> True
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
    _ -> error ("LLVM.Analysis.ClassHierarchy.recordTypeInfo: Expected typeinfo literal " ++ show tbl)

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
    TypePointer t@(TypeStruct (Right _) _ _) _ : _ -> t
    t -> error ("LLVM.Analysis.ClassHierarchy.constructedType: Expected pointer to struct type: " ++ show t)

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

-- | Strip a prefix, operating as the identity if the input string did
-- not have the prefix.
stripPrefix' :: String -> String -> String
stripPrefix' pfx s = fromMaybe s (stripPrefix pfx s)

stripNamePrefix :: String -> String
stripNamePrefix =
  stripPrefix' "struct." . stripPrefix' "class."

typeToName :: Type -> Name
typeToName (TypeStruct (Right n) _ _) =
  case parseTypeName (stripNamePrefix n) of
    Right tn -> tn
    Left e -> error ("LLVM.Analysis.ClassHierarchy.typeToName: " ++ e)
typeToName t = error ("LLVM.Analysis.ClassHierarchy.typeToName: Expected named struct type: " ++ show t)

nameToString :: Name -> String
nameToString n = fromMaybe errMsg (unparseTypeName n)
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

{-# ANN module "HLint: ignore Use if" #-}
