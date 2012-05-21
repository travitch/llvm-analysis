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
import Data.Foldable ( foldl', foldMap, toList )
import Data.Generics.Uniplate.Data
import Data.HashMap.Strict ( HashMap )
import qualified Data.HashMap.Strict as HM
import Data.HashSet ( HashSet )
import qualified Data.HashSet as HS
import Data.List ( stripPrefix )
import Data.Map ( Map )
import qualified Data.Map as M
import Data.Maybe ( mapMaybe )
import Data.Monoid
import Data.Set ( Set )
import qualified Data.Set as S
import Debug.Trace.LocationTH

import LLVM.Analysis

-- | The result of the class hierarchy analysis
data CHA = CHA { childrenMap :: HashMap Type (HashSet Type)
               , parentMap :: HashMap Type (HashSet Type)
               , vtblMap :: HashMap Type VTable
               }

instance Monoid CHA where
  mempty = CHA mempty mempty mempty
  mappend c1 c2 = CHA { childrenMap = HM.unionWith HS.union (childrenMap c1) (childrenMap c2)
                      , parentMap = HM.unionWith HS.union (parentMap c1) (parentMap c2)
                      , vtblMap = vtblMap c1 `mappend` vtblMap c2
                      }

-- | An interface for inspecting virtual function tables
data VTable = VTable { vtableValue :: GlobalVariable
                     , vtableBaseOffset :: Int
                     }

-- | List of all types derived from the given 'Type'.
classSubtypes :: CHA -> Type -> [Type]
classSubtypes cha t =
  toList (HM.lookupDefault mempty t (childrenMap cha))

-- | List of all types *transitively* drived from the given 'Type'
classTransitiveSubtypes :: CHA -> Type -> [Type]
classTransitiveSubtypes = transitiveTypes childrenMap

-- | List of the immediate parent types of the given 'Type'.  The list
-- is only empty for the root of a class hierarchy.
classParents :: CHA -> Type -> [Type]
classParents cha t =
  toList (HM.lookupDefault mempty t (parentMap cha))

-- | List of all (transitive) parent types of the given 'Type'.
classAncestors :: CHA -> Type -> [Type]
classAncestors = transitiveTypes parentMap

transitiveTypes :: (CHA -> HashMap Type (HashSet Type)) -> CHA -> Type -> [Type]
transitiveTypes selector cha t0 = toList (go (HS.singleton t0))
  where
    go ts = ts `mappend` foldMap getParents ts
    getParents t = HM.lookupDefault mempty t (selector cha)

-- | Retrieve the vtbl for a given type.  Will return Nothing if the
-- type is not a class or if the class has no virtual methods.
classVTable :: CHA -> Type -> Maybe VTable
classVTable cha t = HM.lookup t (vtblMap cha)

-- | Algorithm: Basically just examine constructors.  Each constructor
-- for a type encodes the type in the return value and the name.  The
-- constructor body assigns the vtbl (if any) AND calls base class
-- constructors.  Base class constructors must be called for ALL base
-- classes, per the language standard, so this gives us a full picture
-- of the class hierarchy.
runCHA :: Module -> CHA
runCHA = foldr recordParents mempty . moduleConstructors

moduleConstructors :: Module -> [Function]
moduleConstructors = filter isC2Constructor . moduleDefinedFunctions

-- | Determine which constructors this constructor calls.  If it calls
-- none, the type (first argument type (with the pointer stripped off)
-- is a base class.
recordParents :: Function -> CHA -> CHA
recordParents c2 acc =
  case null calledConstructors of
    True -> acc'
    False -> acc' { parentMap = HM.insertWith HS.union thisType (HS.fromList parentTypes) (parentMap acc')
                  , childrenMap = foldr (addChild thisType) (childrenMap acc') parentTypes
                  }
  where
    insts = functionInstructions c2
    calledConstructors = mapMaybe isConstructorCall insts
    vtbl = foldl' findVtbl Nothing insts
    thisType = constructedType c2
    parentTypes = map constructedType calledConstructors
    acc' = case vtbl of
      Nothing -> acc
      Just tbl -> acc { vtblMap = HM.insert thisType tbl (vtblMap acc) }

addChild :: Type -> Type -> HashMap Type (HashSet Type) -> HashMap Type (HashSet Type)
addChild thisType parentType =
  HM.insertWith HS.union parentType (HS.singleton thisType)

constructedType :: Function -> Type
constructedType f =
  case map argumentType $ functionParameters f of
    (TypePointer t@(TypeStruct (Just _) _ _) _):_ -> t
    t -> $failure ("Expected pointer to struct type: " ++ show t)

findVtbl :: Maybe VTable -> Instruction -> Maybe VTable
findVtbl j@(Just _) _ = j
findVtbl Nothing i =
  case i of
    StoreInst { storeValue = (valueContent ->
      ConstantC ConstantValue { constantInstruction =
        GetElementPtrInst { getElementPtrValue = (valueContent -> GlobalVariableC gv)
                          , getElementPtrIndices = [
                               (valueContent -> ConstantC ConstantInt { constantIntValue = 0 }),
                               (valueContent -> ConstantC ConstantInt { constantIntValue = offset })
                               ]
                          }})} ->
      case isVtable gv of
        False -> Nothing
        True -> Just $! VTable gv (fromIntegral offset)
    _ -> Nothing

isVtable :: GlobalVariable -> Bool
isVtable gv =
  case dname of
    Left _ -> False
    Right structuredName ->
      case universeBi structuredName of
        [VirtualTable _] -> True
        _ -> False
  where
    n = identifierAsString (globalVariableName gv)
    dname = demangleName n

isConstructorCall :: Instruction -> Maybe Function
isConstructorCall i =
  case i of
    CallInst { callFunction = (valueContent -> FunctionC f) } ->
      case isC2Constructor f of
        False -> Nothing
        True -> Just f
    InvokeInst { invokeFunction = (valueContent -> FunctionC f) } ->
      case isC2Constructor f of
        False -> Nothing
        True -> Just f
    _ -> Nothing

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


-- Testing

classHierarchyToTestFormat :: CHA -> Map String (Set String)
classHierarchyToTestFormat cha =
  foldr mapify mempty (HM.toList (childrenMap cha))
  where
    mapify (ty, subtypes) =
      let ss = S.map classToString (S.fromList (HS.toList subtypes))
      in M.insertWith (S.union) (classToString ty) ss
    classToString :: Type -> String
    classToString (TypeStruct (Just n) _ _) = maybe n id (stripPrefix "class." n)
    classToString t = $failure ("Expected named struct type: " ++ show t)