module Data.LLVM.Analysis.PointsTo.Andersen (
  -- * Types
  Andersen,
  -- * Constructor
  runPointsToAnalysis
  ) where

import Data.Graph.Inductive hiding ( Gr, (><) )
import Data.IntMap ( IntMap )
import qualified Data.IntMap as IM
import Data.List ( foldl' )
import Data.Set ( Set )
import qualified Data.Set as S
import Data.Sequence ( Seq, (><), ViewL(..), viewl )
import qualified Data.Sequence as Seq


import Data.LLVM.Analysis.PointsTo
import Data.LLVM.Internal.PatriciaTree
import Data.LLVM.Types

data NodeTag = Location Value
             | Argument Value
             | DirectCall Value
             | IndirectCall Value
             deriving (Ord, Eq)

type PTGNode = (Int, NodeTag)
type PTG = Gr NodeTag ()
type Worklist = Seq Instruction
-- | Define a dependency graph.  The keys are the unique IDs of
-- locations.  The value for each ID is the set of instructions that
-- need to be re-processed when a new edge is added to that node id in
-- the points-to graph.
type DepGraph = IntMap (Set Instruction)

data Andersen = Andersen PTG

instance PointsToAnalysis Andersen where
  mayAlias (Andersen g) v1 v2 = True
  pointsTo (Andersen g) v = S.empty

-- | Run the points-to analysis and return an object that is an
-- instance of PointsToAnalysis, which can be used to query the
-- results.
runPointsToAnalysis :: Module -> Andersen
runPointsToAnalysis m = Andersen g
  where
    fs = moduleDefinedFunctions m
    blocks = concatMap functionBody fs
    insts = concatMap basicBlockInstructions blocks
    globalLocations = getGlobalLocations m
    argumentLocations = foldr extractArgs [] fs
    (localLocations, edgeInducers) = foldr extractLocations ([], []) insts
    allLocations = concat [globalLocations, argumentLocations, localLocations]
    graph0 = mkGraph allLocations []
    worklist0 = Seq.fromList edgeInducers
    g = saturate IM.empty worklist0 graph0

extractLocations :: Instruction
                    -> ([PTGNode], [Instruction])
                    -> ([PTGNode], [Instruction])
extractLocations i acc@(ptgNodes, insts) = case i of
  AllocaInst { instructionUniqueId = uid } ->
    ((uid, Location (Value i)) : ptgNodes, insts)
  StoreInst {} -> (ptgNodes, i : insts)
  CallInst {} -> (ptgNodes, i : insts)
  InvokeInst {} -> (ptgNodes, i : insts)
  _ -> acc

-- The initial worklist should be all of the store instructions and
-- calls?  But how do you identify affected stores/calls when adding
-- edges?

-- | Saturate the points-to graph using a worklist algorithm.
saturate :: DepGraph -> Worklist -> PTG -> PTG
saturate dg worklist g = case viewl worklist of
  EmptyL -> g
  itm :< rest -> case itm of
    StoreInst { storeValue = val, storeAddress = dest } ->
      case isPointerType (valueType val) of
        False -> saturate dg rest g
        True -> addStoreEdges dg itm val dest rest g
    CallInst {} -> saturate dg rest g
    InvokeInst {} -> saturate dg rest g
    _ -> error ("Unexpected instruction type: " ++ show itm)


-- | Handle adding edges induced by a @StoreInst@.
addStoreEdges :: DepGraph -> Instruction -> Value -> Value -> Worklist -> PTG -> PTG
addStoreEdges dg i val dest worklist g =
  saturate dg2 worklist' g'
  where
    (dg1, newTargets, depHits1) = getLocationsReferencedBy dg val g
    (dg2, newSrcs, depHits2) = getLocationsReferencedBy dg1 dest g
    newEdges = makeNewEdges g newSrcs newTargets
    worklist' = worklist >< Seq.fromList (depHits1 ++ depHits2)
    g' = foldl' (flip (&)) g newEdges

makeNewEdges :: PTG -> [Node] -> [Node] -> [Context NodeTag ()]
makeNewEdges g newSrcs newTargets =
  map toContext allPairs
  where
    toContext (t, s) = let Just lbl = lab g s
                       in ([], s, lbl, [((), t)])
    allPairs = concatMap (zip newTargets . repeat) newSrcs

getLocationsReferencedBy :: DepGraph -> Value -> PTG -> (DepGraph, [Node], [Instruction])
getLocationsReferencedBy dg v g = getLocs v [valueUniqueId v]
  where
    getLocs :: Value -> [Node] -> (DepGraph, [Node], [Instruction])
    getLocs val locs = case valueContent val of
      InstructionC i -> dispatchInstruction i locs
      _ -> (dg, locs, [])
    -- For field sensitivity here, handle GetElementPtrInst (ignoring
    -- for now, which is unsound)
    dispatchInstruction i locs = case i of
      -- Just walk right through bitcasts
      BitcastInst { castedValue = cv } -> getLocs cv locs
      -- For loads, replace locs with everything pointed to in g by
      -- locs
      LoadInst { loadAddress = addr } ->
        let newLocs = concatMap (suc g) locs
        in getLocs addr newLocs

-- | This only re-allocates the small part of the list each iteration,
-- so should remain efficient.
extractArgs :: Function -> [PTGNode] -> [PTGNode]
extractArgs f acc = concat [map argToNode (functionParameters f), acc]
  where
    argToNode a = (argumentUniqueId a, Location (Value a))

getGlobalLocations :: Module -> [PTGNode]
getGlobalLocations m = es ++ gs
  where
    makeGlobalLocation idExtractor val = (idExtractor val, Location (Value val))
    externVals = moduleExternalValues m
    globalVals = moduleGlobalVariables m
    es = map (makeGlobalLocation externalValueUniqueId) externVals
    gs = map (makeGlobalLocation globalVariableUniqueId) globalVals

isPointerType :: Type -> Bool
isPointerType (TypePointer it _) = True
isPointerType (TypeNamed _ it) = isPointerType it
isPointerType _ = False

