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
             -- | Argument Value
             -- | DirectCall Value
             -- | IndirectCall Value
             deriving (Ord, Eq)

type PTGNode = (Int, NodeTag)
type PTG = Gr NodeTag ()
type Worklist = Seq Instruction
-- | Define a dependency graph.  The keys are the unique IDs of
-- locations.  The value for each ID is the set of instructions that
-- need to be re-processed when a new edge is added to the points-to graph
-- with that node ID as its source.
type DepGraph = IntMap (Set Instruction)

data Andersen = Andersen PTG

instance PointsToAnalysis Andersen where
  mayAlias (Andersen _) _ _ = True
  pointsTo (Andersen _) _ = S.empty

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
  -- Only update g and the worklist if there were new edges added.
  case null newEdges of
    False -> saturate dg2 worklist' g'
    True -> saturate dg worklist g
  where
    accumUsedSrcs acc (_, src, _, _) = S.insert src acc
    (dg1, newTargets) = getLocationsReferencedBy dg i val g
    (dg2, newSrcs) = getLocationsReferencedBy dg1 i dest g
    newEdges = makeNewEdges g newSrcs newTargets
    usedSrcs = foldl accumUsedSrcs S.empty newEdges
    worklist' = worklist >< Seq.fromList (affectedInstructions usedSrcs dg2)
    g' = foldl' (flip (&)) g newEdges

affectedInstructions :: Set Int -> DepGraph -> [Instruction]
affectedInstructions usedSrcs dg = S.toList instSet
  where
    instSet = S.fold findAffected S.empty usedSrcs
    findAffected nodeId acc = S.union acc (IM.findWithDefault S.empty nodeId dg)

-- | Determine which edges need to be added to the graph, based on the
-- set of discovered sources and targets.  Only new edges are
-- returned.
makeNewEdges :: PTG -> [Node] -> [Node] -> [Context NodeTag ()]
makeNewEdges g newSrcs newTargets =
  map toContext newPairs
  where
    notInGraph (src, tgt) = notElem tgt (suc g src)
    toContext (src, tgt) = let Just lbl = lab g src
                           in ([], src, lbl, [((), tgt)])
    allPairs = concatMap (zip newSrcs . repeat) newTargets
    newPairs = filter notInGraph allPairs

-- | Given a @Value@ that is an operand of a @StoreInst@, find all of
-- the locations in the points-to graph that it refers to.  This means
-- that the function starts at the given value @v@ and _dereferences_
-- each @LoadInst@ in the @Value@.
--
-- As it progresses, the function also updates the DepGraph that
-- allows for reverse lookups (which instructions depend on the edges
-- coming from a given node).  This DepGraph is used to determine
-- which instructions need to be added to the Worklist due to
-- additional edges added to the points-to graph each iteration.  Note
-- that the DepGraph should be updated *before* it is used to compute
-- the list of affected instructions.
--
-- This function treats BitcastInsts as no-ops (i.e., it assumes the
-- types work out and only deals with memory references).
getLocationsReferencedBy :: DepGraph -> Instruction -> Value -> PTG -> (DepGraph, [Node])
getLocationsReferencedBy dg0 inst v g = getLocs dg0 v [valueUniqueId v]
  where
    getLocs :: DepGraph -> Value -> [Node] -> (DepGraph, [Node])
    getLocs dg val locs = case valueContent val of
      InstructionC i -> dispatchInstruction dg i locs
      _ -> (dg, locs)
    -- For field sensitivity here, handle GetElementPtrInst (ignoring
    -- for now, which is unsound)
    dispatchInstruction dg i locs = case i of
      -- Just walk right through bitcasts
      BitcastInst { castedValue = cv } -> getLocs dg cv locs
      -- For loads, replace locs with everything pointed to in g by
      -- locs
      LoadInst { loadAddress = addr } ->
        let newLocs = concatMap (suc g) locs
            dg' = IM.insertWith S.union (valueUniqueId addr) (S.singleton inst) dg
        in getLocs dg' addr newLocs

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
isPointerType (TypePointer _ _) = True
isPointerType (TypeNamed _ it) = isPointerType it
isPointerType _ = False

