module Data.LLVM.Analysis.PointsTo.Andersen (
  -- * Types
  Andersen,
  -- * Constructor
  runPointsToAnalysis,
  -- * Debugging aids
  viewPointsToGraph,
  ) where

import Data.Graph.Inductive hiding ( Gr, (><) )
import Data.GraphViz
import Data.IntMap ( IntMap )
import qualified Data.IntMap as IM
import Data.List ( foldl' )
import Data.Maybe ( mapMaybe )
import Data.Set ( Set )
import qualified Data.Set as S
import Data.Sequence ( Seq, (><), ViewL(..), viewl )
import qualified Data.Sequence as Seq

import Data.LLVM.Analysis.PointsTo
import Data.LLVM.Internal.PatriciaTree
import Data.LLVM.Types

import Text.Printf
import Debug.Trace
debug = flip trace

data NodeTag = Location Value
             -- | Argument Value
             -- | DirectCall Value
             -- | IndirectCall Value
             deriving (Ord, Eq, Show)

type PTGEdge = (Int, Int, ())
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
  pointsTo = andersenPointsTo

andersenPointsTo :: (IsValue a) => Andersen -> a -> Set Value
andersenPointsTo (Andersen g) v =
  S.fromList $ map (unloc . lab g) (suc g (valueUniqueId v))
  where
    unloc (Just (Location l)) = l


-- | Run the points-to analysis and return an object that is an
-- instance of PointsToAnalysis, which can be used to query the
-- results.
runPointsToAnalysis :: Module -> Andersen
runPointsToAnalysis m = Andersen g
  where
    fs = moduleDefinedFunctions m
    blocks = concatMap functionBody fs
    insts = concatMap basicBlockInstructions blocks
    (globalLocations, globalEdges) = getGlobalLocations m
    argumentLocations = foldr extractArgs [] fs
    (localLocations, edgeInducers) = foldr extractLocations ([], []) insts
    allLocations = concat [globalLocations, argumentLocations, localLocations]
    -- The initial graph contains all locations in the program, along
    -- with edges induced by global initializers.
    graph0 = mkGraph allLocations globalEdges `debug` printf "Points-to graph has: %s\n" (show allLocations)
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
    -- IMPORTANT: Note that we need to propagate dependencies (the
    -- DepGraph) changes even if we didn't add any edges this time.
    -- Edges could be added later and we need to know all of the
    -- possible dependencies to come back.
    True -> saturate dg2 worklist g
  where
    accumUsedSrcs acc (_, src, _, _) = S.insert src acc
    (dg1, newTargets) = getLocationsReferencedByStore dg i val g
    (dg2, newSrcs) = getLocationsReferencedByStore dg1 i dest g
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
    toContext (src, tgt) =
      let (adjIn, n, lbl, adjOut) = context g src
      in (adjIn, n, lbl, ((), tgt) : adjOut)
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
--
-- Algorithm: walk the chain of loads/geps/bitcasts to find the
-- location being loaded from.  Keep track of the number of
-- dereferences (loads).  Take that many steps (across all outgoing
-- edges) from the location in the points-to graph.
getLocationsReferencedByStore :: DepGraph
                                 -> Instruction
                                 -> Value
                                 -> PTG
                                 -> (DepGraph, [Node])
getLocationsReferencedByStore dg0 storeInst v g = getLocs v 0
  where
    getLocs :: Value -> Int -> (DepGraph, [Node])
    getLocs val derefCount = case valueContent val of
      -- This also needs to handle GEP instructions later, also
      -- (maybe) select, extractelement, and extractvalue.  This also
      -- needs to be careful around bitcasts of non-pointer types
      -- (integers turned into pointers, for example)
      InstructionC (LoadInst { loadAddress = addr }) ->
        getLocs addr (derefCount + 1)
        -- let dg' = IM.insertWith S.union (valueUniqueId addr) (S.singleton storeInst) dg
        -- in
      InstructionC (BitcastInst { castedValue = cv }) ->
        getLocs cv derefCount
      -- In this fallback case, @val@ should be a node in the
      -- Points-to graph.  Collect everything @derefCount@ steps from
      -- it and return that, along with the updated DepGraph.  The
      -- depgraph needs to be updated with an entry for each non-leaf
      -- node.
      _ -> collectLocationNodes g dg0 storeInst val derefCount

collectLocationNodes :: PTG -> DepGraph -> Instruction -> Value -> Int -> (DepGraph, [Node])
collectLocationNodes g dg0 storeInst val derefCount =
  collect dg0 derefCount [valueUniqueId val]
  where
    addDep dg n = IM.insertWith S.union n (S.singleton storeInst) dg
    collect dg 0 resultNodes = (dg, resultNodes)
    collect dg remainingHops startNodes =
      let nextNodes = concatMap (suc g) startNodes
          dg' = foldl' addDep dg startNodes
      in collect dg' (remainingHops - 1) nextNodes


-- | This only re-allocates the small part of the list each iteration,
-- so should remain efficient.
extractArgs :: Function -> [PTGNode] -> [PTGNode]
extractArgs f acc = concat [map argToNode (functionParameters f), acc]
  where
    argToNode a = (argumentUniqueId a, Location (Value a))

-- | Collect all of the global entities representing locations in the
-- Module
getGlobalLocations :: Module -> ([PTGNode], [PTGEdge])
getGlobalLocations m = (concat [es, gs, efs, fs], gedges)
  where
    -- Only add edges for globals that have initializers that are the
    -- address of another value.  Constants aren't very interesting
    -- here.
    makeGlobalEdge gv = case globalVariableInitializer gv of
      Nothing -> Nothing
      Just i -> case valueContent i of
        ConstantC _ -> Nothing
        _ -> Just (globalVariableUniqueId gv, valueUniqueId i, ())
    makeGlobalLocation idExtractor val = (idExtractor val, Location (Value val))
    externVals = moduleExternalValues m
    globalVals = moduleGlobalVariables m
    externFuncs = moduleExternalFunctions m
    funcs = moduleDefinedFunctions m
    es = map (makeGlobalLocation externalValueUniqueId) externVals
    gs = map (makeGlobalLocation globalVariableUniqueId) globalVals
    efs = map (makeGlobalLocation externalFunctionUniqueId) externFuncs
    fs = map (makeGlobalLocation functionUniqueId) funcs
    gedges = mapMaybe makeGlobalEdge globalVals

isPointerType :: Type -> Bool
isPointerType (TypePointer _ _) = True
isPointerType (TypeNamed _ it) = isPointerType it
isPointerType _ = False



-- Debugging visualization stuff

instance Labellable NodeTag where
  toLabelValue (Location v) = toLabelValue v

pointsToParams = nonClusteredParams { fmtNode = \(_,l) -> [toLabel l]
                                    , fmtEdge = \(_,_,_) -> [toLabel ""]
                                    }

viewPointsToGraph :: Andersen -> IO ()
viewPointsToGraph (Andersen g) = do
  let dg = graphToDot pointsToParams g
  _ <- runGraphvizCanvas' dg Gtk
  return ()
