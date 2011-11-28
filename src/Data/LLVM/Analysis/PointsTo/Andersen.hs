{-# LANGUAGE TemplateHaskell, OverloadedStrings, ScopedTypeVariables, BangPatterns #-}
-- | This is a simple implementation of Andersen's points-to analysis.
--
-- TODO:
--
-- * Add initial edges for struct/array constant initializers
--
-- * Add a PHI node test and a NULL pointer test
--
-- * Handle external functions (w/ hooks)
--
-- * Handle SelectInsts (of pointer type)
--
-- * Be more robust against type/memory-unsafe programs.
--
-- * Variable-length argument list functions
--
-- * Add field sensitivity eventually. See http://delivery.acm.org/10.1145/1300000/1290524/a4-pearce.pdf?ip=128.105.181.27&acc=ACTIVE%20SERVICE&CFID=52054919&CFTOKEN=71981976&__acm__=1320350342_65be4c25a6fba7e32d7b4cd60f13fe97
--
-- * On-the-fly allocator discovery
module Data.LLVM.Analysis.PointsTo.Andersen (
  -- * Types
  Andersen,
  -- * Constructor
  runPointsToAnalysis,
  -- * Debugging aids
  savePointsToGraph,
  viewPointsToGraph,
  ) where

import Control.Monad ( forM_, unless )
import Control.Monad.ST
import Data.ByteString.Char8 ( isPrefixOf )
import qualified Data.Graph.Inductive as G
import Data.GraphViz
import Data.IntMap ( IntMap )
import qualified Data.IntMap as IM
import Data.List ( find, foldl' )
import Data.Maybe ( mapMaybe, catMaybes )
import Data.Set ( Set )
import qualified Data.Set as S
import Data.HashSet ( HashSet )
import qualified Data.HashSet as HS
import Data.STRef

import Data.LLVM.Analysis.PointsTo
import Data.LLVM.Internal.PatriciaTree
import Data.LLVM.Internal.ImperativeGraph
import Data.LLVM.Internal.Worklist
import Data.LLVM.Types

import System.IO.Unsafe
import Text.Printf
import Debug.Trace
debug = flip trace

data NodeTag = PtrToLocation Value
             | Location Value
             | PtrToFunction Function
             | UnknownLocation
             deriving (Ord, Eq, Show)

-- data EdgeTag = DirectEdge
--              | ArrayEdge
--              | FieldAccessEdge !Int
--              deriving (Ord, Eq, Show)

type PTGEdge = (Node, Node, ())
type PTGNode = (Node, NodeTag)
type PTG = Gr NodeTag () -- EdgeTag

-- | Define a dependency graph.  The keys are the unique IDs of
-- locations.  The value for each ID is the set of instructions that
-- need to be re-processed when a new edge is added to the points-to graph
-- with that node ID as its source.
type DepGraph = IntMap (HashSet Instruction)

-- | State threaded through the ST monad.  Anything not indexed by the
-- s type variable is constant throughout.
--
-- * @ptWorklist@ is the current worklist
--
-- * @ptNextWorklist@ is actually a set of the Instructions that will
--   be flipped into the @ptWorklist@ once it is emptied.  When both
--   are empty, the algorithm is done
--
-- * @ptDepGraph@ tracks the dependencies between Instructions and
--   Nodes in the points-to graph.  When a new edge is added to a
--   node, all of the instructions that depend on it are re-processed
--
-- * @ptGraph@ is the mutable points-to graph that is being built
--
-- * @ptExternInfo@ is the list of functions that provide information
--   about external functions
--
-- * @ptIsAllocator@ is a list of predicates that recognize
--   call/invoke instructions as allocating unique memory locations
data PTState s = PTState { ptWorklist :: STRef s (Worklist Instruction)
                         , ptDepGraph :: STRef s DepGraph
                         , ptGraph :: ImperativeGraph s NodeTag
                         , ptExternInfo :: [ExternalFunction -> Maybe ExternFunctionDescriptor]
                         , ptIsAllocator :: [Instruction -> Bool]
                         }

-- | A wrapper around the analysis results.  This is opaque to the
-- user.
data Andersen = Andersen PTG

instance PointsToAnalysis Andersen where
  mayAlias (Andersen _) _ _ = True
  pointsTo = andersenPointsTo

-- | The points-to graph node representing the "unknown" memory
-- location.
unknownNode :: Node
unknownNode = -1

-- | The main entry point to see what a given value can point to by
-- just inspecting the points-to graph.
andersenPointsTo :: (IsValue a) => Andersen -> a -> PTResult PTRel
andersenPointsTo (Andersen g) v =
  case S.member unknownNode pointsToNodes of
    False -> PTSet $! toPTResult pointsToNodes
    True -> UniversalSet $! toPTResult $ S.delete unknownNode pointsToNodes
  where
    errMsg = error "No node in graph for andersenPointsTo"
    pointsToNodes = S.fromList $! G.suc g (valueUniqueId v)
    toPTResult = S.map (Direct . maybe errMsg unloc . G.lab g)


-- | Run the points-to analysis and return an object that is an
-- instance of PointsToAnalysis, which can be used to query the
-- results.
runPointsToAnalysis :: [Instruction -> Bool] -> Module -> Andersen
runPointsToAnalysis allocTests m = runST (runPTA allocTests m)

-- | The actual Andersen's algorithm in the ST monad (for efficiency).
-- This implementation uses mutable hash tables to represent the
-- graph.  FGL spends too much time re-allocating data because it is
-- inductive and because it tracks both outgoing and incoming edges.
-- This algorithm only requires outgoing edges for the construction of
-- the graph, so most of this effort is wasted.
--
-- The returned graph is a read-only version of the mutable graph that
-- is constructed; this version uses FGL and has incoming edges
-- explicitly recorded.
runPTA :: [Instruction -> Bool] -> Module -> ST s Andersen
runPTA allocTests m = do
  -- Set up the initial graph
  g <- newGraph
  forM_ allLocations $ \(n, lbl) -> addNode g n lbl
  forM_ initialEdges $ \(src, tgt) -> addEdges g src [tgt]

  -- Allocate the STRefs that will be modified during the computation
  worklistRef <- newSTRef (worklistFromList edgeInducers)
  dgRef <- newSTRef IM.empty
  let state = PTState { ptWorklist = worklistRef
                      , ptDepGraph = dgRef
                      , ptGraph = g
                      , ptExternInfo = []
                      , ptIsAllocator = allocTests
                      }

  -- Saturate the graph
  saturate state

  -- Extract a read-only version of the graph that can be used outside
  -- of the ST monad.
  ig <- toInductive g
  return $! Andersen ig
  where
    fs = moduleDefinedFunctions m
    blocks = concatMap functionBody fs
    insts = concatMap basicBlockInstructions blocks
    (globalLocations, globalEdges) = getGlobalLocations m
    argumentLocations = foldr extractArgs [] fs
    unknownLoc = (unknownNode, UnknownLocation)
    (localLocations, edgeInducers) = foldr extractLocations ([], []) insts
    allLocations = unknownLoc : concat [globalLocations, argumentLocations, localLocations]
    initialEdges = globalEdges

-- | Saturate the points-to graph using a worklist algorithm.  This is
-- the only function in the Module that should have unbounded
-- recursive calls.
saturate :: PTState s -> ST s ()
saturate state = do
  wl <- readSTRef (ptWorklist state)
  case takeWorkItem wl of
    EmptyWorklist -> return ()
    itm :< rest -> do
      writeSTRef (ptWorklist state) rest
      case itm of
        StoreInst { storeValue = val, storeAddress = dest } ->
          case isPointerOrFunction val of
            False -> saturate state
            True -> addStoreEdges state itm val dest >> saturate state
        CallInst { callFunction = cf, callArguments = args } ->
          addCallEdges state itm cf (map fst args) >> saturate state
        InvokeInst { invokeFunction = cf, invokeArguments = args } ->
          addCallEdges state itm cf (map fst args) >> saturate state
        RetInst { retInstValue = Just rv } ->
          addReturnEdges state itm rv >> saturate state
        _ -> error ("Unexpected instruction type in Andersen saturation: " ++ show itm)

-- | Returns can be handled similarly to arguments.  Neither has
-- actual storage associated with it, and returns are essentially just
-- parameters going in the other direction (the caller will copy the
-- value out).  The special return node (the node with the id of the
-- ret inst) gets an edge to every node referenced by the return
-- value.
addReturnEdges :: (Show a, IsValue a) => PTState s -> Instruction -> a -> ST s ()
addReturnEdges state retInst rv = do
  retLocs <- getLocationsReferencedBy state retInst rv
  makeNewEdges state [retNode] retLocs
  where
    retNode = instructionUniqueId retInst

-- | Add edges for calls to functions defined in the Module.
addDefinedFunctionCallEdges :: PTState s
                               -> Instruction -- ^ The CallInst
                               -> [Value] -- ^ Actual arguments
                               -> Function -- ^ A possible callee
                               -> ST s ()
addDefinedFunctionCallEdges state callInst args callee = do
  locMap <- mapM' (getLocs state) justPointers
  let g = ptGraph state
  retSuccs <- mapM' (nodeSuccessors g) retNodes
  let pointedToByRetNodes = concat retSuccs
      locMap' = case retNodes of
        [] -> locMap
        _ -> ([instructionUniqueId callInst], pointedToByRetNodes) : locMap
  mapM_ (addDependency state callInst) retNodes
  mapM_ (uncurry (makeNewEdges state)) locMap'
  where
    formals = functionParameters callee
    -- FIXME: This needs to change a bit for vararg functions - split
    -- into two lists and treat the trailing arguments as a single
    -- memory location
    actualFormalMap = zip args formals
    isRelevant = hasPointerOrFunction
    keepPointerParams = filter (isRelevant . fst)
    justPointers = keepPointerParams actualFormalMap

    -- Now find all possible return nodes for this instruction (one
    -- for each possible callee).  Add edges from the call node to all
    -- of the things that could be pointed to by the return node.
    retInst = functionExitInstruction callee
    retNodes = case isPointerOrFunction callInst of
      True -> [instructionUniqueId retInst]
      False -> []

    getLocs :: PTState s -> (Value, Argument) -> ST s ([Node], [Node])
    getLocs s (actual, formal) = do
      let formalLoc = argumentUniqueId formal
      actualLocs <- getLocationsReferencedBy s callInst actual
      addDependency s callInst formalLoc
      return $! ([formalLoc], actualLocs)

-- | A call is essentially a copy of a pointer in the caller to an
-- argument node (which will later be copied in the callee).
--
-- Start by zipping together the actual arguments and formal argument
-- lists.  Filter out the non-pointer entries.
addCallEdges :: PTState s -> Instruction -> Value -> [Value] -> ST s ()
addCallEdges state callInst calledFunc args = case valueContent calledFunc of
    FunctionC f -> addDefinedFunctionCallEdges state callInst args f

    -- Don't forget case for ExternalFunctionC and then handling both in the fold
    ExternalFunctionC e -> case externalIsIntrinsic e of
      False -> addExternalCallEdges state callInst args e
      True -> addIntrinsicEdges state callInst args e

    _ -> do
      funcLocs <- getLocationsReferencedBy state callInst calledFunc
      -- Add a dependency in the DepGraph on the node representing the
      -- functions that may be pointed to here.  This way, if we learn
      -- about other possible callees, we revisit this call.
      addDependency state callInst $! valueUniqueId calledFunc
      let g = ptGraph state
      calledFuncVals <- mapM' (nodeLabels g) funcLocs
      let handler n = case n of
            Nothing -> conservativeExternalHandler state callInst args Nothing
            Just UnknownLocation -> conservativeExternalHandler state callInst args Nothing
            Just v -> case valueContent (unloc v) of
              FunctionC f -> addDefinedFunctionCallEdges state callInst args f
              ExternalFunctionC ef -> case externalIsIntrinsic ef of
                True -> addIntrinsicEdges state callInst args ef
                False -> addExternalCallEdges state callInst args ef
              _ -> conservativeExternalHandler state callInst args Nothing
      mapM_ handler calledFuncVals

-- | Add edges for LLVM intrinsics.  Intrinsics that touch memory
-- through pointers are handled.  Intrinsics operating only on
-- primitive non-pointer types are no-ops.
--
-- FIXME: Does not currently handle @memset@
addIntrinsicEdges :: PTState s -> Instruction -> [Value] -> ExternalFunction -> ST s ()
addIntrinsicEdges state callInst args ef =
  case find ((`isPrefixOf` fname) . fst) handlerMap of
    Nothing -> return ()
    Just (_, handler) -> handler state callInst args >> return ()
  where
    fname = identifierContent (externalFunctionName ef)
    -- | Need to figure out how to handle memmove - it is more
    -- complicated and can, in the worst case, really shuffle up
    -- points-to relations
    --
    -- The va_* stuff also probably needs to be handled
    --
    -- memset shouldn't need to be handled since it can't create a
    -- "valid" address.
    --
    -- The other intrinsics only deal with non-pointer values and can
    -- probably be ignored.
    handlerMap = [ ("llvm.memcpy.", memcpyHandler)
--                 , ("llvm.memset.", undefined)
                 ]

-- | In handling memcpy, we only care about the first two arguments
-- (until we tackle field-sensitivity): <dest> and <src>.
memcpyHandler :: PTState s -> Instruction -> [Value] -> ST s ()
memcpyHandler state callInst args = do
  let dest : src : _ = args
  newSources <- getLocationsReferencedByOffset (0) state callInst dest
  newTargets <- getLocationsReferencedByOffset (1) state callInst src
  makeNewEdges state newSources newTargets

-- | FIXME: Implement by having a default conservative handler and
-- checking a new field in the PTState that enables analysis users to
-- provide information about external functions.
addExternalCallEdges :: PTState s -> Instruction -> [Value] -> ExternalFunction -> ST s ()
addExternalCallEdges state callInst args ef = do
  let extInfoHndlrs = ptExternInfo state
  case lookupDescriptor extInfoHndlrs of
    Nothing -> conservativeExternalHandler state callInst args (Just ef)
    Just h -> do
      let effects = argumentEffects h
      unless (length effects == length args) (error ("Mismatched effect length for " ++ show callInst))
      let mapping = zip args effects
          ptrMapping = filter (hasPointerOrFunction . fst) mapping
      -- Ignore arguments with no points-to effects
      return ()
  return ()
  where
    fname = identifierContent (externalFunctionName ef)
    lookupDescriptor hlist = foldl lookup' Nothing hlist
    lookup' descriptor hdlr = case descriptor of
      Just _ -> descriptor
      Nothing -> hdlr ef

-- | Conservatively handle external functions (or pointers to unknown
-- functions) that may be called, and for which extra information is
-- not provided.  This isn't currently as pessimistic as possible,
-- since that would always just return top for everything global.
--
-- * Any pointer parameter can point to anything.
--
-- * The return value can point to anything.
--
-- Technically any global can point to anything after the call, but
-- that isn't a very useful deduction...
conservativeExternalHandler :: PTState s
                               -> Instruction
                               -> [Value]
                               -> Maybe ExternalFunction
                               -> ST s ()
conservativeExternalHandler state callInst args ef = do
  let ptrArgs = filter isPointerOrFunction args
  newSources <- mapM' (getLocationsReferencedBy state callInst) ptrArgs
  makeNewEdges state (concat newSources) [unknownNode]

-- | Handle adding edges induced by a @StoreInst@.
addStoreEdges :: PTState s -> Instruction -> Value -> Value -> ST s ()
addStoreEdges state i val dest = do
  newTargets <- getLocationsReferencedBy state i val
  newSources <- getLocationsReferencedBy state i dest
  makeNewEdges state newSources newTargets

-- | Determine which edges need to be added to the graph, based on the
-- set of discovered sources and targets.  The edges not already in
-- the points-to graph are added.  Instructions that are affected by
-- the new edges (as determined by the Node/Instruction dependency
-- graph) are added to the next worklist.
makeNewEdges :: PTState s -> [Node] -> [Node] -> ST s ()
makeNewEdges state srcs targets = do
  let g = ptGraph state
      targetSet = HS.fromList targets
  -- For each source, add edges to the new targets.  Return the src
  -- if it was used.
  usedSrcs <- mapM' (addEdgesForSrc g targetSet) srcs

  -- Now figure out which instructions need to be added to the
  -- worklist because some of the new edges affected them
  depGraph <- readSTRef (ptDepGraph state)
  let newWorklistItems = affectedInstructions depGraph $! catMaybes usedSrcs
  nwl <- readSTRef (ptWorklist state)
  let nwl' = addWorkItems newWorklistItems nwl
  writeSTRef (ptWorklist state) nwl'
  where
    affectedInstructions :: DepGraph -> [Node] -> [Instruction]
    affectedInstructions depGraph used = {-# SCC "affectedInstructions" #-}
      let findAffected acc nodeId =
            HS.union acc $! IM.findWithDefault HS.empty nodeId depGraph
      in HS.toList $! foldl' findAffected HS.empty used

    addEdgesForSrc g targetSet src = {-# SCC "addEdgesForSrc" #-} do
      currentTargets <- nodeSuccessors g src
      let currentTargetSet = HS.fromList currentTargets
          newTargets = HS.difference targetSet currentTargetSet
      case HS.toList newTargets of
        [] -> return Nothing
        tgts -> do
          addEdges g src tgts
          return (Just src)

-- | Given a @Value@, find all of the locations in the points-to graph
-- that it refers to.  This means that the function starts at the
-- given value @v@ and _dereferences_ each @LoadInst@ in the @Value@.
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
getLocationsReferencedBy :: (Show a, IsValue a)
                            => PTState s
                            -> Instruction
                            -> a
                            -> ST s [Node]
getLocationsReferencedBy = getLocationsReferencedByOffset 0

getLocationsReferencedByOffset :: forall s a . (Show a, IsValue a)
                                  => Int
                                  -> PTState s
                                  -> Instruction
                                  -> a
                                  -> ST s [Node]
getLocationsReferencedByOffset offset state inst v = getLocs v 0 S.empty
  where
    getLocs :: (Show b, IsValue b) => b -> Int -> Set Value -> ST s [Node]
    getLocs val !derefCount visited
      | (Value val) `S.member` visited = return []
      | otherwise = {-# SCC "getLocs" #-}
        let visited' = S.insert (Value val) visited
        in case valueContent val of
          -- This also needs to handle GEP instructions later, also
          -- (maybe) select, extractelement, and extractvalue.  This also
          -- needs to be careful around bitcasts of non-pointer types
          -- (integers turned into pointers, for example)
          InstructionC (LoadInst { loadAddress = addr }) ->
            getLocs addr (derefCount + 1) visited'
          InstructionC (BitcastInst { castedValue = cv }) ->
            getLocs cv derefCount visited'
          InstructionC (IntToPtrInst {}) -> return [] -- FIXME: Taint
                                                     -- everything
                                                     -- transitively
                                                     -- touching this
                                                     -- monster
          InstructionC (GetElementPtrInst { getElementPtrValue = base
                                          , getElementPtrIndices = idxs
                                          }) ->
            getLocs base derefCount visited'
          InstructionC (PhiNode { phiIncomingValues = vs }) -> do
            let ivs = map fst vs
                getLocsForIncomingVal iv =
                  {-# SCC "getLocsForIncomingVal" #-} getLocs iv derefCount visited'
            locs <- mapM' getLocsForIncomingVal ivs
            return $! S.toList $ S.fromList (concat locs)
          InstructionC (SelectInst { selectTrueValue = tv, selectFalseValue = fv }) -> do
            tlocs <- getLocs tv derefCount visited'
            flocs <- getLocs fv derefCount visited'
            return $! S.toList $ S.fromList $ tlocs ++ flocs

          ConstantC (ConstantValue { constantInstruction =
                                        IntToPtrInst {}
                                   }) -> return [] -- FIXME: This also needs to taint everything
          ConstantC (ConstantPointerNull {}) -> return []
          ConstantC (UndefValue {}) -> return []
          ConstantC (ConstantValue { constantInstruction =
                                        GetElementPtrInst { getElementPtrValue = base
                                                          , getElementPtrIndices = idxs
                                                          }
                                   }) ->
            getLocs base derefCount visited'
          ConstantC (ConstantValue { constantInstruction =
                                        BitcastInst { castedValue = cv } }) ->
            getLocs cv derefCount visited'
          -- These locations are a bit special.  Unlike the others
          -- (globals, locals, etc), which represent pointers to
          -- locations, these are abstract locations that do not have
          -- storage.  They must not be dereferenced the "extra" time.
          InstructionC (ci@CallInst {}) -> case isAllocator ci of
            False -> collectLocationNodes state inst 1 [instructionUniqueId ci]
            True -> return [instructionUniqueId ci]
          InstructionC (ci@InvokeInst {}) -> case isAllocator ci of
            False -> collectLocationNodes state inst 1 [instructionUniqueId ci]
            True -> return [instructionUniqueId ci]
          ArgumentC a -> collectLocationNodes state inst 1 [argumentUniqueId a]

          -- In this fallback case, @val@ should be a node in the
          -- Points-to graph.  Collect everything @derefCount@ steps from
          -- it and return that, along with the updated DepGraph.  The
          -- depgraph needs to be updated with an entry for each non-leaf
          -- node.
          _ -> collectLocationNodes state inst (derefCount + offset) [valueUniqueId val]

    isAllocator i = let allocTests = ptIsAllocator state
                    in foldl' (\acc p -> acc || p i) False allocTests


-- | The call
--
-- > addDependency state inst n
--
-- Adds a dependency on @n@ for @inst@.  This means that, if any new
-- edges are added from @n@, @inst@ will be added to the worklist.
-- This way it will be re-processed and pick up the new edges.
addDependency :: PTState s -> Instruction -> Node -> ST s ()
addDependency state inst n = do
  s <- readSTRef (ptDepGraph state)
  let v = IM.lookup n s
  case v of
    Nothing -> writeSTRef (ptDepGraph state) $! IM.insert n (HS.singleton inst) s
    Just v' -> do
      let v'' = HS.insert inst v'
      v'' `seq` return ()
      writeSTRef (ptDepGraph state) $! IM.insert n v'' s

-- | Given a starting set of nodes, walk along all points-to edges for
-- the specified number of @steps@.  When there are no more steps to
-- take in the points-to graph, return the set of reached nodes.
--
-- > collectLocationNodes state inst steps seedNodes
--
-- The Instruction @inst@ is the instruction for which the query was
-- initiated.  It is used to track dependencies on nodes.
collectLocationNodes :: PTState s
                        -> Instruction
                        -> Int
                        -> [Node]
                        -> ST s [Node]
collectLocationNodes state inst steps seedNodes = collect steps seedNodes
  where
    collect remainingHops currentNodes
      | remainingHops <= 0 = return currentNodes
      | otherwise = do
        mapM_ (addDependency state inst) currentNodes
        let g = ptGraph state
        nextNodes <- mapM' (nodeSuccessors g) currentNodes
        let nextNodes' = concat nextNodes
        collect (remainingHops - 1) nextNodes'

-- Possible solution: If this is the last step (remainingHops == 1),
-- add a predicate to nodeSuccessors to restrict the results to the
-- required type.  The set of possible nodes here can become quite
-- large due to the lack of field-sensitivity.

-- | This is a version of @mapM@ that accumulates its results on the
-- heap instead of the stack.  This is necessary for dealing with
-- monadic actions over large lists.
mapM' :: Monad m => (a -> m b) -> [a] -> m [b]
mapM' f = go []
  where
    go acc [] = return $! (reverse acc)
    go acc (a:as) = do
      x <- f a
      x `seq` return ()
      go (x:acc) as


-- | Create location nodes for Instructions and accumulate a list of
-- Instructions that induce new points-to edges (calls, stores,
-- returns).  This function is meant to be used via a foldr.
--
-- Return instructions need to create a location (the pseudo-location
-- to communicate return values back to callers).  This doesn't have a
-- real location and should be treated similarly to arguments.
extractLocations :: Instruction
                    -> ([PTGNode], [Instruction])
                    -> ([PTGNode], [Instruction])
extractLocations i acc@(ptgNodes, insts) = case i of
  AllocaInst { instructionUniqueId = uid } ->
    ((uid, PtrToLocation (Value i)) : ptgNodes, insts)
  StoreInst {} -> (ptgNodes, i : insts)

  -- Non-void typed calls (i.e., calls that return values) need to
  -- have a node in the points-to graph representing the returned
  -- value, too.
  CallInst {} ->
    case isPointerOrFunction i of
      False -> (ptgNodes, i : insts)
      True -> ((instructionUniqueId i, Location (Value i)) : ptgNodes, i : insts)
  InvokeInst {} ->
    case isPointerOrFunction i of
      False -> (ptgNodes, i : insts)
      True -> ((instructionUniqueId i, Location (Value i)) : ptgNodes, i : insts)

  -- Only handle RetInsts if they return a pointer or function value
  RetInst { retInstValue = Just rv, instructionUniqueId = uid } ->
    case isPointerOrFunction rv of
      True -> ((uid, Location (Value i)) : ptgNodes, i : insts)
      False -> (ptgNodes, insts)
  _ -> acc


-- | This only re-allocates the small part of the list each iteration,
-- so should remain efficient.
extractArgs :: Function -> [PTGNode] -> [PTGNode]
extractArgs f nacc = map argToNode (functionParameters f) ++ nacc
  where
    argToNode a = (argumentUniqueId a, Location (Value a))

-- | Collect all of the global entities representing locations in the
-- Module.  This includes global variables, functions, and their
-- extern variants.  It also includes initial edges induced by
-- initializers for global variables.
--
-- FIXME: Handle pointers contained in array and struct literal
-- intializers.
getGlobalLocations :: Module -> ([PTGNode], [(Node, Node)])
getGlobalLocations m = (concat [es, gs, efs, fs], gedges)
  where
    -- Only add edges for globals that have initializers that are the
    -- address of another value.  Constants aren't very interesting
    -- here.
    makeGlobalEdge gv = case globalVariableInitializer gv of
      Nothing -> Nothing
      Just i -> case valueContent i of
        ConstantC _ -> Nothing
        _ -> Just (globalVariableUniqueId gv, valueUniqueId i)
    makeGlobalLocation idExtractor val = (idExtractor val, PtrToLocation (Value val))
    makeFunction val = (functionUniqueId val, PtrToFunction val)
    externVals = moduleExternalValues m
    globalVals = moduleGlobalVariables m
    externFuncs = moduleExternalFunctions m
    funcs = moduleDefinedFunctions m
    es = map (makeGlobalLocation externalValueUniqueId) externVals
    gs = map (makeGlobalLocation globalVariableUniqueId) globalVals
    efs = map (makeGlobalLocation externalFunctionUniqueId) externFuncs
    fs = map makeFunction funcs
    gedges = mapMaybe makeGlobalEdge globalVals

-- Little helpers

isPointerType :: Type -> Bool
isPointerType (TypePointer _ _) = True
isPointerType (TypeNamed _ it) = isPointerType it
isPointerType _ = False

isFunctionType :: Type -> Bool
isFunctionType (TypeFunction _ _ _) = True
isFunctionType (TypeNamed _ t) = isFunctionType t
isFunctionType _ = False

isPointerOrFunctionType :: Type -> Bool
isPointerOrFunctionType t = isPointerType t || isFunctionType t

isPointerOrFunction :: IsValue a => a -> Bool
isPointerOrFunction v = isPointerOrFunctionType (valueType v)

hasPointerOrFunctionType :: Type -> Bool
hasPointerOrFunctionType t =
  case t of
    TypePointer _ _ -> True
    TypeNamed _ it -> hasPointerOrFunctionType it
    TypeFunction _ _ _ -> True
    TypeArray _ t' -> hasPointerOrFunctionType t'
    TypeVector _ t' -> hasPointerOrFunctionType t'
    TypeStruct ts _ -> any hasPointerOrFunctionType ts
    _ -> False


hasPointerOrFunction :: IsValue a => a -> Bool
hasPointerOrFunction = hasPointerOrFunctionType . valueType

unloc :: NodeTag -> Value
unloc (Location l) = l
unloc (PtrToLocation l) = l
unloc (PtrToFunction f) = (Value f)


-- Debugging visualization stuff

instance Labellable NodeTag where
  toLabelValue (Location v) = toLabelValue v
  toLabelValue (PtrToLocation v) = toLabelValue v
  toLabelValue (PtrToFunction f) = toLabelValue (Value f)
  toLabelValue UnknownLocation = toLabelValue ("Unknown" :: String)

-- instance Labellable EdgeTag where
--   toLabelValue DirectEdge = toLabelValue ("" :: String)
--   toLabelValue ArrayEdge = toLabelValue ("[*]" :: String)
--   toLabelValue (FieldAccessEdge ix) = toLabelValue $ concat [".<", show ix, ">"]

pointsToParams :: GraphvizParams n NodeTag () () NodeTag
pointsToParams = nonClusteredParams { fmtNode = \(_,l) -> [toLabel l]
                                    , fmtEdge = \(_,_,l) -> [toLabel ("" :: String)]
                                    }


viewPointsToGraph :: Andersen -> IO ()
viewPointsToGraph (Andersen g) = do
  let dg = graphToDot pointsToParams g
  _ <- runGraphvizCanvas' dg Gtk
  return ()

savePointsToGraph :: Andersen -> FilePath -> IO ()
savePointsToGraph (Andersen g) fp = do
  let dg = graphToDot pointsToParams g
  _ <- runGraphvizCommand Dot dg XDot fp
  return ()

debugGraph :: a -> PTG -> a
debugGraph v g = unsafePerformIO $ do
  viewPointsToGraph (Andersen g)
  return v
{-
showContextList :: String -> PTG -> [Context NodeTag ()] -> String
showContextList tag g = (tag'++) . unlines . concatMap toS
  where
    tag' = tag ++ "\n"
    toS (_, _, lbl, adjOut) = map (printf "%s -> %s" (show lbl)) (map (show . lab g . snd) adjOut)

showNodeLabels :: String -> PTG -> [Node] -> String
showNodeLabels tag g = (tag'++) . unlines . map toS
  where
    tag' = tag ++ "\n"
    toS n = let Just lbl = lab g n
            in "  " ++ show lbl
-}