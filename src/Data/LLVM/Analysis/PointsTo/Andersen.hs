{-# LANGUAGE TemplateHaskell, OverloadedStrings, ScopedTypeVariables #-}
-- | This is a simple implementation of Andersen's points-to analysis.
--
-- TODO:
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

-- import Control.Monad.State
import Control.DeepSeq
import Control.Monad ( forM_, forM, unless )
import Control.Monad.ST
import Data.ByteString.Char8 ( isPrefixOf )
import qualified Data.Graph.Inductive as G -- hiding ( Gr, (><) )
import Data.GraphViz
import Data.IntMap ( IntMap )
import qualified Data.IntMap as IM
-- import Data.Lens.Lazy
-- import Data.Lens.Template
import Data.List ( find, foldl' )
import Data.Maybe ( mapMaybe, catMaybes )
import Data.Set ( Set )
import qualified Data.Set as S
import qualified Data.HashSet as HS
import Data.STRef

import Data.LLVM.Analysis.PointsTo
import Data.LLVM.Internal.PatriciaTree
import Data.LLVM.Internal.ImperativeGraph
import Data.LLVM.Types

import System.IO.Unsafe
import Text.Printf
import Debug.Trace
debug = flip trace

data NodeTag = PtrToLocation Value
             | Location Value
             | PtrToFunction Function
             deriving (Ord, Eq, Show)

instance NFData NodeTag where
  rnf a@(PtrToLocation v) = v `seq` a `seq` ()
  rnf a@(Location v) = v `seq` a `seq` ()
  rnf a@(PtrToFunction v) = v `seq` a `seq` ()

-- data EdgeTag = DirectEdge
--              | ArrayEdge
--              | FieldAccessEdge !Int
--              deriving (Ord, Eq, Show)

type PTGEdge = (Node, Node, ())
type PTGNode = (Node, NodeTag)
type PTG = Gr NodeTag () -- EdgeTag
type Worklist = [Instruction]
-- | Define a dependency graph.  The keys are the unique IDs of
-- locations.  The value for each ID is the set of instructions that
-- need to be re-processed when a new edge is added to the points-to graph
-- with that node ID as its source.
type DepGraph = IntMap (Set Instruction)


-- data PTState = PTState { _ptWorklist :: Worklist
--                        , _ptNextWorklist :: Set Instruction
--                        , _ptDepGraph :: DepGraph
--                        , _ptGraph :: PTG
--                        , _ptExternInfo :: [ExternalFunction -> Maybe ExternFunctionDescriptor]
--                        , _ptIsAllocator :: [Instruction -> Bool]
--                        }
-- $(makeLenses [''PTState])

-- type PTMonad = ST

-- | A wrapper around the analysis results.  This is opaque to the
-- user.
data Andersen = Andersen PTG

instance PointsToAnalysis Andersen where
  mayAlias (Andersen _) _ _ = True
  pointsTo = andersenPointsTo


-- | The main entry point to see what a given value can point to by
-- just inspecting the points-to graph.
andersenPointsTo :: (IsValue a) => Andersen -> a -> Set PTRel
andersenPointsTo (Andersen g) v = S.fromList $ map Direct nodeValues
  where
    errMsg = error "No node in graph for andersenPointsTo"
    pointsToNodes = G.suc g (valueUniqueId v)
    nodeValues = map (maybe errMsg unloc . G.lab g) pointsToNodes



-- | Run the points-to analysis and return an object that is an
-- instance of PointsToAnalysis, which can be used to query the
-- results.
runPointsToAnalysis :: [Instruction -> Bool] -> Module -> Andersen
runPointsToAnalysis allocTests m = runST (runPTA allocTests m)
--  Andersen g -- `debugGraph` g
--  where
    {-
    fs = moduleDefinedFunctions m
    blocks = concatMap functionBody fs
    insts = concatMap basicBlockInstructions blocks
    (globalLocations, globalEdges) = getGlobalLocations m
    argumentLocations = foldr extractArgs [] fs
    (localLocations, edgeInducers) = foldr extractLocations ([], []) insts
    allLocations = concat [globalLocations, argumentLocations, localLocations]
    initialEdges = globalEdges

    -- The initial graph contains all locations in the program, along
    -- with edges induced by global initializers.
    graph0 = mkGraph allLocations initialEdges
    worklist0 = edgeInducers
    state0 = PTState { _ptWorklist = worklist0
                     , _ptNextWorklist = S.empty
                     , _ptDepGraph = IM.empty
                     , _ptGraph = graph0
                     , _ptExternInfo = []
                     , _ptIsAllocator = allocTests
                     }
    -}
--    g = runST (runPTA allocTests m)
    -- g = _ptGraph (execState saturate state0)


data PTState s = PTState { ptWorklist :: STRef s Worklist
                         , ptNextWorklist :: STRef s (Set Instruction)
                         , ptDepGraph :: STRef s DepGraph
                         , ptGraph :: ImperativeGraph s NodeTag
                         , ptExternInfo :: [ExternalFunction -> Maybe ExternFunctionDescriptor]
                         , ptIsAllocator :: [Instruction -> Bool]
                         }


runPTA :: [Instruction -> Bool] -> Module -> ST s Andersen
runPTA allocTests m = do
  -- Set up the initial graph
  g <- newGraph
  forM_ allLocations $ \(n, lbl) -> addNode g n lbl
  forM_ initialEdges $ \(src, tgt) -> addEdges g src [tgt]

  -- Allocate the STRefs that will be modified during the computation
  worklistRef <- newSTRef edgeInducers
  nextWorklistRef <- newSTRef S.empty
  dgRef <- newSTRef IM.empty
  let state = PTState { ptWorklist = worklistRef
                      , ptNextWorklist = nextWorklistRef
                      , ptDepGraph = dgRef
                      , ptGraph = g
                      , ptExternInfo = []
                      , ptIsAllocator = allocTests
                      }
  -- Saturate the graph
  saturate state

  -- Extract a read-only version of the graph
  ig <- toInductive g
  return $! Andersen ig
  where
    fs = moduleDefinedFunctions m
    blocks = concatMap functionBody fs
    insts = concatMap basicBlockInstructions blocks
    (globalLocations, globalEdges) = getGlobalLocations m
    argumentLocations = foldr extractArgs [] fs
    (localLocations, edgeInducers) = foldr extractLocations ([], []) insts
    allLocations = concat [globalLocations, argumentLocations, localLocations]
    initialEdges = globalEdges


-- | Saturate the points-to graph using a worklist algorithm.
saturate :: PTState s -> ST s ()
saturate state = do
  wl <- readSTRef (ptWorklist state)
  case wl of
    [] -> do
      nextWL <- readSTRef (ptNextWorklist state)
      case S.null nextWL of
        -- Nothing in the next worklist, so we are done
        True -> return ()
        -- Replace the current worklist with the next set of worklist
        -- items.  TODO: sort this into a better order (e.g., topsort)
        False -> do
          writeSTRef (ptWorklist state) (S.toList nextWL) `debug`
            printf "Flipped worklist with %d new entries" (S.size nextWL)
          -- _ <- ptWorklist ~= S.toList nextWL
          -- _ <- ptNextWorklist ~= S.empty
          writeSTRef (ptNextWorklist state) S.empty
          saturate state
    itm : rest -> do
      writeSTRef (ptWorklist state) rest
      -- _ <- ptWorklist ~= rest
      case itm of
        StoreInst { storeValue = val, storeAddress = dest } ->
          case isPointerOrFunction val of
            False -> saturate state
            True -> addStoreEdges state itm val dest
        CallInst { callFunction = cf, callArguments = args } ->
          addCallEdges state itm cf (map fst args)
        InvokeInst { invokeFunction = cf, invokeArguments = args } ->
          addCallEdges state itm cf (map fst args)
        RetInst { retInstValue = Just rv } ->
          addReturnEdges state itm rv
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
  saturate state
  where
    retNode = instructionUniqueId retInst


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
--  let pointedToByRetNodes = concatMap (suc g) retNodes
      locMap' = case retNodes of
        [] -> locMap
        _ -> ([instructionUniqueId callInst], pointedToByRetNodes) : locMap
  mapM_ (addDependency state callInst) retNodes -- `debug` printf "call id: %d" (instructionUniqueId callInst)
  mapM_ (uncurry (makeNewEdges state)) locMap'
  where
    formals = functionParameters callee
    -- FIXME: This needs to change a bit for vararg functions - split
    -- into two lists and treat the trailing arguments as a single
    -- memory location
    actualFormalMap = zip args formals -- `debug`  (case args of
                                       --              [] -> show args
                                       --              _ -> show $ last args)
    -- isRelevant v = case valueContent v of
    --   ConstantC _ -> False
    --   _ -> hasPointerOrFunction v
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
      return ([formalLoc], actualLocs)

-- | A call is essentially a copy of a pointer in the caller to an
-- argument node (which will later be copied in the callee).
--
-- Start by zipping together the actual arguments and formal argument
-- lists.  Filter out the non-pointer entries.
addCallEdges :: PTState s -> Instruction -> Value -> [Value] -> ST s ()
addCallEdges state callInst calledFunc args = do
  case valueContent calledFunc of
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
      addDependency state callInst (valueUniqueId calledFunc)
      let g = ptGraph state
      calledFuncVals <- mapM' (nodeLabels g) funcLocs
      -- g <- access ptGraph
      let handler n = case n of
            Nothing -> conservativeExternalHandler state callInst args Nothing
            Just v -> case valueContent (unloc v) of
              FunctionC f -> addDefinedFunctionCallEdges state callInst args f
              ExternalFunctionC ef -> case externalIsIntrinsic ef of
                True -> addIntrinsicEdges state callInst args ef
                False -> addExternalCallEdges state callInst args ef
              _ -> conservativeExternalHandler state callInst args Nothing
          -- calledFuncVals = map (lab g) funcLocs
      mapM_ handler calledFuncVals
  saturate state

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
  -- extInfoHndlrs <- access ptExternInfo
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

conservativeExternalHandler :: PTState s
                               -> Instruction
                               -> [Value]
                               -> Maybe ExternalFunction
                               -> ST s ()
conservativeExternalHandler state callInst args ef = return ()

-- | Handle adding edges induced by a @StoreInst@.
addStoreEdges :: PTState s -> Instruction -> Value -> Value -> ST s ()
addStoreEdges state i val dest = do
  newTargets <- getLocationsReferencedBy state i val
  newSources <- getLocationsReferencedBy state i dest
  makeNewEdges state newSources newTargets
  saturate state

forM' ls f = mapM' f ls

-- | Determine which edges need to be added to the graph, based on the
-- set of discovered sources and targets.  Only new edges are
-- returned.
makeNewEdges :: PTState s -> [Node] -> [Node] -> ST s ()
makeNewEdges state srcs targets = do
  let g = ptGraph state
      targetSet = HS.fromList targets
  -- For each source, add edges to the new targets.  Return the src
  -- if it was used.
  usedSrcs <- forM' srcs $ \src -> do
    currentTargets <- nodeSuccessors g src
    let currentTargetSet = HS.fromList currentTargets
        newTargets = HS.difference targetSet currentTargetSet
    case HS.toList newTargets of
      [] -> return Nothing
      tgts -> do
        addEdges g src tgts
        return (Just src)

  -- Now figure out which instructions need to be added to the
  -- worklist because some of the new edges affected them
  depGraph <- readSTRef (ptDepGraph state)
  let newWorklistItems = affectedInstructions depGraph (catMaybes usedSrcs)
  modifySTRef (ptNextWorklist state) $ S.union newWorklistItems
  where
    affectedInstructions :: DepGraph -> [Node] -> Set Instruction
    affectedInstructions depGraph used = {-# SCC "affectedInstructions" #-}
      let findAffected acc nodeId =
            S.union acc (IM.findWithDefault S.empty nodeId depGraph)
      in foldl' findAffected S.empty used

{-
addEdges :: PTState -> [Context NodeTag EdgeTag] -> ST s ()
addEdges state newEdges = do
  newWorklistItems <- affectedInstructions state usedSrcs
  -- Append the new items to the worklist
  modifySTRef (ptNextWorklist state) $ S.union newWorklistItems
  -- _ <- ptNextWorklist %= S.union (S.fromList newWorklistItems)

  -- FIXME
  -- ptg <- access ptGraph
  -- let g' = foldl' (flip (&)) ptg newEdges
  -- _ <- ptGraph ~= g'
  return ()
-}


{-
  g <- access ptGraph
  return $ mapMaybe (toContext g) newSrcs
  where
    possibleTargets = HS.fromList newTargets
    -- | Create a new context for each src in the graph.  A context is
    -- only created if it adds new targets.  This is to make worklist
    -- management easier; if there are no new edges, the worklist is
    -- not updated.
    --
    -- Note that all edges for each source are added at once to the
    -- context.  Using separate contexts for each new edge would
    -- require changes later, otherwise some contexts could overwrite
    -- others, losing edges.
    toContext g src = {-# SCC "toContext" #-}
      let ctx@(_, n, lbl, adjOut) = context g src
          existingTargets = {-# SCC "toContext.toSet" #-} HS.fromList $ suc' ctx
          targets = {-# SCC "toContext.filter" #-}
            HS.difference possibleTargets existingTargets
--            filter (not . (`HS.member` existingTargets)) newTargets
      in case HS.null targets of
        True -> Nothing
        False -> let newOut = {-# SCC "toContext.zip" #-} zip (repeat DirectEdge) (HS.toList targets) ++ adjOut
                 in Just ([], n, lbl, newOut)
-}

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
    getLocs val derefCount visited
      | (Value val) `S.member` visited = return []
      | otherwise =
        let visited' = S.insert (Value val) visited
        in case valueContent val {-`debug` printf "id: %d -- %s\n" (valueUniqueId val) (show val)-} of
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
            locs <- mapM' (\x -> getLocs x derefCount visited') ivs
            return $! S.toList $ S.fromList (concat locs)
          InstructionC (SelectInst { selectTrueValue = tv, selectFalseValue = fv }) -> do
            tlocs <- getLocs tv derefCount visited'
            flocs <- getLocs fv derefCount visited'
            return $! S.toList $ S.fromList $ tlocs ++ flocs


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
          ArgumentC a ->
            collectLocationNodes state inst 1 [argumentUniqueId a]

          -- In this fallback case, @val@ should be a node in the
          -- Points-to graph.  Collect everything @derefCount@ steps from
          -- it and return that, along with the updated DepGraph.  The
          -- depgraph needs to be updated with an entry for each non-leaf
          -- node.
          _ -> collectLocationNodes state inst (derefCount + offset) [valueUniqueId val] -- `debug` show val
      -- case (Value val) `S.member` visited of
      --   True -> return []
      --   False ->
    isAllocator i = let allocTests = ptIsAllocator state
                    in foldl' (\acc p -> acc || p i) False allocTests
--      allocTests <- access ptIsAllocator
--      return $!


addDependency :: PTState s -> Instruction -> Node -> ST s ()
addDependency state inst n = -- do
  modifySTRef (ptDepGraph state) $ IM.insertWith S.union n (S.singleton inst)
--  _ <- ptDepGraph %= (IM.insertWith S.union n (S.singleton inst))
--  return ()

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
        let g = ptGraph state
--        g <- access ptGraph
        nextNodes <- mapM' (nodeSuccessors g) currentNodes
        let nextNodes' = concat nextNodes
--        let nextNodes = concatMap (suc g) currentNodes
        mapM_ (addDependency state inst) currentNodes
        collect (remainingHops - 1) nextNodes'

mapM' f = go []
  where
    go acc [] = return $! (reverse acc)
    go acc (a:as) = do
      x <- f a
      x `deepseq` return ()
      go (x:acc) as


-- | return instructions need to create a location (the
-- pseudo-location to communicate return values back to callers).
-- This doesn't have a real location and should be treated similarly
-- to arguments.
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
      False -> (ptgNodes, i : insts) -- `debug` printf "Skipping call: %d -- %s" (valueUniqueId i) (show i)
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
-- Module
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