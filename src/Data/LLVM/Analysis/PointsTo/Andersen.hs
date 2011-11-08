{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
-- | This is a simple implementation of Andersen's points-to analysis.
--
-- TODO:
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
  viewPointsToGraph,
  ) where

import Control.Monad.State
import Data.ByteString.Char8 ( isPrefixOf )
import Data.Graph.Inductive hiding ( Gr, (><) )
import Data.GraphViz
import Data.IntMap ( IntMap )
import qualified Data.IntMap as IM
import Data.Lens.Lazy
import Data.Lens.Template
import Data.List ( find, foldl' )
import Data.Maybe ( mapMaybe )
import Data.Set ( Set )
import qualified Data.Set as S
import Data.Sequence ( Seq, (><), ViewL(..), viewl )
import qualified Data.Sequence as Seq

import Data.LLVM.Analysis.PointsTo
import Data.LLVM.Internal.PatriciaTree
import Data.LLVM.Types

import System.IO.Unsafe
import Text.Printf
import Debug.Trace
debug = flip trace

data NodeTag = PtrToLocation Value
             | Location Value
             | PtrToFunction Function
             deriving (Ord, Eq, Show)

data EdgeTag = DirectEdge
             | ArrayEdge
             | FieldAccessEdge !Int
             deriving (Ord, Eq, Show)

type PTGEdge = (Int, Int, EdgeTag)
type PTGNode = (Int, NodeTag)
type PTG = Gr NodeTag EdgeTag
type Worklist = Seq Instruction
-- | Define a dependency graph.  The keys are the unique IDs of
-- locations.  The value for each ID is the set of instructions that
-- need to be re-processed when a new edge is added to the points-to graph
-- with that node ID as its source.
type DepGraph = IntMap (Set Instruction)


data PTState = PTState { _ptWorklist :: Worklist
                       , _ptDepGraph :: DepGraph
                       , _ptGraph :: PTG
                       , _ptExternInfo :: [ExternalFunction -> Maybe ExternFunctionDescriptor]
                       , _ptIsAllocator :: [Instruction -> Bool]
                       }
$(makeLenses [''PTState])

type PTMonad = State PTState

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
    pointsToNodes = suc g (valueUniqueId v)
    nodeValues = map (unloc . lab g) pointsToNodes

-- | Run the points-to analysis and return an object that is an
-- instance of PointsToAnalysis, which can be used to query the
-- results.
runPointsToAnalysis :: [Instruction -> Bool] -> Module -> Andersen
runPointsToAnalysis allocTests m = Andersen g -- `debugGraph` g
  where
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
    worklist0 = Seq.fromList edgeInducers
    state0 = PTState { _ptWorklist = worklist0
                     , _ptDepGraph = IM.empty
                     , _ptGraph = graph0
                     , _ptExternInfo = []
                     , _ptIsAllocator = allocTests
                     }
    g = _ptGraph (execState saturate state0)

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


-- | Saturate the points-to graph using a worklist algorithm.
saturate :: PTMonad ()
saturate = do
  wl <- access ptWorklist
  case viewl wl of
    EmptyL -> return ()
    itm :< rest -> do
      _ <- ptWorklist ~= rest
      case itm of
        StoreInst { storeValue = val, storeAddress = dest } ->
          case isPointerOrFunction val of
            False -> saturate
            True -> addStoreEdges itm val dest
        CallInst { callFunction = cf, callArguments = args } ->
          addCallEdges itm cf (map fst args)
        InvokeInst { invokeFunction = cf, invokeArguments = args } ->
          addCallEdges itm cf (map fst args)
        RetInst { retInstValue = Just rv } ->
          addReturnEdges itm rv
        _ -> error ("Unexpected instruction type in Andersen saturation: " ++ show itm)

-- | Returns can be handled similarly to arguments.  Neither has
-- actual storage associated with it, and returns are essentially just
-- parameters going in the other direction (the caller will copy the
-- value out).  The special return node (the node with the id of the
-- ret inst) gets an edge to every node referenced by the return
-- value.
addReturnEdges :: IsValue a => Instruction -> a -> PTMonad ()
addReturnEdges retInst rv = do
  retLocs <- getLocationsReferencedBy retInst rv
  newEdges <- makeNewEdges [retNode] retLocs
  addEdges newEdges
  saturate
  where
    retNode = instructionUniqueId retInst

addEdges :: [Context NodeTag EdgeTag] -> PTMonad ()
addEdges newEdges = do
  newWorklistItems <- affectedInstructions usedSrcs
  -- Append the new items to the worklist
  _ <- ptWorklist %= (>< Seq.fromList newWorklistItems)
  ptg <- access ptGraph
  let g' = foldl' (flip (&)) ptg newEdges
  _ <- ptGraph ~= g'
  return ()
  where
    accumUsedSrcs acc (_, src, _, _) = S.insert src acc
    usedSrcs = foldl' accumUsedSrcs S.empty newEdges

    affectedInstructions :: Set Int -> PTMonad [Instruction]
    affectedInstructions used = do
      depGraph <- access ptDepGraph
      let instSet = S.fold (findAffected depGraph) S.empty used
          findAffected dg nodeId acc = S.union acc (IM.findWithDefault S.empty nodeId dg)
      return $ S.toList instSet


addDefinedFunctionCallEdges :: Instruction -- ^ The CallInst
                               -> [Value] -- ^ Actual arguments
                               -> Function -- ^ A possible callee
                               -> PTMonad ()
addDefinedFunctionCallEdges callInst args callee = do
  locMap <- mapM getLocs justPointers
  g <- access ptGraph
  let pointedToByRetNodes = concatMap (suc g) retNodes
      locMap' = ([instructionUniqueId callInst], pointedToByRetNodes) : locMap
  mapM_ (addDependency callInst) retNodes
  newEdges <- mapM (uncurry makeNewEdges) locMap'
  addEdges (concat newEdges)
  where
    formals = functionParameters callee
    -- FIXME: This needs to change a bit for vararg functions - split
    -- into two lists and treat the trailing arguments as a single
    -- memory location
    actualFormalMap = zip args formals
    keepPointerParams = filter (hasPointerOrFunction . fst)
    justPointers = keepPointerParams actualFormalMap

    -- Now find all possible return nodes for this instruction (one
    -- for each possible callee).  Add edges from the call node to all
    -- of the things that could be pointed to by the return node.
    retNodes = case isPointerOrFunction callInst of
      True -> [instructionUniqueId (functionExitInstruction callee)]
      False -> []

    getLocs :: (Value, Argument) -> PTMonad ([Node], [Node])
    getLocs (actual, formal) = do
      let formalLoc = argumentUniqueId formal
      actualLocs <- getLocationsReferencedBy callInst actual
      addDependency callInst formalLoc
      return ([formalLoc], actualLocs)

-- | A call is essentially a copy of a pointer in the caller to an
-- argument node (which will later be copied in the callee).
--
-- Start by zipping together the actual arguments and formal argument
-- lists.  Filter out the non-pointer entries.
addCallEdges :: Instruction -> Value -> [Value] -> PTMonad ()
addCallEdges callInst calledFunc args = do
  case valueContent calledFunc of
    FunctionC f -> addDefinedFunctionCallEdges callInst args f

    -- Don't forget case for ExternalFunctionC and then handling both in the fold
    ExternalFunctionC e -> case externalIsIntrinsic e of
      False -> addExternalCallEdges callInst args e
      True -> addIntrinsicEdges callInst args e

    _ -> do
      funcLocs <- getLocationsReferencedBy callInst calledFunc
      -- Add a dependency in the DepGraph on the node representing the
      -- functions that may be pointed to here.  This way, if we learn
      -- about other possible callees, we revisit this call.
      addDependency callInst (valueUniqueId calledFunc)
      g <- access ptGraph
      let handler (FunctionC f) = addDefinedFunctionCallEdges callInst args f
          handler (ExternalFunctionC ef)
            | externalIsIntrinsic ef = addIntrinsicEdges callInst args ef
            | otherwise = addExternalCallEdges callInst args ef
      mapM_ handler (map (valueContent . unloc . lab g) funcLocs)
  saturate

addIntrinsicEdges :: Instruction -> [Value] -> ExternalFunction -> PTMonad ()
addIntrinsicEdges callInst args ef =
  case find ((`isPrefixOf` fname) . fst) handlerMap of
    Nothing -> return ()
    Just (_, handler) -> handler callInst args >> return ()
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
                 , ("llvm.memset.", undefined)
                 ]

-- | In handling memcpy, we only care about the first two arguments
-- (until we tackle field-sensitivity): <dest> and <src>.
memcpyHandler :: Instruction -> [Value] -> PTMonad ()
memcpyHandler callInst args = do
  let dest : src : _ = args
  newSources <- getLocationsReferencedByOffset (0) callInst dest
  newTargets <- getLocationsReferencedByOffset (1) callInst src
  newEdges <- makeNewEdges newSources newTargets
  addEdges newEdges

-- | FIXME: Implement by having a default conservative handler and
-- checking a new field in the PTState that enables analysis users to
-- provide information about external functions.
addExternalCallEdges :: Instruction -> [Value] -> ExternalFunction -> PTMonad ()
addExternalCallEdges callInst args ef = do
  extInfoHndlrs <- access ptExternInfo
  case lookupDescriptor extInfoHndlrs of
    Nothing -> conservativeExternalHandler callInst args ef
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

conservativeExternalHandler callInst args ef = return ()

-- | Handle adding edges induced by a @StoreInst@.
addStoreEdges :: Instruction -> Value -> Value -> PTMonad ()
addStoreEdges i val dest = do
  newTargets <- getLocationsReferencedBy i val
  newSources <- getLocationsReferencedBy i dest
  newEdges <- makeNewEdges newSources newTargets
  addEdges newEdges
  saturate

-- | Determine which edges need to be added to the graph, based on the
-- set of discovered sources and targets.  Only new edges are
-- returned.
makeNewEdges :: [Node] -> [Node] -> PTMonad [Context NodeTag EdgeTag]
makeNewEdges newSrcs newTargets = do
  g <- access ptGraph
  let notInGraph src tgt = tgt `notElem` suc g src
      -- | Create a new context for each src in the graph.  A context is
      -- only created if it adds new targets.  This is to make worklist
      -- management easier; if there are no new edges, the worklist is
      -- not updated.
      --
      -- Note that all edges for each source are added at once to the
      -- context.  Using separate contexts for each new edge would
      -- require changes later, otherwise some contexts could overwrite
      -- others, losing edges.
      toContext src =
        let targets = filter (notInGraph src) newTargets
            (_, n, lbl, adjOut) = context g src
        in case targets of
          [] -> Nothing
          _ -> let newOut = zip (repeat DirectEdge) targets ++ adjOut
               in Just ([], n, lbl, newOut)
  return $ mapMaybe toContext newSrcs

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
getLocationsReferencedBy :: IsValue a => Instruction -> a -> PTMonad [Node]
getLocationsReferencedBy = getLocationsReferencedByOffset 0

getLocationsReferencedByOffset :: IsValue a => Int -> Instruction -> a -> PTMonad [Node]
getLocationsReferencedByOffset offset inst v = getLocs v 0
  where
    getLocs :: (IsValue a) => a -> Int -> PTMonad [Node]
    getLocs val derefCount = case valueContent val of
      -- This also needs to handle GEP instructions later, also
      -- (maybe) select, extractelement, and extractvalue.  This also
      -- needs to be careful around bitcasts of non-pointer types
      -- (integers turned into pointers, for example)
      InstructionC (LoadInst { loadAddress = addr }) ->
        getLocs addr (derefCount + 1)
      InstructionC (BitcastInst { castedValue = cv }) ->
        getLocs cv derefCount
      InstructionC (GetElementPtrInst { getElementPtrValue = base
                                      , getElementPtrIndices = idxs
                                      }) ->
        getLocs base derefCount
      ConstantC (ConstantValue { constantInstruction = GetElementPtrInst { getElementPtrValue = base
                                                                         , getElementPtrIndices = idxs
                                                                         }
                               }) ->
        getLocs base derefCount
      ConstantC (ConstantValue { constantInstruction = BitcastInst { castedValue = cv } }) ->
        getLocs cv derefCount
      -- These locations are a bit special.  Unlike the others
      -- (globals, locals, etc), which represent pointers to
      -- locations, these are abstract locations that do not have
      -- storage.  They must not be dereferenced the "extra" time.
      InstructionC (ci@CallInst {}) -> do
        isalloc <- isAllocator ci
        case isalloc of
          False -> collectLocationNodes inst 1 [instructionUniqueId ci]
          True -> return [instructionUniqueId ci]
      InstructionC (ci@InvokeInst {}) -> do
        isalloc <- isAllocator ci
        case isalloc of
          False -> collectLocationNodes inst 1 [instructionUniqueId ci]
          True -> return [instructionUniqueId ci]
      ArgumentC a ->
        collectLocationNodes inst 1 [argumentUniqueId a]

      -- In this fallback case, @val@ should be a node in the
      -- Points-to graph.  Collect everything @derefCount@ steps from
      -- it and return that, along with the updated DepGraph.  The
      -- depgraph needs to be updated with an entry for each non-leaf
      -- node.
      _ -> collectLocationNodes inst (derefCount + offset) [valueUniqueId val]
    isAllocator i = do
      allocTests <- access ptIsAllocator
      return $! foldl' (\acc p -> acc || p i) False allocTests


addDependency :: Instruction -> Node -> PTMonad ()
addDependency inst n = do
  _ <- ptDepGraph %= (IM.insertWith S.union n (S.singleton inst))
  return ()

collectLocationNodes :: Instruction
                        -> Int
                        -> [Node]
                        -> PTMonad [Node]
collectLocationNodes inst steps seedNodes = collect steps seedNodes
  where
    collect remainingHops currentNodes
      | remainingHops <= 0 = return currentNodes
      | otherwise = do
        g <- access ptGraph
        let nextNodes = concatMap (suc g) currentNodes
        mapM_ (addDependency inst) currentNodes
        collect (remainingHops - 1) nextNodes

-- | This only re-allocates the small part of the list each iteration,
-- so should remain efficient.
extractArgs :: Function -> [PTGNode] -> [PTGNode]
extractArgs f nacc = map argToNode (functionParameters f) ++ nacc
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
        _ -> Just (globalVariableUniqueId gv, valueUniqueId i, DirectEdge)
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

unloc :: Maybe NodeTag -> Value
unloc (Just (Location l)) = l
unloc (Just (PtrToLocation l)) = l
unloc (Just (PtrToFunction f)) = (Value f)


-- Debugging visualization stuff

instance Labellable NodeTag where
  toLabelValue (Location v) = toLabelValue v
  toLabelValue (PtrToLocation v) = toLabelValue v
  toLabelValue (PtrToFunction f) = toLabelValue (Value f)

instance Labellable EdgeTag where
  toLabelValue DirectEdge = toLabelValue ("" :: String)
  toLabelValue ArrayEdge = toLabelValue ("[*]" :: String)
  toLabelValue (FieldAccessEdge ix) = toLabelValue $ concat [".<", show ix, ">"]

pointsToParams = nonClusteredParams { fmtNode = \(_,l) -> [toLabel l]
                                    , fmtEdge = \(_,_,l) -> [toLabel l]
                                    }


viewPointsToGraph :: Andersen -> IO ()
viewPointsToGraph (Andersen g) = do
  let dg = graphToDot pointsToParams g
  _ <- runGraphvizCanvas' dg Gtk
  return ()

debugGraph :: a -> PTG -> a
debugGraph v g = unsafePerformIO $ do
  viewPointsToGraph (Andersen g)
  return v

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
