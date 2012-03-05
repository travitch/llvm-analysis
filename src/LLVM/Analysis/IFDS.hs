{-# LANGUAGE MultiParamTypeClasses, TemplateHaskell #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
-- | This is an implementation of the IFDS algorithm as defined by RHS
-- in
--
-- > Precise Interprocedural Dataflow Analysis via Graph Reachability (POPL 1995)
-- > doi: http://doi.acm.org/10.1145/199448.199462
--
-- The algorithm uses dynamic programming to solve some
-- interprocedural dataflow analysis problems precisely, finding the
-- meet-over-all-valid-paths solution (instead of the
-- meet-over-all-paths solution, which includes data flow across
-- unmatched call/return pairs).  It solves a class of problems called
-- the Interprocedural Finite Distributive Subset problems in cubic
-- time; a larger class of problems is addressed by the closely
-- related IDE framework.
--
-- The framework solves _subset_ problems; elements of this set are
-- drawn from a _domain_ @d@.  To solve a problem, the algorithm takes
-- the interprocedural control flow graph (called the supergraph in
-- RHS95) and converts it into an _exploded supergraph_ where each
-- node in the original ICFG is replaced by a node for _each_ possible
-- element in @d@.  Edges are drawn between elements in @d@ according
-- to a set of flow functions (intra-procedural and inter-procedural).
-- RHS95 refers to the flow functions and the edges they induce
-- between two statements as the _representation relation_.
--
-- Some of the efficiency of the algorithm is derived from this
-- "point-wise" treatment of the domain.
--
-- This particular implementation uses the formulation of Naeem et al
-- in
--
-- > Practical Extensions to the IFDS Algorithm (CC 2010)
-- > doi: http://dx.doi.org/10.1007/978-3-642-11970-5_8
--
-- These extensions make the algorithm practical for larger programs.
-- In particular, this formulation avoids constructing the entire
-- exploded supergraph G#; instead, it implicitly constructs only the
-- reachable portion of the graph.
module LLVM.Analysis.IFDS (
  -- * Types
  IFDSAnalysis(..),
  IFDSResult,
  -- * Entry point
  ifds,
  -- * Accessors
  ifdsInstructionResult,
  ifdsResultEdgeCount
  ) where

import Control.Monad.State
import Data.Graph.Inductive hiding ( (><) )
import Data.Map ( Map )
import Data.Set ( Set )
import qualified Data.Map as M
import qualified Data.Set as S
import FileLocation
import Text.Printf

import LLVM.Analysis
import LLVM.Analysis.CFG
import LLVM.Analysis.ICFG
import LLVM.Analysis.Internal.Worklist

-- | The interface to define an IFDS analysis.  There are variants of
-- the interprocedural flow functions to handle /external/ functions.
-- Known references are provided for known external functions.
-- Unknown functions can be called in Modules that do not have a
-- 'main' function or when they contain calls to dlopen.
class IFDSAnalysis a domType where
  flow :: a -> Maybe domType -> Instruction -> [CFGEdge] -> [Maybe domType]
  -- ^ Compute local flow information for the domain element after this
  -- instruction is executed.  The returned list is the list of domain
  -- variables reachable from this domain element after the statement is
  -- executed.  This models local control flow.
  callFlow :: a -> Maybe domType -> Instruction -> [CFGEdge] -> [Maybe domType]
  -- ^ Similar to 'flow', but models local information flow across
  -- call->return edges.

  -- | Similar to 'flow', but models flow from return nodes to their
  -- successors.  The default implementation should be sufficient for
  -- most purposes
  returnNodeFlow :: a -> Maybe domType -> Instruction -> [Maybe domType]
  returnNodeFlow _ v _ = [v]
  passArgs :: a -> Maybe domType -> Instruction -> Function -> [Maybe domType]
  -- ^ Pass information from a call to the entry of the callee
  externPassArgs :: a -> Maybe domType -> Instruction -> Maybe ExternalFunction -> [Maybe domType]
  -- ^ Similar to 'passArgs', but for external (possibly unknown)
  -- functions.  The 'ExternalFunction' is @Nothing@ if the external
  -- function is unknown.
  returnVal :: a -> Maybe domType -> Instruction -> Instruction -> [Maybe domType]
  -- ^ Pass information about globals and the actual return value back
  -- to the caller.
  externReturnVal :: a -> Maybe domType -> Maybe ExternalFunction -> Instruction -> [Maybe domType]
  -- ^ 'retVal' for external functions.  The external function is
  -- 'Nothing' when the return is from an unknown external function.
  entrySetup :: a -> Module -> Function -> [Maybe domType]
  -- ^ Add domain elements to the initial set of PathEdges.


-- | An edge from <s_p, d_1> to <n, d_2> noting that <n, d_2> is
-- reachable in the exploded supergraph from <s_p, d_1>.  s_p is not
-- explicitly recorded because it is uniquely determined by n.
data PathEdge domType = PathEdge !(Maybe domType) !Node !(Maybe domType)
                      deriving (Ord, Eq, Show)

-- After the analysis is done, reduce the Set PathEdge -> Map (Node,
-- domType) [domType] to start answering queries.  Really, only the
-- second two members are useful (reachable vs not reachable).

-- | An edge summarizing the flow information for the call site
-- identified by the node.
data SummaryEdge domType = SummaryEdge !Node !(Maybe domType) !(Maybe domType)
                           deriving (Ord, Eq, Show)

data IFDSNode domType = IFDSNode !Node !(Maybe domType)
                        deriving (Ord, Eq, Show)

-- | The current state of the IFDS analysis.  It includes the PathEdge
-- and SummaryEdge sets, as well as a few important caches.  It also
-- maintains the worklist.  There is a reference to the ICFG being
-- analyzed for convenience.
data IFDS a domType = IFDS { pathEdges :: Set (PathEdge domType)
                             -- ^ The PathEdge set from the algorithm
                           , summaryEdges :: Set (SummaryEdge domType)
                           -- ^ The SummaryEdge set from the algorithm
                           , incomingNodes :: Map (IFDSNode domType) (Set (IFDSNode domType))
                           -- ^ A reverse mapping.  This maps G#
                           -- (exploded supergraph) nodes
                           -- corresponding to function entries to the
                           -- calls that induce them.
                           , endSummary :: Map (IFDSNode domType) (Set (IFDSNode domType))
                           , entryValues :: Map Node (Set (Maybe domType))
                           -- ^ A cache of domain elements reachable
                           -- at the entry node of a procedure (the
                           -- key in the map is the call node that is
                           -- reachable by the entries in the set).
                           , summaryValues :: Map Node (Set (Maybe domType))
                           -- ^ A cache of domain elements at the
                           -- target of a summary edge.  This lets us
                           -- implement the second query at line 17
                           -- efficiently
                           , ifdsWorklist :: Worklist (PathEdge domType)
                           -- ^ A simple worklist
                           , icfg :: ICFG
                           -- ^ The ICFG that this analysis is operating on
                           , ifdsAnalysis :: a
                         }

type IFDSM a domType r = State (IFDS a domType) r

-- | An opaque wrapper around the results of an IFDS analysis.  Use
-- 'ifdsInstructionResult' to extract information.
data IFDSResult domType = IFDSResult Int (Map Instruction (Set domType))

instance (Show domType, Ord domType) => Show (IFDSResult domType) where
  show = showIFDSResult

showIFDSResult :: (Show domType, Ord domType) => IFDSResult domType -> String
showIFDSResult (IFDSResult _ r) = unlines $ map showProgramPoint $ M.toList r
  where
    showProgramPoint (inst, s) =
      let memberStrings = S.toList $ S.map showMember s
          Just bb = instructionBasicBlock inst
          f = basicBlockFunction bb
      in concat $ printf "%s: (%d) %s" (show (functionName f)) (instructionUniqueId inst) (show inst) : "\n" : memberStrings
    showMember m = "  " ++ show m ++ "\n"

-- | Extract the set of values that are reachable from some entry
-- point at the given 'Instruction'.  If the Instruction is not in the
-- Module, returns Nothing.
ifdsInstructionResult :: IFDSResult domType -> Instruction -> Maybe (Set domType)
ifdsInstructionResult (IFDSResult _ m) i = M.lookup i m

-- | Return the number of edges in the reachable portion of the
-- exploded subgraph.  This is mostly useful for regression testing to
-- ensure that there aren't too many edges introduced.
ifdsResultEdgeCount :: IFDSResult domType -> Int
ifdsResultEdgeCount (IFDSResult c _) = c

-- | Run the IFDS analysis on the given ICFG. Currently it is forward
-- only.  Support for backwards analysis could be added somewhat
-- easily.
ifds :: (IFDSAnalysis a domType, Ord domType, Show domType) => a -> ICFG -> IFDSResult domType
ifds analysis g = extractSolution finalState
  where
    initialEdges = concatMap (mkInitialEdges analysis (icfgModule g)) (icfgEntryPoints g)
    initialState = IFDS { pathEdges = S.fromList initialEdges
                        , summaryEdges = S.empty
                        , ifdsWorklist = worklistFromList initialEdges
                        , incomingNodes = M.empty
                        , endSummary = M.empty
                        , entryValues = M.empty
                        , summaryValues = M.empty
                        , icfg = g
                        , ifdsAnalysis = analysis
                        }
    finalState = execState tabulate initialState

-- | Given the final state from the tabulation algorithm, extract a
-- whole-program solution from it
extractSolution :: (IFDSAnalysis a domType, Ord domType, Show domType)
                   => IFDS a domType -> IFDSResult domType
extractSolution s = IFDSResult (S.size ps) $ S.fold populateSolution M.empty ps
  where
    ps = pathEdges s
    g = (icfgGraph . icfg) s
    populateSolution (PathEdge _ n (Just d2)) m =
      case nodeToInstruction n of
        Nothing -> m
        Just inst ->
          let newSet = case M.lookup inst m of
                Nothing -> S.singleton d2
                Just vals -> S.insert d2 vals
          in M.insert inst newSet m
    populateSolution _ m = m
    nodeToInstruction n = case l of
      InstNode i -> Just i
      _ -> Nothing
      where
        Just l = lab g n


-- | Grab the underlying ICFG graph
getICFG :: IFDSM a domType ICFGGraphType
getICFG = do
  g <- gets icfg
  return (icfgGraph g)


-- | The key function that builds up the PathEdge set using a worklist
-- algorithm.  It handles the three cases outlined in the main
-- algorithm: adding interprocedural edges for call/invoke nodes,
-- adding interprocedural (and summary) edges for return nodes, and
-- adding intraprocedural edges for all other instructions.
tabulate :: (IFDSAnalysis a domType, Ord domType, Show domType) => IFDSM a domType ()
tabulate = do
  worklist <- gets ifdsWorklist
  case takeWorkItem worklist of
    EmptyWorklist -> return () -- Done
    e@(PathEdge _{-d1-} n _{-d2-}) :< rest -> do
      modify (\s -> s { ifdsWorklist = rest })
      g <- getICFG
      let Just lbl = lab g n
      case lbl of
        -- Case 1: Calls
        InstNode ci@CallInst {} -> addCallEdges ci e
        InstNode ii@InvokeInst {} -> addCallEdges ii e

        -- Case 2: Returns (including from externs)
        InstNode ri@RetInst {} -> addExitEdges (Right ri) e
        ExternalNode ef -> addExitEdges (Left ef) e

        -- Case 3: intraprocedural control flow
        InstNode i -> addIntraEdges i e
        ReturnNode i -> addReturnNodeEdges i e
      tabulate


-- | Get the list of domain values reachable from 'Node' @n@.  This
-- request is always issued when we know that a particular domain
-- value @d1@ is reachable, so add that to the set of reachable values
-- before returning it.
updateCallReachingValues :: (Ord domType)
                            => Node
                            -> Maybe domType
                            -> IFDSM a domType ()
updateCallReachingValues n d1 = do
  evs <- gets entryValues
  let updatedValues = maybe (S.singleton d1) (S.insert d1) (M.lookup n evs)
  modify (\s -> s { entryValues = M.insert n updatedValues evs})
{-# INLINE updateCallReachingValues #-}

-- | Handle adding interprocedural call edges and intraprocedural
-- call->return edges.
addCallEdges :: (IFDSAnalysis a domType, Ord domType, Show domType)
                => Instruction
                -> PathEdge domType
                -> IFDSM a domType ()
addCallEdges ci e@(PathEdge d1 n _) = do
  g <- gets icfg
  -- Update the entryValues cache here since we know that d1 reaches
  -- the call here.
  updateCallReachingValues n d1
  let possibleCalleeEntryNodes = getICFGCallEntries g n
  mapM_ (addInfoForPotentialCallee ci e) possibleCalleeEntryNodes
{-# INLINE addCallEdges #-}

-- | Add call edges and intraprocedural call->return edges for a
-- single callee of a call site.  The original algorithm assumes that
-- each call site has a single callee.  This helper just handles all
-- possible callees.
addInfoForPotentialCallee :: (IFDSAnalysis a domType, Ord domType, Show domType)
                             => Instruction
                             -> PathEdge domType
                             -> Node
                             -> IFDSM a domType ()
addInfoForPotentialCallee ci e@(PathEdge _ n d2) calleeEntry = do
  g <- getICFG
  analysis <- gets ifdsAnalysis
  let Just calleeEntryLabel = lab g calleeEntry

  -- Call passArgs to figure out how to map domain values in the
  -- caller to the callee
  let argToFormalEdges = case calleeEntryLabel of
        InstNode entryInst -> passArgs analysis d2 ci (instructionFunction entryInst)
        ExternalNode ef -> externPassArgs analysis d2 ci ef
        _ -> $err' ("Expected InstNode or ExternalNode: " ++ show calleeEntryLabel)

  -- This covers lines 14-16 of the algorithm (the call to passArgs
  -- defines argToFormalEntries)
  mapM_ (addInterProcAndSummaryEdges ci calleeEntry e) argToFormalEdges

  -- The rest of this covers lines 17-19
  summEdgeD3s <- getSummaryEdgeD3s n d2
  let intraPredecessorEdges = map toIntraEdge $ lpre g n
      callFlowD3s = callFlow analysis d2 ci intraPredecessorEdges
      d3s = summEdgeD3s ++ callFlowD3s

  mapM_ (extendCallToReturn e) d3s
{-# INLINE addInfoForPotentialCallee #-}

-- | Line 18
extendCallToReturn :: (Ord domType) => PathEdge domType -> Maybe domType -> IFDSM a domType ()
extendCallToReturn (PathEdge d1 n _) d3 =
  propagate (PathEdge d1 retNode d3)
  where
    retNode = callNodeToReturnNode n
{-# INLINE extendCallToReturn #-}

-- | Part of the query on line 17
getSummaryEdgeD3s :: (Ord domType, Show domType)
                     => Node
                     -> Maybe domType
                     -> IFDSM a domType [Maybe domType]
getSummaryEdgeD3s n d2 = do
  summVals <- gets summaryValues
  summEdges <- gets summaryEdges
  let possibleD3s = maybe S.empty id (M.lookup n summVals)
  return $ filter (isInSummaryEdge summEdges) (S.toList possibleD3s)
  where
    isInSummaryEdge summEdges d3 =
      let summEdge = SummaryEdge n d2 d3
      in S.member summEdge summEdges
{-# INLINE getSummaryEdgeD3s #-}

-- | Inner loop on lines 15-16
addInterProcAndSummaryEdges :: (IFDSAnalysis a domType, Ord domType, Show domType)
                               => Instruction
                               -> Node
                               -> PathEdge domType
                               -> Maybe domType
                               -> IFDSM a domType ()
addInterProcAndSummaryEdges ci calleeEntry e@(PathEdge _ n d2) argEdgeD3 = do
  endSummaries <- gets endSummary
  let loop = PathEdge argEdgeD3 calleeEntry argEdgeD3
      entryNode = IFDSNode calleeEntry argEdgeD3
      callNode = IFDSNode n d2
      callerExits = maybe S.empty id (M.lookup entryNode endSummaries)
  -- Mark the entry of the function as reachable by element argEdgeD3
  -- (derived from passArgs(d2) above)
  propagate loop
  addIncomingNode entryNode callNode

  mapM_ (addCallSummaries ci e) (S.toList callerExits)
{-# INLINE addInterProcAndSummaryEdges #-}

-- | Handle adding edges for function call instructions (and invokes).
-- This function covers lines 15.3-15.5 in the algorithm from Naeem et al
addCallSummaries :: (IFDSAnalysis a domType, Ord domType, Show domType)
                    => Instruction
                    -> PathEdge domType
                    -> IFDSNode domType
                    -> IFDSM a domType ()
addCallSummaries ci (PathEdge _ n d2) (IFDSNode e_p d4) = do
  g <- getICFG
  analysis <- gets ifdsAnalysis
  let Just exitNode = lab g e_p
      returnedVals = case exitNode of
        InstNode retInst -> returnVal analysis d4 retInst ci
        ExternalNode ef -> externReturnVal analysis d4 ef ci
        _ -> $err' ("Expected InstNode or ExternalNode: " ++ show exitNode)
      summEdges = map (\d5 -> SummaryEdge n d2 d5) returnedVals
  mapM_ addSummaryEdge summEdges
{-# INLINE addCallSummaries #-}


addIncomingNode :: (Ord domType) => IFDSNode domType -> IFDSNode domType -> IFDSM a domType ()
addIncomingNode entryNode callNode = do
  s <- get
  let currentNodes = incomingNodes s
      updatedNodes =
        maybe (S.singleton callNode) (S.insert callNode) (M.lookup entryNode currentNodes)
  put s { incomingNodes = M.insert entryNode updatedNodes currentNodes }
{-# INLINE addIncomingNode #-}

-- | Given a call node in the ICFG, get all of the entry nodes for its
-- possible targets.  For a call to a defined function, this is the
-- first instruction in the function.  For calls to external
-- functions, this just returns the node representing the external.
getICFGCallEntries :: ICFG -> Node -> [Node]
getICFGCallEntries g callNode =
  map fst $ filter (isCallToEntry . snd) $ lsuc (icfgGraph g) callNode
{-# INLINE getICFGCallEntries #-}

isCallToEntry :: ICFGEdge -> Bool
isCallToEntry (CallToEntry _) = True
isCallToEntry _ = False


-- | Handle case 2 of the algorithm: function exit nodes (e_p, d_2).
-- This function adds a summary edge in the caller and adds propagates
-- local information in the caller along call-to-return edges.
--
-- A lot of the work actually happens in 'summarizeCallEdge', which is
-- broken out just because it was getting large.  Hopefully the
-- inliner will take care of pasting everything back together for
-- additional optimization.
--
-- Note: n is e_p in the algorithm
addExitEdges :: (IFDSAnalysis a domType, Ord domType, Show domType)
                => Either (Maybe ExternalFunction) Instruction
                -> PathEdge domType
                -> IFDSM a domType ()
addExitEdges (Left ef) (PathEdge d1 n d2) = do
  unknownId <- gets (icfgUnknownNode . icfg)
  analysis <- gets ifdsAnalysis
  incNodes <- gets incomingNodes
  let e_p = n
      s_p = case ef of
        Nothing ->
          let Just uid = unknownId
          in uid
        Just ef' -> externalFunctionUniqueId ef'
      funcEntry = IFDSNode s_p d1
      funcExit = IFDSNode e_p d2
      retEdgeF = externReturnVal analysis d2 ef
      callEdges = maybe S.empty id $ M.lookup funcEntry incNodes

  addEndSummary funcEntry funcExit
  mapM_ (summarizeCallEdge retEdgeF) (S.toList callEdges)
addExitEdges (Right ri) (PathEdge d1 n d2) = do
  analysis <- gets ifdsAnalysis
  incNodes <- gets incomingNodes
  let e_p = n
  s_p <- nodeToFunctionEntryNode e_p
  let funcEntry = IFDSNode s_p d1
      funcExit = IFDSNode e_p d2
      retEdgeF = returnVal analysis d2 ri
      callEdges = maybe S.empty id $ M.lookup funcEntry incNodes

  addEndSummary funcEntry funcExit
  mapM_ (summarizeCallEdge retEdgeF) (S.toList callEdges)
{-# INLINE addExitEdges #-}

summarizeCallEdge :: (Ord domType, Show domType)
                     => (Instruction -> [Maybe domType])
                     -> IFDSNode domType
                     -> IFDSM a domType ()
summarizeCallEdge retEdgeF (IFDSNode c d4)= do
  g <- getICFG
  let Just (InstNode callInst) = lab g c
      retEdges = retEdgeF callInst

  mapM_ mkSummaryAndPathEdges retEdges
  where
    mkSummaryAndPathEdges d5 = do
      summEdges <- gets summaryEdges
      let summEdge = SummaryEdge c d4 d5
      case summEdge `S.member` summEdges of
        True -> return ()
        False -> do
          addSummaryEdge summEdge
          entValMap <- gets entryValues
          let entVals = maybe S.empty id (M.lookup c entValMap)
          mapM_ (addCallToReturns d5) (S.toList entVals)
    addCallToReturns d5 d3 = do
      pedges <- gets pathEdges
      let e1 = PathEdge d3 c d4
          callToReturnEdge = PathEdge d3 (callNodeToReturnNode c) d5
      case e1 `S.member` pedges of
        False -> return () -- This edge isn't actually in G#
        True -> propagate callToReturnEdge
{-# INLINE summarizeCallEdge #-}



addEndSummary :: (Ord domType) => IFDSNode domType -> IFDSNode domType -> IFDSM a domType ()
addEndSummary funcEntry funcExit = do
  curSummaries <- gets endSummary
  let updatedSummaries =
        maybe (S.singleton funcExit) (S.insert funcExit) (M.lookup funcEntry curSummaries)
  modify (\s -> s { endSummary = M.insert funcEntry updatedSummaries curSummaries })
{-# INLINE addEndSummary #-}

callNodeToReturnNode :: Node -> Node
callNodeToReturnNode = negate
{-# INLINE callNodeToReturnNode #-}

-- | Given a node in the ICFG corresponding to some instruction, find
-- the entry node for the function containing it.
-- nodeToFunctionEntryNode :: ICFG -> Node -> Node
nodeToFunctionEntryNode :: Node -> IFDSM a domType Node
nodeToFunctionEntryNode n = do
  g <- getICFG
  let Just nodLab = lab g n
      InstNode i = nodLab
      Just bb = instructionBasicBlock i
      f = basicBlockFunction bb
      s = functionEntryInstruction f
  return (instructionUniqueId s)
{-# INLINE nodeToFunctionEntryNode #-}

instructionFunction :: Instruction -> Function
instructionFunction i = basicBlockFunction bb
  where
    Just bb = instructionBasicBlock i
{-# INLINE instructionFunction #-}

-- | Handle the case of local control flow (extending the
-- intraprocedural part of the exploded supergraph).
addIntraEdges :: (IFDSAnalysis a domType, Ord domType, Show domType)
                 => Instruction
                 -> PathEdge domType
                 -> IFDSM a domType ()
addIntraEdges i (PathEdge d1 n d2) = do
  g <- getICFG
  analysis <- gets ifdsAnalysis

  -- Predecessors in the ICFG
  let intraPredEdges = map toIntraEdge $ lpre g n
      intraSuccessors = suc g n
      mkIntraEdge ipes successor = map (\d3 -> PathEdge d1 successor d3) ipes

  -- Domain elements reachable due to this instruction
  let dests = flow analysis d2 i intraPredEdges
      inducedEdges = concatMap (mkIntraEdge dests) intraSuccessors

  mapM_ propagate inducedEdges
{-# INLINE addIntraEdges #-}


{-# INLINE toIntraEdge #-}
toIntraEdge :: (Node, ICFGEdge) -> CFGEdge
toIntraEdge (_, ie) = e
  where
    IntraEdge e = ie

addReturnNodeEdges :: (IFDSAnalysis a domType, Ord domType, Show domType)
                      => Instruction
                      -> PathEdge domType
                      -> IFDSM a domType ()
addReturnNodeEdges i (PathEdge d1 n d2) = do
  g <- getICFG
  analysis <- gets ifdsAnalysis

  let dests = returnNodeFlow analysis d2 i
      intraSuccessors = suc g n
      inducedEdges = concatMap (mkIntraEdge dests) intraSuccessors
      mkIntraEdge ipes successor =
        map (\d3 -> PathEdge d1 successor d3) ipes

  mapM_ propagate inducedEdges
{-# INLINE addReturnNodeEdges #-}

propagate :: (Ord domType) => PathEdge domType -> IFDSM a domType ()
propagate newEdge = do
  s <- get
  case newEdge `S.member` pathEdges s of
    True -> return ()
    False -> put s { pathEdges = newEdge `S.insert` (pathEdges s)
                   , ifdsWorklist = addWorkItem newEdge (ifdsWorklist s)
                   }
{-# INLINE addSummaryEdge #-}

addSummaryEdge :: (Ord domType) => SummaryEdge domType -> IFDSM a domType ()
addSummaryEdge se@(SummaryEdge callNode _ d') = do
  s <- get
  let updatedCache = maybe (S.singleton d') (S.insert d') (M.lookup callNode (summaryValues s))
  put s { summaryEdges = S.insert se (summaryEdges s)
        , summaryValues = M.insert callNode updatedCache (summaryValues s)
        }
{-# INLINE propagate #-}

-- | Build a self loop on the special "null" element for the given
-- entry point
mkInitialEdges :: (IFDSAnalysis a domType) => a -> Module -> Function -> [PathEdge domType]
mkInitialEdges a m f = map makeEdge vs
  where
    inst0id = instructionUniqueId $ functionEntryInstruction f
    vs = entrySetup a m f
    makeEdge v = PathEdge v inst0id v


{-

For the main switch statement, there are three cases:

 * Call node

   Can distinguish these InstNodes because it will always be a call or
   invoke instruction

 * Exit node

   These will always be ret instructions

 * Everything else


The current function is always accessible in constant time because any
instruction has constant-time access to its enclosing function.
Matching edges is a simple equality test on the set of all outgoing
edges from ret nodes.

Don't bother building any of the exploded supergraph explicitly.  Just
keep a Set of the PathEdges.  Reachability queries are then of the form:

  S.member (s_p, nodeid) pathEdge

For nodeid in procedure p.

-}