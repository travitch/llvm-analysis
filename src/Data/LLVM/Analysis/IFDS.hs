{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
module Data.LLVM.Analysis.IFDS where

import Data.Graph.Inductive hiding ( (><) )
import Data.List ( foldl' )
import Data.Map ( Map )
import Data.Sequence ( Seq, ViewL(..), (><), viewl )
import Data.Set ( Set )
import qualified Data.Map as M
import qualified Data.Sequence as Seq
import qualified Data.Set as S

import Text.Printf

import Data.LLVM
import Data.LLVM.CFG
import Data.LLVM.ICFG

type Worklist a = Seq (PathEdge a)

class IFDSAnalysis a domType where
  flow :: a -> Maybe domType -> Instruction -> [CFGEdge] -> [Maybe domType]
  -- ^ Compute local flow information for the domain element after this
  -- instruction is executed.  The returned list is the list of domain
  -- variables reachable from this domain element after the statement is
  -- executed.  This models local control flow.
  callFlow :: a -> Maybe domType -> Instruction -> [Maybe domType]
  -- ^ Similar to 'flow', but models local information flow across
  -- call->return edges.
  passArgs :: a -> Maybe domType -> [Maybe domType]
  returnVal :: a -> Maybe domType -> [Maybe domType]
  analysisBandwidth :: a -> Int


-- | An edge from <s_p, d_1> to <n, d_2> noting that <n, d_2> is
-- reachable in the exploded supergraph from <s_p, d_1>.  s_p is not
-- explicitly recorded because it is uniquely determined by n.
data PathEdge domType = PathEdge !(Maybe domType) !Node !(Maybe domType)
                      deriving (Ord, Eq)

-- After the analysis is done, reduce the Set PathEdge -> Map (Node,
-- domType) [domType] to start answering queries.  Really, only the
-- second two members are useful (reachable vs not reachable).

-- | An edge summarizing the flow information for the call site
-- identified by the node.
data SummaryEdge domType = SummaryEdge !Node !(Maybe domType) !(Maybe domType)
                           deriving (Ord, Eq)

data IFDSNode domType = IFDSNode !Node !(Maybe domType)
                        deriving (Ord, Eq)

data IFDS domType = IFDS { pathEdges :: Set (PathEdge domType)
                         , summaryEdges :: Set (SummaryEdge domType)
                         , incomingEdges :: Map (IFDSNode domType) (Set (IFDSNode domType))
                         , endSummary :: Map (IFDSNode domType) (Set (IFDSNode domType))
                         , entryValues :: Map Node (Set (Maybe domType))
                         , worklist :: Worklist domType
                         , icfg :: ICFG
                         }

ifds :: (IFDSAnalysis a domType, Ord domType) => a -> ICFG -> Set (PathEdge domType)
ifds analysis g =
  tabulate analysis IFDS { pathEdges = S.fromList initialEdges
                         , summaryEdges = S.empty
                         , worklist = Seq.fromList initialEdges
                         , incomingEdges = M.empty
                         , endSummary = M.empty
                         , entryValues = M.empty
                         , icfg = g
                         }
  where
    initialEdges = map mkInitialEdge (icfgEntryPoints g)

tabulate :: (IFDSAnalysis a domType, Ord domType)
            => a
            -> IFDS domType
            -> Set (PathEdge domType)
tabulate analysis currentState = case viewl (worklist currentState) of
  EmptyL -> pathEdges currentState
  e@(PathEdge d1 n d2) :< rest ->
    let nextState = currentState { worklist = rest }
    in case lab ((icfgGraph . icfg) currentState) n of
      Nothing -> error $ printf "Error, node %d is missing from the ICFG" n


      -- Case 1 of the algorithm (call nodes)
      -- FIXME: Remember to populate the cache of call entry values
      Just (InstNode ci@CallInst { }) -> tabulate analysis nextState
      Just (InstNode ii@InvokeInst { }) -> tabulate analysis nextState


      -- Case 2 of the algorithm (return nodes)
      Just (ExternalExit (Just ef)) -> tabulate analysis nextState
      Just (InstNode ri@RetInst { }) -> exitEdges e analysis nextState
      -- Slightly special subcase - will see about how to handle unknown functions
      Just (ExternalExit Nothing) -> tabulate analysis nextState


      -- Case 3 of the algorithm (intraprocedural information flow)
      Just (InstNode i) -> intraEdges i d1 n d2 analysis nextState
      -- FIXME: Handle the case of ExternalEntry?

{-
data IFDS domType = IFDS { pathEdges :: Set (PathEdge domType)
                         , summaryEdges :: Set (SummaryEdge domType)
                         , incomingEdges :: Map (IFDSNode domType) (Set (IFDSNode domType))
                         , worklist :: Worklist domType
                         }

-}

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
exitEdges :: (IFDSAnalysis a domType, Ord domType)
             => PathEdge domType
             -> a
             -> IFDS domType
             -> Set (PathEdge domType)
exitEdges (PathEdge d1 n d2) analysis currentState =
  tabulate analysis nextState { endSummary = nextEndSummary }
  where
    e_p = n
    s_p = nodeToFunctionEntryNode (icfg currentState) e_p

    funcEntry = IFDSNode s_p d1
    funcExit = IFDSNode e_p d2

    currentEndSummary = endSummary currentState
    updatedEndSummary =
      maybe (S.singleton funcExit) (S.insert funcExit) $
        M.lookup funcEntry currentEndSummary
    nextEndSummary = M.insert funcEntry updatedEndSummary currentEndSummary
    -- ^ Add a node to the EndSummary set saying that <e_p, d_2> is an
    -- exit node for <s_p,d_1>.

    callEdges = maybe S.empty id $ M.lookup funcEntry (incomingEdges currentState)
    -- ^ These edges (memoized since we can't compute the inverse flow
    -- function) are edges from call nodes to the beginning of this
    -- function.

    nextState = S.fold (summarizeCallEdge analysis funcExit) currentState callEdges
    -- ^ Insert summary edges for the call edge now that we have
    -- reached the end of this function.
{-# INLINE exitEdges #-}

-- | Insert a summary edge in the caller for this call edge.
--
--  From the algorithm:
--
-- > foreach d_5 in retVal
summarizeCallEdge :: (IFDSAnalysis a domType, Ord domType)
                     => a
                     -- ^ Analysis being run
                     -> IFDSNode domType
                     -- ^ Exit node
                     -> IFDSNode domType
                     -- ^ Call node
                     -> IFDS domType
                     -> IFDS domType
summarizeCallEdge analysis (IFDSNode _ d2) (IFDSNode c d4) currentState =
  foldl' mkSummaryEdges currentState $ returnVal analysis d2
  where
    -- | d5 is one of the values that d4 maps to from the return node
    -- This function makes the summary edges and propagates local
    -- information in the caller along call-to-return edges.
    mkSummaryEdges s d5 = case S.member summEdge (summaryEdges s) of
      True -> s -- Already have summary edge, can skip doing any work here
      False -> S.fold addCallToReturns s' entVals -- Note, state here includes the summary edge
      where
        summEdge = SummaryEdge c d4 d5
        s' = addSummaryEdge summEdge s
        calleeEntryNode = nodeToFunctionEntryNode (icfg currentState) c
        entVals = maybe S.empty id (M.lookup calleeEntryNode (entryValues currentState))

        addCallToReturns d3 summState =
          let e1 = PathEdge d3 c d4
              callToReturnEdge = PathEdge d3 (callNodeToReturnNode c) d5
          in case S.member e1 (pathEdges summState) of
            False -> summState
            True -> propagate [callToReturnEdge] summState
{-# INLINE summarizeCallEdge #-}

callNodeToReturnNode :: Node -> Node
callNodeToReturnNode = negate
{-# INLINE callNodeToReturnNode #-}

-- | Given a node in the ICFG corresponding to some instruction, find
-- the entry node for the function containing it.
nodeToFunctionEntryNode :: ICFG -> Node -> Node
nodeToFunctionEntryNode g n = instructionUniqueId s
  where
    Just (InstNode i) = lab (icfgGraph g) n
    Just bb = instructionBasicBlock i
    f = basicBlockFunction bb
    s = functionEntryInstruction f
{-# INLINE nodeToFunctionEntryNode #-}

-- | Handle the case of local control flow (extending the
-- intraprocedural part of the exploded supergraph).
intraEdges :: (IFDSAnalysis a domType, Ord domType)
              => Instruction
              -> Maybe domType
              -> Node
              -> Maybe domType
              -> a
              -> IFDS domType
              -> Set (PathEdge domType)
intraEdges i d1 n d2 analysis currentState =
  tabulate analysis (propagate newEdges currentState)
  where
    g = (icfgGraph . icfg) currentState
    currentEdges = pathEdges currentState

    dests = flow analysis d2 i intraPredEdges
    intraPredEdges = map (toIntraEdge . snd) $ lpre g n

    intraSuccessors = suc g n
    inducedEdges = concatMap (mkIntraEdge dests) intraSuccessors
    newEdges = filter (not . (flip S.member) currentEdges) inducedEdges
    -- ^ Only keep the edges that are not already known

    mkIntraEdge ipes successor = map (\d3 -> PathEdge d1 successor d3) ipes
{-# INLINE intraEdges #-}

{-# INLINE toIntraEdge #-}
toIntraEdge :: ICFGEdge -> CFGEdge
toIntraEdge (IntraEdge e) = e

{-# INLINE propagate #-}
propagate :: (Ord domType) => [PathEdge domType] -> IFDS domType -> IFDS domType
propagate newEdges s = s { pathEdges = currentEdges `S.union` S.fromList newEdges
                         , worklist = worklist s >< Seq.fromList newEdges
                         }
  where
    currentEdges = pathEdges s

{-# INLINE addSummaryEdge #-}
addSummaryEdge :: (Ord domType) => SummaryEdge domType -> IFDS domType -> IFDS domType
addSummaryEdge se state = state { summaryEdges = S.insert se (summaryEdges state) }

-- | Build a self loop on the special "null" element for the given
-- entry point
mkInitialEdge :: Function -> PathEdge domType
mkInitialEdge f = PathEdge Nothing (instructionUniqueId inst0) Nothing
  where
    inst0 = functionEntryInstruction f


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