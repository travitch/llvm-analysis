{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
module Data.LLVM.Analysis.IFDS where

import Data.Graph.Inductive hiding ( (><) )
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
  flow :: a -> Maybe domType -> Instruction -> [CFGEdge] -> [domType]
  -- ^ Compute local flow information for the domain element after this
  -- instruction is executed.  The returned list is the list of domain
  -- variables reachable from this domain element after the statement is
  -- executed.  This models local control flow.
  callFlow :: a -> Maybe domType -> Instruction -> [domType]
  -- ^ Similar to 'flow', but models local information flow across
  -- call->return edges.
  passArgs :: a -> domType -> [domType]
  returnVal :: a -> domType -> [domType]
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

data IFDSNode domType = IFDSNode !Node !(Maybe domType)

data IFDS domType = IFDS { pathEdges :: Set (PathEdge domType)
                         , summaryEdges :: Set (SummaryEdge domType)
                         , incomingEdges :: Map (IFDSNode domType) (Set (IFDSNode domType))
                         , worklist :: Worklist domType
                         }

ifds :: (IFDSAnalysis a domType, Ord domType) => a -> ICFG -> Set (PathEdge domType)
ifds analysis icfg =
  tabulate analysis icfg IFDS { pathEdges = S.fromList initialEdges
                              , summaryEdges = S.empty
                              , worklist = Seq.fromList initialEdges
                              , incomingEdges = M.empty
                              }
  where
    initialEdges = map mkInitialEdge (icfgEntryPoints icfg)

tabulate :: (IFDSAnalysis a domType, Ord domType)
            => a
            -> ICFG
            -> IFDS domType
            -> Set (PathEdge domType)
tabulate analysis icfg currentState = case viewl (worklist currentState) of
  EmptyL -> pathEdges currentState
  (PathEdge d1 n d2) :< rest -> case lab (icfgGraph icfg) n of
    Nothing -> error $ printf "Error, node %d is missing from the ICFG" n
    -- Case 1 of the algorithm
    Just (InstNode ci@CallInst { }) -> tabulate analysis icfg currentState { worklist = rest }
    Just (InstNode ii@InvokeInst { }) -> tabulate analysis icfg currentState { worklist = rest }
    -- Case 2 of the algorithm
    Just (ExternalExit (Just ef)) -> tabulate analysis icfg currentState { worklist = rest }
    Just (InstNode ri@RetInst { }) -> tabulate analysis icfg currentState { worklist = rest }
    -- Slightly special subcase - will see about how to handle unknown functions
    Just (ExternalExit Nothing) -> tabulate analysis icfg currentState { worklist = rest }
    -- Case 3 of the algorithm
    Just (InstNode i) -> intraEdges i d1 n d2 analysis icfg currentState rest
    -- FIXME: Handle the case of ExternalEntry

intraEdges :: (IFDSAnalysis a domType, Ord domType)
              => Instruction
              -> Maybe domType
              -> Node
              -> Maybe domType
              -> a
              -> ICFG
              -> IFDS domType
              -> Worklist domType
              -> Set (PathEdge domType)
intraEdges i d1 n d2 analysis icfg currentState rest =
  tabulate analysis icfg (propagate newEdges currentState rest)
  where
    g = icfgGraph icfg
    currentEdges = pathEdges currentState

    dests = flow analysis d2 i intraPredEdges
    intraPredEdges = map (toIntraEdge . snd) $ lpre g n

    intraSuccessors = suc g n
    inducedEdges = case dests of
      [] -> map mkIntraNullEdge intraSuccessors
      _ -> concatMap (mkIntraEdge dests) intraSuccessors
    newEdges = filter (not . (flip S.member) currentEdges) inducedEdges
    -- ^ Only keep the edges that are not already known

    mkIntraEdge ipes successor = map (\d3 -> PathEdge d1 successor (Just d3)) ipes
    mkIntraNullEdge successor = PathEdge d1 successor Nothing

toIntraEdge :: ICFGEdge -> CFGEdge
toIntraEdge (IntraEdge e) = e

propagate :: (Ord domType) => [PathEdge domType] -> IFDS domType -> Worklist domType -> IFDS domType
propagate newEdges s rest = s { pathEdges = currentEdges `S.union` S.fromList newEdges
                              , worklist = rest >< Seq.fromList newEdges
                              }
  where
    currentEdges = pathEdges s

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