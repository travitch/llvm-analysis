-- | This module defines an interface for intra-procedural dataflow
-- analysis (forward and backward).
--
-- The user simply defines a type to represent the state of their
-- dataflow analysis as an instance of 'DataflowAnalysis'.  This class
-- adds one function, 'transfer', to the semilattices defined in the
-- lattices package.
--
-- To use this dataflow analysis framework, pass it an initial
-- analysis state and either a control flow graph or a function.  The
-- analysis then returns a function that maps instructions to the
-- dataflow value at that instruction.  For example,
--
-- > let initialState = ...
-- >     cfg = mkCFG f
-- >     results = forwardDataflow initialState cfg
-- > in results (cfgFinalValue cfg)
--
-- gives the dataflow value for the return instruction in function
-- @f@.  Any instruction in @f@ can be used as an argument to the
-- @result@ function.
module Data.LLVM.Analysis.Dataflow (
  DataflowAnalysis(..),
  HasCFG(..),
  forwardDataflow,
  backwardDataflow,
  ) where

import Algebra.Lattice
import Data.Graph.Inductive hiding ( (><) )
import qualified Data.HashMap.Strict as M
import Data.Sequence ( ViewL(..), (><), viewl )
import qualified Data.Sequence as S
import Text.Printf

import Data.LLVM.CFG
import Data.LLVM.Types

-- | A class defining the interface to a dataflow analysis.  The
-- analysis object itself that is passed to one of the dataflow
-- functions acts as the initial state.
class BoundedMeetSemiLattice a => DataflowAnalysis a where
  transfer :: a -> Value -> [CFGEdge] -> a
  -- ^ The transfer function of this analysis.  It is given the
  -- current set of facts, the current instruction, and a list of
  -- incoming edges.


-- | Perform a forward dataflow analysis of the given type over a
-- function or CFG.
forwardDataflow :: (Eq a, DataflowAnalysis a, HasCFG b) => a -> b -> Value -> a
forwardDataflow = dataflowAnalysis lpre lsuc cfgExitNode

-- | Perform a backward dataflow analysis of the given type over a
-- function or CFG.
backwardDataflow :: (Eq a, DataflowAnalysis a, HasCFG b) => a -> b -> Value -> a
backwardDataflow = dataflowAnalysis lsuc lpre cfgEntryNode

dataflowAnalysis :: (Eq a, DataflowAnalysis a, HasCFG b) =>
                    (CFGType -> Node -> [(Node, CFGEdge)]) ->
                    (CFGType -> Node -> [(Node, CFGEdge)]) ->
                    (CFG -> Node) -> a -> b -> Value -> a
dataflowAnalysis predFunc succFunc finalNodeFunc analysis f target =
  lookupFact finalStates (valueUniqueId target)
  where
    finalStates = dataflow initialWorklist initialStates

    cfg = getCFG f
    -- ^ The control flow graph for this function

    initialWorklist = S.fromList (nodes (cfgGraph cfg))
    -- ^ Put all nodes on the worklist
    initialStates = M.fromList $ zip (nodes (cfgGraph cfg)) (repeat analysis)
    -- ^ Start all nodes with the initial state provided by the user

    -- | If there is nothing left in the worklist, return the facts
    -- associated with the exit node.
    dataflow work facts = case viewl work of
      EmptyL -> facts
      nod :< rest -> processNode nod rest facts

    lookupFact facts n = case M.lookup n facts of
      Just fact -> fact
      Nothing -> error $ printf "No facts for CFG node %d" n

    -- | Apply the transfer function to this node.  If the result is
    -- different than the current fact, add all successors to the
    -- worklist.
    processNode nod work outputFacts = case outputFact == lastOutputFact of
      -- No change, don't add successors
      True -> dataflow work outputFacts
      -- Facts changed, update map and add successors
      False -> dataflow work' outputFacts'
      where
        outputFact = transfer inputFact value incomingEdges
        lastOutputFact = lookupFact outputFacts nod

        -- Updated worklist and facts
        work' = work >< S.fromList (fst $ unzip $ succFunc (cfgGraph cfg) nod)
        outputFacts' = M.insert nod outputFact outputFacts

        (preds, incomingEdges) = unzip $ predFunc (cfgGraph cfg) nod
        inputFact = case preds of
          -- For the entry node, the input facts never change and we
          -- can use the input to the dataflow analysis.
          [] -> analysis
          -- Otherwise, get all output facts for the predecessor and
          -- join them.
          _ -> meets $ map (lookupFact outputFacts) preds

        value = case lab (cfgGraph cfg) nod of
          Just v -> v
          Nothing -> error $ printf "No value for CFG node %d" nod

