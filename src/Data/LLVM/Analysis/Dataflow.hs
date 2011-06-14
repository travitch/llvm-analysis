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
  transfer :: a -> Value -> [EdgeCondition] -> a
  -- ^ The transfer function of this analysis.  It is given the
  -- current set of facts, the current instruction, and a list of
  -- incoming edges.

-- | Types that have control flow graphs.
class HasCFG a where
  getCFG :: a -> CFG

instance HasCFG CFG where
  getCFG = id

instance HasCFG Value where
  getCFG = mkCFG

-- | Perform a forward dataflow analysis of the given type over a
-- function or CFG.
forwardDataflow :: (Eq a, DataflowAnalysis a, HasCFG b) => a -> b -> a
forwardDataflow = dataflowAnalysis lpre lsuc cfgExitNode

-- | Perform a backward dataflow analysis of the given type over a
-- function or CFG.
backwardDataflow :: (Eq a, DataflowAnalysis a, HasCFG b) => a -> b -> a
backwardDataflow = dataflowAnalysis lsuc lpre cfgEntryNode

dataflowAnalysis :: (Eq a, DataflowAnalysis a, HasCFG b) =>
                    (CFGType -> Node -> [(Node, EdgeCondition)]) ->
                    (CFGType -> Node -> [(Node, EdgeCondition)]) ->
                    (CFG -> Node) -> a -> b -> a
dataflowAnalysis predFunc succFunc finalNodeFunc analysis f =
  dataflow initialWorklist initialStates
  where
    cfg = getCFG f
    initialWorklist = S.fromList (nodes (cfgGraph cfg))
    -- ^ Put all nodes on the worklist
    initialStates = M.fromList $ zip (nodes (cfgGraph cfg)) (repeat analysis)
    -- ^ Start all nodes with the initial state provided by the user

    -- | If there is nothing left in the worklist, return the facts
    -- associated with the exit node.
    dataflow work facts = case viewl work of
      EmptyL -> lookupFact facts (finalNodeFunc cfg)
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

