{-# LANGUAGE MultiParamTypeClasses #-}
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
--
-- FIXME: Change the algorithm to provide all conditions that *must*
-- hold at the current node, as well as the incoming conditions that
-- *may* hold.  This will give limited but very useful path
-- sensitivity.
module Data.LLVM.Analysis.Dataflow (
  DataflowAnalysis(..),
  HasCFG(..),
  forwardDataflow,
  backwardDataflow,
  ) where

import Algebra.Lattice
import Data.Graph.Inductive hiding ( (><) )
import Data.HashMap.Strict ( HashMap )
import qualified Data.HashMap.Strict as M
import Text.Printf

import Data.LLVM.CFG
import Data.LLVM.Types
import Data.LLVM.Internal.Worklist

-- | A class defining the interface to a dataflow analysis.  The
-- analysis object itself that is passed to one of the dataflow
-- functions acts as the initial state.
--
-- Note that the second type parameter, @c@, can be used to pass
-- information that is global and constant for the analysis of a
-- function.  This can save space by not requiring the information to
-- be stored redundantly in every dataflow fact.  If this parameter is
-- not needed, it can safely be instantiated as () (and subsequently
-- ignored).
class (BoundedMeetSemiLattice a, Monad m) => DataflowAnalysis m a where
  transfer :: a -- ^ The incoming analysis state
              -> Instruction -- ^ The instruction being analyzed
              -> [CFGEdge] -- ^ Incoming CFG edges
              -> m a
  -- ^ The transfer function of this analysis.  It is given any global
  -- constant data, the current set of facts, the current instruction,
  -- and a list of incoming edges.


-- | Perform a forward dataflow analysis of the given type over a
-- function or CFG.
forwardDataflow :: (Eq a, DataflowAnalysis m a, HasCFG b)
                   => a -- ^ The initial state of the analysis to run
                   -> b -- ^ The CFG (or Function) on which to run the dataflow analysis
                   -> m (HashMap Instruction a)
forwardDataflow = dataflowAnalysis lpre lsuc

-- | Perform a backward dataflow analysis of the given type over a
-- function or CFG.
backwardDataflow :: (Eq a, DataflowAnalysis m a, HasCFG b)
                    => a -- ^ The initial state of the analysis to run
                    -> b -- ^ The CFG (or Function) on which to run the dataflow analysis
                    -> m (HashMap Instruction a)
backwardDataflow = dataflowAnalysis lsuc lpre

dataflowAnalysis :: (Eq a, DataflowAnalysis m a, HasCFG b)
                    => (CFGType -> Node -> [(Node, CFGEdge)])
                    -> (CFGType -> Node -> [(Node, CFGEdge)])
                    -> a -> b -> m (HashMap Instruction a)
dataflowAnalysis predFunc succFunc analysis f =
  dataflow initialWorklist initialStates
  where
    cfgWrapper = getCFG f
    cfg = cfgGraph cfgWrapper
    -- ^ The control flow graph for this function

    instructions = map snd $ labNodes cfg

    initialWorklist = worklistFromList instructions
    -- ^ Put all nodes on the worklist
    initialStates = M.fromList $ zip instructions (repeat analysis)
    -- ^ Start all nodes with the initial state provided by the user

    -- | If there is nothing left in the worklist, return the facts
    -- associated with the exit node.
    dataflow work facts = case takeWorkItem work of
      EmptyWorklist -> return facts
      inst :< rest -> processNode inst rest facts

    lookupFact facts inst = case M.lookup inst facts of
      Just fact -> fact
      Nothing -> error $ printf "No facts for CFG node %s" (show inst)

    -- | Apply the transfer function to this node.  If the result is
    -- different than the current fact, add all successors to the
    -- worklist.
    processNode inst work outputFacts = do
      outputFact <- transfer inputFact inst incomingEdges
      -- Updated worklist and facts
      let outputFacts' = M.insert inst outputFact outputFacts
          succNodes = succFunc cfg (instructionUniqueId inst)
          justIds = fst $ unzip succNodes
          work' = addWorkItems (map value justIds) work



      case outputFact == lastOutputFact of
        -- No change, don't add successors
        True -> dataflow work outputFacts
        -- Facts changed, update map and add successors
        False -> dataflow work' outputFacts'
      where
        lastOutputFact = lookupFact outputFacts inst
        (preds, incomingEdges) = unzip $ predFunc cfg (instructionUniqueId inst)
        inputFact = case map value preds of
          -- For the entry node, the input facts never change and we
          -- can use the input to the dataflow analysis.
          [] -> analysis
          -- Otherwise, get all output facts for the predecessor and
          -- join them.
          predInsts -> meets $ map (lookupFact outputFacts) predInsts

        value nod = case lab cfg nod of
          Just v -> v
          Nothing -> error $ printf "No value for CFG node %d" nod
