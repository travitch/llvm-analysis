{-# LANGUAGE MultiParamTypeClasses, BangPatterns #-}
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
import Control.Monad ( foldM )
import Data.Graph.Inductive hiding ( (><) )
import Data.HashMap.Strict ( HashMap )
import qualified Data.HashMap.Strict as M
import qualified Data.Set as S
import Text.Printf

import Data.LLVM.CFG
import Data.LLVM.Types

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
  let instructions = map snd $ labNodes cfg
      initialStates = M.fromList $ zip instructions (repeat analysis)
      s0 = (initialStates, S.empty)
  in dataflow instructions s0
  where
    cfgWrapper = getCFG f
    cfg = cfgGraph cfgWrapper
    -- ^ The control flow graph for this function

    -- | If there is nothing left in the worklist, return the facts
    -- associated with the exit node.
    --
    -- FIXME: When turning the workset into a worklist, see about a
    -- better sort order to visit instructions in.  Currently it
    -- should actually be automatically close to topological sort
    -- order since instruction IDs are assigned sequentially and the
    -- IR is in SSA form (so definitions dominate uses -- topological
    -- order).
    dataflow !work !factsAndWork = do
      (facts', nextWork') <- foldM processNode factsAndWork work
      case S.null nextWork' of
        True -> return facts'
        False -> dataflow (S.toList nextWork') (facts', S.empty)

    lookupFact facts inst = case M.lookup inst facts of
      Just fact -> fact
      Nothing -> error $ printf "No facts for CFG node %s" (show inst)

    -- | Apply the transfer function to this node.  If the result is
    -- different than the current fact, add all successors to the
    -- worklist.
    processNode fw@(outputFacts, nextWork) inst = do
      outputFact <- transfer inputFact inst incomingEdges
      case outputFact == lastOutputFact of
        True -> return fw
        False ->
          -- Updated worklist and facts
          let outputFacts' = M.insert inst outputFact outputFacts
              succNodes = succFunc cfg (instructionUniqueId inst)
              justIds = fst $ unzip succNodes
              q = S.fromList (map (value cfg) justIds)
              nextWork' = S.union nextWork q
          in  return (outputFacts', S.size nextWork' `seq` nextWork')
      where
        lastOutputFact = lookupFact outputFacts inst
        (preds, incomingEdges) = unzip $ predFunc cfg (instructionUniqueId inst)
        inputFact = case null preds of
          True -> analysis
          False -> meets $ map (lookupFact outputFacts . value cfg) preds

value cfg nod = case lab cfg nod of
  Just v -> v
  Nothing -> error $ printf "No value for CFG node %d" nod
