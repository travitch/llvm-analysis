{-# LANGUAGE MultiParamTypeClasses, BangPatterns, NoMonomorphismRestriction #-}
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
  -- * Dataflow analysis
  DataflowAnalysis(..),
  HasCFG(..),
  forwardDataflow,
  backwardDataflow,
  forwardBlockDataflow,
  backwardBlockDataflow,
  -- * Dataflow results
  DataflowResult,
  dataflowResult
  ) where

import Algebra.Lattice
import Control.DeepSeq
import Control.Monad ( foldM )
import Data.Graph.Inductive hiding ( (><) )
import Data.HashMap.Strict ( HashMap )
import qualified Data.HashMap.Strict as M
import qualified Data.Set as S
import Text.Printf

import Data.LLVM.CFG
import Data.LLVM.Types

-- | The opaque result of a dataflow analysis
data DataflowResult a =
    DataflowInstructionResult (HashMap Instruction a)
  | DataflowBlockResult { blockEndResults :: HashMap BasicBlock a
                        , dataflowPredBlocks :: BasicBlock -> [BasicBlock]
                        , dataflowIncomingEdges :: BasicBlock -> [CFGEdge]
                        , dataflowInitial :: a
                        }

instance (Eq a) => Eq (DataflowResult a) where
  (DataflowInstructionResult m1) == (DataflowInstructionResult m2) = m1 == m2
  DataflowBlockResult { blockEndResults = m1 } == DataflowBlockResult { blockEndResults = m2} = m1 == m2
  _ == _ = False

instance (NFData a) => NFData (DataflowResult a) where
  rnf (DataflowInstructionResult m1) = m1 `deepseq` ()
  rnf DataflowBlockResult { blockEndResults = m1 } = m1 `deepseq` ()

-- | Get the result of a dataflow analysis at the given instruction.
-- Will throw an error if the instruction is not in the function for
-- which this analysis was run.
dataflowResult :: (DataflowAnalysis m a)
                  => DataflowResult a
                  -> Instruction -> m a
dataflowResult (DataflowInstructionResult m) i =
  case M.lookup i m of
    Nothing -> error ("Instruction " ++ show i ++ " has no dataflow result")
    Just r -> return r
dataflowResult DataflowBlockResult { blockEndResults = m
                                   , dataflowPredBlocks = preds
                                   , dataflowIncomingEdges = incEdges
                                   , dataflowInitial = s0
                                   } i = do
  let Just bb = instructionBasicBlock i
      predBlocks = preds bb
      predResults = map blockRes predBlocks
      initialInputState = case null predResults of
        True -> s0
        False -> meets predResults
      initialIncomingEdges = incEdges bb
      predInsts = takeWhile (/=i) (basicBlockInstructions bb)
      -- The first instruction in the block has incoming edges decided
      -- by the CFG.  The rest of the instructions have default
      -- internal edges only.
      incomingEdges = initialIncomingEdges : repeat [DefaultEdge]
      -- This is the input state for the requested instruction
      -- (obtained by applying the transfer function to all of the
      -- instructions in the block before it).
  incomingState <- foldM transfer' initialInputState (zip predInsts incomingEdges)
  -- Now apply the transfer function one more time to get the dataflow
  -- fact for this instruction.
  case null predInsts of
    -- This is the first instruction in the block, use the
    -- incoming edges from the CFG
    True -> transfer incomingState i initialIncomingEdges
    -- Otherwise, there is just an internal incoming edge
    False -> transfer incomingState i [DefaultEdge]
  where
    transfer' s (inst, es) = transfer s inst es
    blockRes bb = case M.lookup bb m of
      Nothing -> error ("Basic block " ++ show (Value bb) ++ " has no dataflow result")
      Just r -> r

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
                   -> m (DataflowResult a)
forwardDataflow = dataflowAnalysis lpre lsuc

-- | Perform a backward dataflow analysis of the given type over a
-- function or CFG.
backwardDataflow :: (Eq a, DataflowAnalysis m a, HasCFG b)
                    => a -- ^ The initial state of the analysis to run
                    -> b -- ^ The CFG (or Function) on which to run the dataflow analysis
                    -> m (DataflowResult a)
backwardDataflow = dataflowAnalysis lsuc lpre

dataflowAnalysis :: (Eq a, DataflowAnalysis m a, HasCFG b)
                    => (CFGType -> Node -> [(Node, CFGEdge)])
                    -> (CFGType -> Node -> [(Node, CFGEdge)])
                    -> a -> b -> m (DataflowResult a)
dataflowAnalysis predFunc succFunc analysis f = do
  let instructions = map snd $ labNodes cfg
      initialStates = M.fromList $ zip instructions (repeat top)
      s0 = (initialStates, S.empty)
  res <- dataflow instructions s0
  return (DataflowInstructionResult res)
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
          in return (outputFacts', S.size nextWork' `seq` nextWork')
      where
        lastOutputFact = lookupFact outputFacts inst
        (preds, incomingEdges) = unzip $ predFunc cfg (instructionUniqueId inst)
        inputFact = case null preds of
          True -> analysis
          False -> meets $ map (lookupFact outputFacts . value cfg) preds

{-# INLINE value #-}
value :: CFGType -> Int -> Instruction
value cfg nod = case lab cfg nod of
  Just v -> v
  Nothing -> error $ printf "No value for CFG node %d" nod

basicBlockSuccessors :: CFGType -> BasicBlock -> [BasicBlock]
basicBlockSuccessors cfg bb = map (toBlock cfg) ss
  where
    exitInst = basicBlockTerminatorInstruction bb
    ss = suc cfg (instructionUniqueId exitInst)

{-# INLINE toBlock #-}
toBlock :: CFGType -> Int -> BasicBlock
toBlock cfg n =
  case lab cfg n of
    Nothing -> error "Instruction missing from CFG"
    Just i ->
      case instructionBasicBlock i of
        Nothing -> error "Instruction in CFG should have a basic block"
        Just b -> b

basicBlockPredecessors :: CFGType -> BasicBlock -> [BasicBlock]
basicBlockPredecessors cfg bb = map (toBlock cfg) ps
  where
    firstInst : _ = basicBlockInstructions bb
    ps = pre cfg (instructionUniqueId firstInst)

basicBlockPredEdges :: CFGType -> BasicBlock -> [CFGEdge]
basicBlockPredEdges cfg bb =
  map (\(_, _, l) -> l) $ inn cfg (instructionUniqueId startInst)
  where
    startInst : _ = basicBlockInstructions bb

basicBlockSuccEdges :: CFGType -> BasicBlock -> [CFGEdge]
basicBlockSuccEdges cfg bb =
  map (\(_, _, l) -> l) $ out cfg (instructionUniqueId exitInst)
  where
    exitInst = basicBlockTerminatorInstruction bb

forwardBlockDataflow :: (Eq a, HasCFG b, DataflowAnalysis m a)
                        => a -> b -> m (DataflowResult a)
forwardBlockDataflow =
  blockDataflowAnalysis basicBlockInstructions basicBlockPredEdges
      basicBlockPredecessors basicBlockSuccessors lpre

backwardBlockDataflow :: (Eq a, HasCFG b, DataflowAnalysis m a)
                         => a -> b -> m (DataflowResult a)
backwardBlockDataflow =
  blockDataflowAnalysis (reverse . basicBlockInstructions) basicBlockSuccEdges
      basicBlockSuccessors basicBlockPredecessors lsuc

blockDataflowAnalysis :: (Eq a, DataflowAnalysis m a, HasCFG b)
                         => (BasicBlock -> [Instruction])
                         -> (CFGType -> BasicBlock -> [CFGEdge])
                         -> (CFGType -> BasicBlock -> [BasicBlock])
                         -> (CFGType -> BasicBlock -> [BasicBlock])
                         -> (CFGType -> Node -> [(Node, CFGEdge)])
                         -> a -> b -> m (DataflowResult a)
blockDataflowAnalysis orderedBlockInsts blockIncomingEdges blockPreds blockSuccs predFunc analysis f = do
  let blocks = functionBody func
      initialStates = M.fromList $ zip blocks (repeat top)
      s0 = (initialStates, S.empty)
  res <- dataflow blocks s0
  return DataflowBlockResult { blockEndResults = res
                             , dataflowPredBlocks = blockPreds cfg
                             , dataflowIncomingEdges = blockIncomingEdges cfg
                             , dataflowInitial = analysis
                             }
  where
    cfgWrapper = getCFG f
    cfg = cfgGraph cfgWrapper
    -- ^ The control flow graph for this function
    func = cfgFunction cfgWrapper

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
      (facts', nextWork') <- foldM processBlock factsAndWork work
      case S.null nextWork' of
        True -> return facts'
        False -> dataflow (S.toList nextWork') (facts', S.empty)

    lookupFact facts block = case M.lookup block facts of
      Just fact -> fact
      Nothing -> error $ printf "No facts for block %s" (show (Value block))

    processBlock fw@(outputFacts, nextWork) block = do
      let inputFact = case null preds of
            True -> analysis
            False -> meets $ map (lookupFact outputFacts) preds
      outputFact <- foldM processNode inputFact (orderedBlockInsts block)
      case outputFact == lastOutputFact of
        True -> return fw
        False ->
          -- Update the worklist with the successors of this block
          -- and the facts with what we just computed
          let outputFacts' = M.insert block outputFact outputFacts
              succBlocks = blockSuccs cfg block
              nextWork' = S.union nextWork (S.fromList succBlocks)
          in return (outputFacts', S.size nextWork' `seq` nextWork')
      where
        lastOutputFact = lookupFact outputFacts block
        preds = blockPreds cfg block

    -- | Apply the transfer function to each instruction in this
    -- block.  If the result is different than the current fact, add
    -- all successors to the worklist.
    processNode inputFact inst =
      let incomingEdges = snd $ unzip $ predFunc cfg (instructionUniqueId inst)
      in transfer inputFact inst incomingEdges
