{-# LANGUAGE MultiParamTypeClasses, BangPatterns, NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
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
  MeetSemiLattice(..),
  BoundedMeetSemiLattice(..),
  meets,
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
import Data.HashMap.Strict ( HashMap )
import qualified Data.HashMap.Strict as M
import Data.Set ( Set )
import qualified Data.Set as S
import FileLocation
import Text.Printf

import Data.LLVM.Analysis.CFG
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
  phiTransfer ::  [Instruction] -- ^ The instruction being analyzed
                 -> [(BasicBlock, a, CFGEdge)] -- ^ Incoming CFG edges
                 -> m a
  phiTransfer _ = return . meets . map (\(_, e, _) -> e)
  -- ^ The transfer function that will process all Phi nodes in
  -- parallel.

  -- | This is a hook to perform some analysis on an _edge_ before the
  -- normal transfer functions run on the next instruction.
  --
  -- This is a convenient separation of edge and instruction
  -- processing, but it also improves precision in cases like:
  --
  -- > define void @f(i8* %p) nounwind uwtable {
  -- >   %1 = icmp ne i8* %p, null
  -- >   br i1 %1, label %2, label %3
  -- >
  -- > ; <label>:2                                       ; preds = %0
  -- >   call void @free(i8* %p) nounwind
  -- >   br label %3
  -- >
  -- > ; <label>:3                                       ; preds = %2, %0
  -- >   ret void
  -- > }
  --
  -- Without separate edge processing, the join at the return
  -- statement executes before we can use information about %p being
  -- NULL on one edge of the branch.
  edgeTransfer :: a -> CFGEdge -> m a
  edgeTransfer e _ = return e


-- | The opaque result of a dataflow analysis
data DataflowResult a =
    DataflowInstructionResult (HashMap Instruction a)
  | DataflowBlockResult { blockEndResults :: HashMap BasicBlock a
                        , dataflowPredBlocks :: BasicBlock -> [(BasicBlock, CFGEdge)]
                        , dataflowPhiInput :: (BasicBlock -> a) -> BasicBlock -> [(BasicBlock, a, CFGEdge)]
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
--
-- FIXME: The block result here needs to be able to reverse the instruction
-- list if doing a backwards analysis
dataflowResult :: (DataflowAnalysis m a)
                  => DataflowResult a
                  -> Instruction -> m a
dataflowResult (DataflowInstructionResult m) i =
  case M.lookup i m of
    Nothing -> $err' ("Instruction " ++ show i ++ " has no dataflow result")
    Just r -> return r
dataflowResult DataflowBlockResult { blockEndResults = m
                                   , dataflowPredBlocks = preds
                                   , dataflowPhiInput = toPhiInput
                                   , dataflowInitial = s0
                                   } i = do
  let blockLookup x = M.lookupDefault ($err' "No block entry") x m

  let Just bb = instructionBasicBlock i
      (phiNodes, otherInsts) = basicBlockSplitPhiNodes bb
      phiPreds = toPhiInput blockLookup bb
      predBlocksAndEdges = preds bb
      predBlocks = map fst predBlocksAndEdges
      predResults = map blockRes predBlocks
      -- initialInputState = case null predResults of
      --   True -> s0
      --   False -> meets predResults
      initialIncomingEdges = map snd $ preds bb
      predInsts = takeWhile (/=i) otherInsts -- FIXME: need to reverse for backwards analysis
      -- The first instruction in the block has incoming edges decided
      -- by the CFG.  The rest of the instructions have default
      -- internal edges only.
      incomingEdges = initialIncomingEdges : repeat [DefaultEdge]
      -- This is the input state for the requested instruction
      -- (obtained by applying the transfer function to all of the
      -- instructions in the block before it).

  let predsWithFacts = map (\(b, e) -> (b, blockRes b, e)) predBlocksAndEdges
      applyEdgeTransfer (b, f, e) = do
        f' <- edgeTransfer f e
        return (b, f', e)

  predsWithEdgeFacts <- mapM applyEdgeTransfer predsWithFacts

  -- This is the state coming into the basic block (possibly after
  -- processing all phi nodes in parallel)
  initialInputState <- case null predResults of
    True -> case firstInst i of
      True -> return s0
      False -> return top
    False -> case null phiNodes of
      True -> return $! meets (map (\(_, f, _) -> f) predsWithEdgeFacts) -- return $! meets predResults
      False -> phiTransfer phiNodes predsWithEdgeFacts -- phiPreds

  -- FIXME: If there are phi nodes and a phi transfer function, that
  -- will be processing the incoming edges.... we might not want to
  -- re-process them here
  incomingState <- foldM transfer' initialInputState (zip predInsts incomingEdges)
  -- Now apply the transfer function one more time to get the dataflow
  -- fact for this instruction.
  case null predInsts of
    True -> transfer incomingState i initialIncomingEdges
    False -> transfer incomingState i [DefaultEdge]
  where
    transfer' s (inst, es) = transfer s inst es
    blockRes bb = case M.lookup bb m of
      Nothing -> $err' ("Basic block " ++ show (Value bb) ++ " has no dataflow result")
      Just r -> r


-- | Perform a forward dataflow analysis of the given type over a
-- function or CFG.
forwardDataflow :: (Eq a, DataflowAnalysis m a, HasCFG b)
                   => a -- ^ The initial state of the analysis to run
                   -> b -- ^ The CFG (or Function) on which to run the dataflow analysis
                   -> m (DataflowResult a)
forwardDataflow analysis f =
  dataflowAnalysis (instructionLabeledPredecessors cfg)
    (instructionLabeledSuccessors cfg)
    (basicBlockLabeledPredecessors cfg)
    analysis cfg
  where
    cfg = getCFG f

-- | Perform a backward dataflow analysis of the given type over a
-- function or CFG.
backwardDataflow :: (Eq a, DataflowAnalysis m a, HasCFG b)
                    => a -- ^ The initial state of the analysis to run
                    -> b -- ^ The CFG (or Function) on which to run the dataflow analysis
                    -> m (DataflowResult a)
backwardDataflow analysis f =
  dataflowAnalysis (instructionLabeledSuccessors cfg)
    (instructionLabeledPredecessors cfg)
    (basicBlockLabeledSuccessors cfg)
    analysis cfg
  where
    cfg = getCFG f

-- | FIXME: Remove phis from the instruction stream (they are handled
-- separately).
--
-- FIXME: This is actually broken.. successors to individual
-- instructions must also be fixed up (to skip phis)
dataflowAnalysis :: forall a m . (Eq a, DataflowAnalysis m a)
                    => (Instruction -> [(Instruction, CFGEdge)])
                    -> (Instruction -> [(Instruction, CFGEdge)])
                    -> (BasicBlock -> [(BasicBlock, CFGEdge)])
                    -> a -> CFG -> m (DataflowResult a)
dataflowAnalysis predFunc succFunc blockPreds analysis cfg = do
  let instructions = concatMap basicBlockInstructions (functionBody func)
      initialStates = M.fromList $ zip instructions (repeat top)
      s0 = (initialStates, S.empty)
  res <- dataflow instructions s0
  return (DataflowInstructionResult res)
  where
    func = cfgFunction cfg

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

    lookupFact :: (DataflowAnalysis m a) => HashMap Instruction a -> Instruction -> a
    lookupFact facts inst = case M.lookup inst facts of
      Just fact -> fact
      Nothing -> $err' $ printf "No facts for CFG node %s" (show inst)

    -- | Apply the transfer function to this node.  If the result is
    -- different than the current fact, add all successors to the
    -- worklist.
    processNode :: (DataflowAnalysis m a)
                   => (HashMap Instruction a, Set Instruction)
                   -> Instruction
                   -> m (HashMap Instruction a, Set Instruction)
    processNode fw@(outputFacts, nextWork) inst = do
      let blockLookup = lookupFact outputFacts . basicBlockTerminatorInstruction

      inputFact <- case null preds of
        True -> case firstInst inst of
          True -> return analysis
          False -> return top
        False -> case isFirstNonPhiInstruction inst of
          False -> return $! meets $ map (lookupFact outputFacts) preds
          True ->
            let (phiNodes, _) = basicBlockSplitPhiNodes bb
                phiPreds = buildPhiPredsInst blockPreds blockLookup inst
            in phiTransfer phiNodes phiPreds

      outputFact <- transfer inputFact inst incomingEdges
      case outputFact == lastOutputFact of
        True -> return fw
        False ->
          -- Updated worklist and facts
          let outputFacts' = M.insert inst outputFact outputFacts
              succNodes = succFunc inst
              justIds = fst $ unzip succNodes
              q = S.fromList justIds
              nextWork' = S.union nextWork q
          in return (outputFacts', S.size nextWork' `seq` nextWork')
      where
        Just bb = instructionBasicBlock inst
        lastOutputFact = lookupFact outputFacts inst
        (preds, incomingEdges) = unzip $ predFunc inst

firstInst :: Instruction -> Bool
firstInst i = firstBlock bb
  where
    Just bb = instructionBasicBlock i

forwardBlockDataflow :: (Eq a, HasCFG b, DataflowAnalysis m a)
                        => a -> b -> m (DataflowResult a)
forwardBlockDataflow analysis f =
  blockDataflowAnalysis id
    (basicBlockLabeledPredecessors cfg)
    (basicBlockLabeledSuccessors cfg)
    (instructionLabeledPredecessors cfg)
    analysis cfg
  where
    cfg = getCFG f

backwardBlockDataflow :: (Eq a, HasCFG b, DataflowAnalysis m a)
                         => a -> b -> m (DataflowResult a)
backwardBlockDataflow analysis f =
  blockDataflowAnalysis reverse
    (basicBlockLabeledSuccessors cfg)
    (basicBlockLabeledPredecessors cfg)
    (instructionLabeledSuccessors cfg)
    analysis cfg
  where
    cfg = getCFG f

blockDataflowAnalysis :: forall a m . (Eq a, DataflowAnalysis m a)
                         => ([Instruction] -> [Instruction])
                         -> (BasicBlock -> [(BasicBlock, CFGEdge)])
                         -> (BasicBlock -> [(BasicBlock, CFGEdge)])
                         -> (Instruction -> [(Instruction, CFGEdge)])
                         -> a -> CFG -> m (DataflowResult a)
blockDataflowAnalysis orderedBlockInsts blockPreds blockSuccs predFunc analysis cfg = do
  let blocks = functionBody func
      initialStates = M.fromList $ zip blocks (repeat top)
      s0 = (initialStates, S.empty)
  res <- dataflow blocks s0
  return DataflowBlockResult { blockEndResults = res
                             , dataflowPredBlocks = blockPreds
                             , dataflowPhiInput = buildPhiPreds blockPreds
                             , dataflowInitial = analysis
                             }
  where
    func = cfgFunction cfg

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

    lookupFact :: (DataflowAnalysis m a) => HashMap BasicBlock a -> BasicBlock -> a
    lookupFact facts block =
      case M.lookup block facts of
        Just fact -> fact
        Nothing -> $err' $ printf "No facts for block %s" (show (Value block))

    processBlock :: (DataflowAnalysis m a)
                    => (HashMap BasicBlock a, Set BasicBlock)
                    -> BasicBlock
                    -> m (HashMap BasicBlock a, Set BasicBlock)
    processBlock fw@(outputFacts, nextWork) block = do
      let predsWithFacts = map (\(b, e) -> (b, lookupFact outputFacts b, e)) preds
          applyEdgeTransfer (b, f, e) = do
            f' <- edgeTransfer f e
            return (b, f', e)

      predsWithEdgeFacts <- mapM applyEdgeTransfer predsWithFacts
      let (phiNodes, otherInsts) = basicBlockSplitPhiNodes block
      {-
          phiPreds = buildPhiPreds blockPreds (lookupFact outputFacts) block
-}
      inputFact <- case null preds of
        True -> case firstBlock block of
          True -> return analysis
          False -> return top
        False -> case null phiNodes of
          True -> return $! meets (map (\(_, f, _) -> f) predsWithEdgeFacts)
--          True -> return $! meets (map (lookupFact outputFacts . fst) preds)
          False -> phiTransfer phiNodes  predsWithEdgeFacts -- phiPreds
      outputFact <- foldM processNode inputFact (orderedBlockInsts otherInsts)

      case outputFact == lastOutputFact of
        True -> return fw
        False ->
          -- Update the worklist with the successors of this block
          -- and the facts with what we just computed
          let outputFacts' = M.insert block outputFact outputFacts
              succBlocks = map fst $ blockSuccs block
              nextWork' = S.union nextWork (S.fromList succBlocks)
          in return (outputFacts', S.size nextWork' `seq` nextWork')
      where
        lastOutputFact = lookupFact outputFacts block
        preds = blockPreds block

    -- | Apply the transfer function to each instruction in this
    -- block.  If the result is different than the current fact, add
    -- all successors to the worklist.
    processNode inputFact inst =
      let incomingEdges = snd $ unzip $ predFunc inst
      in transfer inputFact inst incomingEdges

firstBlock :: BasicBlock -> Bool
firstBlock bb = bb == fb
  where
    f = basicBlockFunction bb
    fb : _ = functionBody f

buildPhiPreds :: (BasicBlock -> [(BasicBlock, CFGEdge)])
                 -> (BasicBlock -> a)
                 -> BasicBlock
                 -> [(BasicBlock, a, CFGEdge)]
buildPhiPreds predFunc lookupFact block =
  map (\(b, e) -> (b, lookupFact b, e)) (predFunc block)

buildPhiPredsInst :: (BasicBlock -> [(BasicBlock, CFGEdge)])
                 -> (BasicBlock -> a)
                 -> Instruction
                 -> [(BasicBlock, a, CFGEdge)]
buildPhiPredsInst predFunc lookupFact i =
  buildPhiPreds predFunc lookupFact bb
  where
    Just bb = instructionBasicBlock i
