{-# LANGUAGE MultiParamTypeClasses, BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs, TypeFamilies #-}
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
module LLVM.Analysis.Dataflow (
  -- * Dataflow analysis
  DataflowAnalysis(..),
  MeetSemiLattice(..),
  BoundedMeetSemiLattice(..),
  -- meets,
  forwardDataflow,
  -- backwardDataflow,
  -- -- * Dataflow results
  -- DataflowResult,
  -- dataflowResult
  ) where

import Algebra.Lattice
import Compiler.Hoopl
import Control.DeepSeq
import Control.Monad ( (>=>) )
import Data.Map ( Map )
-- import Control.Monad ( foldM )
-- import Data.HashMap.Strict ( HashMap )
-- import qualified Data.HashMap.Strict as M
-- import Data.HashSet ( HashSet )
-- import qualified Data.HashSet as S
-- import Data.List ( sort )
-- import Data.Maybe ( fromMaybe )
-- import Text.Printf

import LLVM.Analysis
import LLVM.Analysis.CFG

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
              -> m a
  -- ^ The transfer function of this analysis.  It is given any global
  -- constant data, the current set of facts, the current instruction,
  -- and a list of incoming edges.
  phiTransfer ::  [(BasicBlock, a)] -- ^ Incoming CFG edges
                  -> [Instruction] -- ^ The instruction being analyzed
                  -> m a
  -- phiTransfer _ = return . meets . map (\(_, e, _) -> e)

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
  --
  -- NOTE: This transfer function is only applied to edges between
  -- basic blocks.  Within a basic block, the edge is always an
  -- unconditional edge and there is no merging anyway.
  -- edgeTransfer :: a -> CFGEdge -> m a
  -- edgeTransfer e _ = return e


-- | The opaque result of a dataflow analysis
data DataflowResult a =
  DataflowResult (Map Instruction a)

instance (Eq a) => Eq (DataflowResult a) where
  (DataflowResult m1) == (DataflowResult m2) = m1 == m2

instance (NFData a) => NFData (DataflowResult a) where
  rnf (DataflowResult m) = m `deepseq` ()

forwardDataflow :: forall m f . (DataflowAnalysis m f)
                   => CFG
                   -> Label -- MaybeC t1 t2
                   -> f -- FactBase f
                   -> m (Fact C f)
forwardDataflow cfg entryPoint f0 = graph (cfgBody cfg) noFacts
  where
    -- We'll record the entry block in the CFG later
--    entryPoint = cfgEntry cfg
    graph :: Graph Insn C C -> Fact C f -> m (Fact C f)
    -- graph GNil = return
    -- graph (GUnit blk) = block blk
    graph (GMany e bdy x) = (e `ebcat` bdy) >=> exit x
      where
        exit :: MaybeO x (Block Insn C O) -> Fact C f -> m (Fact x f)
        exit (JustO blk) = arfx block blk
        exit NothingO = return
        ebcat entry cbdy = c entryPoint entry
          where
            -- FIXME: It looks like the entry point here isn't used... could
            -- refer back to the argument?
            c :: Label -> MaybeO e (Block Insn O C)
                 -> Fact e f -> m (Fact C f)
--            c NothingC (JustO entry) = block entry `cat` body (successors entry) bdy
            c ep NothingO = body ep cbdy
            c _ _ = error "Bogus GADT pattern match failure"

    arfx = undefined

    body :: Label
            -> LabelMap (Block Insn C C)
            -> Fact C f
            -> m (Fact C f)
    body bentries blockmap initFbase =
      fixpoint Fwd doBlock bentries blockmap initFbase
      where
        doBlock :: forall x . Block Insn C x
                   -> FactBase f
                   -> m (Fact x f)
        doBlock b fb = block b entryFact
          where
            entryFact = getFact (entryLabel b) fb

    getFact = undefined

    node :: forall e x . Insn e x -> f -> m (Fact x f)
    -- Labels aren't visible to the user and don't add facts for us.
    -- Now, the phi variant *can* add facts
    node (Lbl _ _) f = return f
    -- Let all phi nodes be processed at the same time; we need to
    -- take @f@ here and figure out all of the incoming facts.
    -- Actually we only ever get an f.... probably can't do this.
    --
    -- Perhaps we can sneak something into the meet if the block
    -- begins with a PhiLbl?
    node (PhiLbl _ phis _) f = phiTransfer undefined phis
    -- Standard transfer function
    node (Normal i) f = transfer f i
    -- This gets a single input fact and needs to produce a
    -- *factbase*.  This should actually be fairly simple; run the
    -- transfer function on the instruction and update all of the lbls
    node (Terminator i lbls) f = do
      f' <- transfer f i
      return undefined


    block :: Block Insn e x -> f -> m (Fact x f)
    block BNil = return
    block (BlockCO l b) = node l >=> block b
    block (BlockCC l b n) = node l >=> block b >=> node n
    block (BlockOC b n) = block b >=> node n
    block (BMiddle n) = node n
    block (BCat b1 b2) = block b1 >=> block b2
    block (BSnoc h n) = block h >=> node n
    block (BCons n t) = node n >=> block t

data Direction = Fwd | Bwd

fixpoint = undefined


{-
-- | Get the result of a dataflow analysis at the given instruction.
-- Will throw an error if the instruction is not in the function for
-- which this analysis was run.
--
-- FIXME: The block result here needs to be able to reverse the instruction
-- list if doing a backwards analysis
dataflowResult :: DataflowResult a -> Instruction -> a
dataflowResult (DataflowResult m) i =
  fromMaybe errMsg $ M.lookup i m
  where
    errMsg = error ("LLVM.Analysis.Dataflow.dataflowResult: Instruction " ++ show i ++ " has no dataflow result")





firstInst :: Instruction -> Bool
firstInst i = firstBlock bb
  where
    Just bb = instructionBasicBlock i

forwardDataflow :: (Eq a, HasCFG b, DataflowAnalysis m a)
                   => a -> b -> m (DataflowResult a)
forwardDataflow analysis f =
  dataflowAnalysis basicBlockTerminatorInstruction id
    (basicBlockLabeledPredecessors cfg)
    (basicBlockLabeledSuccessors cfg)
    analysis cfg
  where
    cfg = getCFG f

-- | FIXME: This is currently broken.  When should the phi
-- instructions be processed in parallel? Before or after the
-- instructions in the block?  It also needs to look up the output
-- fact of the *first* instruction of the block instead of the last
backwardDataflow :: (Eq a, HasCFG b, DataflowAnalysis m a)
                    => a -> b -> m (DataflowResult a)
backwardDataflow analysis f =
  dataflowAnalysis firstNonPhiInstruction reverse
    (basicBlockLabeledSuccessors cfg)
    (basicBlockLabeledPredecessors cfg)
    analysis cfg
  where
    cfg = getCFG f

dataflowAnalysis :: forall a m . (Eq a, DataflowAnalysis m a)
                    => (BasicBlock -> Instruction)
                    -> ([Instruction] -> [Instruction])
                    -> (BasicBlock -> [(BasicBlock, CFGEdge)])
                    -> (BasicBlock -> [(BasicBlock, CFGEdge)])
                    -> a -> CFG -> m (DataflowResult a)
dataflowAnalysis lastInstruction orderedBlockInsts blockPreds blockSuccs fact0 cfg = do
  let blocks = functionBody func
      insts = concatMap basicBlockInstructions blocks
      initialStates = M.fromList $ zip insts (repeat top)
      s0 = (initialStates, S.empty)
  res <- dataflow blocks s0
  return (DataflowResult res)
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
        False -> dataflow (sort (S.toList nextWork')) (facts', S.empty)

    -- | FIXME the ordered block instructions should probably exclude
    -- phi nodes here since we don't track facts for phi nodes.  This
    -- will cause problems for a backwards analysis.
    lookupBlockFact :: (DataflowAnalysis m a) => HashMap Instruction a -> BasicBlock -> a
    lookupBlockFact facts block =
      fromMaybe errMsg $ M.lookup (lastInstruction block) facts
      where
        errMsg = error $ printf "LLVM.Analysis.Dataflow.dataflowAnalysis.lookupBlockFact: No facts for block %s" (show (toValue block))

    processBlock :: (DataflowAnalysis m a)
                    => (HashMap Instruction a, HashSet BasicBlock)
                    -> BasicBlock
                    -> m (HashMap Instruction a, HashSet BasicBlock)
    processBlock (outputFacts, nextWork) block = do

      -- First, match up the output facts for each incoming basic
      -- block with the associated CFG edge.
      let predsWithFacts = map (\(b, e) -> (b, lookupBlockFact outputFacts b, e)) preds
          applyEdgeTransfer (b, f, e) = do
            f' <- edgeTransfer f e
            return (b, f', e)

      -- Now apply the edge transfer function to each CFG edge.  This
      -- lets us generate facts before meeting all of the incoming
      -- edges.
      predsWithEdgeFacts <- mapM applyEdgeTransfer predsWithFacts

      -- Separate out Phi nodes so that we can process them all in parallel before
      -- touching the other instructions.
      let (phiNodes, otherInsts) = basicBlockSplitPhiNodes block

      -- Determine the input fact.  If there are no predecessors and
      -- this is the first block, use the initial analysis fact
      -- (fact0).  If there are no predecessors and this is not the
      -- first block, this block is not reachable and we use top as
      -- its input fact.
      --
      -- Otherwise, we just generate an initial input fact for the
      -- first instruction of the block (either through meeting all
      -- incoming inputs OR while processing the Phi nodes.
      inputFact <- case null preds of
        True -> case firstBlock block of
          True -> return fact0
          False -> return top
        False -> case null phiNodes of
          True -> return $! meets (map (\(_, f, _) -> f) predsWithEdgeFacts)
          False -> phiTransfer phiNodes  predsWithEdgeFacts

      -- Apply the transfer function to each instruction in the block.
      -- The fold chains output facts to input facts.  This step also
      -- (purely) updates the output fact map.
      let insts = orderedBlockInsts otherInsts
      (_, outputFacts') <- foldM processNode (inputFact, outputFacts) insts

      -- If the output fact of the block did not change, we don't need
      -- to update the successors at all and so we don't modify the
      -- worklist.
      --
      -- Since the facts for some instructions inside this block may
      -- have changed, we update the output facts either way (it
      -- doesn't really cost any extra).
      case lookupBlockFact outputFacts' block == lastOutputFact of
        True -> return (outputFacts', nextWork)
        False ->
          -- Update the worklist with the successors of this block and the facts
          let succBlocks = map fst $ blockSuccs block
              nextWork' = nextWork `S.union` S.fromList succBlocks
          in return (outputFacts', nextWork')
      where
        lastOutputFact = lookupBlockFact outputFacts block
        preds = blockPreds block

    -- | Apply the transfer function to each instruction in this
    -- block.  If the result is different than the current fact, add
    -- all successors to the worklist.
    processNode (inputFact, outputFacts) inst = do
      outFact <- transfer inputFact inst
      let outputFacts' = M.insert inst outFact outputFacts
      return (outFact, outputFacts')

firstBlock :: BasicBlock -> Bool
firstBlock bb = bb == fb
  where
    f = basicBlockFunction bb
    fb : _ = functionBody f
-}
{-# ANN module "HLint: ignore Use if" #-}
