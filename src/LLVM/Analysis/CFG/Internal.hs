{-# OPTIONS_HADDOCK not-home #-}
{-# LANGUAGE TypeFamilies, MultiParamTypeClasses #-}
{-# LANGUAGE ExistentialQuantification, GADTs #-}
{-# LANGUAGE BangPatterns, ViewPatterns, ScopedTypeVariables #-}
module LLVM.Analysis.CFG.Internal (
  -- * CFG
  CFG(..),
  HasCFG(..),
  mkCFG,
  basicBlockPredecessors,
  basicBlockSuccessors,
  -- * Dataflow
  DataflowAnalysis(..),
  MeetSemiLattice(..),
  BoundedMeetSemiLattice(..),
  meets,
  forwardDataflow,
  DataflowResult(..),
  dataflowResult,
  dataflowResultAt,
  -- * Internal types
  Insn(..),
  ) where

import Algebra.Lattice
import Compiler.Hoopl
import Control.DeepSeq
import Control.Monad.State.Strict
import Data.Function ( on )
import qualified Data.List as L
import Data.Map ( Map )
import qualified Data.Map as M
import Data.Maybe ( fromMaybe, mapMaybe )
import Data.Monoid
import qualified Data.Set as S
import Data.Tuple ( swap )

import LLVM.Analysis
import LLVM.Analysis.Types -- move this to LLVM.Analysis?

-- CFG stuff

-- | A class for things from which a CFG can be obtained.
class HasCFG a where
  getCFG :: a -> CFG

instance HasCFG CFG where
  getCFG = id

instance HasCFG Function where
  getCFG = mkCFG

instance HasFunction CFG where
  getFunction = cfgFunction

instance FuncLike CFG where
  fromFunction = mkCFG

-- | The type of function control flow graphs.
data CFG = CFG { cfgFunction :: Function
               , cfgLabelMap :: Map BasicBlock Label
               , cfgBlockMap :: Map Label BasicBlock
               , cfgBody :: Graph Insn C C
               , cfgEntryLabel :: Label
               , cfgExitLabel :: Label
               , cfgPredecessors :: Map BasicBlock [BasicBlock]
               }
-- See Note [CFG Back Edges]

{- Note [CFG Back Edges]

The control flow graph provided by hoopl only tracks forward edges.
Since we want to let users query predecessor blocks, we need to record
predecessors on the side at CFG construction time (see
cfgPredecessors).

We build the cache with a single pass over the successors of the CFG.

-}

-- | This instance does not compare the graphs directly - instead it
-- compares just the function from which the graph is constructed.
-- The construction is completely deterministic so this should be
-- fine.  It is also fast because function comparison just compares
-- unique integer IDs.
instance Eq CFG where
  (==) = on (==) cfgFunction

-- | This is a wrapper GADT around the LLVM IR to mesh with Hoopl.  It
-- won't be exported or exposed to the user at all.  We need this for
-- two reasons:
--
-- 1) Hoopl requires explicit Label instructions.  In LLVM these are
--    implicit in the function structure through BasicBlocks
--
-- 2) Additionally, LLVM doens't have a unique exit instruction per
-- function.  resume, ret, and unreachable all terminate execution.
-- c.f. UniqueExitLabel and ExitLabel (both seem to be needed because
-- hoopl blocks need an entry and an exit).
data Insn e x where
  Lbl :: BasicBlock -> Label -> Insn C O
  Terminator :: Instruction -> [Label] -> Insn O C
  UniqueExitLabel :: Label -> Insn C O
  UniqueExit :: Insn O C
  Normal :: Instruction -> Insn O O

instance NonLocal Insn where
  entryLabel (Lbl _ lbl) = lbl
  entryLabel (UniqueExitLabel lbl) = lbl
  successors (Terminator _ lbls) = lbls
  successors UniqueExit = []

instance Show (Insn e x) where
  show (Lbl bb _) = identifierAsString (basicBlockName bb) ++ ":"
  show (Terminator t _) = "  " ++ show t
  show (Normal i) = "  " ++ show i
  show (UniqueExitLabel _) = "UniqueExit:"
  show UniqueExit = "  done"

-- | Create a CFG for a function
mkCFG :: Function -> CFG
mkCFG f = runSimpleUniqueMonad (evalStateT builder mempty)
  where
    builder = do
      -- This is a unique label not associated with any block.  All of
      -- the instructions that exit a function get an edge to this
      -- virtual label.
      exitLabel <- lift $ freshLabel
      gs <- mapM (fromBlock exitLabel) (functionBody f)
      let g = L.foldl' (|*><*|) emptyClosedGraph gs
          x = mkFirst (UniqueExitLabel exitLabel) <*> mkLast UniqueExit
          g' = g |*><*| x
      m <- get
      let i0 = functionEntryInstruction f
          Just bb0 = instructionBasicBlock i0
          Just fEntryLabel = M.lookup bb0 m
          cfg = CFG { cfgFunction = f
                   , cfgBody = g'
                   , cfgLabelMap = m
                   , cfgBlockMap = M.fromList $ map swap $ M.toList m
                   , cfgEntryLabel = fEntryLabel
                   , cfgExitLabel = exitLabel
                   , cfgPredecessors = mempty
                   }
          preds = foldr (recordPreds cfg) mempty (functionBody f)
      return $ cfg { cfgPredecessors = fmap S.toList preds }
    addPred pblock b =
      M.insertWith S.union b (S.singleton pblock)
    recordPreds cfg bb acc =
      let succs = basicBlockSuccessors cfg bb
      in foldr (addPred bb) acc succs

-- | A builder environment for constructing CFGs.  Mostly needed for
-- generating and tracking block labels.
type Builder a = StateT (Map BasicBlock Label) SimpleUniqueMonad a

-- | Return the Label for the given BasicBlock.  Generates a new Label
-- and caches it, if necessary.
blockLabel :: BasicBlock -> Builder Label
blockLabel bb = do
  m <- get
  case M.lookup bb m of
    Just l -> return l
    Nothing -> do
      l <- lift $ freshLabel
      put $ M.insert bb l m
      return l

-- | Convert a BasicBlock into a CFG chunk (the caller will combine
-- all of the chunks).  The block is C C shaped.  The first argument
-- is the unique exit label that certain instructions generated edges
-- to.
fromBlock :: Label -> BasicBlock -> Builder (Graph Insn C C)
fromBlock xlabel bb = do
  lbl <- blockLabel bb
  let body = basicBlockInstructions bb
      (body', [term]) = L.splitAt (length body - 1) body
      normalNodes = map Normal body'
  tlbls <- terminatorLabels xlabel term
  let termNode = Terminator term tlbls
      entry = Lbl bb lbl
  return $ mkFirst entry <*> mkMiddles normalNodes <*> mkLast termNode

-- | All instructions that exit a function get an edge to the special
-- ExitLabel.  This allows all results along all branches (even those
-- with non-standard exits) to be collected.  If only normal exit
-- results are desired, just check the dataflow result for RetInst
-- results.
terminatorLabels :: Label -> Instruction -> Builder [Label]
terminatorLabels xlabel i =
  case i of
    RetInst {} -> return [xlabel]
    UnconditionalBranchInst { unconditionalBranchTarget = t } -> do
      bl <- blockLabel t
      return [bl]
    BranchInst { branchTrueTarget = tt, branchFalseTarget = ft } -> do
      tl <- blockLabel tt
      fl <- blockLabel ft
      return [tl, fl]
    SwitchInst { switchDefaultTarget = dt, switchCases = (map snd -> ts) } -> do
      dl <- blockLabel dt
      tls <- mapM blockLabel ts
      return $ dl : tls
    IndirectBranchInst { indirectBranchTargets = ts } ->
      mapM blockLabel ts
    ResumeInst {} -> return [xlabel]
    UnreachableInst {} -> return [xlabel]
    InvokeInst { invokeNormalLabel = nt, invokeUnwindLabel = ut } -> do
      nl <- blockLabel nt
      ul <- blockLabel ut
      return [nl, ul]
    _ -> error "LLVM.Analysis.CFG.successors: non-terminator instruction"

basicBlockPredecessors :: CFG -> BasicBlock -> [BasicBlock]
basicBlockPredecessors cfg bb =
  fromMaybe [] $ M.lookup bb (cfgPredecessors cfg)

basicBlockSuccessors :: CFG -> BasicBlock -> [BasicBlock]
basicBlockSuccessors cfg bb = case cfgBody cfg of
  GMany _ lm _ -> fromMaybe [] $ do
    blbl <- basicBlockToLabel cfg bb
    blk <- mapLookup blbl lm
    return $ mapMaybe (labelToBasicBlock cfg) (successors blk)

basicBlockToLabel :: CFG -> BasicBlock -> Maybe Label
basicBlockToLabel cfg bb = M.lookup bb (cfgLabelMap cfg)

labelToBasicBlock :: CFG -> Label -> Maybe BasicBlock
labelToBasicBlock cfg l = M.lookup l (cfgBlockMap cfg)


-- Dataflow analysis stuff

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
class (BoundedMeetSemiLattice a, Monad m, Eq a) => DataflowAnalysis m a where
  transfer :: a -- ^ The incoming analysis state
              -> Instruction -- ^ The instruction being analyzed
              -> m a

-- | The opaque result of a dataflow analysis
data DataflowResult f = DataflowResult CFG (Fact C f)

instance (Show f) => Show (DataflowResult f) where
  show (DataflowResult cfg fb) = show fb ++ "\n" ++ showGraph show (cfgBody cfg)

instance (Eq f) => Eq (DataflowResult f) where
  (DataflowResult c1 m1) == (DataflowResult c2 m2) =
    c1 == c2 && m1 == m2

-- This may have to cheat... LabelMap doesn't have an NFData instance.
-- Not sure if this will affect monad-par or not.
instance (NFData a) => NFData (DataflowResult a) where
  rnf _ = () -- (DataflowResult m) = m `deepseq` ()

-- | Look up the dataflow fact at a particular Instruction.
dataflowResultAt :: (DataflowAnalysis m f)
                    => DataflowResult f
                    -> Instruction
                    -> m f
dataflowResultAt (DataflowResult cfg m) i = do
  let Just bb = instructionBasicBlock i
      Just lbl = M.lookup bb (cfgLabelMap cfg)
  case lookupFact lbl m of
    Nothing -> return top
    Just bres -> replayTransfer (basicBlockInstructions bb) bres
  where
    replayTransfer [] _ = error "LLVM.Analysis.Dataflow.dataflowResult: replayed past end of block, impossible"
    replayTransfer (thisI:rest) r
      | thisI == i = return r
      | otherwise = do
        r' <- transfer r thisI
        replayTransfer rest r'

-- | Look up the dataflow fact at the virtual exit note.  This
-- combines the results along /all/ paths, including those ending in
-- "termination" instructions like Unreachable and Resume.
--
-- If you want the result at only the return instruction(s), use
-- 'dataflowResultAt' and 'meets' the results together.
dataflowResult :: (BoundedMeetSemiLattice f) => DataflowResult f -> f
dataflowResult (DataflowResult cfg m) =
  fromMaybe top $ lookupFact (cfgExitLabel cfg) m

forwardDataflow :: forall m f . (DataflowAnalysis m f)
                   => CFG
                   -> m (DataflowResult f)
forwardDataflow cfg = do
  r <- graph (cfgBody cfg) noFacts
  return $ DataflowResult cfg r
  where
    entryPoints = [cfgEntryLabel cfg]
    -- We'll record the entry block in the CFG later
    graph :: Graph Insn C C -> Fact C f -> m (Fact C f)
    -- graph GNil = return
    -- graph (GUnit blk) = block blk
    graph (GMany e bdy x) = (e `ebcat` bdy) >=> exit x
      where
        exit :: MaybeO x (Block Insn C O) -> Fact C f -> m (Fact x f)
        exit (JustO blk) = arfx block blk
        exit NothingO = return
        ebcat entry cbdy = c entryPoints entry
          where
            c :: [Label] -> MaybeO e (Block Insn O C)
                 -> Fact e f -> m (Fact C f)
--            c NothingC (JustO entry) = block entry `cat` body (successors entry) bdy
            c eps NothingO = body eps cbdy
            c _ _ = error "Bogus GADT pattern match failure"

    -- Analyze Rewrite Forward Transformer?
    arfx :: forall thing x . (NonLocal thing)
            => (thing C x -> f -> m (Fact x f))
            -> (thing C x -> Fact C f -> m (Fact x f))
    arfx arf thing fb = arf thing f'
      where
        -- We don't do the meet operation here (unlike hoopl).  They
        -- only performed it (knowing it is a no-op) to preserve side
        -- effects.
        Just f' = lookupFact (entryLabel thing) fb

    body :: [Label]
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
            entryFact = fromMaybe top $ lookupFact (entryLabel b) fb

    node :: forall e x . Insn e x -> f -> m (Fact x f)
    -- Labels aren't visible to the user and don't add facts for us.
    -- Now, the phi variant *can* add facts
    node (Lbl _ _) f = return f
    node (UniqueExitLabel _) f = return f
    -- Standard transfer function
    node (Normal i) f = transfer f i
    -- This gets a single input fact and needs to produce a
    -- *factbase*.  This should actually be fairly simple; run the
    -- transfer function on the instruction and update all of the lbl
    node (Terminator i lbls) f = do
      f' <- transfer f i
      -- Now create a new map with all of the labels mapped to
      -- f'.  Code later will handle merging this result.
      return $ mapFromList $ zip lbls (repeat f')
    -- The unique exit doesn't do anything - it just collects the
    -- final results.
    node UniqueExit _ = return mapEmpty



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

-- The fixedpoint calculations (and joins) all happen in here.
-- Try to find a spot to possibly add the phi transfer...
fixpoint :: forall m f . (DataflowAnalysis m f)
            => Direction
            -> (Block Insn C C -> Fact C f -> m (Fact C f))
            -> [Label]
            -> LabelMap (Block Insn C C)
            -> (Fact C f -> m (Fact C f))
fixpoint dir doBlock entries blockmap initFbase =
  -- See Note [Fixpoint]
  loop initFbase entries
  where
    -- This is a map from label L to all of its dependencies; if L
    -- changes, all of its dependencies need to be re-analyzed.
    depBlocks :: LabelMap [Label]
    depBlocks = mapFromListWith (++) [ (l, [entryLabel b])
                                     | b <- mapElems blockmap
                                     , l <- case dir of
                                       Fwd -> [entryLabel b]
                                       Bwd -> successors b
                                     ]

    loop :: FactBase f -> [Label] -> m (FactBase f)
    loop fbase [] = return fbase
    loop fbase (lbl:todo) =
      case mapLookup lbl blockmap of
        Nothing -> loop fbase todo
        Just blk -> do
          outFacts <- doBlock blk fbase
          -- Fold updateFact over each fact in the result from doBlock
          -- updateFact; facts are meet-ed pairwise.
          let (changed, fbase') = mapFoldWithKey updateFact ([], fbase) outFacts
              depLookup l = mapFindWithDefault [] l depBlocks
              toAnalyze = filter (`notElem` todo) $ concatMap depLookup changed

          -- In the original code, there is a binding @newblocks'@
          -- that includes any new blocks added by the graph rewriting
          -- step.  This analysis does not rewrite any blocks, so we
          -- only need @newblocks@ here.
          loop fbase' (todo ++ toAnalyze)

    -- We also have a simpler update condition in updateFact since we
    -- don't carry around newBlocks.
    updateFact :: (DataflowAnalysis m f)
                  => Label
                  -> f
                  -> ([Label], FactBase f)
                  -> ([Label], FactBase f)
    updateFact lbl newFact acc@(cha, fbase) =
      case lookupFact lbl fbase of
        Nothing -> (lbl:cha,  mapInsert lbl newFact fbase)
        Just oldFact ->
          let fact' = oldFact `meet` newFact
          in case fact' == oldFact of
            True -> acc
            False -> (lbl:cha, mapInsert lbl fact' fbase)

{- Note [Fixpoint]

In hoopl, the fixpoint returns a factbase that includes only the facts
that are not in the body.  Facts for the body are in the rewritten
body nodes in the DG.  Since we are not rewriting the graph, we keep
all facts in the factbase in fixpoint.

-}
