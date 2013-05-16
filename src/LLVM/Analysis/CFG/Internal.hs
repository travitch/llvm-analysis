{-# OPTIONS_HADDOCK not-home #-}
{-# LANGUAGE ExistentialQuantification, GADTs #-}
{-# LANGUAGE ViewPatterns, ScopedTypeVariables, PatternGuards #-}
module LLVM.Analysis.CFG.Internal (
  -- * CFG
  CFG(..),
  HasCFG(..),
  controlFlowGraph,
  basicBlockPredecessors,
  basicBlockSuccessors,
  -- * Dataflow
  DataflowAnalysis(..),
  dataflowAnalysis,
  fwdDataflowEdgeAnalysis,
  forwardDataflow,
  backwardDataflow,
  DataflowResult(..),
  dataflowResult,
  dataflowResultAt,
  -- * Internal types
  Insn(..),
  ) where

import Compiler.Hoopl
import Control.DeepSeq
import Control.Monad ( (>=>), (<=<) )
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import Data.Function ( on )
import qualified Data.GraphViz as GV
import qualified Data.List as L
import Data.Map ( Map )
import qualified Data.Map as M
import Data.Maybe ( fromMaybe, mapMaybe )
import Data.Monoid
import Data.Set ( Set )
import qualified Data.Set as S
import Data.Tuple ( swap )
import qualified Text.PrettyPrint.GenericPretty as PP

import LLVM.Analysis

-- CFG stuff

-- | A class for things from which a CFG can be obtained.
class HasCFG a where
  getCFG :: a -> CFG

instance HasCFG CFG where
  getCFG = id

instance HasCFG Function where
  getCFG = controlFlowGraph

instance HasFunction CFG where
  getFunction = cfgFunction

instance FuncLike CFG where
  fromFunction = controlFlowGraph

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
controlFlowGraph :: Function -> CFG
controlFlowGraph f = runSimpleUniqueMonad (evalStateT builder mempty)
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

basicBlockPredecessors :: (HasCFG cfgLike) => cfgLike -> BasicBlock -> [BasicBlock]
basicBlockPredecessors cfgLike bb =
  fromMaybe [] $ M.lookup bb (cfgPredecessors cfg)
  where
    cfg = getCFG cfgLike

basicBlockSuccessors :: (HasCFG cfgLike) => cfgLike -> BasicBlock -> [BasicBlock]
basicBlockSuccessors cfgLike bb = case cfgBody cfg of
  GMany _ lm _ -> fromMaybe [] $ do
    blbl <- basicBlockToLabel cfg bb
    blk <- mapLookup blbl lm
    return $ mapMaybe (labelToBasicBlock cfg) (successors blk)
  where
    cfg = getCFG cfgLike

basicBlockToLabel :: CFG -> BasicBlock -> Maybe Label
basicBlockToLabel cfg bb = M.lookup bb (cfgLabelMap cfg)

labelToBasicBlock :: CFG -> Label -> Maybe BasicBlock
labelToBasicBlock cfg l = M.lookup l (cfgBlockMap cfg)


-- Visualization

cfgGraphvizParams :: GV.GraphvizParams n Instruction CFGEdge BasicBlock Instruction
cfgGraphvizParams =
  GV.defaultParams { GV.fmtNode = \(_,l) -> [GV.toLabel (toValue l)]
                   , GV.fmtEdge = formatEdge
                   , GV.clusterID = GV.Int . basicBlockUniqueId
                   , GV.fmtCluster = formatCluster
                   , GV.clusterBy = nodeCluster
                   }
  where
    nodeCluster l@(_, i) =
      let Just bb = instructionBasicBlock i
      in GV.C bb (GV.N l)
    formatCluster bb = [GV.GraphAttrs [GV.toLabel (show (basicBlockName bb))]]
    formatEdge (_, _, l) =
      let lbl = GV.toLabel l
      in case l of
        TrueEdge -> [lbl, GV.color GV.ForestGreen]
        FalseEdge -> [lbl, GV.color GV.Crimson]
        EqualityEdge _ -> [lbl, GV.color GV.DeepSkyBlue]
        IndirectEdge -> [lbl, GV.color GV.Indigo, GV.style GV.dashed]
        UnwindEdge -> [lbl, GV.color GV.Tomato4, GV.style GV.dotted]
        OtherEdge -> [lbl]

data CFGEdge = TrueEdge
             | FalseEdge
             | EqualityEdge Value
             | IndirectEdge
             | UnwindEdge
             | OtherEdge
             deriving (Eq, Show)

instance GV.Labellable CFGEdge where
  toLabelValue TrueEdge = GV.toLabelValue "True"
  toLabelValue FalseEdge = GV.toLabelValue "False"
  toLabelValue (EqualityEdge v) = GV.toLabelValue ("== " ++ show v)
  toLabelValue IndirectEdge = GV.toLabelValue "Indirect"
  toLabelValue UnwindEdge = GV.toLabelValue "Unwind"
  toLabelValue OtherEdge = GV.toLabelValue ""

instance ToGraphviz CFG where
  toGraphviz = cfgGraphvizRepr

cfgGraphvizRepr :: CFG -> GV.DotGraph Int
cfgGraphvizRepr cfg = GV.graphElemsToDot cfgGraphvizParams ns es
  where
    f = getFunction cfg
    ns = map toGNode (functionInstructions f)
    es = concatMap toEdges (functionBody f)

-- | There is an edge from the terminator of the BB to the entry of
-- each of its successors.  The edges should be labelled according to
-- the type of the terminator.  There are OtherEdge markers on between
-- each instruction in the BB.
toEdges :: BasicBlock -> [(Int, Int, CFGEdge)]
toEdges bb =
  case ti of
    RetInst {} -> intraEdges
    UnreachableInst {} -> intraEdges
    UnconditionalBranchInst { unconditionalBranchTarget = t } ->
      let (ei:_) = basicBlockInstructions t
      in (instructionUniqueId ti, instructionUniqueId ei, OtherEdge) : intraEdges
    BranchInst { branchTrueTarget = tt, branchFalseTarget = ft } ->
      let (tei:_) = basicBlockInstructions tt
          (fei:_) = basicBlockInstructions ft
      in (instructionUniqueId ti, instructionUniqueId tei, TrueEdge) :
         (instructionUniqueId ti, instructionUniqueId fei, FalseEdge) :
         intraEdges
    SwitchInst { switchDefaultTarget = dt, switchCases = cases } ->
      let (dei:_) = basicBlockInstructions dt
          caseNodes = map toCaseNode cases
      in (instructionUniqueId ti, instructionUniqueId dei, OtherEdge):caseNodes ++ intraEdges
    IndirectBranchInst { indirectBranchTargets = bs } ->
      map toIndirectEdge bs ++ intraEdges
    ResumeInst {} -> intraEdges
    InvokeInst { invokeUnwindLabel = ul, invokeNormalLabel = nl } ->
      let (nei:_) = basicBlockInstructions nl
          (uei:_) = basicBlockInstructions ul
      in (instructionUniqueId ti, instructionUniqueId nei, OtherEdge):
         (instructionUniqueId ti, instructionUniqueId uei, UnwindEdge):
         intraEdges
    _ -> error "Not a terminator instruction"
  where
    -- Basic blocks are not allowed to be empty so this pattern match
    -- should never fail.
    is@(_:rest) = basicBlockInstructions bb
    intraEdges = map toIntraEdge (zip is rest)
    toIntraEdge (s,d) = (instructionUniqueId s, instructionUniqueId d, OtherEdge)
    ti = basicBlockTerminatorInstruction bb

    toIndirectEdge tgt =
      let (ei:_) = basicBlockInstructions tgt
      in (instructionUniqueId ti, instructionUniqueId ei, IndirectEdge)

    toCaseNode (val, tgt) =
      let (ei:_) = basicBlockInstructions tgt
      in (instructionUniqueId ti, instructionUniqueId ei, EqualityEdge val)

toGNode :: Instruction -> (Int, Instruction)
toGNode i = (instructionUniqueId i, i)


-- Dataflow analysis stuff

-- | An opaque representation of a dataflow analysis.  Analyses of
-- this type are suitable for both forward and backward use.
--
-- For all dataflow analyses, the standard rules apply.
--
-- 1) @meet a top == a@
--
-- 2) Your lattice @f@ must have finite height
--
-- The @m@ type parameter is a 'Monad'; this dataflow framework
-- provides a /monadic/ transfer function.  This is intended to allow
-- transfer functions to have monadic contexts that provide
-- MonadReader and MonadWriter-like functionality.  State is also
-- useful for caching expensive sub-computations.  Keep in mind that
-- the analysis iterates to a fixedpoint and side effects in the monad
-- will be repeated.
data DataflowAnalysis m f where
  DataflowAnalysis :: (Eq f, Monad m) => { analysisTop :: f
                                         , analysisMeet :: f -> f -> f
                                         , analysisTransfer :: f -> Instruction -> m f
                                         , analysisFwdEdgeTransfer :: Maybe (f -> Instruction -> m [(BasicBlock, f)])
                                         , analysisBwdEdgeTransfer :: Maybe ([(BasicBlock, f)] -> Instruction -> m f)
                                         } -> DataflowAnalysis m f

-- | Define a basic 'DataflowAnalysis'
dataflowAnalysis :: (Eq f, Monad m)
                    => f -- ^ Top
                    -> (f -> f -> f) -- ^ Meet
                    -> (f -> Instruction -> m f) -- ^ Transfer
                    -> DataflowAnalysis m f
dataflowAnalysis top m t = DataflowAnalysis top m t Nothing Nothing

-- | A forward dataflow analysis that provides an addition /edge
-- transfer function/.  This function is run with each Terminator
-- instruction (/after/ the normal transfer function, whose results
-- are fed to the edge transfer function).  The edge transfer function
-- allows you to let different information flow to each successor
-- block of a terminator instruction.
--
-- If a BasicBlock in the edge transfer result is not a successor of
-- the input instruction, that mapping is discarded.  Multiples are
-- @meet@ed together.  Missing values are taken from the result of the
-- normal transfer function.
fwdDataflowEdgeAnalysis :: (Eq f, Monad m)
                           => f -- ^ Top
                           -> (f -> f -> f) -- ^ meet
                           -> (f -> Instruction -> m f) -- ^ Transfer
                           -> (f -> Instruction -> m [(BasicBlock, f)]) -- ^ Edge Transfer
                           -> DataflowAnalysis m f
fwdDataflowEdgeAnalysis top m t e =
  DataflowAnalysis top m t (Just e) Nothing

-- | The opaque result of a dataflow analysis.  Use the functions
-- 'dataflowResult' and 'dataflowResultAt' to extract results.
data DataflowResult m f where
  DataflowResult :: CFG
                    -> DataflowAnalysis m f
                    -> Fact C f
                    -> Direction
                    -> DataflowResult m f

-- See Note [Dataflow Results]

instance (Show f) => Show (DataflowResult m f) where
  show (DataflowResult _ _ fb _) =
    PP.pretty (map (\(f,s) -> (show f, show s)) (mapToList fb))

instance (Eq f) => Eq (DataflowResult m f) where
  (DataflowResult c1 _ m1 d1) == (DataflowResult c2 _ m2 d2) =
    c1 == c2 && m1 == m2 && d1 == d2

-- This may have to cheat... LabelMap doesn't have an NFData instance.
-- Not sure if this will affect monad-par or not.
instance (NFData f) => NFData (DataflowResult m f) where
  rnf _ = () -- (DataflowResult m) = m `deepseq` ()

-- | Look up the dataflow fact at a particular Instruction.
dataflowResultAt :: DataflowResult m f
                    -> Instruction
                    -> m f
dataflowResultAt (DataflowResult cfg (DataflowAnalysis top meet transfer _ _) m dir) i = do
  let Just bb = instructionBasicBlock i
      Just lbl = M.lookup bb (cfgLabelMap cfg)
      initialFactAndInsts = findInitialFact bb lbl dir
  case initialFactAndInsts of
    Nothing -> return top
    Just (bres, is) -> replayTransfer is bres
  where
    findInitialFact bb lbl Fwd = do
      f0 <- lookupFact lbl m
      return (f0, basicBlockInstructions bb)
    -- Here, look up the facts for all successors
    findInitialFact bb _ Bwd =
      case basicBlockSuccessors cfg bb of
        [] -> do
          f0 <- lookupFact (cfgExitLabel cfg) m
          return (f0, reverse (basicBlockInstructions bb))
        ss -> do
          let trBlock b = do
                l <- basicBlockToLabel cfg b
                lookupFact l m
              f0 = foldr meet top (mapMaybe trBlock ss)
          return (f0, reverse (basicBlockInstructions bb))
    replayTransfer [] _ = error "LLVM.Analysis.Dataflow.dataflowResult: replayed past end of block, impossible"
    replayTransfer (thisI:rest) r
      | thisI == i = transfer r i
      | otherwise = do
        r' <- transfer r thisI
        replayTransfer rest r'


-- | Look up the dataflow fact at the virtual exit note.  This
-- combines the results along /all/ paths, including those ending in
-- "termination" instructions like Unreachable and Resume.
--
-- If you want the result at only the return instruction(s), use
-- 'dataflowResultAt' and 'meets' the results together.
dataflowResult :: DataflowResult m f -> f
dataflowResult (DataflowResult cfg (DataflowAnalysis top _ _ _ _) m _) =
  fromMaybe top $ lookupFact (cfgExitLabel cfg) m

-- | Run a forward dataflow analysis
forwardDataflow :: forall m f cfgLike . (HasCFG cfgLike)
                   => cfgLike -- ^ Something providing a CFG
                   -> DataflowAnalysis m f -- ^ The analysis to run
                   -> f -- ^ Initial fact for the entry node
                   -> m (DataflowResult m f)
forwardDataflow cfgLike da@DataflowAnalysis { analysisTop = top
                                            , analysisMeet = meet
                                            , analysisTransfer = transfer
                                            , analysisFwdEdgeTransfer = etransfer
                                            } fact0 = do
  r <- graph (cfgBody cfg) (mapSingleton elbl fact0)
  return $ DataflowResult cfg da r Fwd
  where
    cfg = getCFG cfgLike
    elbl = cfgEntryLabel cfg
    entryPoints = [elbl]
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
      fixpoint Fwd da doBlock bentries blockmap initFbase
      where
        doBlock :: forall x . Block Insn C x -> FactBase f -> m (Fact x f)
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
      let baseResult = mapFromList $ zip lbls (repeat f')
      case etransfer of
        Nothing -> return baseResult
        Just etransfer' -> do
          -- Now convert BasicBlocks to their labels (discarding
          -- mappings where the label is not in @lbls@.  Duplicates
          -- are meeted together.  Missing elements are filled in by
          -- the result of the normal transfer function
          blockOuts <- etransfer' f' i
          let res = foldr (addBlockEdgeResult lbls) mapEmpty blockOuts
          return $ mapUnion res (mapDeleteList (mapKeys res) baseResult)
    -- The unique exit doesn't do anything - it just collects the
    -- final results.
    node UniqueExit _ = return mapEmpty

    addBlockEdgeResult :: [Label] -> (BasicBlock, f) -> FactBase f -> FactBase f
    addBlockEdgeResult lbls (bb, res) acc
      | Just lbl <- basicBlockToLabel cfg bb, lbl `elem` lbls =
        case mapLookup lbl acc of
          Nothing -> mapInsert lbl res acc
          Just ex -> mapInsert lbl (meet res ex) acc
      | otherwise = acc

    block :: Block Insn e x -> f -> m (Fact x f)
    block BNil = return
    block (BlockCO l b) = node l >=> block b
    block (BlockCC l b n) = node l >=> block b >=> node n
    block (BlockOC b n) = block b >=> node n
    block (BMiddle n) = node n
    block (BCat b1 b2) = block b1 >=> block b2
    block (BSnoc h n) = block h >=> node n
    block (BCons n t) = node n >=> block t

-- | Run a backward dataflow analysis
backwardDataflow :: forall m f cfgLike . (HasCFG cfgLike)
                    => cfgLike -- ^ Something providing a CFG
                    -> DataflowAnalysis m f -- ^ The analysis to run
                    -> f -- ^ Initial fact for the entry node
                    -> m (DataflowResult m f)
backwardDataflow cfgLike da@DataflowAnalysis { analysisTop = top
                                         , analysisMeet = meet
                                         , analysisTransfer = transfer
                                         } fact0 = do
  r <- graph (cfgBody cfg) (mapSingleton xlbl fact0)
  return $ DataflowResult cfg da r Bwd
  where
    cfg = getCFG cfgLike
    xlbl = cfgExitLabel cfg
    entryPoints = [xlbl]
    -- We'll record the entry block in the CFG later
    graph :: Graph Insn C C -> Fact C f -> m (Fact C f)
    -- graph GNil = return
    -- graph (GUnit blk) = block blk
    graph (GMany e bdy x) = (e `ebcat` bdy) <=< exit x
      where
        exit :: MaybeO x (Block Insn C O) -> Fact C f -> m (Fact x f)
        exit (JustO blk) = arbx block blk
        exit NothingO = return
        ebcat entry cbdy = c entryPoints entry
          where
            c :: [Label] -> MaybeO e (Block Insn O C)
                 -> Fact e f -> m (Fact C f)
--            c NothingC (JustO entry) = block entry <=< body (successors entry) bdy
            c eps NothingO = body eps cbdy
            c _ _ = error "Bogus GADT pattern match failure"

    -- Analyze Rewrite Backward Transformer?
    arbx :: forall thing x . (NonLocal thing)
            => (thing C x -> f -> m (Fact x f))
            -> (thing C x -> Fact C f -> m (Fact x f))
    arbx arf thing fb = arf thing f'
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
      fixpoint Bwd da doBlock (map entryLabel (backwardBlockList bentries blockmap)) blockmap initFbase
      where
        doBlock :: forall x . Block Insn C x -> Fact x f -> m (LabelMap f)
        doBlock b fb = do
          f <- block b fb
          return $ mapSingleton (entryLabel b) f

    node :: forall e x . Insn e x -> Fact x f -> m f
    -- Labels aren't visible to the user and don't add facts for us.
    -- Now, the phi variant *can* add facts
    node (Lbl _ _) f = return f
    node (UniqueExitLabel _) f = return f
    -- Standard transfer function
    node (Normal i) f = transfer f i
    -- In backward mode, the transfer function gets a FactBase and
    -- returns a single Fact.
    node (Terminator i lbls) fbase = do
      let fs = mapMaybe (\l -> lookupFact l fbase) lbls
          f = foldr meet top fs
      transfer f i
    -- The unique exit doesn't do anything - it just collects the
    -- final results.
    node UniqueExit fbase =
      return $ foldr meet top (mapElems fbase)

    block :: Block Insn e x -> Fact x f -> m f
    block BNil = return
    block (BlockCO l b) = node l <=< block b
    block (BlockCC l b n) = node l <=< block b <=< node n
    block (BlockOC b n) = block b <=< node n
    block (BMiddle n) = node n
    block (BCat b1 b2) = block b1 <=< block b2
    block (BSnoc h n) = block h <=< node n
    block (BCons n t) = node n <=< block t

forwardBlockList :: (NonLocal n, LabelsPtr entry)
                    => entry -> Body n -> [Block n C C]
forwardBlockList entries blks = postorder_dfs_from blks entries

backwardBlockList :: (NonLocal n, LabelsPtr entries)
                     => entries -> Body n -> [Block n C C]
backwardBlockList entries body = reverse $ forwardBlockList entries body

data Direction = Fwd | Bwd
               deriving (Eq)

-- | The fixedpoint calculations (and joins) all happen in here.
fixpoint :: forall m f . Direction
            -> DataflowAnalysis m f
            -> (Block Insn C C -> Fact C f -> m (Fact C f))
            -> [Label]
            -> LabelMap (Block Insn C C)
            -> (Fact C f -> m (Fact C f))
fixpoint dir (DataflowAnalysis _ meet _ _ _) doBlock entries blockmap initFbase =
  -- See Note [Fixpoint]
  loop initFbase entries mempty
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

    loop :: FactBase f -> [Label] -> Set Label -> m (FactBase f)
    loop fbase [] _ = return fbase
    loop fbase (lbl:todo) visited  =
      case mapLookup lbl blockmap of
        Nothing -> loop fbase todo (S.insert lbl visited)
        Just blk -> do
          outFacts <- doBlock blk fbase
          -- Fold updateFact over each fact in the result from doBlock
          -- updateFact; facts are meet-ed pairwise.
          let (changed, fbase') = mapFoldWithKey (updateFact visited) ([], fbase) outFacts
              depLookup l = mapFindWithDefault [] l depBlocks
              toAnalyze = filter (`notElem` todo) $ concatMap depLookup changed

          -- In the original code, there is a binding @newblocks'@
          -- that includes any new blocks added by the graph rewriting
          -- step.  This analysis does not rewrite any blocks, so we
          -- only need @newblocks@ here.
          loop fbase' (todo ++ toAnalyze) (S.insert lbl visited)

    -- We also have a simpler update condition in updateFact since we
    -- don't carry around newBlocks.
    updateFact :: Set Label
                  -> Label
                  -> f
                  -> ([Label], FactBase f)
                  -> ([Label], FactBase f)
    updateFact visited lbl newFact acc@(cha, fbase) =
      case lookupFact lbl fbase of
        Nothing -> (lbl:cha,  mapInsert lbl newFact fbase)
        Just oldFact ->
          let fact' = oldFact `meet` newFact
          in case fact' == oldFact && S.member lbl visited of
            True -> acc
            False -> (lbl:cha, mapInsert lbl fact' fbase)

{- Note [Fixpoint]

In hoopl, the fixpoint returns a factbase that includes only the facts
that are not in the body.  Facts for the body are in the rewritten
body nodes in the DG.  Since we are not rewriting the graph, we keep
all facts in the factbase in fixpoint.

-}

{- Note [Dataflow Results]

To get a forward result, we have to look up the result for the block
of the instruction and then run the analysis forward to the target
instruction.

For a /backward/ analysis, we have to do a bit more.  Instead, we need
all of the successors of the block (if there are none, then we have
to take the summary for the unique exit node).  Then we have to meet
those and use that as the initial fact.  Then replay over the basic
block instructoins /in reverse/.

Also note that the dataflowResultAt function does not need to use the
edge transfer function because the result replay only needs to work
within a single block.

-}
