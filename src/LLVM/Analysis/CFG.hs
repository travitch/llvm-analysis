{-# LANGUAGE ExistentialQuantification, GADTs, ViewPatterns #-}
-- | This module defines control flow graphs over the LLVM IR.
module LLVM.Analysis.CFG (
  -- * Types
  CFG(..),
  Insn(..),
  -- CFG(..),
  -- RCFG(..),
  -- CFGEdge(..),
  -- CFGType,
  -- HasCFG(..),
  -- -- * Constructors
  mkCFG,
  -- reverseCFG,
  -- -- * Accessors
  -- basicBlockPredecessors,
  -- basicBlockSuccessors,
  -- basicBlockPredecessorEdges,
  -- basicBlockSuccessorEdges,
  -- basicBlockLabeledPredecessors,
  -- basicBlockLabeledSuccessors,
  -- instructionReachable,
  -- -- * Visualization
  -- cfgGraphvizRepr
  ) where

import Compiler.Hoopl
import Control.Monad.State.Strict
import qualified Data.List as L
import Data.Map ( Map )
import qualified Data.Map as M
import Data.Monoid

import LLVM.Analysis

data CFG = CFG { cfgFunction :: Function
               , cfgLabelMap :: Map BasicBlock Label
               , cfgBody :: Graph Insn C C
               }

-- | This instance does not compare the graphs directly - instead it
-- compares just the function from which the graph is constructed.
-- The construction is completely deterministic so this should be
-- fine.  It is also fast because function comparison just compares
-- unique integer IDs.
instance Eq CFG where
  (CFG f1 _ _) == (CFG f2 _ _) = f1 == f2

-- | This is a wrapper GADT around the LLVM IR to mesh with Hoopl.  It
-- won't be exported or exposed to the user at all.  We need this for
-- two reasons:
--
-- 1) Hoopl requires explicit Label instructions.  In LLVM these are
--    implicit in the function structure through BasicBlocks
--
-- 2) We want to make it easy to process Phi nodes as a unit during
-- dataflow analysis.  To that end, there is a virtual node here that
-- bundles all of the phi nodes in a block together.  The idea is that
-- the dataflow analysis framework will deal with the details,
-- presenting a simple interface to the user.
data Insn e x where
  -- | Hoopl uses explicit labels, so we will construct these from our
  -- BasicBlocks.
  Lbl :: BasicBlock -> Label -> Insn C O

  -- | Terminator instructions always end a BasicBlock.  These include
  -- Invokes.
  Terminator :: Instruction -> [Label] -> Insn O C

  -- | Everything else
  Normal :: Instruction -> Insn O O

-- FIXME: One nice thing we could do: add virtual entry and exit
-- nodes.  Would need to modify unreachable/return to have edges to
-- this virtual node.  Question, should unreachable have that edge?
-- What about resume?

instance NonLocal Insn where
  entryLabel (Lbl _ lbl) = lbl
  successors (Terminator _ lbls) = lbls

blockLabel :: BasicBlock -> Builder Label
blockLabel bb = do
  m <- get
  case M.lookup bb m of
    Just l -> return l
    Nothing -> do
      l <- lift $ freshLabel
      put $ M.insert bb l m
      return l

terminatorLabels :: Instruction -> Builder [Label]
terminatorLabels i =
  case i of
    RetInst {} -> return []
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
    ResumeInst {} -> return []
    UnreachableInst {} -> return []
    InvokeInst { invokeNormalLabel = nt, invokeUnwindLabel = ut } -> do
      nl <- blockLabel nt
      ul <- blockLabel ut
      return [nl, ul]
    _ -> error "LLVM.Analysis.CFG.successors: non-terminator instruction"

instance Show (Insn e x) where
  show (Lbl bb _) = identifierAsString (basicBlockName bb) ++ ":"
  show (Terminator t _) = "  " ++ show t
  show (Normal i) = "  " ++ show i

mkCFG :: Function -> CFG
mkCFG f = runSimpleUniqueMonad (evalStateT builder mempty)
  where
    builder = do
      gs <- mapM fromBlock (functionBody f)
      let g = L.foldl' (|*><*|) emptyClosedGraph gs
      m <- get
      return $ CFG { cfgFunction = f
                   , cfgBody = g
                   , cfgLabelMap = m
                   }

type Builder a = StateT (Map BasicBlock Label) SimpleUniqueMonad a

fromBlock :: BasicBlock -> Builder (Graph Insn C C)
fromBlock bb = do
  lbl <- blockLabel bb
  let body = basicBlockInstructions bb
      (body', [term]) = L.splitAt (length body - 1) body
      normalNodes = map Normal body'
  tlbls <- terminatorLabels term
  let termNode = Terminator term tlbls
      entry = Lbl bb lbl
  return $ mkFirst entry <*> mkMiddles normalNodes <*> mkLast termNode


-- FIXME Change this to be a graph of basic blocks.  The accessors
-- below can be updated to work around this.  The graphs will be much
-- smaller.  The CDG and Dominance analyses will also need to be
-- updated.

{-
import Control.Arrow ( first )
import Data.GraphViz
import Data.List ( foldl' )
import Data.Maybe ( fromMaybe )
import Text.Printf

import Data.Graph.Interface
import Data.Graph.MutableDigraph
import Data.Graph.Algorithms.Basic

import LLVM.Analysis
import LLVM.Analysis.Types

type CFGType = DenseDigraph Instruction CFGEdge
type LEdgeType = Edge CFGType
type NodeType = Vertex
type LNodeType = (Vertex, VertexLabel CFGType)

instance Labellable CFGEdge where
  toLabelValue = toLabelValue . show

-- | The control flow graph representation
data CFG = CFG { cfgGraph :: CFGType
               , cfgEntryValue :: Instruction
               , cfgEntryNode :: NodeType
               , cfgExitValue :: Instruction
               , cfgExitNode :: NodeType
               , cfgFunction :: Function
               }

-- | The control flow graph with the edges reversed
data RCFG = RCFG { rcfgGraph :: CFGType
                 , rcfgEntryValue :: Instruction
                 , rcfgEntryNode :: NodeType
                 , rcfgExitValue :: Instruction
                 , rcfgExitNode :: NodeType
                 , rcfgFunction :: Function
                 }

-- | The types of edges that appear in the 'CFG'
data CFGEdge =
  UnconditionalEdge
  -- ^ An unconditional jump from somewhere
  | DefaultEdge
    -- ^ A default jump due to a case statement
  | TrueEdge Value
    -- ^ True edge successor for the comparison contained in the
    -- value.  This value was the argument to the branch instruction.
  | FalseEdge Value
    -- ^ False edge successor for the comparison contained in the
    -- value.  This value was the argument to a branch instruction.
  | EqualityEdge Value Value
    -- ^ A case equality edge (the case value v1 was equal to val v2)
  | IndirectEdge Value
    -- ^ Jump from the given indirect branch value
  | NormalEdge Instruction
    -- ^ The normal return from an invoke
  | UnwindEdge Instruction
    -- ^ Exceptional return from an invoke
  deriving (Ord, Eq)

instance Show CFGEdge where
  show UnconditionalEdge = ""
  show DefaultEdge = "<default>"
  show (TrueEdge v) = printf "[%s] is true" (show v)
  show (FalseEdge v) = printf "[%s] is false" (show v)
  show (EqualityEdge v1 v2) = printf "[%s] is [%s]" (show v1) (show v2)
  show (IndirectEdge v) = printf "[%s] (indirect)" (show v)
  show (NormalEdge i) = printf "[%s] (invoke normal)" (show i)
  show (UnwindEdge i) = printf "[%s] (invoke unwind)" (show i)

-- | Types that have control flow graphs.
class HasCFG a where
  getCFG :: a -> CFG

instance HasCFG CFG where
  getCFG = id

instance HasCFG Function where
  getCFG = mkCFG

instance HasFunction CFG where
  getFunction = cfgFunction

instance HasFunction RCFG where
  getFunction = rcfgFunction

instance FuncLike CFG where
  fromFunction = mkCFG

-- | Build a control flow graph for the given function.  Each
-- instruction in the function body is a node in the graph.  Branching
-- instructions induce edges.  This form of the CFG is fine-grained in
-- that each instruction has its own CFG node.
--
-- The other function, 'mkCompactCFG', has a basic-block-granularity
-- CFG that can be easier to visualize.
mkCFG :: Function -> CFG
mkCFG func = CFG { cfgGraph = g
                 , cfgFunction = func
                 , cfgEntryValue = entryVal
                 , cfgEntryNode = instructionUniqueId entryVal
                 , cfgExitValue = exitVal
                 , cfgExitNode = instructionUniqueId exitVal
                 }
  where
    entryVal = functionEntryInstruction func
    -- FIXME: there can possibly be more than one exit inst...
    Just exitVal = functionExitInstruction func

    g = mkGraph cfgNodes (concat cfgEdges)
    (cfgNodes, cfgEdges) = foldl' buildBlockGraph ([], []) (functionBody func)

reverseCFG :: CFG -> RCFG
reverseCFG g = RCFG { rcfgGraph = grev (cfgGraph g)
                    , rcfgFunction = cfgFunction g
                    , rcfgEntryValue = cfgExitValue g
                    , rcfgEntryNode = cfgExitNode g
                    , rcfgExitValue = cfgEntryValue g
                    , rcfgExitNode = cfgEntryNode g
                    }

toInternalEdge :: (Instruction, Instruction) -> LEdgeType
toInternalEdge (s, d) = Edge sid did UnconditionalEdge
  where
    sid = instructionUniqueId s
    did = instructionUniqueId d

buildBlockGraph :: ([LNodeType], [[LEdgeType]]) -> BasicBlock -> ([LNodeType], [[LEdgeType]])
buildBlockGraph (nacc, eacc) bb = (newNodes ++ nacc, (termEdges ++ internalEdges) : eacc)
  where
    blockInsts = basicBlockInstructions bb
    newNodes = map (\i -> (instructionUniqueId i, i)) blockInsts
    termInst = basicBlockTerminatorInstruction bb
    termNodeId = instructionUniqueId termInst
    otherInsts = filter (/= termInst) blockInsts
    internalEdgePairings = case null otherInsts of
      True -> []
      False -> zip blockInsts (tail blockInsts)

    internalEdges = map toInternalEdge internalEdgePairings

    -- FIXME: Examine call instructions to pick out calls to the
    -- libstdc++ unwind function, which marks the end of control flow
    -- in a function... that said, calls to __cxa_throw are properly
    -- marked as not returning (and are followed by the unreachable
    -- instruction), so maybe no special handling is required at the
    -- intraprocedural level.  This will still be important for
    -- interprocedural analyses, though.
    termEdges = case termInst of
      -- Returns have no intraprocedural edges
      RetInst {} -> []
      -- A single target (no label needed)
      UnconditionalBranchInst { unconditionalBranchTarget = tgt } ->
        [ Edge termNodeId (jumpTargetId tgt) UnconditionalEdge ]
      -- Two edges (cond is true, cond is false)
      BranchInst { branchCondition = cond
                 , branchTrueTarget = tTarget
                 , branchFalseTarget = fTarget
                 } ->
        [ Edge termNodeId (jumpTargetId tTarget) (TrueEdge cond)
        , Edge termNodeId (jumpTargetId fTarget) (FalseEdge cond)
        ]
      SwitchInst { switchValue = cond
                 , switchDefaultTarget = defTarget
                 , switchCases = cases
                 } ->
        Edge termNodeId (jumpTargetId defTarget) DefaultEdge :
          map (caseEdge termNodeId cond) cases
      IndirectBranchInst { indirectBranchAddress = addr
                         , indirectBranchTargets = targets
                         } ->
        map (indirectEdge termNodeId addr) targets
      InvokeInst { invokeNormalLabel = n
                 , invokeUnwindLabel = u
                 } ->
        [ Edge termNodeId (jumpTargetId n) (NormalEdge termInst)
        , Edge termNodeId (jumpTargetId u) (UnwindEdge termInst)
        ]
      -- No code after unreachable instructions is executed
      UnreachableInst {} -> []
      -- The resume instruction resumes propagating exceptions, so
      -- control will transfer to the caller.  In theory, another
      -- handler in the same function could pick it up...  Resolving
      -- that might require some more sophisticated analysis.
      ResumeInst {} -> []
      _ -> error ("LLVM.Analysis.CFG.buildBlockGraph: Last instruction in a block should be a terminator: " ++ show (toValue termInst))

-- | Only BasicBlocks are targets of jumps.  This function finds the
-- identifier for the given block.
jumpTargetId :: BasicBlock -> Int
jumpTargetId bb = instructionUniqueId t
  where
    (t:_) = basicBlockInstructions bb

caseEdge :: NodeType -> Value -> (Value, BasicBlock) -> LEdgeType
caseEdge thisNodeId cond (val, dest) =
  Edge thisNodeId (jumpTargetId dest) (EqualityEdge cond val)

indirectEdge :: NodeType -> Value -> BasicBlock -> LEdgeType
indirectEdge thisNodeId addr target =
  Edge thisNodeId (jumpTargetId target) (IndirectEdge addr)

{-# INLINE toBlock #-}
toBlock :: CFGType -> NodeType -> BasicBlock
toBlock cfg n =
  case lab cfg n of
    Nothing -> error ("LLVM.Analysis.CFG.toBlock: Instruction missing from CFG: " ++ show n)
    Just i ->
      let errMsg = error ("LLVM.Analysis.CFG.toBlock: Instruction in CFG should have a basic block: " ++ show i)
      in fromMaybe errMsg (instructionBasicBlock i)

-- | Get all of the predecessor blocks for basic block @bb@
--
-- > basicBlockPredecessors cfg bb
basicBlockPredecessors :: CFG -> BasicBlock -> [BasicBlock]
basicBlockPredecessors cfg bb = map (toBlock cfg') ps
  where
    cfg' = cfgGraph cfg
    firstInst : _ = basicBlockInstructions bb
    ps = pre cfg' (instructionUniqueId firstInst)

-- | Get all of the successor blocks for basic block @bb@
--
-- > basicBlockSuccessors cfg bb
basicBlockSuccessors :: CFG -> BasicBlock -> [BasicBlock]
basicBlockSuccessors cfg bb = map (toBlock cfg') ss
  where
    cfg' = cfgGraph cfg
    exitInst = basicBlockTerminatorInstruction bb
    ss = suc cfg' (instructionUniqueId exitInst)

basicBlockPredecessorEdges :: CFG -> BasicBlock -> [CFGEdge]
basicBlockPredecessorEdges cfg bb =
  map edgeLabel $ inn (cfgGraph cfg) (instructionUniqueId startInst)
  where
    startInst : _ = basicBlockInstructions bb

basicBlockSuccessorEdges :: CFG -> BasicBlock -> [CFGEdge]
basicBlockSuccessorEdges cfg bb =
  map edgeLabel $ out (cfgGraph cfg) (instructionUniqueId exitInst)
  where
    exitInst = basicBlockTerminatorInstruction bb

basicBlockLabeledSuccessors :: CFG -> BasicBlock -> [(BasicBlock, CFGEdge)]
basicBlockLabeledSuccessors cfg bb =
  map (first (toBlock cfg')) ss
  where
    cfg' = cfgGraph cfg
    exitInst = basicBlockTerminatorInstruction bb
    ss = lsuc cfg' (instructionUniqueId exitInst)

basicBlockLabeledPredecessors :: CFG -> BasicBlock -> [(BasicBlock, CFGEdge)]
basicBlockLabeledPredecessors cfg bb =
  map (first (toBlock cfg')) ps
  where
    cfg' = cfgGraph cfg
    startInst : _ = basicBlockInstructions bb
    ps = lpre cfg' (instructionUniqueId startInst)

-- | An instruction is reachable if its basic block has predecessors
-- *OR* (if there are no predecessors) it is the first basic block.
instructionReachable :: CFG -> Instruction -> Bool
instructionReachable cfg i =
  case null (basicBlockPredecessors cfg bb) of
    True -> bb == firstBlock
    False -> True
  where
    Just bb = instructionBasicBlock i
    f = basicBlockFunction bb
    firstBlock : _ = functionBody f

-- Visualization

cfgGraphvizParams :: GraphvizParams n Instruction CFGEdge BasicBlock Instruction
cfgGraphvizParams =
  defaultParams { fmtNode = \(_,l) -> [toLabel (toValue l)]
                , fmtEdge = formatEdge
                , clusterID = Int . basicBlockUniqueId
                , fmtCluster = formatCluster
                , clusterBy = nodeCluster
                }
  where
    nodeCluster l@(_, i) =
      let Just bb = instructionBasicBlock i
      in C bb (N l)
    formatCluster bb = [GraphAttrs [toLabel (show (basicBlockName bb))]]
    formatEdge (_, _, l) =
      let lbl = toLabel l
      in case l of
        TrueEdge _ -> [lbl, color ForestGreen]
        FalseEdge _ -> [lbl, color Crimson]
        EqualityEdge _ _ -> [lbl, color DeepSkyBlue]
        IndirectEdge _ -> [lbl, color Indigo, style dashed]
        UnwindEdge _ -> [lbl, color Tomato4, style dotted]
        _ -> [lbl]

cfgGraphvizRepr :: CFG -> DotGraph NodeType
cfgGraphvizRepr cfg = graphElemsToDot cfgGraphvizParams ns es
  where
    g = cfgGraph cfg
    ns = labeledVertices g
    es = map (\(Edge s d l) -> (s, d, l)) (edges g)

-}
{-# ANN module "HLint: ignore Use if" #-}
