{-# LANGUAGE ExistentialQuantification #-}
-- | This module defines control flow graphs over the LLVM IR.
module Data.LLVM.CFG (
  -- * Types
  CFG(..),
  CFGEdge(..),
  CFGType,
  HasCFG(..),
  -- * Constructors
  mkCFG,
  buildLocalGraph
  ) where

import Data.Graph.Inductive
import Data.GraphViz
import Text.Printf

import Data.LLVM.Types

type CFGType = Gr Instruction CFGEdge

instance Labellable CFGEdge where
  toLabel = (Label . StrLabel) . show

-- | The control flow graph representation
data CFG = CFG { cfgGraph :: CFGType
               , cfgEntryValue :: Instruction
               , cfgEntryNode :: Node
               , cfgExitValue :: Instruction
               , cfgExitNode :: Node
               , cfgFunction :: Function
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
  | UnwindEdge Instruction
  deriving (Ord, Eq)

instance Show CFGEdge where
  show UnconditionalEdge = ""
  show DefaultEdge = "<default>"
  show (TrueEdge v) = printf "%s is true" (show v)
  show (FalseEdge v) = printf "%s is false" (show v)
  show (EqualityEdge v1 v2) = printf "%s is %s" (show v1) (show v2)
  show (IndirectEdge v) = printf "%s (indirect)" (show v)
  show (NormalEdge i) = printf "%s (invoke normal)" (show i)
  show (UnwindEdge i) = printf "%s (invoke unwind)" (show i)

-- | Types that have control flow graphs.
class HasCFG a where
  getCFG :: a -> CFG

instance HasCFG CFG where
  getCFG = id

instance HasCFG Function where
  getCFG = mkCFG

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
    exitVal = functionExitInstruction func

    g = mkGraph cfgNodes cfgEdges

    (cfgNodes, cfgEdges) = buildLocalGraph id id callEdgeNStub id (const []) func ([], [])

callEdgeNStub :: a -> Maybe b
callEdgeNStub _ = Nothing

-- | Build the local control flow nodes and edges for the given
-- 'Function'.  The nodes and edges are placed on the front of the
-- input lists.
--
-- Note: This function is only exposed to make the ICFG construction
-- more efficient without copying code.
buildLocalGraph :: (LEdge CFGEdge -> LEdge a)        -- ^ A function to convert CFG edges to another type of edge
                   -> (LEdge CFGEdge -> LEdge a)     -- ^ A function to convert a CFG edge from a Call or Invoke to another type of edge
                   -> (Instruction -> Maybe (LNode b)) -- ^ A function to generate an extra node from a Call or Invoke edge
                   -> (LNode Instruction -> LNode b) -- ^ A function to convert CFG nodes to another type of node
                   -> (Instruction -> [LEdge a])     -- ^ A function to apply to Call and Invoke instructions to generate extra edges
                   -> Function                      -- ^ The current function
                   -> ([LNode b], [LEdge a])        -- ^ Accumulator
                   -> ([LNode b], [LEdge a])
buildLocalGraph edgeF callEdgeF callEdgeN nodeF callF f acc =
  foldr (buildBlockGraph edgeF callEdgeF callEdgeN nodeF callF) acc (functionBody f)

buildBlockGraph :: (LEdge CFGEdge -> LEdge a)        -- ^ A function to convert CFG edges to another type of edge
                   -> (LEdge CFGEdge -> LEdge a)     -- ^ A function to convert a CFG edge from a Call or Invoke to another type of edge
                   -> (Instruction -> Maybe (LNode b)) -- ^ A function to generate an extra node from a Call or Invoke edge
                   -> (LNode Instruction -> LNode b) -- ^ A function to convert CFG nodes to another type of node
                   -> (Instruction -> [LEdge a])     -- ^ A function to apply to Call and Invoke instructions to generate extra edges
                   -> BasicBlock
                   -> ([LNode b], [LEdge a])
                   -> ([LNode b], [LEdge a])
buildBlockGraph edgeF callEdgeF callEdgeN nodeF callF bb acc =
  foldr (buildGraphInst edgeF callEdgeF callEdgeN nodeF callF) acc instsAndSuccessors
  where
    blockInsts = basicBlockInstructions bb
    (_:successors) = blockInsts
    instsAndSuccessors = case null successors of
      True -> terminator
      False -> offsetPairs ++ terminator
    offsetPairs = zip blockInsts $ map Just successors
    terminator = [(last blockInsts, Nothing)]

buildGraphInst :: (LEdge CFGEdge -> LEdge a)        -- ^ A function to convert CFG edges to another type of edge
                  -> (LEdge CFGEdge -> LEdge a)     -- ^ A function to convert a CFG edge from a Call or Invoke to another type of edge
                  -> (Instruction -> Maybe (LNode b)) -- ^ A function to generate an extra node from a Call or Invoke edge
                  -> (LNode Instruction -> LNode b) -- ^ A function to convert CFG nodes to another type of node
                  -> (Instruction -> [LEdge a])     -- ^ A function to apply to Call and Invoke instructions to generate extra edges
                  -> (Instruction, Maybe Instruction)    -- ^ Current instruction and successor (if any)
                  -> ([LNode b], [LEdge a])        -- ^ Accumulator
                  -> ([LNode b], [LEdge a])
buildGraphInst edgeF callEdgeF callEdgeN nodeF callF (inst, Nothing) (nodeAcc, edgeAcc) =
   -- Note, when adding the edges, put the accumulator second in the
   -- list append so that only the short list (new edges) needs to be
   -- reallocated
  case (callEdgeN inst, inst) of
    (Just en, InvokeInst { }) -> (nodeF thisNode : en : nodeAcc, allEdges ++ edgeAcc)
    _ -> (nodeF thisNode : nodeAcc, allEdges ++ edgeAcc)
  where
    thisNodeId = instructionUniqueId inst
    thisNode = (thisNodeId, inst)
    allEdges = case inst of
      InvokeInst { } -> callF inst ++ map callEdgeF theseEdges
      _ -> map edgeF theseEdges

    -- Note: branch targets are all basic blocks.  The
    -- lookup function handles grabbing the first real
    -- instruction from the basic block.
    theseEdges = case inst of
      -- Returns have no intraprocedural edges
      RetInst {} -> []
      -- Unwinds also have no intraprocedural edges
      UnwindInst {} -> []
      -- A single target (no label needed)
      UnconditionalBranchInst { unconditionalBranchTarget = tgt } ->
        [ (thisNodeId, jumpTargetId tgt, UnconditionalEdge) ]
      -- Two edges (cond is true, cond is false)
      BranchInst { branchCondition = cond
                 , branchTrueTarget = tTarget
                 , branchFalseTarget = fTarget
                 } ->
        [ (thisNodeId, jumpTargetId tTarget, TrueEdge cond)
        , (thisNodeId, jumpTargetId fTarget, FalseEdge cond)
        ]
      SwitchInst { switchValue = cond
                 , switchDefaultTarget = defTarget
                 , switchCases = cases
                 } ->
        ( thisNodeId, jumpTargetId defTarget, DefaultEdge) :
        map (caseEdge thisNodeId cond) cases
      IndirectBranchInst { indirectBranchAddress = addr
                         , indirectBranchTargets = targets
                         } ->
        map (indirectEdge thisNodeId addr) targets
      InvokeInst { invokeNormalLabel = n
                 , invokeUnwindLabel = u
                 } ->
        [ (thisNodeId, jumpTargetId n, NormalEdge inst)
        , (thisNodeId, jumpTargetId u, UnwindEdge inst)
        ]
        -- No edges from the unreachable instruction, either
      UnreachableInst {} -> []
      _ -> error ("Last instruction in a block should be a terminator: " ++ show (Value inst))
buildGraphInst edgeF callEdgeF callEdgeN nodeF callF (inst, Just successor) (nodeAcc, edgeAcc) =
  case (callEdgeN inst, inst) of
    (Just en, CallInst { }) -> (nodeF thisNode : en : nodeAcc, theseEdges ++ edgeAcc)
    _ -> (nodeF thisNode : nodeAcc, theseEdges ++ edgeAcc)
  where
    thisNodeId = instructionUniqueId inst
    thisNode = (thisNodeId, inst)
    thisEdge = (thisNodeId, instructionUniqueId successor, UnconditionalEdge)
    theseEdges = case inst of
      CallInst { } -> callEdgeF thisEdge : callF inst
      _ -> [edgeF thisEdge]

-- | Only BasicBlocks are targets of jumps.  This function finds the
-- identifier for the given block.
jumpTargetId :: BasicBlock -> Int
jumpTargetId bb = instructionUniqueId t
  where
    (t:_) = basicBlockInstructions bb

caseEdge :: Node -> Value -> (Value, BasicBlock) -> LEdge CFGEdge
caseEdge thisNodeId cond (val, dest) =
  (thisNodeId, jumpTargetId dest, EqualityEdge cond val)

indirectEdge :: Node -> Value -> BasicBlock -> LEdge CFGEdge
indirectEdge thisNodeId addr target =
  (thisNodeId, jumpTargetId target, IndirectEdge addr)
