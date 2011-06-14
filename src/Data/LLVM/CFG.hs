module Data.LLVM.CFG (
  -- * Types
  CFG,
  EdgeCondition(..),
  -- * Constructors
  mkCFG
  ) where

import Data.List ( foldl' )
import Data.Graph.Inductive
import qualified Data.HashMap.Strict as M
import Text.Printf

import Data.LLVM.Types

-- | The control flow graph representation
type CFG = Gr Value EdgeCondition

-- | The types of edges that appear in the 'CFG'
data EdgeCondition = UnconditionalEdge
                   | DefaultEdge
                   | TrueEdge Value
                   | FalseEdge Value
                   | EqualityEdge Value Value
                   | IndirectEdge Value
                   deriving (Ord, Eq)

instance Show EdgeCondition where
  show UnconditionalEdge = ""
  show DefaultEdge = "<default>"
  show (TrueEdge v) = printf "%s is true" (show v)
  show (FalseEdge v) = printf "%s is false" (show v)
  show (EqualityEdge v1 v2) = printf "%s is %s" (show v1) (show v2)
  show (IndirectEdge v) = printf "%s (indirect)" (show v)

-- | Get the instructions for a BasicBlock.
blockInstructions :: Value -> [Value]
blockInstructions Value { valueContent = BasicBlock is } = is
blockInstructions v = error $ printf "Value is not a basic block: %s" (show v)


-- | Build a control flow graph for the given function.  Each
-- instruction in the function body is a node in the graph.  Branching
-- instructions induce edges.  This form of the CFG is fine-grained in
-- that each instruction has its own CFG node.
--
-- The other function, 'mkCompactCFG', has a basic-block-granularity
-- CFG that can be easier to visualize.
mkCFG :: Value -> CFG
mkCFG func = mkGraph (concat cfgNodes) (concat $ concat cfgEdges)
  where
    body = functionBody $ valueContent func
    allInstructions = concatMap blockInstructions body

    (_, nodeIDs) = foldl' labelInstruction (0, M.empty) allInstructions
    labelInstruction (idx, m) val = (idx+1, M.insert val idx m)


    instIdent :: Value -> Int
    instIdent val = case M.lookup val nodeIDs of
      Just i -> i
      Nothing -> error $ printf "No ID for value [%s]" (show val)

    -- | Only BasicBlocks are targets of jumps.  This function finds the
    -- identifier for the given block.
    jumpTargetId :: Value -> Int
    jumpTargetId Value { valueContent = BasicBlock (t:_) } = instIdent t
    jumpTargetId v = error $ printf "Value is not a basic block %s" (show v)

    caseEdge thisNodeId cond (val, dest) =
      (thisNodeId, jumpTargetId dest, EqualityEdge cond val)
    indirectEdge thisNodeId addr target =
      (thisNodeId, jumpTargetId target, IndirectEdge addr)

    (cfgNodes, cfgEdges) = unzip $ map buildBlockGraph body

    buildBlockGraph Value { valueContent = BasicBlock blockInsts@(_:successors) } =
      foldl' buildGraphInst ([], []) instsAndSuccessors
      where
        instsAndSuccessors = if null successors
                             then terminator
                             else offsetPairs ++ terminator
        offsetPairs = zip blockInsts $ map Just successors
        terminator = [(last blockInsts, Nothing)]
    buildBlockGraph v = error $ "Not a BasicBlock: " ++ show v

    buildGraphInst (nodeAcc, edgeAcc) (v@Value { valueContent = c }, Nothing) =
      (thisNode : nodeAcc, theseEdges : edgeAcc)
      where
        thisNodeId = instIdent v
        thisNode = (thisNodeId, v)
        -- Note: branch targets are all basic blocks.  The
        -- lookup function handles grabbing the first real
        -- instruction from the basic block.
        theseEdges = case c of
          -- Returns have no intraprocedural edges
          RetInst _ -> []
          -- Unwinds also have no intraprocedural edges
          UnwindInst -> []
          -- A single target (no label needed)
          UnconditionalBranchInst tgt ->
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
          -- No edges from the unreachable instruction, either
          UnreachableInst -> []
          _ -> error ("Last instruction in a block should be a jump: " ++ show v)
    buildGraphInst (nodeAcc, edgeAcc) (v@Value { valueContent = c }, Just successor) =
      (thisNode : nodeAcc, theseEdges : edgeAcc)
      where
        thisNodeId = instIdent v
        thisNode = (thisNodeId, v)
        theseEdges = case c of
          Function {} -> error "Functions should not be in the CFG"
          GlobalDeclaration {} -> error "Globals should not be in the CFG"
          GlobalAlias {} -> error "Global aliases should not be in the CFG"
          ExternalValue -> error "External values should not be in the CFG"
          ExternalFunction _ -> error "External functions should not be in the CFG"
          BasicBlock _ -> error "Basic blocks should not be in the CFG"
          Argument _ -> error "Arguments should not be in the CFG"
          _ -> [(thisNodeId, instIdent successor, UnconditionalEdge)]
