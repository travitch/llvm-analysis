module Data.LLVM.CFG ( CFG
                     , EdgeCondition(..)
                     , makeCFG
                     ) where

import Data.List (foldl')
import Data.Graph.Inductive
import Data.HamtMap ((!))
import qualified Data.HamtMap as M

import Data.LLVM.Private.Printers ( )
import Data.LLVM.Private.ReferentialTypes

type CFG = Gr Value EdgeCondition

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
  show (TrueEdge v) = show v ++ " is true"
  show (FalseEdge v) = show v ++ " is false"
  show (EqualityEdge v1 v2) = concat [ show v1, " is ", show v2 ]
  show (IndirectEdge v) = show v ++ " (indirect)"

-- | Each instruction in the function body is a node in the graph.
-- Branching instructions induce edges.  This form of the CFG is
-- fine-grained in that each instruction has its own CFG node.  This
-- is simpler to analyze, generally.
--
-- The other function, makeCompactCFG, has a basic-block-granularity
-- CFG that can be easier to visualize.
makeCFG :: Value -> CFG
makeCFG func = mkGraph (concat cfgNodes) (concat $ concat cfgEdges)
  where body = functionBody $ valueContent func
        allInstructions = concatMap (\(Value { valueContent = BasicBlock is }) -> is) body
        (nodeCount, nodeIDs) = foldl' labelInstruction (0, M.empty) allInstructions
        labelInstruction (idx, m) val = (idx+1, M.insert val idx m)

        instIdent :: Value -> Int
        instIdent = (nodeIDs !)
        jumpTargetId :: Value -> Int
        jumpTargetId Value { valueContent = BasicBlock (t:_) } = instIdent t
        jumpTargetId v = error $ "Value is not a basic block: " ++ show v

        caseEdge thisNodeId cond (val, dest) =
          (thisNodeId, jumpTargetId dest, EqualityEdge cond val)
        indirectEdge thisNodeId addr target =
          (thisNodeId, jumpTargetId target, IndirectEdge addr)

        (cfgNodes, cfgEdges) = unzip $ map buildBlockGraph body

        buildBlockGraph Value { valueContent = BasicBlock blockInsts@(_:successors) } =
          foldl' buildGraphInst ([], []) instsAndSuccessors
          where instsAndSuccessors = if null successors
                                     then terminator
                                     else offsetPairs ++ terminator
                offsetPairs = zip blockInsts $ map Just successors
                terminator = [(last blockInsts, Nothing)]
        buildBlockGraph v = error $ "Not a BasicBlock: " ++ show v

        buildGraphInst (nodeAcc, edgeAcc) (v@Value { valueContent = c }, Nothing) =
          (thisNode : nodeAcc, theseEdges : edgeAcc)
          where thisNodeId = instIdent v
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
                    ( thisNodeId
                    , jumpTargetId defTarget
                    , DefaultEdge) : map (caseEdge thisNodeId cond) cases
                  IndirectBranchInst { indirectBranchAddress = addr
                                     , indirectBranchTargets = targets
                                     } ->
                    map (indirectEdge thisNodeId addr) targets
                  -- No edges from the unreachable instruction, either
                  UnreachableInst -> []
                  _ -> error ("Last instruction in a block should be a jump: " ++ show v)
        buildGraphInst (nodeAcc, edgeAcc) (v@Value { valueContent = c }, Just successor) =
          (thisNode : nodeAcc, theseEdges : edgeAcc)
          where thisNodeId = instIdent v
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
