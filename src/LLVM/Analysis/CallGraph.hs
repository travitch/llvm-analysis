{-# LANGUAGE TypeFamilies, ExistentialQuantification #-}
-- | This module defines a call graph and related functions.  The call
-- graph is a static view of the calls between functions in a
-- 'Module'.  The nodes of the graph are global functions and the
-- edges are calls made to other functions.
--
-- This call graph attempts to provide as much information as possible
-- about calls through function pointers.  Direct calls have a single
-- outgoing edge.  Indirect calls that can be augmented with
-- information from a points-to analysis can induce many IndirectCall
-- edges.
--
-- For now, all indirect calls also induce an UnknownCall edge, under
-- the assumption that externally-obtained function pointers may also
-- be called somehow.  This restriction will eventually be lifted and
-- indirect calls that can be identified as completely internal will
-- not have the UnknownCall edge.  The preconditions for this will be:
--
-- * The 'Module' must have an entry point (otherwise it is a library)
--
-- * The function pointer must not be able to alias the result of a
--   dlopen or similar call
--
-- Again, the more sophisticated callgraph is still pending.
module LLVM.Analysis.CallGraph (
  -- * Types
  CallGraph,
  CG,
  CallEdge(..),
  CallNode(..),
  -- * Constructor
  mkCallGraph,
  -- * Accessors
  callGraphRepr,
  callValueTargets,
  callInstructionTargets,
  callGraphFunctions,
  -- * Visualization
  cgGraphvizRepr
  ) where

import Data.GraphViz
import Data.Maybe ( mapMaybe )
import Data.Hashable
import qualified Data.HashSet as HS

import Data.Graph.Interface
import Data.Graph.LazyHAMT

import LLVM.Analysis
import LLVM.Analysis.PointsTo

-- | A type synonym for the underlying graph
type CG = Gr CallNode CallEdge

-- | The nodes are actually a wrapper type:
data CallNode = DefinedFunction Function
                -- ^ An actual function defined in this 'Module'
              | ExtFunction ExternalFunction
                -- ^ An externally-defined function with a declaration
                -- in the 'Module'
              | UnknownFunction
                -- ^ A function called indirectly that may not have
                -- any definition or declaration within the 'Module'
              deriving (Eq)

instance Show CallNode where
  show (DefinedFunction v) = show $ functionName v
  show (ExtFunction v) = "extern " ++ show (externalFunctionName v)
  show UnknownFunction = "unknown"

instance Labellable CallNode where
  toLabelValue = toLabelValue . show

data CallEdge = DirectCall
                -- ^ A static call to a known function
              | IndirectCall
                -- ^ A possible call to a known function through a
                -- function pointer
              | UnknownCall
                -- ^ A possible call to an unknown function through a
                -- function pointer
              deriving (Ord, Eq)

instance Hashable CallEdge where
  hash DirectCall = hash (1 :: Int)
  hash IndirectCall = hash (2 :: Int)
  hash UnknownCall = hash (3 :: Int)

instance Show CallEdge where
  show DirectCall = ""
  show IndirectCall = "?"
  show UnknownCall = "??"

instance Labellable CallEdge where
  toLabelValue = toLabelValue . show

-- | An opaque wrapper for the callgraph.  The nodes are functions and
-- the edges are calls between them.
data CallGraph = forall pta . (PointsToAnalysis pta) => CallGraph CG pta

-- | Get all of the functions defined in this module from the
-- CallGraph
callGraphFunctions :: CallGraph -> [Function]
callGraphFunctions (CallGraph cg _) =
  mapMaybe extractDefinedFunction (labNodes cg)
  where
    extractDefinedFunction (LNode _ (DefinedFunction f)) = Just f
    extractDefinedFunction _ = Nothing

-- | Convert the CallGraph to an FGL graph that can be traversed,
-- manipulated, or easily displayed with graphviz.
callGraphRepr :: CallGraph -> CG
callGraphRepr (CallGraph g _) = g

-- | Given a Call or Invoke instruction, return the list of possible
-- callees.  All returned Values will be either Functions or
-- ExternalFunctions.
--
-- Passing a non-call/invoke instruction will trigger a noisy pattern
-- matching failure.
callInstructionTargets :: CallGraph -> Instruction -> [Value]
callInstructionTargets cg (CallInst { callFunction = f }) =
  callValueTargets cg f
callInstructionTargets cg (InvokeInst { invokeFunction = f}) =
  callValueTargets cg f
callInstructionTargets _ i =
  error ("LLVM.Analysis.CallGraph.callInstructionTargets: Expected a Call or Invoke instruction: " ++ show i)

-- | Given the value called by a Call or Invoke instruction, return
-- all of the possible Functions or ExternalFunctions that it could
-- be.
callValueTargets :: CallGraph -> Value -> [Value]
callValueTargets (CallGraph _ pta) v =
  let v' = stripBitcasts v
  in case valueContent v' of
    FunctionC _ -> [v']
    ExternalFunctionC _ -> [v']
    _ -> pointsTo pta v

-- | Build a call graph for the given 'Module' using a pre-computed
-- points-to analysis.  The String parameter identifies the program
-- entry point.
--
-- FIXME: @entryPoint@ is not respected.
--
-- FIXME: Function pointers can be bitcasted - be sure to respect
-- those when adding indirect edges.
mkCallGraph :: (PointsToAnalysis a) => Module
               -> a            -- ^ A points-to analysis (to resolve function pointers)
               -> [Function]   -- ^ The entry points to the 'Module'
               -> CallGraph
mkCallGraph m pta entryPoints =
  CallGraph (mkGraph allNodes (unique allEdges)) pta
  where
    allNodes = concat [ knownNodes, unknownNodes, externNodes ]
    (allEdges, unknownNodes) = buildEdges pta funcs
    -- ^ Build up all of the edges and accumulate unknown nodes as
    -- they are created on-the-fly
    -- knownNodes = map (\v -> (valueUniqueId v, DefinedFunction v)) funcs
    knownNodes = map (\f -> LNode (valueUniqueId f) (DefinedFunction f)) funcs
    -- ^ Add nodes for unknown functions (one unknown node for each
    -- type signature in an indirect call).  The unknown nodes can use
    -- negative numbers for nodeids since actual Value IDs start at 0.

    externNodes = map mkExternFunc $ moduleExternalFunctions m

    funcs = moduleDefinedFunctions m

unique :: (Hashable a, Eq a) => [a] -> [a]
unique = HS.toList . HS.fromList

-- | This is the ID for the single "Unknown function" call graph node.
unknownNodeId :: Node CG
unknownNodeId = -100

mkExternFunc :: ExternalFunction -> LNode CG
mkExternFunc v = LNode (valueUniqueId v) (ExtFunction v)

buildEdges :: (PointsToAnalysis a) => a -> [Function] -> ([LEdge CG], [LNode CG])
buildEdges pta funcs = do
  let es = map (buildFuncEdges pta) funcs
      unknownNodes = [LNode unknownNodeId UnknownFunction]
  (concat es, unknownNodes)

isCall :: Instruction -> Bool
isCall CallInst {} = True
isCall InvokeInst {} = True
isCall _ = False

buildFuncEdges :: (PointsToAnalysis a) => a -> Function -> [LEdge CG]
buildFuncEdges pta f = concat es
  where
    insts = concatMap basicBlockInstructions $ functionBody f
    calls = filter isCall insts
    es = map (buildCallEdges pta f) calls

getCallee :: Instruction -> Value
getCallee CallInst { callFunction = f } = f
getCallee InvokeInst { invokeFunction = f } = f
getCallee i = error ("LLVM.Analysis.CallGraph.getCallee: Expected a function in getCallee: " ++ show i)

buildCallEdges :: (PointsToAnalysis a) => a -> Function -> Instruction -> [LEdge CG]
buildCallEdges pta caller callInst = build' (getCallee callInst)
  where
    callerId = valueUniqueId caller
    build' calledFunc =
      case valueContent' calledFunc of
        FunctionC f ->
          [LEdge (Edge callerId (valueUniqueId f)) DirectCall]
        GlobalAliasC GlobalAlias { globalAliasTarget = aliasee } ->
          [LEdge (Edge callerId (valueUniqueId aliasee)) DirectCall]
        ExternalFunctionC ef ->
          [LEdge (Edge callerId (valueUniqueId ef)) DirectCall]
        -- Functions can be bitcasted before being called - trace
        -- through those to find the underlying function
        InstructionC BitcastInst { castedValue = bcv } -> build' bcv
        _ ->
          let targets = resolveIndirectCall pta callInst
              indirectEdges = map (\t -> LEdge (Edge callerId (valueUniqueId t)) IndirectCall) targets
              unknownEdge = LEdge (Edge callerId unknownNodeId) UnknownCall
          in unknownEdge : indirectEdges

cgGraphvizParams :: GraphvizParams n CallNode CallEdge () CallNode
cgGraphvizParams =
  nonClusteredParams { fmtNode = \(_,l) -> [toLabel l]
                     , fmtEdge = \(_,_,l) -> [toLabel l]
                     }

cgGraphvizRepr :: CallGraph -> DotGraph Int
cgGraphvizRepr (CallGraph g _) = graphElemsToDot cgGraphvizParams ns es
  where
    ns = map toNodeTuple $ labNodes g
    es = map toEdgeTuple $ labEdges g
