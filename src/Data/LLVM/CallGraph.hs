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
module Data.LLVM.CallGraph (
  -- * Types
  CallGraph,
  CallEdge(..),
  CallNode(..),
  -- * Constructor
  mkCallGraph,
  -- * Accessors
  callGraphRepr
  ) where

import Data.Graph.Inductive
import Data.GraphViz
import qualified Data.Set as S
import Data.Maybe ( fromJust )

import Data.LLVM.Types
import Data.LLVM.Analysis.PointsTo

-- | A type synonym for the underlying graph
type CGType = Gr CallNode CallEdge

-- | The nodes are actually a wrapper type:
data CallNode = DefinedFunction Value
                -- ^ An actual function (either defined or an External)
              | ExtFunction Value
              | UnknownFunction
                -- ^ A function called indirectly that may not have
                -- any definition within the 'Module'
              deriving (Eq)

instance Show CallNode where
  show (DefinedFunction v) = show $ fromJust (valueName v)
  show (ExtFunction v) = "extern " ++ (show $ fromJust (valueName v))
  show UnknownFunction = "unknown"

instance Labellable CallNode where
  toLabel = (Label . StrLabel) . show

data CallEdge = DirectCall
                -- ^ A static call to a known function
              | IndirectCall
                -- ^ A possible call to a known function through a
                -- function pointer
              | UnknownCall
                -- ^ A possible call to an unknown function through a
                -- function pointer
              deriving (Ord, Eq)

instance Show CallEdge where
  show DirectCall = ""
  show IndirectCall = "?"
  show UnknownCall = "??"

instance Labellable CallEdge where
  toLabel = (Label . StrLabel) . show

-- | An opaque wrapper for the callgraph.  The nodes are functions and
-- the edges are calls between them.
data CallGraph = CallGraph CGType

-- | Convert the CallGraph to an FGL graph that can be traversed,
-- manipulated, or easily displayed with graphviz.
callGraphRepr :: CallGraph -> CGType
callGraphRepr (CallGraph g) = g

-- | Build a call graph for the given 'Module' using a pre-computed
-- points-to analysis.  The String parameter identifies the program
-- entry point.
--
-- FIXME: @entryPoint@ is not respected.
--
-- FIXME: Function pointers can be bitcasted - be sure to respect
-- those when adding indirect edges.
mkCallGraph :: (PointsToAnalysis a) => Module -> a -> [Value] -> CallGraph
mkCallGraph m pta entryPoints =
  CallGraph $ mkGraph allNodes (unique allEdges)
  where
    allNodes = concat [ knownNodes, unknownNodes, externNodes ]
    (allEdges, unknownNodes) = buildEdges pta funcs
    -- ^ Build up all of the edges and accumulate unknown nodes as
    -- they are created on-the-fly
    knownNodes = map (\v -> (valueUniqueId v, DefinedFunction v)) funcs
    -- ^ Add nodes for unknown functions (one unknown node for each
    -- type signature in an indirect call).  The unknown nodes can use
    -- negative numbers for nodeids since actual Value IDs start at 0.

    externNodes = map mkExternFunc $ filter isExternFunc (moduleGlobals m)

    funcs = moduleFunctions m

unique :: (Ord a) => [a] -> [a]
unique = S.toList . S.fromList

-- | This is the ID for the single "Unknown function" call graph node.
unknownNodeId :: Node
unknownNodeId = -100

isExternFunc :: Value -> Bool
isExternFunc Value { valueContent = ExternalFunction _ } = True
isExternFunc _ = False

mkExternFunc :: Value -> LNode CallNode
mkExternFunc v = (valueUniqueId v, ExtFunction v)

buildEdges :: (PointsToAnalysis a) => a -> [Value] -> ([LEdge CallEdge], [LNode CallNode])
buildEdges pta funcs = do
  let es = map (buildFuncEdges pta) funcs
      unknownNodes = [(unknownNodeId, UnknownFunction)]
  (concat es, unknownNodes)

isCall :: Value -> Bool
isCall Value { valueContent = CallInst {} } = True
isCall Value { valueContent = InvokeInst {} } = True
isCall _ = False

buildFuncEdges :: (PointsToAnalysis a) => a -> Value -> [LEdge CallEdge]
buildFuncEdges pta v@Value { valueContent = f } = concat es
  where
    insts = concatMap blockInstructions $ functionBody f
    calls = filter isCall insts
    es = map (buildCallEdges pta v) calls

getCallee :: ValueT -> Value
getCallee CallInst { callFunction = f } = f
getCallee InvokeInst { invokeFunction = f } = f
getCallee _ = error $ "Not a function in getCallee"

buildCallEdges :: (PointsToAnalysis a) => a -> Value -> Value -> [LEdge CallEdge]
buildCallEdges pta caller Value { valueContent = c } = build' (getCallee c)
  where
    callerId = valueUniqueId caller
    build' calledFunc =
      case valueContent calledFunc of
        Function {} ->
          [(callerId, valueUniqueId calledFunc, DirectCall)]
        GlobalAlias { globalAliasValue = aliasee } ->
          [(callerId, valueUniqueId aliasee, DirectCall)]
        ExternalFunction _ -> [(callerId, valueUniqueId calledFunc, DirectCall)]
        -- Functions can be bitcasted before being called - trace
        -- through those to find the underlying function
        BitcastInst bcv -> build' bcv
        _ -> let targets = S.toList $ pointsTo pta calledFunc
                 indirectEdges = map (\t -> (callerId, valueUniqueId t, IndirectCall)) targets
                 unknownEdge = (callerId, unknownNodeId, UnknownCall)
             in unknownEdge : indirectEdges
