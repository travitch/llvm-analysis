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
-- * The function pointer must not be able to alias the result of a
--   dlopen or similar call
--
-- Again, the more sophisticated callgraph is still pending.
module Data.LLVM.CallGraph (
  -- * Types
  CallGraph(..),
  CallEdge(..),
  CallNode(..),
  -- * Constructor
  mkCallGraph
  ) where

import Control.Monad.State
import Data.Graph.Inductive
import Data.GraphViz
import qualified Data.HashMap.Strict as M
import qualified Data.Set as S
import Data.HashMap.Strict ( HashMap )
import Data.Maybe ( fromJust )

import Data.LLVM.Types
import Data.LLVM.Analysis.PointsTo

-- | A type synonym for the underlying graph
type CGType = Gr CallNode CallEdge

-- | The nodes are actually a wrapper type:
data CallNode = DefinedFunction Value
                -- ^ An actual function (either defined or an External)
              | ExtFunction Value
--              | AliasFunction Value
              | UnknownFunction Type
                -- ^ A function called indirectly that may not have
                -- any definition within the 'Module'
              deriving (Eq)

instance Show CallNode where
  show (DefinedFunction v) = show $ fromJust (valueName v)
  show (ExtFunction v) = "extern " ++ (show $ fromJust (valueName v))
--  show (AliasFunction v) = printf "alias %s -> %s" (show $ fromJust (valueName v)) (show $ fromJust (valueName (globalAliasValue)))
  show (UnknownFunction t) = show t

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

-- | Build a call graph for the given 'Module' using a pre-computed
-- points-to analysis.  The String parameter identifies the program
-- entry point.
--
-- FIXME: @entryPoint@ is not respected.
--
-- FIXME: Function pointers can be bitcasted - be sure to respect
-- those when adding indirect edges.
mkCallGraph :: (PointsToAnalysis a) => Module -> a -> String -> CallGraph
mkCallGraph m pta entryPoint =
  CallGraph $ mkGraph allNodes allEdges
  where
    allNodes = concat [ knownNodes, unknownNodes, externNodes ]
    (allEdges, unknownNodes) = evalState (buildEdges pta funcs) M.empty
    -- ^ Build up all of the edges and accumulate unknown nodes as
    -- they are created on-the-fly
    knownNodes = map (\v -> (valueUniqueId v, DefinedFunction v)) funcs
    -- ^ Add nodes for unknown functions (one unknown node for each
    -- type signature in an indirect call).  The unknown nodes can use
    -- negative numbers for nodeids since actual Value IDs start at 0.

    externNodes = map mkExternFunc $ filter isExternFunc (moduleGlobals m)

    funcs = moduleFunctions m

isExternFunc :: Value -> Bool
isExternFunc Value { valueContent = ExternalFunction _ } = True
isExternFunc _ = False

mkExternFunc :: Value -> LNode CallNode
mkExternFunc v = (valueUniqueId v, ExtFunction v)

type CGBuilder a = State (HashMap Type Node) a

buildEdges :: (PointsToAnalysis a) => a -> [Value] -> CGBuilder ([LEdge CallEdge], [LNode CallNode])
buildEdges pta funcs = do
  es <- mapM (buildFuncEdges pta) funcs
  unknownNodes <- extractUnknowns
  return (concat es, unknownNodes)

extractUnknowns :: CGBuilder [LNode CallNode]
extractUnknowns = do
  m <- get
  return $ M.foldlWithKey' mkUnknownNode [] m
  where
    mkUnknownNode a k v = (v, UnknownFunction k) : a

isCall :: Value -> Bool
isCall Value { valueContent = CallInst {} } = True
isCall Value { valueContent = InvokeInst {} } = True
isCall _ = False

buildFuncEdges :: (PointsToAnalysis a) => a -> Value -> CGBuilder [LEdge CallEdge]
buildFuncEdges pta v@Value { valueContent = f } = do
  let insts = concatMap blockInstructions $ functionBody f
      calls = filter isCall insts

  es <- mapM (buildCallEdges pta v) calls
  return $ concat es

getCallee :: ValueT -> Value
getCallee CallInst { callFunction = f } = f
getCallee InvokeInst { invokeFunction = f } = f
getCallee _ = error $ "Not a function in getCallee"

buildCallEdges :: (PointsToAnalysis a) => a -> Value -> Value -> CGBuilder [LEdge CallEdge]
buildCallEdges pta caller Value { valueContent = c } = do
  let callee = getCallee c
      callerId = valueUniqueId caller

  case valueContent callee of
    Function {} -> return [(callerId, valueUniqueId callee, DirectCall)]
    GlobalAlias { globalAliasValue = aliasee } ->
      return [(callerId, valueUniqueId aliasee, DirectCall)]
    ExternalFunction _ -> return [(callerId, valueUniqueId callee, DirectCall)]
    _ -> do
      -- Build all of the necessary indirect call edges, then build an
      -- unknown call edge using an existing id if possible.  If not,
      -- make a new unknown call id.
      let targets = S.toList $ pointsTo pta callee
          indirectEdges = map (\t -> (callerId, valueUniqueId t, IndirectCall)) targets
          sig = valueType callee
      m <- get
      let msigid = M.lookup sig m
      sigid <- case msigid of
        Just i -> return i
        Nothing -> do
          let sid = -(M.size m + 10)
              m' = M.insert sig sid m
          put m'
          return sid
      let unknownEdge = (callerId, sigid, UnknownCall)
      return $ unknownEdge : indirectEdges