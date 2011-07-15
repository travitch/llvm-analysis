module Data.LLVM.ICFG (
  -- * Types
  ICFG(..),
  EdgeType(..),
  -- * Constructor
  mkICFG
  ) where

import Data.Graph.Inductive
import Data.HashMap.Strict ( HashMap )
import Data.Set ( Set )
import qualified Data.HashMap.Strict as M
import qualified Data.Set as S

import Data.LLVM.Types
import Data.LLVM.CFG
import Data.LLVM.Analysis.PointsTo

data EdgeType = CallToEntry Value
              | ReturnToCall Value
              | CallToReturn
              | CallToExtern (Maybe Value)
              | ReturnFromExtern (Maybe Value)
              | IntraEdge CFGEdge

data ICFG = ICFG { icfgGraph :: Gr Value EdgeType
                 , icfgEntryPoints :: [Value]
                 , icfgModule :: Module
                 }

-- | Build the interprocedural CFG for the given 'Module'.  The ICFG
-- has all of the instructions in the module as nodes, augmented by a
-- special "return" node for each call node.  There are several types
-- of edges:
--
-- * Standard intraprocedural edges as in a CFG
--
-- * Edges from calls to the entry node of the target function(s)
--
-- * Edges from ret instructions to the special "return" node of the
--   corresponding call node that led to the function
--
-- * Edges from call nodes to their corresponding special "return"
--   node.  These are used to propagate information about local
--   variables
--
-- * Edges to and from called external functions.  The external
--   function is represented by two nodes: a fake entry and fake exit.
--
-- This graph is meant for use in interprocedural analysis.  The
-- @entryPoints@ parameter directs interprocedural analyses where they
-- should start.  If no entry points are specified, the 'Module' will
-- be considered to be a library and all functions will be considered
-- as entry points (with multiple entries permitted for each).
--
-- Additionally, if no entry points are specified (or there are calls
-- to dlopen), there will be edges to Unknown functions that are called
-- through function pointers.
mkICFG :: (PointsToAnalysis a, HasCFG b) => Module
          -> a       -- ^ A points-to analysis
          -> [b]     -- ^ Values with control-flow graphs (either functions or pre-computed CFGs)
          -> [Value] -- ^ Entry points.  This could be just main or a larger list for a library
          -> ICFG
mkICFG m pta fcfgs entryPoints =
  ICFG { icfgGraph = mkGraph allNodes allEdges
       , icfgEntryPoints = entryPoints
       , icfgModule = m
       }
  where
    allNodes = cfgNodes ++ callReturnNodes
    -- ^ The only extra nodes in the ICFG are call return nodes
    -- (representing the place to which flow resumes after a function
    -- call)

    allEdges = concat [ intraIcfgEdges, callEdges, returnEdges, callReturnEdges ]
    -- ^ The ICFG adds call/return edges on top of the original
    -- intraprocedural edges.

    unknownCallNode = case entryPoints of
      [_] -> Nothing
      _ -> Just $ moduleNextId m
    -- ^ With a single entry point we have a closed system and
    -- "unknown" functions can only be introduced through calls like
    -- dlopen and its Windows equivalents.  Otherwise we need to
    -- represent calls to unknown functions explicitly. FIXME: Search
    -- for dlopen

    cfgs = map getCFG fcfgs
    funcCfgs = map (\x -> (cfgFunction x, x)) cfgs
    funcCfgMap = M.fromList funcCfgs

    cfgNodes = concatMap (labNodes . cfgGraph) cfgs
    cfgEdges = concatMap (labEdges . cfgGraph) cfgs
    intraIcfgEdges = map (convertEdge callSet) cfgEdges

    callNodes = filter isCall cfgNodes
    callSet = S.fromList $ map fst callNodes

    callReturnNodes = map transformCallToReturnNode callNodes
    callReturnEdges = map makeCallToReturnEdge callNodes
    callEdgeMaker = makeCallEdges pta funcCfgMap unknownCallNode
    (callEdges, returnEdges) = unzip $ concatMap callEdgeMaker callNodes

-- FIXME: This does not handle invoke instructions yet.

-- | The major workhorse that constructs interprocedural edges.  For
-- the given call/invoke node, create an edge from the call to the
-- entry of the callee AND an edge from the return node of the callee
-- to the "return" pseudo-node for the call instruction.
--
-- This function will add edges to the special "unknown function" for
-- calls through function pointers in 'Module's that do not have a
-- single entry point.  Single-entry point modules (without calls to
-- dlopen) are closed systems where there are no unknown functions.
makeCallEdges :: (PointsToAnalysis a) => a -> HashMap Value CFG
                 -> Maybe Node -> LNode Value -> [(LEdge EdgeType, LEdge EdgeType)]
makeCallEdges pta valCfgs unknownCallNode (n, v) = case (isDirectCall v, unknownCallNode) of
  (_, Nothing) -> callEdges
  (True, _) -> callEdges
  (False, Just unknownId) -> unknownEdges unknownId : callEdges
  where
    unknownEdges unid = ((n, unid, CallToExtern Nothing),
                         (-unid, n, ReturnFromExtern Nothing))
    callEdges = S.fold mkCallEdge [] calledFuncs
    calledFuncs = pointsTo pta (calledValue v)
    mkCallEdge cf acc =
      case M.lookup cf valCfgs of
        Nothing ->
          let calleeEntryId = valueUniqueId cf
          in ((n, calleeEntryId, CallToExtern (Just cf)),
              (-calleeEntryId, n, ReturnFromExtern (Just cf))) : acc
        Just calleeCfg ->
          let calleeEntryId = valueUniqueId $ cfgEntryValue calleeCfg
          in ((n, calleeEntryId, CallToEntry cf),
              (-calleeEntryId, n, ReturnToCall cf)) : acc

-- | Get the value called by a Call or Invoke instruction
calledValue :: Value -> Value
calledValue Value { valueContent = CallInst { callFunction = v } } = v
calledValue Value { valueContent = InvokeInst { invokeFunction = v } } = v

-- | Return True if the given call (or invoke) instruction is a call
-- to a statically known function (rather than a function pointer).
isDirectCall :: Value -> Bool
isDirectCall ci = case cv of
  Value { valueContent = Function {} } -> True
  Value { valueContent = GlobalDeclaration {} } -> True
  Value { valueContent = GlobalAlias {} } -> True
  Value { valueContent = ExternalFunction {} } -> True
  _ -> False
  where
    cv = calledValue ci

-- | Make an intraprocedural edge from a call node to the
-- corresponding return node.
makeCallToReturnEdge :: LNode Value -> LEdge EdgeType
makeCallToReturnEdge (nid, _) = (nid, -nid, CallToReturn)

-- | Given a call node, create the corresponding return-site node.
-- This is implemented by simply negating the node ID (since all
-- normal node IDs are positive ints, this is fine.)
transformCallToReturnNode :: LNode Value -> LNode Value
transformCallToReturnNode (nid, v) = (-nid, v)

isCall :: LNode Value -> Bool
isCall (_, Value { valueContent = CallInst {} }) = True
isCall (_, Value { valueContent = InvokeInst {} }) = True
isCall _ = False

-- | The edges extracted from CFGs have different label types than in
-- the ICFG.  This function wraps them in the ICFG type denoting an
-- interprocedural edge.  Additionally, if the edge is from a call to
-- its intraprocedural successor, modify the edge to instead be from
-- the corresponding call "return" node to the successor.  The edge
-- from the call to the call return node is added later, as are
-- interprocedural edges.
convertEdge :: Set Node -> LEdge CFGEdge -> LEdge EdgeType
convertEdge callNodes (src, dst, lbl) =
  case S.member src callNodes of
    False -> (src, dst, IntraEdge lbl)
    True -> (-src, dst, IntraEdge lbl)
