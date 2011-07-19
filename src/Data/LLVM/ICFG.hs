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

data EdgeType = CallToEntry Function
              | ReturnToCall Function
              | CallToReturn
              | CallToExtern (Maybe ExternalFunction)
              | ReturnFromExtern (Maybe ExternalFunction)
              | IntraEdge CFGEdge

data ICFG = ICFG { icfgGraph :: Gr Instruction EdgeType
                 , icfgEntryPoints :: [Function]
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
          -> a          -- ^ A points-to analysis
          -> [b]        -- ^ Values with control-flow graphs (either functions or pre-computed CFGs)
          -> [Function] -- ^ Entry points.  This could be just main or a larger list for a library
          -> ICFG
mkICFG m pta fcfgs entryPoints =
  ICFG { icfgGraph = mkGraph allNodes allEdges
       , icfgEntryPoints = entryPoints
       , icfgModule = m
       }
  where
    allNodes = concat [ cfgNodes, callReturnNodes, externEntryNodes, externExitNodes ]
    -- ^ The only extra nodes in the ICFG are call return nodes
    -- (representing the place to which flow resumes after a function
    -- call)

    allEdges = concat [ intraIcfgEdges, callEdges, returnEdges, callReturnEdges, externInternalEdges ]
    -- ^ The ICFG adds call/return edges on top of the original
    -- intraprocedural edges.

    unknownCallNodeId = case entryPoints of
      [_] -> Nothing
      _ -> Just $ moduleNextId m
    -- ^ With a single entry point we have a closed system and
    -- "unknown" functions can only be introduced through calls like
    -- dlopen and its Windows equivalents.  Otherwise we need to
    -- represent calls to unknown functions explicitly. FIXME: Search
    -- for dlopen

    externEntryNodes = map mkExternEntryNode (moduleExternalFunctions m)
    externExitNodes = map mkExternExitNode (moduleExternalFunctions m)
    externInternalEdges = map mkExternIntraEdge (moduleExternalFunctions m)

    -- FIXME: Need an unknown node for each call site?


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
    callEdgeMaker = makeCallEdges pta funcCfgMap unknownCallNodeId
    (callEdges, returnEdges) = unzip $ concatMap callEdgeMaker callNodes

mkExternEntryNode :: ExternalFunction -> LNode Instruction
mkExternEntryNode ef = (valueUniqueId ef, undefined)
mkExternExitNode :: ExternalFunction -> LNode Instruction
mkExternExitNode ef = (-(valueUniqueId ef), undefined)
mkExternIntraEdge :: ExternalFunction -> LEdge EdgeType
mkExternIntraEdge ef = (valueUniqueId ef, -(valueUniqueId ef), IntraEdge UnconditionalEdge)

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
makeCallEdges :: (PointsToAnalysis a) => a -> HashMap Function CFG
                 -> Maybe Node -> LNode Instruction
                 -> [(LEdge EdgeType, LEdge EdgeType)]
makeCallEdges pta valCfgs unknownCallNode (n, v) =
  case (isDirectCall v, unknownCallNode) of
    (_, Nothing) -> callEdges
    (True, _) -> callEdges
    (False, Just unknownId) -> unknownEdges unknownId : callEdges
  where
    unknownEdges unid = ((n, unid, CallToExtern Nothing),
                         (-unid, n, ReturnFromExtern Nothing))
    callEdges = S.fold mkCallEdge [] calledFuncs
    calledFuncs = pointsTo pta (calledValue v)
    mkCallEdge :: Value -> [(LEdge EdgeType, LEdge EdgeType)] -> [(LEdge EdgeType, LEdge EdgeType)]
    mkCallEdge cf acc =
      case valueContent cf of
        FunctionC f -> case M.lookup f valCfgs of
          Nothing -> error ("Missing function CFG in reverse map " ++ show (valueUniqueId f))
          Just calleeCfg ->
            let calleeEntryId = valueUniqueId $ cfgEntryValue calleeCfg
            in ((n, calleeEntryId, CallToEntry f),
                (-calleeEntryId, n, ReturnToCall f)) : acc
        ExternalFunctionC ef ->
          let calleeEntryId = valueUniqueId ef
          in ((n, calleeEntryId, CallToExtern (Just ef)),
              (-calleeEntryId, n, ReturnFromExtern (Just ef))) : acc
        GlobalAliasC GlobalAlias { globalAliasTarget = t } -> mkCallEdge t acc

-- | Get the value called by a Call or Invoke instruction
calledValue :: Instruction -> Value
calledValue CallInst { callFunction = v } = v
calledValue InvokeInst { invokeFunction = v } = v

-- | Return True if the given call (or invoke) instruction is a call
-- to a statically known function (rather than a function pointer).
isDirectCall :: Instruction -> Bool
isDirectCall ci = isDirectCall' cv
  where
    cv = calledValue ci
    isDirectCall' :: Value -> Bool
    isDirectCall' v = case valueContent v of
      FunctionC _ -> True
      ExternalFunctionC _ -> True
      GlobalAliasC GlobalAlias { globalAliasTarget = t } -> isDirectCall' t
      _ -> False

-- | Make an intraprocedural edge from a call node to the
-- corresponding return node.
makeCallToReturnEdge :: LNode Instruction -> LEdge EdgeType
makeCallToReturnEdge (nid, _) = (nid, -nid, CallToReturn)

-- | Given a call node, create the corresponding return-site node.
-- This is implemented by simply negating the node ID (since all
-- normal node IDs are positive ints, this is fine.)
transformCallToReturnNode :: LNode Instruction -> LNode Instruction
transformCallToReturnNode (nid, v) = (-nid, v)

isCall :: LNode Instruction -> Bool
isCall (_, CallInst {}) = True
isCall (_, InvokeInst {}) = True
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
