module Data.LLVM.ICFG (
  -- * Types
  ICFG(..),
  EdgeType(..),
  NodeType(..),
  -- * Constructor
  mkICFG
  ) where

import Data.Graph.Inductive hiding ( Gr, UGr )
import Data.GraphViz ( toLabel )
import qualified Data.GraphViz as GV
import qualified Data.Set as S

import Text.Printf

import Data.LLVM.Types
import Data.LLVM.CFG
import Data.LLVM.Analysis.PointsTo
import Data.LLVM.Private.PatriciaTree

data NodeType = InstNode Instruction
              | ExternalEntry (Maybe ExternalFunction)
              | ExternalExit (Maybe ExternalFunction)

data EdgeType = CallToEntry Instruction
              | ReturnToCall Instruction
              | CallToReturn
              | IntraEdge CFGEdge

instance Show EdgeType where
  show (CallToEntry v) = printf "(_[%s]" (show (Value v))
  show (ReturnToCall v) = printf ")_[%s]" (show (Value v))
  show CallToReturn = "<call-to-return>"
  show (IntraEdge ce) = show ce

instance GV.Labellable EdgeType where
  toLabel = (GV.Label . GV.StrLabel) . show

data ICFG = ICFG { icfgGraph :: Gr NodeType EdgeType
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
mkICFG :: (PointsToAnalysis a) => Module
          -> a          -- ^ A points-to analysis
          -> [Function] -- ^ Entry points.  This could be just main or a larger list for a library
          -> ICFG
mkICFG m pta entryPoints =
  ICFG { icfgGraph = mkGraph allNodes allEdges
       , icfgEntryPoints = entryPoints
       , icfgModule = m
       }
  where
    initialData = (externEntryNodes ++ externExitNodes, externInternalEdges)
    (allNodes, allEdges) =
      foldr localBuilder initialData (moduleDefinedFunctions m)
    localBuilder :: Function -> ([LNode NodeType], [LEdge EdgeType]) -> ([LNode NodeType], [LEdge EdgeType])
    localBuilder = buildLocalGraph convertEdge convertCallEdge
                      transformCallToReturnNode convertNode
                      (buildCallEdges pta unknownCallNodeId)

    unknownCallNodeId = case (entryPoints, usesDlopen m) of
      ([_], False) -> Nothing
      _ -> Just $ moduleNextId m
    -- ^ With a single entry point we have a closed system and
    -- "unknown" functions can only be introduced through calls like
    -- dlopen and its Windows equivalents.  Otherwise we need to
    -- represent calls to unknown functions explicitly.
    externEntryNodes =
      let ns = map mkExternEntryNode (moduleExternalFunctions m)
      in case unknownCallNodeId of
        Nothing -> ns
        Just uid -> (uid, ExternalEntry Nothing) : ns
    externExitNodes =
      let ns = map mkExternExitNode (moduleExternalFunctions m)
      in case unknownCallNodeId of
        Nothing -> ns
        Just uid -> (-uid, ExternalExit Nothing) : ns
    externInternalEdges =
      let ns = map mkExternIntraEdge (moduleExternalFunctions m)
      in case unknownCallNodeId of
        Nothing -> ns
        Just uid -> (uid, -uid, CallToReturn) : ns

mkExternEntryNode :: ExternalFunction -> LNode NodeType
mkExternEntryNode ef = (valueUniqueId ef, ExternalEntry (Just ef))
mkExternExitNode :: ExternalFunction -> LNode NodeType
mkExternExitNode ef = (-(valueUniqueId ef), ExternalExit (Just ef))
mkExternIntraEdge :: ExternalFunction -> LEdge EdgeType
mkExternIntraEdge ef = (valueUniqueId ef, -(valueUniqueId ef), IntraEdge UnconditionalEdge)

usesDlopen :: Module -> Bool
usesDlopen m = foldr dlfold False (moduleExternalFunctions m)
  where
    dlfold ef acc = acc || show (externalFunctionName ef) == "@dlopen"


-- | The major workhorse that constructs interprocedural edges.  For
-- the given call/invoke node, create an edge from the call to the
-- entry of the callee AND an edge from the return node of the callee
-- to the "return" pseudo-node for the call instruction.
--
-- This function will add edges to the special "unknown function" for
-- calls through function pointers in 'Module's that do not have a
-- single entry point.  Single-entry point modules (without calls to
-- dlopen) are closed systems where there are no unknown functions.
buildCallEdges :: (PointsToAnalysis a)
                  => a
                  -> Maybe Node
                  -> Instruction
                  -> [LEdge EdgeType]
buildCallEdges pta unknownCallNode inst =
  case (isDirectCall inst, unknownCallNode) of
    (_, Nothing) -> callEdges'
    (True, _) -> callEdges'
    (False, Just uid) -> unknownEdges uid ++ callEdges'
  where
    instid = instructionUniqueId inst
    unknownEdges uid = [ (instid, uid, CallToEntry inst)
                       , (-uid, -instid, ReturnToCall inst)
                       ]
    calledFuncs = S.elems $ pointsTo pta (calledValue inst)
    callEdges = foldr mkCallEdge [] calledFuncs
    callEdges' = (instid, -instid, CallToReturn) : callEdges
    mkCallEdge :: Value -> [LEdge EdgeType] -> [LEdge EdgeType]
    mkCallEdge cf acc =
      case valueContent cf of
        FunctionC f ->
          let calleeEntryId = instructionUniqueId (functionEntryInstruction f)
              calleeExitId = instructionUniqueId (functionExitInstruction f)
          in (instid, calleeEntryId, CallToEntry inst) :
             (calleeExitId, -instid, ReturnToCall inst) : acc
        ExternalFunctionC ef ->
          let calleeEntryId = externalFunctionUniqueId ef
          in (instid, calleeEntryId, CallToEntry inst) :
             (-calleeEntryId, -instid, ReturnToCall inst) : acc
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
      InstructionC BitcastInst { castedValue = c } -> isDirectCall' c
      _ -> False

-- | Given a call node, create the corresponding return-site node.
-- This is implemented by simply negating the node ID (since all
-- normal node IDs are positive ints, this is fine.)
transformCallToReturnNode :: Instruction -> Maybe (LNode NodeType)
transformCallToReturnNode i = Just (-(instructionUniqueId i), InstNode i)

-- | The edges extracted from CFGs have different label types than in
-- the ICFG.  This function wraps them in the ICFG type denoting an
-- interprocedural edge.  Additionally, if the edge is from a call to
-- its intraprocedural successor, modify the edge to instead be from
-- the corresponding call "return" node to the successor.  The edge
-- from the call to the call return node is added later, as are
-- interprocedural edges.
-- convertEdge :: Set Node -> LEdge CFGEdge -> LEdge EdgeType
-- convertEdge callNodes (src, dst, lbl) =
--   case S.member src callNodes of
--     False -> (src, dst, IntraEdge lbl)
--     True -> (-src, dst, IntraEdge lbl)

convertEdge :: LEdge CFGEdge -> LEdge EdgeType
convertEdge (src, dst, lbl) = (src, dst, IntraEdge lbl)

convertCallEdge :: LEdge CFGEdge -> LEdge EdgeType
convertCallEdge (src, dst, lbl) = (-src, dst, IntraEdge lbl)

convertNode :: LNode Instruction -> LNode NodeType
convertNode (nid, nv) = (nid, InstNode nv)
