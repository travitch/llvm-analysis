module Data.LLVM.ICFG (
  -- * Types
  ICFG(..),
  EdgeType(..),
  -- * Constructor
  mkICFG
  ) where

import Data.Graph.Inductive
import Data.HashMap.Strict ( HashMap )
import qualified Data.HashMap.Strict as M

import Data.LLVM.Types
import Data.LLVM.CFG
import Data.LLVM.Analysis.PointsTo

data EdgeType = CallToEntry
              | ReturnToCall
              | CallToReturn
              | CallToExtern
              | ReturnFromExtern
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

    cfgs = map getCFG fcfgs
    funcCfgs = map (\x -> (cfgFunction x, x)) cfgs
    funcCfgMap = M.fromList funcCfgs

    cfgNodes = concatMap (labNodes . cfgGraph) cfgs
    cfgEdges = concatMap (labEdges . cfgGraph) cfgs
    intraIcfgEdges = map convertEdge cfgEdges

    callNodes = filter isCall cfgNodes

    callReturnNodes = map transformCallToReturnNode callNodes
    callReturnEdges = map makeCallToReturnEdge callNodes
    callEdges = map (makeCallEdge pta funcCfgMap) callNodes
    returnEdges = undefined

-- FIXME: This does not handle invoke instructions yet.

makeCallEdge :: (PointsToAnalysis a) => a -> HashMap Value CFG -> LNode Value -> LEdge EdgeType
makeCallEdge pta valCfgs (n, v) = undefined

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

convertEdge :: LEdge CFGEdge -> LEdge EdgeType
convertEdge (src, dst, lbl) = (src, dst, IntraEdge lbl)

-- Take all of the nodes and edges from all of the CFGs and put them
-- in this graph.  Then, for every call, insert a return-from-call
-- node.  Hook up the edges as appropriate using information from the
-- callgraph.