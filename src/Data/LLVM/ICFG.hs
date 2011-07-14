module Data.LLVM.ICFG where

import Data.Graph.Inductive
import Data.HashMap.Strict ( HashMap )
import qualified Data.HashMap.Strict as M

import Data.LLVM.Types
import Data.LLVM.CFG
import Data.LLVM.CallGraph
import Data.LLVM.Analysis.PointsTo

data EdgeType = CallToEntry
              | ReturnToCall
              | CallToReturn
              | IntraEdge CFGEdge

data ICFG = ICFG { icfgGraph :: Gr Value EdgeType
                 , icfgEntryPoints :: [Value]
                 , icfgModule :: Module
                 }

mkICFG :: (PointsToAnalysis a) => Module -> a -> [Value] -> ICFG
mkICFG m pta entries = mkICFG' m entries cfgs callGraph
  where
    funcs = moduleDefinedFunctions m
    cfgs = zip funcs $ map mkCFG funcs
    callGraph = mkCallGraph m pta entries

-- FIXME: This does not handle invoke instructions yet.

mkICFG' :: Module -> [Value] -> [(Value, CFG)] -> CallGraph -> ICFG
mkICFG' m entries cfgs cg =
  ICFG { icfgGraph = mkGraph allNodes allEdges
       , icfgEntryPoints = entries
       , icfgModule = m
       }
  where
    allNodes = concat [ cfgNodes, callReturnNodes ]
    allEdges = concat [ cfgEdges', callEdges, returnEdges, callReturnEdges ]

    cfgNodes = concatMap (labNodes . cfgGraph . snd) cfgs
    cfgEdges = concatMap (labEdges . cfgGraph . snd) cfgs
    cfgEdges' = map convertEdge cfgEdges

    callNodes = filter isCall cfgNodes

    callReturnNodes = map transformCallToReturnNode callNodes
    callEdges = undefined
    returnEdges = undefined
    callReturnEdges = map makeCallToReturnEdge callNodes

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