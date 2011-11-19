module Data.LLVM.Internal.Condense ( condense ) where

import Data.Graph.Inductive
import qualified Data.Map as M
import Data.Map ( Map, (!) )
import Data.List ( nub )

-- | Turn a graph @g@ into a DAG by identifying its strongly-connected
-- components and re-connecting them.  This differs from the standard
-- fgl 'scc' function in that 'scc' only returns lists of nodes in
-- each strongly-connected component.  There are no edges connecting
-- these components.  This function restores the edges between
-- strongly-connected components.  The resulting graph only has a
-- single edge between each SCC, even if there was more than one in
-- the original graph.
condense :: (Eq b, Graph gr1, Graph gr2) => gr1 a b -> gr2 [LNode a] ()
condense g = mkGraph condensedNodes condensedEdges
  where
    -- That was a list of SCC nodes with integer ids.  We need to
    -- add the original labels to each of the SCC nodes.
    condensedNodes = map sccToNode sccIds
    condensedEdges = nub $ concatMap (remapEdges g nodeToSCCMap) (M.toList nodeToSCCMap)

    sccIds = zip [0..] $ scc g

    sccToNode (sccId, nodeIds) = (sccId, map (labNode' . context g) nodeIds)
    -- Map the nodeids in the original graph to their SCC node ids
    nodeToSCCMap = foldr buildRevMap M.empty condensedNodes

buildRevMap :: Ord k => (a, [(k, b)]) -> Map k (a, [(k, b)]) -> Map k (a, [(k, b)])
buildRevMap component@(_, oldNodes) acc =
  foldr build' acc oldNodes
  where
    build' (oldId,_) m = M.insert oldId component m

remapEdges :: Graph gr => gr a b -> Map Node (c, d) -> (Node, (e, f)) -> [(e, c, ())]
remapEdges g nodeToSCCMap (oldNodeId, (cid, _)) = map addEdge outEdges
  where
    (_, _, _, outEdges) = context g oldNodeId
    addEdge (_, oldDest) = (cid, fst (nodeToSCCMap ! oldDest), ())
