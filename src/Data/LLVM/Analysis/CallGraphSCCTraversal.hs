module Data.LLVM.Analysis.CallGraphSCCTraversal (
  callGraphSCCTraversal
  ) where

import Data.Graph.Inductive

import Data.LLVM.CallGraph
import Data.LLVM.Types

import Data.LLVM.Internal.Condense

-- | Traverse the callgraph bottom-up with an accumulator function.
--
-- > callGraphSCCTraversal cg f seed
--
-- This example applies the folding function @f@ over the callgraph
-- bottom-up with a starting @seed@.  Each strongly-connected
-- component is processed as a unit.  The final accumulated value
-- (based on @seed@) is returned.
--
-- The function @f@ is responsible for approximating the analysis
-- value for the SCC in whatever way makes sense for the analysis.
--
-- FIXME: Add a flag that says whether or not to include indirect
-- function calls
callGraphSCCTraversal :: CallGraph -> ([Function] -> a -> a) -> a -> a
callGraphSCCTraversal callgraph f seed =
  foldr applyAnalysis seed sccList
  where
    g = projectDefinedFunctions $ callGraphRepr callgraph
    cg :: Gr [LNode Function] ()
    cg = condense g
    sccList = topsort' cg
    applyAnalysis component = f (map snd component)

projectDefinedFunctions :: Gr CallNode b -> Gr Function b
projectDefinedFunctions g = nmap unwrap g'
  where
    unwrap (DefinedFunction f) = f
    g' = delNodes undefinedNodes $ delEdges edgesToUndefined g
    (undefinedNodes, edgesToUndefined) =
      foldr (extractUndefined g) ([], []) (nodes g)

extractUndefined :: Graph gr => gr CallNode b -> Node -> ([Node], [Edge]) -> ([Node], [Edge])
extractUndefined g n (nacc, eacc) =
  case lbl of
    DefinedFunction _ -> (nacc, newEdges ++ eacc)
    _ -> (n : nacc, newEdges ++ eacc)
  where
    (_, _, lbl, adjOut) = context g n
    newEdges = filter eitherUndefined $ zip (repeat n) (map snd adjOut)
    eitherUndefined (src, tgt) = isNotDefined src || isNotDefined tgt
    isNotDefined nid =
      let Just l = lab g nid
      in case l of
        DefinedFunction _ -> False
        _ -> True
