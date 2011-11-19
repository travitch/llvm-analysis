module Data.LLVM.Analysis.CallGraphSCCTraversal (
  callGraphSCCTraversal,
  basicCallGraphSCCTraversal
  ) where

import Data.Graph.Inductive

import Data.LLVM.CallGraph
import Data.LLVM.Types

import Data.LLVM.Internal.Condense

-- | This is the high-level interface to the CallGraphSCCTraversal.
-- It encapsualtes the logic to find a fixed-point for each SCC for
-- any analysis.
callGraphSCCTraversal :: (Eq summary)
                         => CallGraph -- ^ The callgraph
                         -> (Function -> summary -> summary) -- ^ A function to analyze a single Function and merge its results into a summary value
                         -> summary -- ^ An initial summary value
                         -> summary
callGraphSCCTraversal cg analyzeFunction initialSummary =
  basicCallGraphSCCTraversal cg f initialSummary
  where
    -- If there is just a single component, we only need to analyze it once.
    f [singleComponent] summ = analyzeFunction singleComponent summ
    -- Otherwise, we need to find a fixed-point of the summary over
    -- all of the functions in this SCC.
    f sccComponents summ =
      let newSummary = foldr analyzeFunction summ sccComponents
      in case newSummary == summ of
        True -> summ
        False -> f sccComponents newSummary

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
basicCallGraphSCCTraversal :: CallGraph -- ^ The callgraph
                              -> ([Function] -> summary -> summary) -- ^ A function to process a strongly-connected component
                              -> summary -- ^ An initial summary value
                              -> summary
basicCallGraphSCCTraversal callgraph f seed =
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
