{-# LANGUAGE TemplateHaskell #-}
module Data.LLVM.Analysis.CallGraphSCCTraversal (
  callGraphSCCTraversal,
  basicCallGraphSCCTraversal,
  parallelCallGraphSCCTraversal
  ) where

import Control.DeepSeq
import Control.Monad ( foldM, replicateM )
import Control.Monad.Par
import Data.Graph.Inductive hiding ( new )
import Data.Map ( Map )
import qualified Data.Map as M
import Data.Monoid
import FileLocation

import Data.LLVM.Analysis.CallGraph
import Data.LLVM.Types

import Data.LLVM.Internal.Condense

import Text.Printf
import Debug.Trace
debug' = flip trace

-- | This is the high-level interface to the CallGraphSCCTraversal.
-- It encapsualtes the logic to find a fixed-point for each SCC for
-- any analysis.
callGraphSCCTraversal :: (Monad m, Eq summary)
                         => CallGraph -- ^ The callgraph
                         -> (Function -> summary -> m summary) -- ^ A function to analyze a single Function and merge its results into a summary value
                         -> summary -- ^ An initial summary value
                         -> m summary
callGraphSCCTraversal cg analyzeFunction initialSummary =
  basicCallGraphSCCTraversal cg f initialSummary
  where
    -- If there is just a single component, we only need to analyze it once.
    f [singleComponent] summ = analyzeFunction singleComponent summ
    -- Otherwise, we need to find a fixed-point of the summary over
    -- all of the functions in this SCC.
    f sccComponents summ = do
      newSummary <- foldM (flip analyzeFunction) summ sccComponents
      case newSummary == summ of
        True -> return summ
        False -> f sccComponents newSummary

parallelCallGraphSCCTraversal :: (NFData summary, Monad m, Monoid summary, Eq summary)
                                 => CallGraph
                                 -> (m summary -> summary)
                                 -> (Function -> summary -> m summary)
                                 -> summary
                                 -> summary
parallelCallGraphSCCTraversal cg unwrap analyzeFunction initialSummary =
  parallelBasicCallGraphSCCTraversal cg unwrap f initialSummary
  where
    f [singleComponent] summ = analyzeFunction singleComponent summ
    f sccComponents summ = do
      newSummary <- foldM (flip analyzeFunction) summ sccComponents
      case newSummary == summ of
        True -> return summ
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
basicCallGraphSCCTraversal :: (Monad m)
                              => CallGraph -- ^ The callgraph
                              -> ([Function] -> summary -> m summary) -- ^ A function to process a strongly-connected component
                              -> summary -- ^ An initial summary value
                              -> m summary
basicCallGraphSCCTraversal callgraph f seed =
  -- Note, have to reverse the list here to process in bottom-up order
  -- since foldM is a left fold
  foldM applyAnalysis seed (reverse sccList)
  where
    g = projectDefinedFunctions $ callGraphRepr callgraph
    cg :: Gr [LNode Function] ()
    cg = condense g
    sccList = topsort' cg
    applyAnalysis summ component = f (map snd component) summ

-- The components of each SCC are processed serially, but the SCCs
-- themselves are processed in parallel using monad-par.  Each SCC
-- gets forked off into its own monad-par thread and the dependencies
-- are determined by the call graph.  summary will need to be an
-- instance of monoid so that all dependencies can be combined purely.
parallelBasicCallGraphSCCTraversal :: (NFData summary, Monad m, Monoid summary)
                                      => CallGraph
                                      -> (m summary -> summary)
                                      -> ([Function] -> summary -> m summary)
                                      -> summary
                                      -> summary
parallelBasicCallGraphSCCTraversal callgraph unwrap f seed = runPar $ do
  -- Make an output variable for each SCC in the call graph.
  outputVars <- replicateM (noNodes cg) new
  let sccs = labNodes cg
      varMap = M.fromList (zip (map fst sccs) outputVars)
  mapM_ (forkSCC cg varMap unwrap f seed) sccs
  let graphRoots = filter (isGraphRoot cg) (nodes cg)
      rootVars = map (getDep varMap) graphRoots
  finalVals <- mapM get rootVars
  return $! mconcat finalVals
  where
    g = projectDefinedFunctions $ callGraphRepr callgraph
    cg :: Gr [LNode Function] ()
    cg = condense g

isGraphRoot :: (Graph gr) => gr a b -> Node -> Bool
isGraphRoot cg = null . pre cg

getDep :: Map Node c -> Node -> c
getDep m n =
  case M.lookup n m of
    Nothing -> $err' ("Missing expected output var for node: " ++ show n)
    Just v -> v

forkSCC :: (NFData summary, Monad m, Monoid summary)
           => Gr [LNode Function] ()
           -> Map Node (IVar summary)
           -> (m summary -> summary)
           -> ([Function] -> summary -> m summary)
           -> summary
           -> LNode [LNode Function]
           -> Par ()
forkSCC cg varMap unwrap f val0 (nid, component) = fork $ do
  -- SCCs can contain self-loops in the condensed call graph, so
  -- remove those self loops here so we don't block the entire
  -- parallel computation with a thread waiting on itself.
  let deps = filter (/=nid) $ suc cg nid
      depVars = map (getDep varMap) deps
  depVals <- mapM get depVars
  let seed = case null deps of
        True -> val0
        False -> mconcat depVals
      sccSummary = f (map snd component) seed
      sccVar = getDep varMap nid
  put sccVar (unwrap sccSummary)


projectDefinedFunctions :: Gr CallNode b -> Gr Function b
projectDefinedFunctions g = nmap unwrap g'
  where
    unwrap (DefinedFunction f) = f
    unwrap _ = $err' "Expected a defined function"
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
