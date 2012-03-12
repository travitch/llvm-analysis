{-# LANGUAGE TemplateHaskell, ExistentialQuantification #-}
module LLVM.Analysis.CallGraphSCCTraversal (
  -- * Traversals
  callGraphSCCTraversal,
  parallelCallGraphSCCTraversal,

  -- * Types
  FuncLike(..),
  ComposableAnalysis,
  Lens,

  -- * Adaptors
  callGraphAnalysis,
  callGraphAnalysisM,
  callGraphComposeAnalysis,
  composableAnalysis,
  composableDependencyAnalysis,
  composableAnalysisM,
  composableDependencyAnalysisM,
  ) where

import Control.DeepSeq
import Control.Monad ( foldM, replicateM )
import Control.Monad.Par
import Data.Graph.Inductive hiding ( new )
import Data.Lens.Common
import Data.List ( foldl' )
import Data.Map ( Map )
import qualified Data.Map as M
import Data.Monoid
import Debug.Trace.LocationTH

import LLVM.Analysis
import LLVM.Analysis.CallGraph
import LLVM.Analysis.Types

import LLVM.Analysis.Internal.Condense

-- import Text.Printf
-- import Debug.Trace
-- debug' = flip trace


-- | An abstract representation of a composable analysis.  Construct
-- these with the smart constructors 'composableAnalysis',
-- 'composableDependencyAnalysis', 'composableAnalysisM', and
-- 'composableDependencyAnalysisM'.
--
-- Use 'callGraphComposeAnalysis' to convert a list of these into a
-- summary function for use with the call graph traversals.
data ComposableAnalysis compSumm funcLike =
  forall summary m . (NFData summary, Monoid summary, Eq summary, Monad m)
  => ComposableAnalysisM { analysisUnwrap :: m summary -> summary
                       , analysisFunctionM :: funcLike -> summary -> m summary
                       , summaryLens :: Lens compSumm summary
                       }
  | forall summary deps m . (NFData summary, Monoid summary, Eq summary, Monad m)
  => ComposableAnalysisDM { analysisUnwrap :: m summary -> summary
                          , analysisFunctionDM :: deps -> funcLike -> summary -> m summary
                          , summaryLens :: Lens compSumm summary
                          , dependencyLens :: Lens compSumm deps
                         }
  | forall summary . (NFData summary, Monoid summary, Eq summary)
    => ComposableAnalysis { analysisFunction :: funcLike -> summary -> summary
                          , summaryLens :: Lens compSumm summary
                          }
  | forall summary deps . (NFData summary, Monoid summary, Eq summary)
    => ComposableAnalysisD { analysisFunctionD :: deps -> funcLike -> summary -> summary
                           , summaryLens :: Lens compSumm summary
                           , dependencyLens :: Lens compSumm deps
                           }


-- | Traverse the callgraph bottom-up with an accumulator function.
--
-- > callGraphSCCTraversal cg f seed
--
-- This example applies the folding function @f@ over each
-- strongly-connected component in the callgraph bottom-up with a
-- starting @seed@.  Each strongly-connected component is processed as
-- a unit.  The final accumulated value (based on @seed@) is returned.
--
-- The function @f@ is responsible for approximating the analysis
-- value for the SCC in whatever way makes sense for the analysis.
callGraphSCCTraversal :: (FuncLike funcLike)
                         => CallGraph -- ^ The callgraph
                         -> ([funcLike] -> summary -> summary) -- ^ A function to process a strongly-connected component
                         -> summary -- ^ An initial summary value
                         -> summary
callGraphSCCTraversal callgraph f seed =
  foldr applyAnalysis seed sccList
  -- Note, have to reverse the list here to process in bottom-up order
  -- since foldM is a left fold
  --
  -- NOTE now not reversing the SCC list because it is now a right
  -- fold
  where
    g = projectDefinedFunctions $ callGraphRepr callgraph
    cg :: Gr [LNode Function] ()
    cg = condense g
    sccList = topsort' cg
    applyAnalysis component = f (map (fromFunction . snd) component)

-- | Just like 'callGraphSCCTraversal', except strongly-connected
-- components are analyzed in parallel.  Each component is analyzed as
-- soon as possible after its dependencies have been analyzed.
parallelCallGraphSCCTraversal :: (NFData summary, Monoid summary, FuncLike funcLike)
                                 => CallGraph
                                 -> ([funcLike] -> summary -> summary)
                                 -> summary
                                 -> summary
parallelCallGraphSCCTraversal callgraph f seed = runPar $ do
  -- Make an output variable for each SCC in the call graph.
  outputVars <- replicateM (noNodes cg) new
  let sccs = labNodes cg
      varMap = M.fromList (zip (map fst sccs) outputVars)
      sccsWithVars = map (attachVars cg varMap) sccs

  -- Spawn a thread for each SCC that waits until its dependencies are
  -- analyzed (by blocking on the IVars above).  Each SCC fills its
  -- IVar after it has been analyzed.
  --
  -- The fold accumulates the output vars of the functions that are
  -- not depended on by any others.  These are the roots of the call
  -- graph and combining their summaries will yield the summary for
  -- the whole library.  This selectivity is explicit so that we
  -- retain as few outputVars as possible.  If we retain all of the
  -- output vars for the duration of the program, we get an explosion
  -- of retained summaries and waste a lot of space.
  rootOutVars <- foldM (forkSCC f seed) [] (force sccsWithVars)

  -- Merge all of the results from all of the SCCs
  finalVals <- mapM get rootOutVars
  return $! mconcat finalVals
  where
    g = projectDefinedFunctions $ callGraphRepr callgraph
    cg :: Gr [LNode Function] ()
    cg = condense g

attachVars :: Gr [LNode Function] ()
              -> Map Node (IVar summary)
              -> LNode [LNode Function]
              -> ([Function], [IVar summary], IVar summary, Bool)
attachVars cg varMap (nid, component) =
  (map snd component, inVars, outVar, isRoot)
  where
    outVar = varMap M.! nid
    inVars = map (getDep varMap) deps
    deps = filter (/=nid) $ suc cg nid
    isRoot = null (pre cg nid)

-- | Fork off a thread (using the Par monad) to process a
-- strongly-connected component in the call graph in its own thread.
-- The thread will block on IVars until the components dependencies
-- have been analyzed.  When the component is analyzed, it will fill
-- its IVar with a value to unblock the other threads waiting on it.
forkSCC :: (NFData summary, Monoid summary, FuncLike funcLike)
           => ([funcLike] -> summary -> summary) -- ^ The summary function to apply
           -> summary -- ^ The seed value
           -> [IVar summary]
           -> ([Function], [IVar summary], IVar summary, Bool)
           -> Par [IVar summary]
forkSCC f val0 acc (component, inVars, outVar, isRoot) = do
  fork $ do
    -- SCCs can contain self-loops in the condensed call graph, so
    -- remove those self loops here so we don't block the entire
    -- parallel computation with a thread waiting on itself.
    depVals <- mapM get inVars
    let seed = case null inVars of
          True -> val0
          False -> force $ mconcat depVals
        funcLikes = map fromFunction component
        sccSummary = f funcLikes seed
    put outVar sccSummary
  case isRoot of
    False -> return acc
    True -> return (outVar : acc)

-- | Make a call-graph SCC summary function from a basic monadic
-- summary function and a function to evaluate the function in its
-- monad and unwrap the monadic value.
--
-- The monadic equivalent of 'callGraphAnalysis'.
callGraphAnalysisM :: (FuncLike funcLike, NFData summary, Monoid summary, Eq summary, Monad m)
                      => (m summary -> summary) -- ^ A function to unwrap a monadic result from the summary
                      -> (funcLike -> summary -> m summary) -- ^ Summary function
                      -> ([funcLike] -> summary -> summary)
callGraphAnalysisM unwrap analyzeFunc = f
  where
    f [singleFunc] summ = unwrap $ analyzeFunc singleFunc summ
    f funcs summ = unwrap $ go funcs summ

    go funcs summ = do
      newSumm <- foldM (flip analyzeFunc) summ funcs
      case newSumm == summ of
        True -> return summ
        False -> go funcs newSumm

-- | Make a call-graph SCC summary function from a pure summary
-- function.  The function is applied to each function in the SCC in
-- an arbitrary order.  It returns the resulting summary obtained by
-- repeated evaluation until a fixed-point is reached.
callGraphAnalysis :: (FuncLike funcLike, NFData summary, Monoid summary, Eq summary)
                     => (funcLike -> summary -> summary)
                     -> ([funcLike] -> summary -> summary)
callGraphAnalysis analyzeFunc = f
  where
    f [singleFunc] summ = analyzeFunc singleFunc summ
    f funcs summ =
      let newSumm = foldr analyzeFunc summ funcs
      in case newSumm == summ of
        True -> summ
        False -> f funcs newSumm

-- | Compose a list of analyses into a pure summary function for use
-- in a callGraphSCCTraversal.  The advantage of using a composable
-- analysis is that it only traverses the call graph once.  At each
-- SCC, all analyses are applied until their fixed-point is reached.
--
-- This makes it easier to share intermediate values (like CFGs)
-- between analyses without having to recompute them or store them on
-- the side.
--
-- The input analyses are processed *in order* (left-to-right).  This
-- means that analyses with dependencies should come *after* the
-- analyses they depend on in the list.  This is not currently
-- statically enforced - your dependency summaries will just be
-- missing information you might have expected if you get the order
-- wrong.
callGraphComposeAnalysis :: (FuncLike funcLike, NFData compSumm, Monoid compSumm, Eq compSumm)
                            => [ComposableAnalysis compSumm funcLike]
                            -> ([funcLike] -> compSumm -> compSumm)
callGraphComposeAnalysis analyses = f
  where
    f [singleFunc] summ =
      foldl' (applyAnalysis1 singleFunc) summ analyses
    f funcs summ =
      foldl' (applyAnalysisN funcs) summ analyses

    applyAnalysisN funcs summ a@ComposableAnalysisM { analysisUnwrap = unwrap
                                                    , analysisFunctionM = af
                                                    , summaryLens = lns
                                                    } =
      let inputSummary = getL lns summ
          res = unwrap $ foldM (flip af) inputSummary funcs
      in case res == inputSummary of
        True -> summ
        False -> applyAnalysisN funcs (setL lns res summ) a
    applyAnalysisN funcs summ a@ComposableAnalysisDM { analysisUnwrap = unwrap
                                                     , analysisFunctionDM = af
                                                     , summaryLens = lns
                                                     , dependencyLens = dlns
                                                     } =
      let inputSummary = getL lns summ
          deps = getL dlns summ
          af' = af deps
          res = unwrap $ foldM (flip af') inputSummary funcs
      in case res == inputSummary of
        True -> summ
        False -> applyAnalysisN funcs (setL lns res summ) a




    applyAnalysis1 func summ ComposableAnalysisM { analysisUnwrap = unwrap
                                                 , analysisFunctionM = af
                                                 , summaryLens = lns
                                                 } =
      let analysisSumm = getL lns summ
          res = af func analysisSumm
      in setL lns (unwrap res) summ
    applyAnalysis1 func summ ComposableAnalysisDM { analysisUnwrap = unwrap
                                                  , analysisFunctionDM = af
                                                  , summaryLens = lns
                                                  , dependencyLens = dlns
                                                  } =
      let analysisSumm = getL lns summ
          deps = getL dlns summ
          res = af deps func analysisSumm
      in setL lns (unwrap res) summ

-- | A monadic version of 'composableAnalysis'.  The first argument
-- here is a function to unwrap a monadic value (something like
-- runIdentity or runReader).
composableAnalysisM :: (NFData summary, Monoid summary, Eq summary, Monad m, FuncLike funcLike)
                       => (m summary -> summary)
                       -> (funcLike -> summary -> m summary)
                       -> Lens compSumm summary
                       -> ComposableAnalysis compSumm funcLike
composableAnalysisM = ComposableAnalysisM

-- | A monadic version of 'composableDependencyAnalysis'.
composableDependencyAnalysisM :: (NFData summary, Monoid summary, Eq summary, Monad m, FuncLike funcLike)
                                       => (m summary -> summary)
                                       -> (deps -> funcLike -> summary -> m summary)
                                       -> Lens compSumm summary
                                       -> Lens compSumm deps
                                       -> ComposableAnalysis compSumm funcLike
composableDependencyAnalysisM = ComposableAnalysisDM

-- | Create a pure composable analysis from a summary function and a
-- Lens that accesses the summary for this function (given the
-- composite summary).  The lens is used to access the current state
-- of this analysis and to update the state for this analysis after it
-- is run.
composableAnalysis :: (NFData summary, Monoid summary, Eq summary, FuncLike funcLike)
                          => (funcLike -> summary -> summary)
                          -> Lens compSumm summary
                          -> ComposableAnalysis compSumm funcLike
composableAnalysis = ComposableAnalysis

-- | Like 'composableAnalysis', but with an extra lens that is used to
-- extract *dependency* information from the composite summary, which
-- is then fed into this summary function.
--
-- The intended use is that some analysis will have a dependency on an
-- earlier analysis summary.  The lens is used to extract the relevant
-- part of the composite summary.  A dependency on multiple earlier
-- analysis summaries can be expressed by providing a lens that
-- extracts a *tuple* containing all relevant analyses.
composableDependencyAnalysis :: (NFData summary, Monoid summary, Eq summary, FuncLike funcLike)
                          => (deps -> funcLike -> summary -> summary)
                          -> Lens compSumm summary
                          -> Lens compSumm deps
                          -> ComposableAnalysis compSumm funcLike
composableDependencyAnalysis = ComposableAnalysisD




-- Helpers

projectDefinedFunctions :: Gr CallNode b -> Gr Function b
projectDefinedFunctions g = mkGraph ns' es'
  where
    es = labEdges g
    ns = labNodes g
    ns' = foldr keepDefinedFunctions [] ns
    es' = filter (edgeIsBetweenDefined m) es
    m = M.fromList ns

keepDefinedFunctions :: LNode CallNode -> [LNode Function] -> [LNode Function]
keepDefinedFunctions (nid, DefinedFunction f) acc = (nid, f) : acc
keepDefinedFunctions _ acc = acc

edgeIsBetweenDefined :: Map Node CallNode -> LEdge b -> Bool
edgeIsBetweenDefined m (src, dst, _) =
  nodeIsDefined m src && nodeIsDefined m dst

nodeIsDefined :: Map Node CallNode -> Node -> Bool
nodeIsDefined m n =
  case m M.! n of
    DefinedFunction _ -> True
    _ -> False

getDep :: Map Node c -> Node -> c
getDep m n =
  case M.lookup n m of
    Nothing -> $failure ("Missing expected output var for node: " ++ show n)
    Just v -> v
