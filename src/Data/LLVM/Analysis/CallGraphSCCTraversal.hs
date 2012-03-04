{-# LANGUAGE TemplateHaskell, ExistentialQuantification #-}
module Data.LLVM.Analysis.CallGraphSCCTraversal (
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
import Data.Graph.Inductive hiding ( Gr, new )
import Data.Lens.Common
import Data.List ( foldl' )
import Data.Map ( Map )
import qualified Data.Map as M
import Data.Monoid
import FileLocation

import Data.LLVM.Analysis.CallGraph
import Data.LLVM.Analysis.Types
import Data.LLVM.Types

import Data.LLVM.Internal.Condense
import Data.LLVM.Internal.PatriciaTree

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
--
-- FIXME: The IVars are holding on to large state summaries for the
-- entire length of the analysis...  maybe that is unavoidable.
--
-- Possible solution: instead of having a big map of Node to IVar,
-- just pass each SCC its output var and its list of input IVars.
-- That way, as threads die, so do references to IVars and they can be
-- collected ASAP.
--
-- Use something like zip3 to make a list of tuples (SCC, inVars,
-- outVar) that can be folded over.
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

  -- Spawn a thread for each SCC that waits until its dependencies are
  -- analyzed (by blocking on the IVars above).  Each SCC fills its
  -- IVar after it has been analyzed.
  mapM_ (forkSCC cg varMap f seed) sccs

  -- let graphRoots = filter (isGraphRoot cg) (nodes cg)
  --     rootVars = map (getDep varMap) graphRoots

  -- Merge all of the results from all of the SCCs
  finalVals <- mapM get outputVars
  return $! mconcat finalVals
  where
    g = projectDefinedFunctions $ callGraphRepr callgraph
    cg :: Gr [LNode Function] ()
    cg = condense g

-- | Fork off a thread (using the Par monad) to process a
-- strongly-connected component in the call graph in its own thread.
-- The thread will block on IVars until the components dependencies
-- have been analyzed.  When the component is analyzed, it will fill
-- its IVar with a value to unblock the other threads waiting on it.
forkSCC :: (NFData summary, Monoid summary, FuncLike funcLike)
           => Gr [LNode Function] () -- ^ The call graph
           -> Map Node (IVar summary) -- ^ The IVars for each node in the call graph
           -> ([funcLike] -> summary -> summary) -- ^ The summary function to apply
           -> summary -- ^ The seed value
           -> LNode [LNode Function] -- ^ The strongly-connected component
           -> Par ()
forkSCC cg varMap f val0 (nid, component) = fork $ do
  -- SCCs can contain self-loops in the condensed call graph, so
  -- remove those self loops here so we don't block the entire
  -- parallel computation with a thread waiting on itself.
  let deps = filter (/=nid) $ suc cg nid
      depVars = map (getDep varMap) deps
  depVals <- mapM get depVars
  let seed = case null deps of
        True -> val0
        False -> force $ mconcat depVals
      funcLikes = map (fromFunction . snd) component
      sccSummary = f funcLikes seed
      sccVar = getDep varMap nid
  put sccVar sccSummary

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
    Nothing -> $err' ("Missing expected output var for node: " ++ show n)
    Just v -> v
