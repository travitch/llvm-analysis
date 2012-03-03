{-# LANGUAGE TemplateHaskell, ExistentialQuantification #-}
module Data.LLVM.Analysis.CallGraphSCCTraversal (
  callGraphSCCTraversal,
--  basicCallGraphSCCTraversal,
  parallelCallGraphSCCTraversal,
  callGraphAnalysis,
  callGraphAnalysisM,
  composableAnalysis,
  ComposableAnalysis,
  pureComposableAnalysis,
  pureComposableDependencyAnalysis,
  monadicComposableAnalysis,
  monadicComposableDependencyAnalysis,
  FuncLike(..)
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
import Data.LLVM.Types

import Data.LLVM.Internal.Condense
import Data.LLVM.Internal.PatriciaTree

import Text.Printf
import Debug.Trace
debug' = flip trace

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
  mapM_ (forkSCC cg varMap f seed) sccs
  -- let graphRoots = filter (isGraphRoot cg) (nodes cg)
  --     rootVars = map (getDep varMap) graphRoots
  finalVals <- mapM get outputVars -- rootVars
  return $! mconcat finalVals
  where
    g = projectDefinedFunctions $ callGraphRepr callgraph
    cg :: Gr [LNode Function] ()
    cg = condense g

callGraphAnalysisM :: (FuncLike funcLike, NFData summary, Monoid summary, Eq summary, Monad m)
                      => (m summary -> summary)
                      -> (funcLike -> summary -> m summary)
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

composableAnalysis :: (FuncLike funcLike, NFData compSumm, Monoid compSumm, Eq compSumm)
                      => [ComposableAnalysis compSumm funcLike]
                      -> ([funcLike] -> compSumm -> compSumm)
composableAnalysis analyses = f
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

monadicComposableAnalysis :: (NFData summary, Monoid summary, Eq summary, Monad m, FuncLike funcLike)
                             => (m summary -> summary)
                             -> (funcLike -> summary -> m summary)
                             -> Lens compSumm summary
                             -> ComposableAnalysis compSumm funcLike
monadicComposableAnalysis = ComposableAnalysisM

monadicComposableDependencyAnalysis :: (NFData summary, Monoid summary, Eq summary, Monad m, FuncLike funcLike)
                                       => (m summary -> summary)
                                       -> (deps -> funcLike -> summary -> m summary)
                                       -> Lens compSumm summary
                                       -> Lens compSumm deps
                                       -> ComposableAnalysis compSumm funcLike
monadicComposableDependencyAnalysis = ComposableAnalysisDM

pureComposableAnalysis :: (NFData summary, Monoid summary, Eq summary, FuncLike funcLike)
                          => (funcLike -> summary -> summary)
                          -> Lens compSumm summary
                          -> ComposableAnalysis compSumm funcLike
pureComposableAnalysis = ComposableAnalysis

pureComposableDependencyAnalysis :: (NFData summary, Monoid summary, Eq summary, FuncLike funcLike)
                          => (deps -> funcLike -> summary -> summary)
                          -> Lens compSumm summary
                          -> Lens compSumm deps
                          -> ComposableAnalysis compSumm funcLike
pureComposableDependencyAnalysis = ComposableAnalysisD

class FuncLike a where
  convertFunction :: Function -> a

instance FuncLike Function where
  convertFunction = id


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
    applyAnalysis component = f (map (convertFunction . snd) component)


getDep :: Map Node c -> Node -> c
getDep m n =
  case M.lookup n m of
    Nothing -> $err' ("Missing expected output var for node: " ++ show n)
    Just v -> v

forkSCC :: (NFData summary, Monoid summary, FuncLike funcLike)
           => Gr [LNode Function] ()
           -> Map Node (IVar summary)
           -> ([funcLike] -> summary -> summary)
           -> summary
           -> LNode [LNode Function]
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
      funcLikes = map (convertFunction . snd) component
      sccSummary = f funcLikes seed
      sccVar = getDep varMap nid
  put sccVar sccSummary

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
