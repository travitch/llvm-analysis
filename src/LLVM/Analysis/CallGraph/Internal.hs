{-# LANGUAGE ExistentialQuantification, RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
-- | This internal module implements the CallGraph and the
-- CallGraphSCC traversal together because the traversal depends on
-- CallGraph internals.  They are meant to be used through their
-- respective interfaces, but this internal module is accessible in
-- case their APIs are insufficient to do something a user might want.
-- These internals are not stable.
module LLVM.Analysis.CallGraph.Internal (
  -- * Types
  CallGraph(..),
  CG,
  CallEdge(..),
  CallNode(..),
  -- * Constructor
  callGraph,
  -- * Accessors
  callGraphRepr,
  callValueTargets,
  callSiteTargets,
  callGraphFunctions,
  functionCallees,
  allFunctionCallees,
  functionCallers,
  allFunctionCallers,
  -- * Visualization
  cgGraphvizRepr,

  -- * CallGraphSCC Traversal
  ComposableAnalysis,
  Lens,
  callGraphSCCTraversal,
  parallelCallGraphSCCTraversal,

  -- * Adaptors
  callGraphAnalysis,
  callGraphAnalysisM,
  callGraphComposeAnalysis,
  composableAnalysis,
  composableDependencyAnalysis,
  composableAnalysisM,
  composableDependencyAnalysisM
  ) where

import Control.DeepSeq
import Control.Lens ( Simple, Lens, set, (^.) )
import Control.Monad ( foldM, replicateM )
import Control.Monad.Par.Scheds.Direct
import Data.GraphViz
import qualified Data.List as L
import Data.Maybe ( fromMaybe, mapMaybe )
import Data.Hashable
import Data.HashSet ( HashSet )
import qualified Data.HashSet as HS
import Data.HashMap.Strict ( HashMap )
import qualified Data.HashMap.Strict as HM
import Data.Map ( Map )
import qualified Data.Map as M
import Data.Monoid

import Data.Graph.Interface
import Data.Graph.MutableDigraph
import Data.Graph.Algorithms.Condense
import Data.Graph.Algorithms.DFS

import LLVM.Analysis
import LLVM.Analysis.PointsTo

-- | A type synonym for the underlying graph
type CG = SparseDigraph CallNode CallEdge

-- | The nodes are actually a wrapper type:
data CallNode = DefinedFunction Function
                -- ^ An actual function defined in this 'Module'
              | ExtFunction ExternalFunction
                -- ^ An externally-defined function with a declaration
                -- in the 'Module'
              | UnknownFunction
                -- ^ A function called indirectly that may not have
                -- any definition or declaration within the 'Module'
              deriving (Eq)

instance Show CallNode where
  show (DefinedFunction v) = show $ functionName v
  show (ExtFunction v) = "extern " ++ show (externalFunctionName v)
  show UnknownFunction = "unknown"

instance Labellable CallNode where
  toLabelValue = toLabelValue . show

data CallEdge = DirectCall
                -- ^ A static call to a known function
              | IndirectCall
                -- ^ A possible call to a known function through a
                -- function pointer
              | UnknownCall
                -- ^ A possible call to an unknown function through a
                -- function pointer
              deriving (Ord, Eq)

instance Hashable CallEdge where
  hashWithSalt s DirectCall = s `hashWithSalt` (1 :: Int)
  hashWithSalt s IndirectCall = s `hashWithSalt` (2 :: Int)
  hashWithSalt s UnknownCall = s `hashWithSalt` (3 :: Int)

instance Show CallEdge where
  show DirectCall = ""
  show IndirectCall = "?"
  show UnknownCall = "??"

instance Labellable CallEdge where
  toLabelValue = toLabelValue . show

-- | An opaque wrapper for the callgraph.  The nodes are functions and
-- the edges are calls between them.
data CallGraph = forall pta . (PointsToAnalysis pta) => CallGraph CG pta

-- | Get all of the functions defined in this module from the
-- CallGraph
callGraphFunctions :: CallGraph -> [Function]
callGraphFunctions (CallGraph cg _) =
  mapMaybe extractDefinedFunction (labeledVertices cg)
  where
    extractDefinedFunction (_, DefinedFunction f) = Just f
    extractDefinedFunction _ = Nothing

-- | Convert the CallGraph to a graph ADT that can be traversed,
-- manipulated, or easily displayed with graphviz.
--
-- For now, this representation is not guaranteed to remain stable.
callGraphRepr :: CallGraph -> CG
callGraphRepr (CallGraph g _) = g

-- | Given a Call or Invoke instruction, return the list of possible
-- callees.  All returned Values will be either Functions or
-- ExternalFunctions.
--
-- Passing a non-call/invoke instruction will trigger a noisy pattern
-- matching failure.
callSiteTargets :: CallGraph -> Instruction -> [Value]
callSiteTargets cg (CallInst { callFunction = f }) =
  callValueTargets cg f
callSiteTargets cg (InvokeInst { invokeFunction = f}) =
  callValueTargets cg f
callSiteTargets _ i =
  error ("LLVM.Analysis.CallGraph.callSiteTargets: Expected a Call or Invoke instruction: " ++ show i)

-- | Given the value called by a Call or Invoke instruction, return
-- all of the possible Functions or ExternalFunctions that it could
-- be.
callValueTargets :: CallGraph -> Value -> [Value]
callValueTargets (CallGraph _ pta) v =
  let v' = stripBitcasts v
  in case valueContent v' of
    FunctionC _ -> [v']
    ExternalFunctionC _ -> [v']
    _ -> pointsTo pta v

functionCallees :: CallGraph -> Function -> [Value]
functionCallees (CallGraph g _) =
  mapMaybe (toCallValue g) . suc g . functionUniqueId

allFunctionCallees :: CallGraph -> Function -> [Value]
allFunctionCallees (CallGraph g _) =
  mapMaybe (toCallValue g) . flip dfs g . (:[]) . functionUniqueId

functionCallers :: CallGraph -> Function -> [Value]
functionCallers (CallGraph g _) =
  mapMaybe (toCallValue g) . pre g . functionUniqueId

allFunctionCallers :: CallGraph -> Function -> [Value]
allFunctionCallers (CallGraph g _) =
  mapMaybe (toCallValue g) . flip rdfs g . (:[]) . functionUniqueId

toCallValue :: CG -> Vertex -> Maybe Value
toCallValue g v = do
  l <- lab g v
  case l of
    DefinedFunction f -> return (toValue f)
    ExtFunction ef -> return (toValue ef)
    _ -> Nothing

-- | Build a call graph for the given 'Module' using a pre-computed
-- points-to analysis.  The String parameter identifies the program
-- entry point.
--
-- FIXME: @entryPoint@ is not respected.
--
-- FIXME: Function pointers can be bitcasted - be sure to respect
-- those when adding indirect edges.
callGraph :: (PointsToAnalysis a)
             => Module
             -> a            -- ^ A points-to analysis (to resolve function pointers)
             -> [Function]   -- ^ The entry points to the 'Module'
             -> CallGraph
callGraph m pta _ {-entryPoints-} =
  CallGraph (mkGraph allNodes (unique allEdges)) pta
  where
    allNodes = concat [ knownNodes, unknownNodes, externNodes ]
    (allEdges, unknownNodes) = buildEdges pta funcs
    -- ^ Build up all of the edges and accumulate unknown nodes as
    -- they are created on-the-fly
    -- knownNodes = map (\v -> (valueUniqueId v, DefinedFunction v)) funcs
    knownNodes = map (\f -> (valueUniqueId f, DefinedFunction f)) funcs
    -- ^ Add nodes for unknown functions (one unknown node for each
    -- type signature in an indirect call).  The unknown nodes can use
    -- negative numbers for nodeids since actual Value IDs start at 0.

    externNodes = map mkExternFunc $ moduleExternalFunctions m

    funcs = moduleDefinedFunctions m

unique :: (Hashable a, Eq a) => [a] -> [a]
unique = HS.toList . HS.fromList

-- | This is the ID for the single "Unknown function" call graph node.
unknownNodeId :: Vertex
unknownNodeId = -100

mkExternFunc :: ExternalFunction -> (Vertex, VertexLabel CG)
mkExternFunc v = (valueUniqueId v, ExtFunction v)

buildEdges :: (PointsToAnalysis a) => a -> [Function] -> ([Edge CG], [(Vertex, VertexLabel CG)])
buildEdges pta funcs = do
  let es = map (buildFuncEdges pta) funcs
      unknownNodes = [(unknownNodeId, UnknownFunction)]
  (concat es, unknownNodes)

isCall :: Instruction -> Bool
isCall CallInst {} = True
isCall InvokeInst {} = True
isCall _ = False

buildFuncEdges :: (PointsToAnalysis a) => a -> Function -> [Edge CG]
buildFuncEdges pta f = concat es
  where
    insts = concatMap basicBlockInstructions $ functionBody f
    calls = filter isCall insts
    es = map (buildCallEdges pta f) calls

getCallee :: Instruction -> Value
getCallee CallInst { callFunction = f } = f
getCallee InvokeInst { invokeFunction = f } = f
getCallee i = error ("LLVM.Analysis.CallGraph.getCallee: Expected a function in getCallee: " ++ show i)

buildCallEdges :: (PointsToAnalysis a) => a -> Function -> Instruction -> [Edge CG]
buildCallEdges pta caller callInst = build' (getCallee callInst)
  where
    callerId = valueUniqueId caller
    build' calledFunc =
      case valueContent' calledFunc of
        FunctionC f ->
          [Edge callerId (valueUniqueId f) DirectCall]
        GlobalAliasC GlobalAlias { globalAliasTarget = aliasee } ->
          [Edge callerId (valueUniqueId aliasee) DirectCall]
        ExternalFunctionC ef ->
          [Edge callerId (valueUniqueId ef) DirectCall]
        -- Functions can be bitcasted before being called - trace
        -- through those to find the underlying function
        InstructionC BitcastInst { castedValue = bcv } -> build' bcv
        _ ->
          let targets = resolveIndirectCall pta callInst
              indirectEdges = map (\t -> Edge callerId (valueUniqueId t) IndirectCall) targets
              unknownEdge = Edge callerId unknownNodeId UnknownCall
          in unknownEdge : indirectEdges

cgGraphvizParams :: HashMap Int Int -> HashSet Int -> GraphvizParams Int CallNode CallEdge Int CallNode
cgGraphvizParams compMap singletons =
  defaultParams { fmtNode = \(_,l) -> [toLabel l]
                , fmtEdge = \(_,_,l) -> [toLabel l]
                , clusterBy = clusterByFunc
                , clusterID = clusterIDFunc
                }
  where
    clusterIDFunc cid =
      case cid `HS.member` singletons of
        True -> Str ""
        False -> Int cid
    clusterByFunc n@(nid, _) =
      let cid = HM.lookupDefault (-1) nid compMap
      in case cid `HS.member` singletons of
        True -> N n
        False -> C cid (N n)

cgGraphvizRepr :: CallGraph -> DotGraph Int
cgGraphvizRepr (CallGraph g _) =
  graphElemsToDot (cgGraphvizParams compMap singletons) ns es
  where
    ns = labeledVertices g
    es = map (\(Edge s d l) -> (s, d, l)) $ edges g
    comps = zip [0..] $ scc g
    singletons = HS.fromList $ map fst $ filter ((==0) . length . snd) comps
    compMap = foldr assignComponent mempty comps

assignComponent :: (Int, [Int]) -> HashMap Int Int -> HashMap Int Int
assignComponent (compId, nodeIds) acc =
  foldr (\nid -> HM.insert nid compId) acc nodeIds


-- CallGraphSCC Traversal

type FunctionGraph = SparseDigraph Function ()
type SCCGraph = SparseDigraph [(Vertex, VertexLabel FunctionGraph)] ()

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
                       , summaryLens :: Simple Lens compSumm summary
                       }
  | forall summary deps m . (NFData summary, Monoid summary, Eq summary, Monad m)
  => ComposableAnalysisDM { analysisUnwrap :: m summary -> summary
                          , analysisFunctionDM :: deps -> funcLike -> summary -> m summary
                          , summaryLens :: Simple Lens compSumm summary
                          , dependencyLens :: Simple Lens compSumm deps
                         }
  | forall summary . (NFData summary, Monoid summary, Eq summary)
    => ComposableAnalysis { analysisFunction :: funcLike -> summary -> summary
                          , summaryLens :: Simple Lens compSumm summary
                          }
  | forall summary deps . (NFData summary, Monoid summary, Eq summary)
    => ComposableAnalysisD { analysisFunctionD :: deps -> funcLike -> summary -> summary
                           , summaryLens :: Simple Lens compSumm summary
                           , dependencyLens :: Simple Lens compSumm deps
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
    cg = definedCallGraph callgraph
    sccList = topsort' cg
    applyAnalysis component =
      f (map (fromFunction . snd) component)

-- | The projection of the call graph containing only defined
-- functions (no externals)
definedCallGraph :: CallGraph -> SCCGraph
definedCallGraph = condense . projectDefinedFunctions . callGraphRepr

-- FIXME: Have this function take a list of funcLikes; it will
-- construct a @Map Function funcLike@ and pass that down to the
-- thread spawner, which will do map lookups instead of re-computing
-- the funcLike each time.

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
  outputVars <- replicateM (numVertices cg) new
  let sccs = labeledVertices cg
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
    cg = definedCallGraph callgraph

attachVars :: SCCGraph -> Map Int (IVar summary) -> (Vertex, VertexLabel SCCGraph)
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
          -- FIXME parmap
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
callGraphAnalysisM :: (FuncLike funcLike, Eq summary, Monad m)
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
callGraphAnalysis :: (FuncLike funcLike, Eq summary)
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
callGraphComposeAnalysis :: (FuncLike funcLike, Monoid compSumm, Eq compSumm)
                            => [ComposableAnalysis compSumm funcLike]
                            -> ([funcLike] -> compSumm -> compSumm)
callGraphComposeAnalysis analyses = f
  where
    f funcs summ =
      L.foldl' (applyAnalysisN funcs) summ analyses

    applyAnalysisN funcs summ a@ComposableAnalysisM { analysisUnwrap = unwrap
                                                    , analysisFunctionM = af
                                                    , summaryLens = lns
                                                    } =
      let inputSummary = summ ^. lns
          res = unwrap $ foldM (flip af) inputSummary funcs
      in case res == inputSummary of
        True -> summ
        False -> applyAnalysisN funcs (set lns res summ) a
    applyAnalysisN funcs summ a@ComposableAnalysisDM { analysisUnwrap = unwrap
                                                     , analysisFunctionDM = af
                                                     , summaryLens = lns
                                                     , dependencyLens = dlns
                                                     } =
      let inputSummary = summ ^. lns
          deps = summ ^. dlns
          af' = af deps
          res = unwrap $ foldM (flip af') inputSummary funcs
      in case res == inputSummary of
        True -> summ
        False -> applyAnalysisN funcs (set lns res summ) a
    applyAnalysisN funcs summ a@ComposableAnalysis { analysisFunction = af
                                                   , summaryLens = lns
                                                   } =
      let inputSummary = summ ^. lns
          res = foldr af inputSummary funcs
      in case res == inputSummary of
        True -> summ
        False -> applyAnalysisN funcs (set lns res summ) a
    applyAnalysisN funcs summ a@ComposableAnalysisD { analysisFunctionD = af
                                                    , summaryLens = lns
                                                    , dependencyLens = dlns
                                                    } =
      let inputSummary = summ ^. lns
          deps = summ ^. dlns
          res = foldr (af deps) inputSummary funcs
      in case res == inputSummary of
        True -> summ
        False -> applyAnalysisN funcs (set lns res summ) a


-- | A monadic version of 'composableAnalysis'.  The first argument
-- here is a function to unwrap a monadic value (something like
-- runIdentity or runReader).
composableAnalysisM :: (NFData summary, Monoid summary, Eq summary, Monad m, FuncLike funcLike)
                       => (m summary -> summary)
                       -> (funcLike -> summary -> m summary)
                       -> Simple Lens compSumm summary
                       -> ComposableAnalysis compSumm funcLike
composableAnalysisM = ComposableAnalysisM

-- | A monadic version of 'composableDependencyAnalysis'.
composableDependencyAnalysisM :: (NFData summary, Monoid summary, Eq summary, Monad m, FuncLike funcLike)
                                 => (m summary -> summary)
                                 -> (deps -> funcLike -> summary -> m summary)
                                 -> Simple Lens compSumm summary
                                 -> Simple Lens compSumm deps
                                 -> ComposableAnalysis compSumm funcLike
composableDependencyAnalysisM = ComposableAnalysisDM

-- | Create a pure composable analysis from a summary function and a
-- Lens that accesses the summary for this function (given the
-- composite summary).  The lens is used to access the current state
-- of this analysis and to update the state for this analysis after it
-- is run.
composableAnalysis :: (NFData summary, Monoid summary, Eq summary, FuncLike funcLike)
                          => (funcLike -> summary -> summary)
                          -> Simple Lens compSumm summary
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
                          -> Simple Lens compSumm summary
                          -> Simple Lens compSumm deps
                          -> ComposableAnalysis compSumm funcLike
composableDependencyAnalysis = ComposableAnalysisD




-- Helpers

projectDefinedFunctions :: CG -> FunctionGraph
projectDefinedFunctions g = mkGraph ns' es'
  where
    es = edges g
    ns = labeledVertices g
    ns' = foldr keepDefinedFunctions [] ns
    es' = map (\(Edge s d _) -> (Edge s d ())) $ filter (edgeIsBetweenDefined m) es
    m = M.fromList ns

keepDefinedFunctions :: (Vertex, VertexLabel CG)
                        -> [(Vertex, VertexLabel FunctionGraph)] -- [LNode FunctionGraph]
                        -> [(Vertex, VertexLabel FunctionGraph)] -- [LNode FunctionGraph]
keepDefinedFunctions (nid, DefinedFunction f) acc = (nid, f) : acc
keepDefinedFunctions _ acc = acc

edgeIsBetweenDefined :: Map Int CallNode -> Edge CG -> Bool
edgeIsBetweenDefined m (Edge src dst _) =
  nodeIsDefined m src && nodeIsDefined m dst

nodeIsDefined :: Map Int CallNode -> Int -> Bool
nodeIsDefined m n =
  case M.lookup n m of
    Just (DefinedFunction _) -> True
    _ -> False

getDep :: Map Int c -> Int -> c
getDep m n = fromMaybe errMsg (M.lookup n m)
  where
    errMsg = error ("LLVM.Analysis.CallGraphSCCTraversal.getDep: Missing expected output var for node: " ++ show n)

-- Some of the type signatures have redundant brackets to emphasize
-- that they are intended to be partially applied.
