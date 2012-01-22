{-# LANGUAGE ViewPatterns, BangPatterns, TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
-- | This module implements the compositional pointer/escape analysis
-- described by Whaley and Rinard (http://doi.acm.org/10.1145/320384.320400).
--
-- This version is adapted to the LLVM IR (originally for Java).
--
-- Each program variable has a VariableNode to enable lookups easily
-- during the analysis (the ID in the graph is the ID of the LLVM IR
-- object).  Each VariableNode has a corresponding location that it
-- represents (either an internal node or an external node).  The
-- types of each node correspond to those in the bitcode.  The
-- location node for a VariableNode has an ID that is the negated ID
-- of the corresponding name.
--
-- Memoize the representative field GEP for each operation by caching
-- the deduced value in the EscapeGraph structure.
--
-- Add a sequence number to the EscapeGraph and increment it whenever
-- edges are added or removed.  This should make graph equality tests
-- much faster.
module Data.LLVM.Analysis.Escape (
  -- * Types
  EscapeResult,
  EscapeGraph(..),
  EscapeNode(..),
  EscapeEdge(..),
  AccessType(Array, Field),
  PTEGraph,
  -- * Functions
  runEscapeAnalysis,
  escapeGraphAtLocation,
  localPointsTo,
  valueEscaped,
  valueProperlyEscaped,
  valueWillEscape,
  valueInGraph,
  followEscapeEdge,
  -- * Debugging
  -- viewEscapeGraph
  ) where

import Algebra.Lattice
import Control.DeepSeq
import Control.Monad.Identity
import Control.Monad.RWS.Strict
import qualified Data.Foldable as F
import Data.GraphViz
import Data.List ( foldl', mapAccumR )
import Data.Map ( Map )
import qualified Data.Map as M
import Data.Set ( Set )
import qualified Data.Set as S
import Data.Hashable
import Data.HashSet ( HashSet )
import qualified Data.HashSet as HS
import FileLocation

import Data.LLVM
import Data.LLVM.CFG
import Data.LLVM.CallGraph
import Data.LLVM.Analysis.CallGraphSCCTraversal
import Data.LLVM.Analysis.Dataflow

import Data.TransitiveClosure

import Data.Graph.Interface
import Data.Graph.PatriciaTree
import Data.Graph.Algorithms.Matching.DFS
-- import qualified Data.Graph.Inductive as FGL

-- import Text.Printf
-- import Debug.Trace
-- debug = flip trace

-- | The types of nodes in the graph
data EscapeNode = VariableNode { escapeNodeValue :: !Value }
                | OParameterNode { escapeNodeValue :: !Value }
                | OGlobalNode { escapeNodeValue :: !Value }
                | OReturnNode { escapeNodeValue :: !Value }
                | INode { escapeNodeValue :: !Value } -- Allocas and allocators
                | IVirtual { escapeNodeValue :: !Value }
                deriving (Eq, Ord, Show)

-- Don't need to define an instance here because the field is already strict
instance NFData EscapeNode

data AccessType = Direct
                | Array
                | Field !Int !Type
                deriving (Eq, Ord, Show)
instance NFData AccessType

-- | Edges labels for the points-to escape graph.  These differentiate
-- between internal and external edges.
data EscapeEdge = IEdge !AccessType
                | OEdge !AccessType
                deriving (Eq, Ord, Show)
instance NFData EscapeEdge

-- | A type synonym for the underlying points-to escape graph
type PTEGraph = SLGraph EscapeNode EscapeEdge

data EscapeGraphId = GrUID !Int -- ^ A unique ID generated for a
                                -- specific graph after it is modified
                                -- by the transfer function
                   | GrCompId !(HashSet Int) -- ^ A composite ID created by meeting graphs
                   deriving (Eq, Show)

instance NFData EscapeGraphId where
  rnf (GrUID _) = ()
  rnf (GrCompId hs) = hs `deepseq` ()

-- | The escape graph that is constructed for each program point.
-- They should all share considerable structure.
data EscapeGraph = EG { escapeGraph :: !PTEGraph
                      , escapeGraphId :: !EscapeGraphId
                      , escapeCalleeMap :: !(Map (Node PTEGraph) (HashSet Instruction))
                      , escapeReturns :: !(HashSet (Node PTEGraph))
                      }

instance NFData EscapeGraph where
  rnf eg@(EG g gid cm rs) =
    g `deepseq` gid `deepseq` cm `deepseq` rs `deepseq` eg `seq` ()

instance Hashable EscapeEdge where
  hash (IEdge at) = 3 `combine` hash at
  hash (OEdge at) = 7 `combine` hash at

instance Hashable AccessType where
  hash Direct = 11
  hash Array = 13
  hash (Field i t) = 17 `combine` hash i `combine` hash t

instance Hashable EscapeNode where
  hash (VariableNode v) = 3 `combine` hash v
  hash (OParameterNode v) = 5 `combine` hash v
  hash (OGlobalNode v) = 7 `combine` hash v
  hash (OReturnNode v) = 11 `combine` hash v
  hash (INode v) = 13 `combine` hash v
  hash (IVirtual v) = 17 `combine` hash v

instance Eq EscapeGraph where
  (==) !eg1 !eg2 = (escapeGraphId eg1 == escapeGraphId eg2 ||
                    escapeGraph eg1 `graphEqual` escapeGraph eg2) &&
                   escapeReturns eg1 == escapeReturns eg2 &&
                   escapeCalleeMap eg1 == escapeCalleeMap eg2

instance Monoid EscapeGraphId where
  mempty = GrUID (-1)
  mappend = meetGraphId

meetGraphId :: EscapeGraphId -> EscapeGraphId -> EscapeGraphId
meetGraphId (GrUID i1) (GrUID i2) = GrCompId (HS.fromList [i1, i2])
meetGraphId (GrUID i1) (GrCompId ids) = GrCompId (HS.insert i1 ids)
meetGraphId (GrCompId ids) (GrUID i2) = GrCompId (HS.insert i2 ids)
meetGraphId (GrCompId ids1) (GrCompId ids2) = GrCompId (HS.union ids1 ids2)

instance MeetSemiLattice EscapeGraph where
  meet eg1 eg2 = EG { escapeGraph = g''
                    , escapeGraphId = nid
                    , escapeCalleeMap = ecm
                    , escapeReturns = er
                    }
    where
      nid = escapeGraphId eg1 `mappend` escapeGraphId eg2
      ecm = M.unionWith HS.union (escapeCalleeMap eg1) (escapeCalleeMap eg2)
      er = escapeReturns eg1 `HS.union` escapeReturns eg2
      g'' = escapeGraph eg1 `mappend` escapeGraph eg2

instance BoundedMeetSemiLattice EscapeGraph where
  top = EG { escapeGraph = mkGraph [] []
           , escapeGraphId = GrUID 0
           , escapeCalleeMap = M.empty
           , escapeReturns = HS.empty
           }

data EscapeState = EscapeState { graphIdSource :: !Int }
type EscapeAnalysis = RWS EscapeData () EscapeState


instance DataflowAnalysis EscapeAnalysis EscapeGraph where
  transfer = escapeTransfer

-- | This is a module-internal datatype to represent information that
-- is constant throughout the analysis of a single function.  This
-- saves us from having to waste extra space in the dataflow fact,
-- since the dataflow analysis engine can just hand it off in constant
-- space.
data EscapeData =
  EscapeData { externalP :: ExternalFunction -> Int -> Bool
             , escapeSummary :: Map Function (DataflowResult EscapeGraph)
             }

-- | An opaque result type for the analysis.  Use
-- @escapeGraphAtLocation@ to access it.
data EscapeResult =
  ER { escapeResultMapping :: Map Function (DataflowResult EscapeGraph)
     , escapeExternalSummary :: ExternalFunction -> Int -> Bool
     }

instance NFData EscapeResult where
  rnf (ER m _) = m `deepseq` ()

instance Eq EscapeResult where
  (ER e1 _) == (ER e2 _) = e1 == e2

-- | An accessor to retrieve the @EscapeGraph@ for any program point.
escapeGraphAtLocation :: EscapeResult -> Instruction -> EscapeGraph
escapeGraphAtLocation ER { escapeResultMapping = er
                         , escapeExternalSummary = extSumm
                         } i =
  let (eg, ()) = evalRWS (dataflowResult funcRes i) ed es
  in eg
  where
    es = EscapeState 1
    ed = EscapeData { externalP = extSumm, escapeSummary = er }
    Just bb = instructionBasicBlock i
    f = basicBlockFunction bb
    errMsg = $err' ("No escape result for function" ++ show (functionName f))
    funcRes = M.findWithDefault errMsg f er

-- | Run the Whaley-Rinard escape analysis on a Module.  This returns
-- an opaque result that can be accessed via @escapeGraphAtLocation@.
--
-- This variant conservatively assumes that any parameter passed to an
-- external function escapes.
runEscapeAnalysis :: Module -> CallGraph -> EscapeResult
runEscapeAnalysis m cg = runEscapeAnalysis' m cg (\_ _ -> True)

-- | A variant of @runEscapeAnalysis@ that accepts a function to
-- provide escape information about arguments for external functions.
--
-- > runEscapeAnalysis' externP m
--
-- The function @externP@ will be called for each argument of each
-- external function.  The @externP ef ix@ should return @True@ if the
-- @ix@th argument of @ef@ causes the argument to escape.
--
-- FIXME: After a function is fully analyzed, convert its graphs into a more
-- compact form (based on vectors?)
runEscapeAnalysis' :: Module
                      -> CallGraph
                      -> (ExternalFunction -> Int -> Bool)
                      -> EscapeResult
runEscapeAnalysis' m cg externP =
  let analysis = callGraphSCCTraversal cg summarizeFunction (ER M.empty externP)
  in runIdentity analysis
  where
    globalGraph = buildBaseGlobalGraph m

    -- | This is the sub-analysis applied to each function
    -- individually.  Note that each instance of the sub-analysis gets
    -- a new EscapeData and EscapeState.  The state is fresh each time
    -- because the analyses are separate and the data is separate
    -- because the summaries are updated each invocation.
    summarizeFunction :: Function -> EscapeResult -> Identity EscapeResult
    summarizeFunction f (ER summ _) =
      let s0 = mkInitialGraph globalGraph f
          ed = EscapeData externP summ
          es = EscapeState 1
          (perInstLookupTable, ()) = evalRWS (forwardBlockDataflow s0 f) ed es
      in return $! ER (M.insert f perInstLookupTable summ) externP

-- | Provide local points-to information for a value @v@:
--
-- > localPointsTo eg v
--
-- This information is flow sensitive (there is an EscapeGraph for
-- each program point).  The returned set of nodes reflect the things
-- that the value can point to.
--
-- The set will be empty if the value is not a location (alloca,
-- argument, newly allocated storage, or global) or if it does not
-- point to anything yet.
--
-- This information does not include edges between globals or from
-- globals that are not established locally to the function call.  For
-- information at that level, use one of the global PointsTo analyses.
localPointsTo :: EscapeGraph -> Value -> Set EscapeNode
localPointsTo eg v = S.fromList (map (lab' . fromJust . context g) succs)
  where
    locid = valueUniqueId v
    g = escapeGraph eg
    succs = suc g locid

    errMsg = "localPointsTo: expected context not found"
    fromJust = maybe ($err' errMsg) id

-- | Determine whether or not the value has a representation in the
-- escape graph.
valueInGraph :: EscapeGraph -> Value -> Bool
valueInGraph eg v = gelem (valueUniqueId v) g
  where
    g = escapeGraph eg

-- | The value escaped from the current context because it is
-- reachable from an outside node.
valueEscaped :: EscapeGraph -> Value -> Bool
valueEscaped eg v = isGlobalNode g n || valueProperlyEscaped eg v
  where
    n = valueUniqueId v
    g = escapeGraph eg

-- | The value escaped from the current context because it is
-- reachable from an ouside node *besides itself*.  This is analagous
-- to the subset vs. proper subset distinction.
--
-- This is most useful to determine when one argument escapes by being
-- assigned into another.
valueProperlyEscaped :: EscapeGraph -> Value -> Bool
valueProperlyEscaped eg v = nodeProperlyEscaped (escapeGraph eg) (valueUniqueId v)

-- | The value will escape from the current context when the function
-- returns (i.e., through the return value).
valueWillEscape :: EscapeGraph -> Value -> Bool
valueWillEscape eg v = HS.member n escapingNodes
  where
    n = valueUniqueId v
    g = escapeGraph eg
    erList = HS.toList (escapeReturns eg)
    escapingNodes = escapeReturns eg `HS.union` HS.fromList (dfs erList g)

-- | From the base value @v@, follow the edge for the provided
-- @accessType@.  AccessTypes describe either array accesses or field
-- accesses.  There should only be one edge for each access type.
--
-- This function returns the Value at the indicated node (a
-- GetElementPtrInst).
--
-- > followEscapeEdge eg v accessType
followEscapeEdge :: EscapeGraph -> Value -> AccessType -> Maybe Value
followEscapeEdge eg v at =
  case targetSucs of
    [] -> Nothing
    [ts] -> Just $ (escapeNodeValue . lab' . fromJust . context g . fst) ts
    _ -> $err' ("Expected zero or one target successor: " ++ show targetSucs)
  where
    g = escapeGraph eg
    ss = lsuc g (valueUniqueId v)
    targetSucs = filter ((\x -> x==IEdge at || x==OEdge at) . snd) ss

    errMsg = $err' "followEscapeEdge: expected context not found"
    fromJust = maybe errMsg id

-- Internal stuff

nodeEscaped :: PTEGraph -> Node PTEGraph -> Bool
nodeEscaped escGr n = isGlobalNode escGr n || nodeProperlyEscaped escGr n

nodeProperlyEscaped :: PTEGraph -> Node PTEGraph -> Bool
nodeProperlyEscaped escGr n = any (isGlobalNode escGr) nodesReachableFrom
  where
    -- Remove the variable node corresponding to this node so that we
    -- don't say that it is escaping because its own variable node
    -- points to it.
    nodesReachableFrom =
      case match (-n) escGr of
        Nothing -> []
        Just (_, g) -> filter (/= n) $ rdfs [n] g

-- | The transfer function to add/remove edges to the points-to escape
-- graph for each instruction.
escapeTransfer :: EscapeGraph
                  -> Instruction
                  -> [CFGEdge]
                  -> EscapeAnalysis EscapeGraph
escapeTransfer eg i _ = do
-- FIXME: Strip the bitcasts of the value and address here.  In the
-- rest of the analysis- they are implicitly stripped by valueContent'
  (newGraph, passthrough) <- case i of
    StoreInst { storeValue = sv, storeAddress = sa } ->
      let sv' = stripBitcasts sv
          sa' = stripBitcasts sa
      in case isPointerType sv' of
        True -> do
          eg' <- updatePTEGraph sv' sa' eg
          return (eg', False)
        False -> return (eg, True)
    RetInst { retInstValue = Just rv } ->
      case isPointerType rv of
        True -> do
          (eg', targets) <- targetNodes eg rv
          return $! (eg' { escapeReturns = HS.fromList targets }, False)
        False -> return (eg, True)
    _ -> return (eg, True)

  case passthrough of
    True -> return newGraph
    False -> do
      gid <- nextGraphId
      return newGraph { escapeGraphId = gid }

-- isIntToPtrInst :: Value -> Bool
-- isIntToPtrInst v = case valueContent v of
--   InstructionC IntToPtrInst {} -> True
--   _ -> False

-- | Add/Remove edges from the PTE graph due to a store instruction
updatePTEGraph :: Value -> Value -> EscapeGraph -> EscapeAnalysis EscapeGraph
updatePTEGraph sv sa !eg = do
  (eg', valueNodes) <- targetNodes eg sv
  (eg'', addrNodes) <- targetNodes eg' sa
  egKilled <- killModifiedLocalEdges eg'' addrNodes
  foldM (genEdges valueNodes) egKilled addrNodes


-- | Add edges from addrNode to all of the valueNodes.  If
-- addrNode is global, do NOT kill its current edges.  If it is
-- local, kill the current edges.
genEdges :: [Node PTEGraph] -> EscapeGraph -> Node PTEGraph
            -> EscapeAnalysis EscapeGraph
genEdges valueNodes escGr addrNode =
  case null valueNodes of
    True -> return escGr
    False -> do
      let newEdges = map (\vnode -> LEdge (Edge addrNode vnode) (IEdge Direct)) valueNodes
          g = escapeGraph escGr
          g' = insEdges newEdges g
      return escGr { escapeGraph = g' }

-- | Given an EscapeGraph @eg@ and a list of location nodes, kill all
-- of the edges from the *local* locations.  Note that this returns a
-- bare PTE graph instead of the wrapped dataflow fact.
killModifiedLocalEdges :: EscapeGraph -> [Node PTEGraph]
                          -> EscapeAnalysis EscapeGraph
killModifiedLocalEdges !eg addrNodes =
  case null addrNodes of
    True -> return eg
    False ->
      let (g', modified) = foldl' killLocalEdges (escapeGraph eg, False) addrNodes
      in case modified of
        -- No change, return the originals
        False -> return eg
        -- Otherwise, use the updated versions
        True -> return eg { escapeGraph = g' }

killLocalEdges :: (PTEGraph, Bool) -> Node PTEGraph -> (PTEGraph, Bool)
killLocalEdges (escGr, modified) n =
  case nodeEscaped escGr n || isNotSingularNode escGr n || null killedEdges of
    True -> (escGr, modified)
    False -> (delEdges killedEdges escGr, True)
  where
    killedEdges = out escGr n
    -- es = map unlabelEdge killedEdges
--    unLabel (s, d, _) = (s, d)

-- | Determine whether or not a node is singular (e.g., represents a
-- single value).  Nodes that were obtained by an array access are not
-- singular.  Values returned by a function call in a loop are not
-- singular (they are summary values).
--
-- Non-singular values cannot be updated strongly.
--
-- FIXME: This is a stub for now and should be filled in.
isNotSingularNode :: PTEGraph -> Node PTEGraph -> Bool
isNotSingularNode _ _ = False

-- If storing to a global node, do NOT kill the edges from it.  Edges
-- should be killed for stores to locals.  Other than that, add edges
-- from the storeAddress to all of the storeValues.  Apparently loads
-- from local fields that may escape induce an extra Outside edge.


isGlobalNode :: PTEGraph -> Node PTEGraph -> Bool
isGlobalNode g n = case lbl of
  OParameterNode _ -> True
  OGlobalNode _ -> True
  _ -> False
  where
    Just lbl = lab g n

-- | Find the nodes that are pointed to by a Value (following pointer
-- dereferences).
targetNodes :: EscapeGraph -> Value
               -> EscapeAnalysis (EscapeGraph, [Node PTEGraph])
targetNodes eg val =
  let ((g', _), !targets) = targetNodes' ((escapeGraph eg), HS.empty) val
  in return (eg { escapeGraph = g'}, HS.toList targets)
  where
    targetNodes' (g, visited) v = case v `HS.member` visited of
      True -> ((g, visited), HS.empty)
      False -> case valueContent' v of
        -- Return the actual *locations* denoted by variable references.
        ArgumentC a -> ((g, vis'), HS.singleton (argumentUniqueId a))
        GlobalVariableC gv -> ((g, vis'), HS.singleton (globalVariableUniqueId gv))
        ExternalValueC e -> ((g, vis'), HS.singleton (externalValueUniqueId e))
        FunctionC f -> ((g, vis'), HS.singleton (functionUniqueId f))
        ExternalFunctionC e -> ((g, vis'), HS.singleton (externalFunctionUniqueId e))
        -- The NULL pointer doesn't point to anything
        ConstantC ConstantPointerNull {} -> ((g, vis'), HS.empty)
        -- Now deal with the instructions we might see in a memory
        -- reference.  There are many extras here (beyond just field
        -- sensitivity): select, phi, etc.
        InstructionC AllocaInst {} -> ((g, vis'), HS.singleton (valueUniqueId v))
        -- We can't really say anything useful with pointers generated
        -- from ints (without a huge amount of effort, and only for
        -- whole programs), Just punt
        InstructionC IntToPtrInst {} -> ((g, vis'), HS.empty)
        ConstantC ConstantValue { constantInstruction = IntToPtrInst {} } ->
          ((g, vis'), HS.empty)
        InstructionC LoadInst { loadAddress =
          (valueContent' -> InstructionC i@GetElementPtrInst { getElementPtrValue = base
                                                             , getElementPtrIndices = idxs
                                                             }) } ->
          gepInstTargets (g, vis') i (stripBitcasts base) idxs
        InstructionC LoadInst { loadAddress =
          (valueContent' -> ConstantC ConstantValue { constantInstruction =
            i@GetElementPtrInst { getElementPtrValue = base
                                , getElementPtrIndices = idxs
                                } }) } ->
          gepInstTargets (g, vis') i (stripBitcasts base) idxs

        -- Follow chains of loads (dereferences).  If there is no
        -- successor for the current LoadInst, we have a situation like
        -- a global pointer with no points-to target.  In that case, we
        -- need to create a virtual location node based on this load.
        --
        -- NOTE: check to see if this provides consistent behavior if
        -- different virtual nodes are chosen for the same logical
        -- location (e.g., in separate branches of an if statement).
        InstructionC i@LoadInst { loadAddress = la } ->
          let ((g', vis''), targets) = targetNodes' (g, vis') la
              (g'', successors) = mapAccumR (augmentingSuc i) g' (HS.toList targets)
          in ((g'', vis''), mconcat successors)
        InstructionC i@CallInst { } -> case gelem (valueUniqueId i) g of
          False -> $err' "Result of void return used"
          True -> ((g, vis'), HS.singleton (valueUniqueId i))
        InstructionC SelectInst { selectTrueValue = tv, selectFalseValue = fv } ->
          let ((g', vis''), tTargets) = targetNodes' (g, vis') tv
              ((g'', vis'''), fTargets) = targetNodes' (g', vis'') fv
          in ((g'', vis'''), tTargets `HS.union` fTargets)
        InstructionC PhiNode { phiIncomingValues = incoming } ->
          let ((g', vis''), targets) = mapAccumR targetNodes' (g, vis') (map fst incoming)
          in ((g', vis''), mconcat targets)
        -- It is also possible to store into the result of a GEP
        -- instruction (without a load), so add a case to handle
        -- un-loaded GEPs.
        InstructionC i@GetElementPtrInst { getElementPtrValue = base
                                         , getElementPtrIndices = idxs
                                         } ->
          gepInstTargets (g, vis') i (stripBitcasts base) idxs
        ConstantC ConstantValue { constantInstruction =
          i@GetElementPtrInst { getElementPtrValue = base
                              , getElementPtrIndices = idxs
                              } } ->
          gepInstTargets (g, vis') i (stripBitcasts base) idxs

        _ -> $err' $ "Escape Analysis unmatched: " ++ show v
      where
        vis' = HS.insert v visited

    gepInstTargets :: (PTEGraph, HashSet Value) -> Instruction -> Value -> [Value]
                      -> ((PTEGraph, HashSet Value), HashSet (Node PTEGraph))
    gepInstTargets (g, vis) i base idxs =
      case idxs of
        [] -> $err' ("Escape analysis: GEP with no indexes: " ++ show i)
        [_] ->
          let ((g', vis'), targets) = targetNodes' (g, vis) base -- `debug` printf "GEP Base: %s" (show base)
              (g'', successors) = mapAccumR (augmentingArraySuc i) g' (HS.toList targets) -- `debug` printf "  %s // %s" (show targets) (show (fst (match (head (HS.toList targets)) g')))
          in ((g'', vis'), mconcat successors)
        -- For this to be a simple field access, the array indexing
        -- offset must be zero and the field index must be some
        -- constant.
        (valueContent -> ConstantC ConstantInt { constantIntValue = 0}) :
          (valueContent -> ConstantC ConstantInt { constantIntValue = fieldNo }) : _ ->
            let ((g', vis'), targets) = targetNodes' (g, vis) base
                baseIsEscaped = F.any (nodeEscaped g') targets
                accumF = augmentingFieldSuc (fromIntegral fieldNo) (getBaseType base) i baseIsEscaped
                (g'', successors) = mapAccumR accumF g' (HS.toList targets)
            in ((g'', vis'), mconcat successors)
        -- Otherwise this is something really fancy and we can just
        -- treat it as an array
        _ ->
          let ((g', vis'), targets) = targetNodes' (g, vis) base
              (g'', successors) = mapAccumR (augmentingArraySuc i) g' (HS.toList targets)
          in ((g'', vis'), mconcat successors)

-- Above in gepInstTargets, the crash in baseIsEscaped is because
-- there is no explicit node in the graph for PHI (or Select)
-- instructions.  The check should really be over all possible phi
-- targets.
--
-- This pattern probably needs to be generalized throughout the
-- analysis....

-- FIXME: Filter out Phi instructions?  It is easiest to do that as a
-- post-processing step than to handle it in the algorithm itself.


possibleValues :: Value -> [Value]
possibleValues = filter notMultiInst . markVisited pvs . (:[])
  where
    notMultiInst val = case valueContent val of
      InstructionC SelectInst {} -> False
      InstructionC PhiNode {} -> False
      _ -> True
    pvs val =
      case valueContent val of
        InstructionC SelectInst { selectTrueValue = tv, selectFalseValue = fv } ->
          [ tv, fv ]
        InstructionC PhiNode { phiIncomingValues = inc } ->
          (map fst inc)
        InstructionC BitcastInst { castedValue = cv } ->
          [cv, val]
        _ -> [val]


getBaseType :: Value -> Type
getBaseType v = case valueType v of
  TypePointer t _ -> t
  _ -> $err' $ "Array base value has illegal type: " ++ show v

augmentingFieldSuc :: Int -> Type -> Instruction -> Bool -> PTEGraph -> Node PTEGraph
                      -> (PTEGraph, HashSet (Node PTEGraph))
augmentingFieldSuc ix ty i baseEscaped g tgt = case null fieldSucs of
  -- FIXME: There are some cases where this should be an OEdge!  If
  -- the base object of the field access is escaped, this should be an
  -- OEdge
  True -> addVirtual (edgeCon (Field ix ty)) i g tgt
  False -> (g, HS.fromList fieldSucs)
  where
    edgeCon = if baseEscaped then OEdge else IEdge
    fieldSucs = map fst $ filter (isFieldSuc ix baseEscaped) $ lsuc g tgt

augmentingArraySuc :: Instruction -> PTEGraph -> Node PTEGraph
                      -> (PTEGraph, HashSet (Node PTEGraph))
augmentingArraySuc i g tgt = case null arraySucs of
  True -> addVirtual (IEdge Array) i g tgt
  False -> (g, HS.fromList arraySucs)
  where
    arraySucs = map fst $ filter isArraySuc $ lsuc g tgt

-- FIXME: Can't kill edges if dealing with an array or escaped node

-- | This helper follows "pointers" in the points-to-escape graph by
-- one step, effectively dereferencing a pointer.  This is basically
-- just chasing the successors of the input node.
--
-- In some cases, though, a successor might not exist where the
-- dereference chain indicates that there should be one.  This means
-- that no points-to links/locations were set up in the local scope
-- for the dereference.  This can easily happen with struct field
-- accesses and references to global pointers.
--
-- In these unfortunate cases, the successor operation inserts
-- *virtual* nodes (and edges) to stand in for these unknown
-- locations.
augmentingSuc :: Instruction -> PTEGraph -> Node PTEGraph
                 -> (PTEGraph, HashSet (Node PTEGraph))
augmentingSuc i g tgt = case directSucs of
  [] -> addVirtual (IEdge Direct) i g tgt
  _ -> (g, HS.fromList directSucs)
  where
    labeledSucs = lsuc g tgt
    directSucs = map fst $ filter isDirectSuc labeledSucs

isDirectSuc :: (Node PTEGraph, EscapeEdge) -> Bool
isDirectSuc (_, IEdge Direct) = True
isDirectSuc (_, OEdge Direct) = True
isDirectSuc _ = False

isArraySuc :: (Node PTEGraph, EscapeEdge) -> Bool
isArraySuc (_, IEdge Array) = True
isArraySuc (_, OEdge Array) = True
isArraySuc _ = False

-- | When checking field successors, also match on the edge type.  If
-- the base node is escaped, we need to ensure we have an OEdge here
-- (if not, we make one in the caller).  If the base does escape, the
-- boolean flag here should match anyway...
isFieldSuc :: Int -> Bool -> (Node PTEGraph, EscapeEdge) -> Bool
isFieldSuc ix False (_, IEdge (Field fieldNo _)) = ix == fieldNo
isFieldSuc ix _ (_, OEdge (Field fieldNo _)) = ix == fieldNo
isFieldSuc _ _ _ = False

-- | A small helper to add a new virtual node (based on a load
-- instruction) and an edge from @tgt@ to the virtual instruction:
--
-- > addVirtual edgeLabel loadInst g tgt
--
-- It returns the modified graph and the singleton set containing the
-- new Node.  This returns an additional Bool flag to note that it has
-- modified the graph.
addVirtual :: EscapeEdge -> Instruction -> PTEGraph -> Node PTEGraph
              -> (PTEGraph, HashSet (Node PTEGraph))
addVirtual elbl i g tgt = (g'', HS.singleton iid)
  where
    iid = instructionUniqueId i
    newNode = LNode iid (IVirtual (Value i))
    newEdge = LEdge (Edge tgt iid) elbl
    g' = insNode newNode g
    g'' = insEdge newEdge g'

-- | Build the initial EscapeGraph <O_0, I_0, e_0, r_0> for the given
-- Function.  This adds local edges to the base global graph
-- (hopefully sharing some structure).
mkInitialGraph :: PTEGraph -> Function -> EscapeGraph
mkInitialGraph globalGraph f =
  EG { escapeGraph = g'
     , escapeGraphId = GrUID (-1)
     , escapeCalleeMap = M.empty
     , escapeReturns = HS.empty
     }
  where
    g = length nods `seq` insNodes nods globalGraph
    g' = length allEdges `seq` insEdges allEdges g
    allEdges = insideEdges ++ paramEdges
    nods = concat [ paramNodes, returnNodes, insideNodes ]
    insts = concatMap basicBlockInstructions (functionBody f)
    paramNodes = concatMap (mkVarCtxt OParameterNode . Value) (functionParameters f)
    paramEdges = map mkIEdge (functionParameters f)
    internalNodes = filter isInternal insts
    insideNodes = concatMap (mkVarCtxt INode . Value) internalNodes
    insideEdges = map mkIEdge internalNodes
    returnNodes = map (mkCtxt OReturnNode . Value) $ filter isNonVoidCall insts

mkCtxt :: (Value -> EscapeNode) -> Value -> LNode PTEGraph
mkCtxt ctor v = LNode (valueUniqueId v) (ctor v)

mkVarCtxt :: (Value -> EscapeNode) -> Value -> [LNode PTEGraph]
mkVarCtxt ctor v = [ LNode (-valueUniqueId v) (VariableNode v)
                   , LNode (valueUniqueId v) (ctor v)
                   ]

mkIEdge :: IsValue a => a -> LEdge PTEGraph
mkIEdge v = LEdge (Edge (-valueUniqueId v) (valueUniqueId v)) (IEdge Direct)

isNonVoidCall :: Instruction -> Bool
isNonVoidCall inst = case inst of
  CallInst { instructionType = TypeVoid } -> False
  CallInst {} -> True
  InvokeInst { instructionType = TypeVoid } -> False
  InvokeInst {} -> True
  _ -> False

isInternal :: Instruction -> Bool
isInternal inst = case inst of
  AllocaInst {} -> True
  _ -> False

-- | Construct the base graph that contains all of the global nodes in
-- the program.  The hope is that by having a common base graph, some
-- of the common structure can be shared.
--
-- FIXME: Add edges induced by global initializers - actually that
-- might be a bad idea since those edges may not actually exist when a
-- local function is analyzed.  The on-demand virtual node
-- construction should make them unnecessary anyway.
buildBaseGlobalGraph :: Module -> PTEGraph
buildBaseGlobalGraph m = mkGraph nodes0 edges0
  where
    globals = map Value $ moduleGlobalVariables m
    externs = map Value $ moduleExternalValues m
    efuncs = map Value $ moduleExternalFunctions m
    dfuncs = map Value $ moduleDefinedFunctions m
    globalVals = concat [ globals, externs, efuncs, dfuncs ]
    nodes0 = concatMap mkNod globalVals
    edges0 = map mkInitEdge globalVals
    mkNod v = [ LNode (valueUniqueId v) (OGlobalNode v)
              , LNode (-valueUniqueId v) (VariableNode v)
              ]
    mkInitEdge v = LEdge (Edge (-valueUniqueId v) (valueUniqueId v)) (OEdge Direct)

isPointerType :: Value -> Bool
isPointerType = isPointer' . valueType
  where
    isPointer' t = case t of
      TypePointer _ _ -> True
      _ -> False


-- | Get the next unique graph id and increment the id counter
nextGraphId :: EscapeAnalysis EscapeGraphId
nextGraphId = do
  s <- get
  let thisId = graphIdSource s
      nextId = thisId + 1
  nextId `seq` return ()
  put $! s { graphIdSource = nextId }
  return $! GrUID thisId

-- Debugging and visualization stuff

escapeParams :: Labellable a => a -> GraphvizParams n EscapeNode EscapeEdge () EscapeNode
escapeParams funcName =
  nonClusteredParams { fmtNode = formatEscapeNode
                     , fmtEdge = formatEscapeEdge
                     , globalAttributes = graphTitle
                     }
  where
    graphTitle = [GraphAttrs [toLabel funcName]]
    formatEscapeNode (_,l) = case l of
      VariableNode v ->
        let Just vname = valueName v
        in [toLabel (show vname), shape PlainText]
      OParameterNode _ -> [toLabel "p", shape Circle]
      OGlobalNode _ -> [toLabel "g", shape Circle]
      OReturnNode _ -> [toLabel "ret", shape Triangle]
      INode _ -> [toLabel "", shape BoxShape]
      IVirtual _ -> [toLabel "v", shape BoxShape, color Brown]
    formatEscapeEdge (_,_,l) = case l of
      IEdge Direct -> []
      IEdge Array -> [toLabel "[*]"]
      IEdge (Field ix t) -> fieldAccessToLabel ix t []
      OEdge Direct -> [style dotted, color Crimson]
      OEdge Array -> [toLabel "[*]", style dotted, color Crimson]
      OEdge (Field ix t) -> fieldAccessToLabel ix t [style dotted, color Crimson]

fieldAccessToLabel :: Int -> Type -> [Attribute] -> [Attribute]
fieldAccessToLabel ix t initSet = case t of
  TypeStruct (Just n) _ _ ->
    let accessStr = concat [ n, ".", show ix ]
    in toLabel accessStr : initSet
  TypeStruct Nothing _ _ ->
    let accessStr = "<anon>." ++ show ix
    in toLabel accessStr : initSet
  _ -> $err' ("Only struct types are expected in fieldAccessToLabel: " ++ show t)

{-
viewEscapeGraph :: EscapeResult -> Function -> IO ()
viewEscapeGraph e f = do
  let dg = graphToDot (escapeParams fname) exitGraph
  _ <- runGraphvizCanvas' dg Gtk
  return ()
  where
    fname = show (functionName f)
    exitFact = escapeGraphAtLocation e (functionExitInstruction f)
    exitGraph = escapeGraph exitFact
    g = FGL.mkGraph (labNodes exitGraph) (labEdges exitGraph)
-}
