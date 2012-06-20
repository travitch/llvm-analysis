{-# LANGUAGE TemplateHaskell, ViewPatterns, FlexibleInstances, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | This is a very conservative flow- and context-insensitive escape
-- analysis based on graph reachability.  The underlying graph is a used-by
-- graph.  A value gets an edge to an instruction if the instruction uses the
-- value.
--
--  * A value Escapes if a store destination uses it *OR* it is passed
--    as an argument to a function pointer.
--
--  * A value WillEscape if it does not Escape but it is used by a ret
--    instruction
--
-- This graph has a few special properties and is not exactly a
-- used-by graph.  Since we only care about escape properties, store
-- destinations do not need to be represented (store x -> y cannot
-- possibly make y escape) and the value used to call a function
-- pointer does not need to be represented.
--
-- This analysis assumes that arguments passed as varargs do *not*
-- escape.
--
-- Only pointers can escape.  If a sub-component of an object escapes,
-- as in:
--
-- > global = s->foo;
--
-- this analysis will not report that @s@ escapes.  This type of
-- escaping can be identified by a derived analysis using the results
-- of this analysis.  This analysis will identify the loaded result of
-- @s->foo@ as escaping.
--
-- Notes on precision:
--
-- This analysis does not do any sophisticated value tracking through
-- memory references and it does not distinguish stores to locals
-- (allocas) from stores to globals.  With no optimization, it will
-- provide useless results (basically everything will escape).
--
-- With simple optimizations (-mem2reg and -basicaa) it will be very
-- precise.
module LLVM.Analysis.Escape (
  EscapeResult,
  escapeAnalysis,
  argumentEscapes,
  argumentFptrEscapes,
--  argumentWillEscape,
  instructionEscapes,
  instructionEscapesWith,
--  instructionWillEscape,

  -- * Testing
  EscapeGraph,
  escapeResultToTestFormat,
--  willEscapeResultToTestFormat,
  escapeUseGraphs,
  useGraphvizRepr
  ) where

import Control.Arrow
import Control.DeepSeq
import Control.Monad ( foldM )
import Data.Default
import Data.Function ( on )
import Data.GraphViz
import Data.Lens.Strict
import Data.Lens.Template
import Data.List ( find, foldl', groupBy, maximumBy, sortBy )
import Data.HashMap.Strict ( HashMap )
import qualified Data.HashMap.Strict as HM
import Data.Ord ( comparing )
import Data.Map ( Map )
import qualified Data.Map as Map
import Data.Maybe ( mapMaybe )
import Data.Monoid
import Data.Set ( Set )
import qualified Data.Set as S
import Debug.Trace.LocationTH
import Text.Printf

import Data.Graph.Interface
import Data.Graph.LazyHAMT
import Data.Graph.Algorithms.Matching.DFS

import LLVM.Analysis
import LLVM.Analysis.AccessPath

-- | This is an internal structure to record how arguments to function
-- calls in a function body let their arguments escape.  This is built
-- up in the first pass of the algorithm and used in the second.
data CallEscapes = CallEscapes { _fieldEscapes :: HashMap Value [AbstractAccessPath]
                               , _valueEscapes :: HashMap Value Instruction
                               , _fptrEscapes :: HashMap Value Instruction
                               }

instance Default CallEscapes where
  def = CallEscapes mempty mempty mempty

$(makeLens ''CallEscapes)

data NodeType = ArgumentSource Argument
              | FieldSource Argument AbstractAccessPath
                -- ^ Load (GEP Argument)
              | CallSource Instruction
                -- ^ Non-void call inst
              | FptrSink { sinkInstruction :: Instruction }
                -- ^ Indirect call inst
              | EscapeSink { sinkInstruction :: Instruction }
                -- ^ Passing a value to an escaping call argument
              | FieldEscapeSink { sinkInstruction :: Instruction }
                -- ^ Storing a value into a field that escapes
              | WillEscapeSink { sinkInstruction :: Instruction }
              | InternalNode Value
                -- ^ Other relevant nodes that pass references around.
                -- This can't just be an Instruction because it could
                -- be an Argument (constants and globals don't
                -- actually matter for this analysis)
              deriving (Eq, Ord, Show)

instance NFData NodeType where
  rnf (ArgumentSource a) = a `seq` ()
  rnf (FieldSource a aap) = a `seq` aap `seq` ()
  rnf (CallSource i) = i `seq` ()
  rnf (FptrSink i) = i `seq` ()
  rnf (EscapeSink i) = i `seq` ()
  rnf (FieldEscapeSink i) = i `seq` ()
  rnf (WillEscapeSink i) = i `seq` ()
  rnf (InternalNode v) = v `seq` ()

instance Labellable NodeType where
  toLabelValue (ArgumentSource a) = toLabelValue $ "Arg " ++ show a
  toLabelValue (FieldSource a aap) = toLabelValue $ "FldSrc " ++ show a ++ "@" ++ show aap
  toLabelValue (CallSource i) = toLabelValue $ "CallSrc " ++ show i
  toLabelValue (FptrSink i) = toLabelValue $ "FptrSink " ++ show i
  toLabelValue (EscapeSink i) = toLabelValue $ "EscSink " ++ show i
  toLabelValue (FieldEscapeSink i) = toLabelValue $ "FldEscSink " ++ show i
  toLabelValue (WillEscapeSink i) = toLabelValue $ "WillEscSink " ++ show i
  toLabelValue (InternalNode v) = toLabelValue $ "Int " ++ show v

-- | The acctual graph type
type EscapeGraph = Gr NodeType ()
type EscapeNode = LNode EscapeGraph
type EscapeEdge = LEdge EscapeGraph
type EscapeContext = Context EscapeGraph

-- | An opaque representation of escape information for a Module.
data EscapeResult =
  EscapeResult { _escapeGraphs :: HashMap Function EscapeGraph
                 -- ^ The escape graphs for each function
               , _escapeArguments :: HashMap Argument Instruction
                 -- ^ The arguments that escape simply, mapped to the witness instruction
               , _fptrEscapeArguments :: HashMap Argument Instruction
                 -- ^ Arguments that escape via function pointers
               , _escapeFields :: HashMap Argument [(AbstractAccessPath, Instruction)]
                 -- ^ Arguments that have at least one field escaping, paired with the relevant witness
               , _fptrEscapeFields :: HashMap Argument [(AbstractAccessPath, Instruction)]
               }

$(makeLens ''EscapeResult)

instance Eq EscapeResult where
  (EscapeResult g1 e1 f1 w1 fef1) == (EscapeResult g2 e2 f2 w2 fef2) =
    e1 == e2 && f1 == f2 && w1 == w2 && g1 == g2 && fef1 == fef2

instance Eq EscapeGraph where
  (==) = graphEqual

emptyResult :: EscapeResult
emptyResult = EscapeResult mempty mempty mempty mempty mempty

instance Default EscapeResult where
  def = mempty

instance Monoid EscapeResult where
  mempty = emptyResult
  mappend (EscapeResult gs1 as1 f1 was1 faf1) (EscapeResult gs2 as2 f2 was2 faf2) =
    EscapeResult { _escapeGraphs = HM.union gs1 gs2
                 , _escapeArguments = HM.union as1 as2
                 , _fptrEscapeArguments = HM.union f1 f2
                 , _escapeFields = HM.union was1 was2
                 , _fptrEscapeFields = HM.union faf1 faf2
                 }

instance NFData EscapeResult where
  rnf r@(EscapeResult gs as fs was faf) =
    gs `deepseq` as `deepseq` was `deepseq` fs `deepseq` faf `deepseq` r `seq` ()

-- | Get the set of escaped arguments for a function.  This function
-- will throw an error if the function is not in the escape result set
-- since that implies a programming error.
argumentEscapes :: EscapeResult -> Argument -> Maybe Instruction
argumentEscapes er a = HM.lookup a (er ^. escapeArguments)

argumentFptrEscapes :: EscapeResult -> Argument -> Maybe Instruction
argumentFptrEscapes er a = HM.lookup a (er ^. fptrEscapeArguments)

-- | A generalization of 'instructionEscapes'.  The first argument is
-- a predicate that returns True if the input Instruction (which is a
-- sink) should be excluded in the Escape graph.  The set of reachable
-- locations for the input instruction is computed as normal, but the
-- instructions in the @ignore@ list are removed from the set before
-- it is used to determine what escapes.
--
-- This arrangement means that @ignore@d nodes do *not* affect the
-- reachability computation.  That is critical for transitive
-- assignments to be treated properly (that is, for the transitive
-- links to be included).
--
-- The intended use of this variant is to issue escape queries for
-- instructions that are known to escape via some desired means (e.g.,
-- an out parameter) and to determine if they also escape via some
-- other means.  In that case, the @ignore@ list should be just the
-- store instruction that created the known escape.
instructionEscapesWith :: (Instruction -> Bool) -> Instruction -> EscapeResult -> Maybe Instruction
instructionEscapesWith = instructionEscapeCore

-- | Returns the instruction (if any) that causes the input
-- instruction to escape.  This does *not* cover WillEscape at all.
instructionEscapes :: Instruction -> EscapeResult -> Maybe Instruction
instructionEscapes = instructionEscapeCore (const False)

-- | Returns the return instruction (if any) that causes the given
-- instruction to be marked as WillEscape.
-- instructionWillEscape :: Instruction -> EscapeResult -> Maybe Instruction
-- instructionWillEscape = instructionEscapeCore extract (const False)
--   where
--     extract (WillEscapeWitness w) = Just w
--     extract _ = Nothing

instructionEscapeCore :: (Instruction -> Bool)
                         -> Instruction
                         -> EscapeResult
                         -> Maybe Instruction
instructionEscapeCore ignoreValue i er = do
  escNode <- find nodeIsAnySink reached
  return $! sinkInstruction escNode
  where
    Just bb = instructionBasicBlock i
    f = basicBlockFunction bb
    errMsg = $failure ("Expected escape graph for " ++ show (functionName f))
    g = HM.lookupDefault errMsg f (er ^. escapeGraphs)
    reached = filter notIgnoredSink $ reachableValues i g
    notIgnoredSink nt =
      case nt of
        FptrSink sink -> not (ignoreValue sink)
        EscapeSink sink -> not (ignoreValue sink)
        FieldEscapeSink sink -> not (ignoreValue sink)
        WillEscapeSink sink -> not (ignoreValue sink)
        _ -> True

-- | Get the list of values reachable from the given instruction in
-- the use graph.  An instruction is not reachable from itself unless
-- it is in a cycle.
--
-- Sinks are the only things that allow escaping, and the filtering
-- step here only need to worry about those.  Sinks are always store
-- instructions or call instructions
--
-- We can always remove the node because call escape nodes have
-- negated ids?
reachableValues :: Instruction -> EscapeGraph -> [NodeType]
reachableValues i g =
  let reached = filter (/= valueUniqueId i) $ dfs [instructionUniqueId i] g
  in map (safeLab $__LOCATION__ g) reached

{-

1) Collect a @Map Instruction [AccessPath]@ that describes the fields
of each alloca instruction passed to them that escapes.  Entries in
this map are made for each call instruction argument that allows a(t
least one) field to escape.

> call void @fldEsc(%struct.S* %s)

If this call allows the sP field of %s to escape, the resuling Map
entry is:

> %s -> [Field 0]

Also collect a set of values passed to escaping function arguments.

2) Populate the escape graph.  Arguments get ArgSrc nodes.  Loads of
GEPs (rooted at arguments) get FieldSource nodes.  All other
instructions that are pointer-typed get SimpleSrc nodes.  If the base
of a GEP Load is in the field escape map AND the field matches one of
the access paths in the map, make an edge from the src to a
FieldEscapeSink.

For each value in the normal escape set, make an edge from the source
to the appropriate escapesink node.

Stores add edges, loads add internal nodes.

3) All ArgumentSource nodes that reach a Sink escape.  If the sink is
a FieldEscapeSink, they escape through a field (though the distinction
doesn't really matter).

Later queries will similarly only check to see if the instruction can
reach a sink.  There will need to be a bit of filtering done on sinks
in the same way as now, but the filtering now has to unwrap the node
type instead of being able to just look directly at the node Value.
If the only reachable sink is a FptrSink, treat this as we do in the
case where the Value is tupled with True now.


-}


-- | This is the underlying bottom-up analysis to identify which
-- arguments escape.  It builds a UseGraph for the function
-- (incorporating information from other functions that have already
-- been analyzed) and then checks to see which arguments escape using
-- that graph.
--
-- The function summary should return true if the ith argument of the
-- given external function escapes.
escapeAnalysis :: (Monad m, HasFunction funcLike)
                  => (ExternalFunction -> Int -> m Bool)
                  -> funcLike
                  -> EscapeResult
                  -> m EscapeResult
escapeAnalysis extSumm funcLike summ = do
  callEscapes <- foldM (buildEscapeMaps extSumm summ) def (functionInstructions f)
  let g = buildEscapeGraph callEscapes f
      summ1 = summ { _escapeGraphs = HM.insert f g (summ ^. escapeGraphs) }
  return $ foldr (summarizeArgumentEscapes g) summ1 (labNodes g)
  where
    f = getFunction funcLike

-- | For the given 'EscapeNode' input, if it is an argument source,
-- run a reachability computation and see if it reaches any sinks.  If
-- it does, the argument (or field) escapes.
--
-- Note that the DFS in each case should start from n - the input
-- node.  FieldSources do not have the ID of their Argument (because
-- arguments can have multiple fields, the FieldSource has the ID of
-- the relevant Load instruction).
summarizeArgumentEscapes :: EscapeGraph -> EscapeNode -> EscapeResult -> EscapeResult
summarizeArgumentEscapes g n summ =
  case nodeLabel n of
    ArgumentSource a ->
      case argumentType a of
        TypePointer _ _ ->
          let reached = map (safeLab $__LOCATION__ g) $ dfs [unlabelNode n] g
          in case find nodeIsSink reached of
            Just sink -> (escapeArguments ^!%= HM.insert a (sinkInstruction sink)) summ
            Nothing -> case find nodeIsFptrSink reached of
              Nothing -> summ
              Just fsink -> (fptrEscapeArguments ^!%= HM.insert a (sinkInstruction fsink)) summ
        _ -> summ
    FieldSource a absPath ->
      case argumentType a of
        TypePointer _ _ ->
          let reached = map (safeLab $__LOCATION__ g) $ dfs [unlabelNode n] g
          in case find nodeIsSink reached of
            Just sink -> (escapeFields ^!%= HM.insertWith (++) a [(absPath, sinkInstruction sink)]) summ
            Nothing -> case find nodeIsFptrSink reached of
              Nothing -> summ
              Just fsink -> (fptrEscapeFields ^!%= HM.insertWith (++) a [(absPath, sinkInstruction fsink)]) summ
        _ -> summ
    _ -> summ

nodeIsSink :: NodeType -> Bool
nodeIsSink t =
  case t of
    EscapeSink _ -> True
    FieldEscapeSink _ -> True
    _ -> False

nodeIsFptrSink :: NodeType -> Bool
nodeIsFptrSink t =
  case t of
    FptrSink _ -> True
    _ -> False

nodeIsAnySink :: NodeType -> Bool
nodeIsAnySink t =
  case t of
    EscapeSink _ -> True
    FieldEscapeSink _ -> True
    FptrSink _ -> True
    WillEscapeSink _ -> True
    _ -> False

buildEscapeGraph :: CallEscapes -> Function -> EscapeGraph
buildEscapeGraph callEscapes f =
  mkGraph (uniqueNodes) (callEdges ++ escapeEdges)
  where
    argNodes = map toArgumentNode (functionParameters f)
    (bodyNodes, escapeEdges) = foldl' (collectEdges callEscapes) ([], []) (functionInstructions f)

    -- When making sinks for calls, *negate* the ID of the call
    -- instruction.  This will let instructions be both sources and
    -- sinks (which will be useful).
    (callArgNodes, callEdges) = buildCallEscapeSubgraph callEscapes
    allNodes = concat [ argNodes, callArgNodes, bodyNodes ]
    nodeId = comparing unlabelNode


    -- To unique the nodes, first sortBy on the node ID, then use
    -- groupBy on the same nodeid.  This will yield lists of lists;
    -- any list of length > 1 needs to be folded over to select the
    -- most specific node available (mostly discarding generic
    -- InternalNodes).  Edges do not need to be fixed at all since
    -- they are only keyed on ID
    uniqueNodeGroups = groupBy (on (==) unlabelNode) $ sortBy nodeId allNodes
    uniqueNodes = foldr takeMostSpecific [] uniqueNodeGroups

-- | This helper needs to traverse valueEscapes and fptrEscapes and
-- make appropriate sink nodes (and edges).  fieldEscapes are taken
-- care of in the main function body traversal.  Note that the node
-- IDs of call sinks are negated to prevent collisions with call
-- sources.
buildCallEscapeSubgraph :: CallEscapes -> ([EscapeNode], [EscapeEdge])
buildCallEscapeSubgraph callEscapes =
  foldr (makeCallEscape EscapeSink) s0 $ HM.toList $ callEscapes ^. valueEscapes
  where
    s0 = foldr (makeCallEscape FptrSink) ([], []) $ HM.toList $ callEscapes ^. fptrEscapes
    makeCallEscape constructor (val, call) (ns, es) =
      let newNode = LNode (-valueUniqueId call) (constructor call)
          newEdge = LEdge (Edge (valueUniqueId val) (-valueUniqueId call)) ()
      in (newNode : ns, newEdge : es)

takeMostSpecific :: [EscapeNode] -> [EscapeNode] -> [EscapeNode]
takeMostSpecific ens acc =
  case ens of
    [] -> $failure "groupBy produced an empty group"
    [elt] -> elt : acc
    elts -> maximumBy escapeStrengthOrder elts : acc
  where
    -- Anything has higher precedence than an internal node.  Also,
    -- fptrescape can be superceded by any other sink.  Source nodes
    -- are superceded by sinks, though that should only happen for
    -- CallSource, but the call sinks have negated IDs
    escapeStrengthOrder nt1 nt2 =
      case (nodeLabel nt1, nodeLabel nt2) of
        (InternalNode _, InternalNode _) -> EQ
        (InternalNode _, _) -> LT
        (_, InternalNode _) -> GT
        (FptrSink _, FptrSink _) -> EQ
        (FptrSink _, _) -> LT
        (_, FptrSink _) -> GT
        _ -> $failure ("Unexpected escape order overlap " ++ show nt1 ++ " and " ++ show nt2)


toArgumentNode :: Argument -> EscapeNode
toArgumentNode a = LNode (argumentUniqueId a) (ArgumentSource a)


-- | Build nodes an edges in the escape graph.  Note that we have a
-- very specific notion of escape here.  The following constructs
-- cause pointers to escape:
--
--  * ret instructions
--
--  * stores into arguments
--
--  * stores into globals
--
--  * passing a value as an argument that is known to escape
--
-- Note that we don't actually need to handle call instructions here
-- (except in that we need to create CallSource nodes for them) since
-- we have all of the relevant escape information bundled up in
-- @callEscapes@.  We can generate the necessary nodes and edges
-- directly from that much more easily.
collectEdges :: CallEscapes -> ([EscapeNode], [EscapeEdge])
                    -> Instruction -> ([EscapeNode], [EscapeEdge])
collectEdges callEscapes acc@(ns, es) i =
  case i of
    AllocaInst {} ->
      let newNode = toInternalNode (Value i)
      in (newNode : ns, es)

    -- A return node gets a WillEscapeSink.  Only make this sink if
    -- the returned value is a pointer type (to keep graph sizes
    -- smaller)
    RetInst { retInstValue = Just rv } ->
      case valueType rv of
        TypePointer _ _ ->
          let newNode = LNode (instructionUniqueId i) (WillEscapeSink i)
              rnode = toInternalNode rv
              e = LEdge (Edge (valueUniqueId rv) (instructionUniqueId i)) ()
          in (newNode : rnode : ns, e : es)
        _ -> acc

    -- This is a load of a field of an argument (from a pointer to a
    -- struct).  These are important FieldSinks.  Note that argument
    -- sources are already in the graph so we don't need to make a new
    -- node for the argument.  There is no edge here yet.  Do not
    -- bother tracking non-pointer fields.
    LoadInst { loadAddress = (valueContent' -> InstructionC
      GetElementPtrInst { getElementPtrValue = (valueContent' -> ArgumentC a)})} ->
      case valueType i of
        TypePointer _ _ ->
          let Just apath = accessPath i
              absPath = abstractAccessPath apath
              newNode = LNode (instructionUniqueId i) (FieldSource a absPath)
          in (newNode : ns, es)
        _ -> acc

    LoadInst { } ->
      case valueType i of
        TypePointer _ _ ->
          let newNode = toInternalNode (Value i)
          in (newNode : ns, es)
        _ -> acc

    -- A store to a global generates a sink (the global) and an edge
    -- from the store value to the sink
    StoreInst { storeValue = sv, storeAddress = (valueContent' -> GlobalVariableC _) } ->
      case valueType sv of
        TypePointer _ _ ->
          let newNode = LNode (valueUniqueId i) (EscapeSink i)
              newEdge = LEdge (Edge (valueUniqueId sv) (valueUniqueId i)) ()
          in (newNode : ns, newEdge : es)
        _ -> acc
    StoreInst { storeValue = sv, storeAddress = (valueContent' -> ExternalValueC _) } ->
      case valueType sv of
        TypePointer _ _ ->
          let newNode = LNode (valueUniqueId i) (EscapeSink i)
              newEdge = LEdge (Edge (valueUniqueId sv) (valueUniqueId i)) ()
          in (newNode : ns, newEdge : es)
        _ -> acc
    StoreInst { storeValue = sv, storeAddress = (valueContent' -> ArgumentC _) } ->
      case valueType sv of
        TypePointer _ _ ->
          let newNode = LNode (valueUniqueId i) (EscapeSink i)
              newEdge = LEdge (Edge (valueUniqueId sv) (valueUniqueId i)) ()
          in (newNode : ns, newEdge : es)
        _ -> acc


    -- In this case, we have a store to a field of a global (also an escape)
    StoreInst { storeValue = sv, storeAddress = (valueContent' -> InstructionC
      GetElementPtrInst { getElementPtrValue = (valueContent' -> GlobalVariableC _)})} ->
      case valueType sv of
        TypePointer _ _ ->
          let newNode = LNode (valueUniqueId i) (EscapeSink i)
              newEdge = LEdge (Edge (valueUniqueId sv) (valueUniqueId i)) ()
          in (newNode : ns, newEdge : es)
        _ -> acc
    StoreInst { storeValue = sv, storeAddress = (valueContent' -> InstructionC
      GetElementPtrInst { getElementPtrValue = (valueContent' -> ExternalValueC _)})} ->
      case valueType sv of
        TypePointer _ _ ->
          let newNode = LNode (valueUniqueId i) (EscapeSink i)
              newEdge = LEdge (Edge (valueUniqueId sv) (valueUniqueId i)) ()
          in (newNode : ns, newEdge : es)
        _ -> acc
    StoreInst { storeValue = sv, storeAddress = (valueContent' -> InstructionC
      GetElementPtrInst { getElementPtrValue = (valueContent' -> ArgumentC _)})} ->
      case valueType sv of
        TypePointer _ _ ->
          let newNode = LNode (valueUniqueId i) (EscapeSink i)
              newEdge = LEdge (Edge (valueUniqueId sv) (valueUniqueId i)) ()
          in (newNode : ns, newEdge : es)
        _ -> acc

    -- Another interesting case is if the store address is a GEP whose
    -- base is in the callEscapes map (noted as escaping via function
    -- argument).  If the GEP points to one of the fields that
    -- escapes, this instruction generates a FieldSink node
    --
    -- This case handles all escapes via assignments to fields of
    -- structures that escape via function calls.
    StoreInst { storeValue = sv, storeAddress = (valueContent' -> InstructionC
      GetElementPtrInst { getElementPtrValue = base })} ->
      case valueType sv of
        TypePointer _ _ ->
          case HM.lookup base (callEscapes ^. fieldEscapes) of
            Nothing -> -- Just create an edge because this store into a
                      -- GEP doesn't escape here
              let newEdge = LEdge (Edge (valueUniqueId sv) (valueUniqueId base)) ()
              in (ns, newEdge : es)
            Just paths ->
              let Just cpath = accessPath i
                  absPath = abstractAccessPath cpath
              in case absPath `elem` paths of
                False ->
                  -- This field does *not* escape in a callee, so do
                  -- not add an edge (note, sv could still escape via
                  -- something else).
                  acc
                True ->
                  -- This field being stored to escapes in a callee,
                  -- so the stored value escapes
                  let newNode = LNode (valueUniqueId i) (FieldEscapeSink i)
                      newEdge = LEdge (Edge (valueUniqueId sv) (valueUniqueId i)) ()
                  in (newNode : ns, newEdge : es)
        _ -> acc -- Not a pointer, so it can't escape


    -- Other stores add edges but not sinks.  Other sinks may become
    -- reachable.
    StoreInst { storeValue = sv, storeAddress = sa } ->
      case valueType sv of
        TypePointer _ _ ->
          -- FIXME: This probably needs a node for the address, but we
          -- have to be careful to allow that node to be superceded by
          -- a more specific type of node if we happen to find one.
          -- This will require post-processing at graph creation time
          -- to select the most specific node type with a given ID
          let newEdge = LEdge (Edge (valueUniqueId sv) (valueUniqueId sa)) ()
          in (ns, newEdge : es)
        _ -> acc

    -- FIXME: We could treat PtrToInt casts as escaping, but that
    -- seems overly strict.  Maybe track all int types too?
    --
    -- PtrToIntInst {} -> undefined

    BitcastInst { castedValue = cv } ->
      let cn = toInternalNode cv
          ino = toInternalNode (Value i)
          e = toInternalEdge i cv
      in (cn : ino : ns, e : es)

    -- We have all of the call escape information in @callEscapes@, so
    -- we can more simply just traverse that to make the necessary
    -- edges and nodes.
    --
    -- Note, we use the un-negated ID here to treat call instructions
    -- as sources.  When treating them as escape sinks, negate the ID.
    CallInst {} ->
      let newNode = LNode (valueUniqueId i) (CallSource i)
      in (newNode : ns, es)
    InvokeInst {} ->
      let newNode = LNode (valueUniqueId i) (CallSource i)
      in (newNode : ns, es)

    -- Instructions representing more than one value get their own
    -- node with an edge from each of their possible values.
    SelectInst { selectTrueValue = tv, selectFalseValue = fv } ->
      let tn = toInternalNode tv
          fn = toInternalNode fv
          te = toInternalEdge i tv
          fe = toInternalEdge i fv
      in (tn : fn : ns, te : fe : es)
    PhiNode { phiIncomingValues = ivs } ->
      let newNodes = map toInternalNode (map fst ivs)
          newEdges = map (toInternalEdge i) (map fst ivs)
      in (newNodes ++ ns, newEdges ++ es)

    -- InsertElementInst {} -> undefined
    -- InsertValueInst {} -> undefined
    _ -> acc


toInternalNode :: Value -> EscapeNode
toInternalNode v = LNode (valueUniqueId v) (InternalNode v)

toInternalEdge :: (IsValue a, IsValue b) => a -> b -> EscapeEdge
toInternalEdge i v = LEdge (Edge (valueUniqueId v) (valueUniqueId i)) ()


-- FIXME: It could increase precision to add another parameter
--
-- > (ExternalFunction -> Int -> m [AbstractAccessPath]
--
-- To summarize the field escapes of external functions.  This is
-- unlikely to be particularly useful, since most complicated
-- relationships like that would be mostly restricted to the internals
-- of a library
buildEscapeMaps :: (Monad m) => (ExternalFunction -> Int -> m Bool) -> EscapeResult
                   -> CallEscapes -> Instruction -> m CallEscapes
buildEscapeMaps extSumm summ acc i =
  case i of
    CallInst { callFunction = f, callArguments = args } ->
      collectEscapes extSumm summ i acc f (map fst args)
    InvokeInst { invokeFunction = f, invokeArguments = args } ->
      collectEscapes extSumm summ i acc f (map fst args)
    _ -> return acc

-- | The real worker that determines the escape properties of each
-- actual argument based on what functions might be called by this
-- instruction.
collectEscapes :: (Monad m) => (ExternalFunction -> Int -> m Bool) -> EscapeResult
                  -> Instruction -> CallEscapes -> Value -> [Value] -> m CallEscapes
collectEscapes extSumm summ ci ces callee args =
  case valueContent' callee of
    -- Use the external summary function to check each argument
    ExternalFunctionC ef -> foldM (checkExt ef) ces (zip [0..] args)

    -- Use the internal summary (EscapeResult) to figure out what
    -- arguments are doing in a more granular way (including field
    -- escapes)
    FunctionC f ->
      let formals = functionParameters f
      in return $! foldl' (checkFuncArg summ ci) ces (zip formals args)

    -- This is a call through a function pointer, and all of its
    -- arguments have fptr-escape.
    _ -> return $ foldr (\arg -> fptrEscapes ^!%= HM.insert arg ci) ces args
  where
    checkExt ef acc (ix, arg) = do
      doesEscape <- extSumm ef ix
      case doesEscape of
        False -> return acc
        True -> return $! (valueEscapes ^!%= HM.insert arg ci) acc

-- | Check these in order because there is a superceding relationship
-- here.  General escapes take precedence over field escapes, which in
-- turn take precedence over fptr escapes.
checkFuncArg :: EscapeResult -> Instruction -> CallEscapes -> (Argument, Value) -> CallEscapes
checkFuncArg summ ci ces (formal, arg)
  | not (isPointerValue arg) = ces
  | otherwise =
    case HM.lookup formal (summ ^. escapeArguments) of
      Just _ -> (valueEscapes ^!%= HM.insert arg ci) ces
      Nothing -> case HM.lookup formal (summ ^. escapeFields) of
        Just apsAndWitnesses ->
          let aps = map fst apsAndWitnesses
          in (fieldEscapes ^!%= HM.insertWith (++) arg aps) ces
        Nothing -> case HM.lookup formal (summ ^. fptrEscapeArguments) of
          Just _ -> (fptrEscapes ^!%= HM.insert arg ci) ces
          Nothing -> ces


isPointerValue :: (IsValue a) => a -> Bool
isPointerValue v =
  case valueType v of
    TypePointer _ _ -> True
    _ -> False

safeLab :: String -> EscapeGraph -> Int -> NodeType
safeLab loc g n =
  case lab g n of
    Nothing -> error (loc ++ ": missing label for use graph node")
    Just l -> l

-- Testing

-- | Extract the arguments for each function that escape.  The keys of
-- the map are function names and the set elements are argument names.
-- This format exposes the internal results for testing purposes.
--
-- For actual use in a program, use one of 'functionEscapeArguments',
-- 'functionWillEscapeArguments', or 'instructionEscapes' instead.
escapeResultToTestFormat :: EscapeResult -> Map String (Set String)
escapeResultToTestFormat er =
  foldr fieldTransform directEscapes (HM.toList fm)
  where
    directEscapes = foldr transform mempty (HM.keys m)
    m = (er ^. escapeArguments) `HM.union` (er ^. fptrEscapeArguments)
    fm = (er ^. escapeFields) `HM.union` (er ^. fptrEscapeFields)
    transform a acc =
      let f = argumentFunction a
          fname = show (functionName f)
          aname = show (argumentName a)
      in Map.insertWith' S.union fname (S.singleton aname) acc
    fieldTransform (a, fieldsAndInsts) acc =
      let f = argumentFunction a
          fname = show (functionName f)
          aname = show (argumentName a)
          fields = map fst fieldsAndInsts
          newEntries = S.fromList $ mapMaybe (toFieldRef aname) fields
      in Map.insertWith' S.union fname newEntries acc
    toFieldRef aname fld =
      case abstractAccessPathComponents fld of
        [AccessField ix] -> Just $ printf "%s.<%d>" aname ix
        _ -> Nothing


-- | The same as 'escapeResultToTestFormat', but for the willEscape
-- arguments.
-- willEscapeResultToTestFormat :: EscapeResult -> Map String (Set String)
-- willEscapeResultToTestFormat er = undefined
{-
  foldr transform Map.empty (HM.keys m)
  where
    m = willEscapeArguments er
    transform a acc =
      let f = argumentFunction a
          fname = show (functionName f)
          aname = show (argumentName a)
      in Map.insertWith' S.union fname (S.singleton aname) acc
-}


escapeUseGraphs :: EscapeResult -> [(String, EscapeGraph)]
escapeUseGraphs = map (first (show . functionName)) . HM.toList . (^. escapeGraphs)

useGraphvizParams :: GraphvizParams n NodeType el () NodeType
useGraphvizParams =
  nonClusteredParams { fmtNode = \(_, l) -> [toLabel l]
                     , fmtEdge = \_ -> []
                     }

useGraphvizRepr :: EscapeGraph -> DotGraph Int
useGraphvizRepr g = graphElemsToDot useGraphvizParams ns es
  where
    ns = map toNodeTuple $ labNodes g
    es = map toEdgeTuple $ labEdges g
