module Data.LLVM.Analysis.PointsTo.Andersen ( analysis, toLLVMValue ) where

import Data.Hashable
import Language.Datalog

import Data.LLVM
import Data.LLVM.Types

import Debug.Trace
debug = flip trace

data LogicValue = LLVMValue !Value
                | IndexValue !Int
                deriving (Show, Eq, Ord)

instance Hashable LogicValue where
  hash (LLVMValue v) = hash v
  hash (IndexValue i) = hash i

toLLVMValue :: LogicValue -> Value
toLLVMValue (LLVMValue v) = v
toLLVMValue _ = error "Non-llvm value, corrupt domains"

analysis :: Module -> Datalog LogicValue (QueryResult LogicValue)
analysis m = do
  values <- newDomain "values"
  index <- newDomain "integer"

  storeValToDst <- newRelation "storeValToDst" [ values, values ]
  loadValAtLoc <- newRelation "loadValAtLoc" [ values, values ]

  -- pointsToEdge(a, b) is a fact that means that everything that a
  -- points to gets an edge to everything b points to.  This is equivalent
  -- to: Store b a
--  pointsToEdge <- newRelation "pointsToEdge" [ values, values ]

  -- followEdges(dst, val) is a fact that means that the points-to set
  -- of dst is the set of values that can be pointed to by anything in
  -- the points-to set of val.
  -- equivSet <- newRelation "equivSet" [ values, values ]

  pointsTo <- newRelation "pointsTo" [ values, values ]

  [ dst, val, loc, a, b, c ] <- mapM newLogicVar [ "dst", "val", "loc", "a", "b", "c" ]

  extractFacts (storeValToDst, loadValAtLoc, undefined) m

  assertRule pointsTo [ a, b ] [ rel storeValToDst [ val, dst ]
                               , rel pointsTo [ dst, a ]
                               , rel pointsTo [ val, b ]
                               ]
  assertRule pointsTo [ a, b ] [ rel loadValAtLoc [ val, loc ]
                               , rel pointsTo [ val, a ]
                               , rel pointsTo [ loc, c ]
                               , rel pointsTo [ c, b ]
                               ]

  -- Simple case of p = &a
  -- assertRule pointsTo [ a, b ] [ rel pointsToEdge [ a, b ] ]
  -- assertRule pointsTo [ a, b ] [ rel equivSet [ a, c ]
  --                              , rel pointsTo [ c, b ]
  --                              ]
  -- assertRule pointsTo [ a, b ] [ rel p
  -- assertRule pointsTo [ a, b ] [ rel pointsTo [ c, d ]
  --                              , rel pointsTo [ c, b ]
  --                              , rel pointsTo [ a, c ]
  --                              ]
  -- assertRule pointsTo [ a, b ] [ rel pointsTo [ b, c ]
  --                              , rel pointsTo [ a, d ]
  --                              , rel pointsTo [ d, c ]
  --                              ]
  -- [ va, vb, vc, vd ] <- mapM newLogicVar [ "va", "vb", "loadVal", "RefdVar" ]
  -- assertRule pointsTo [ va, vb ] [ rel followEdges [ vc, vd ]
  --                              , rel pointsToEdge [ va, vc ]
  --                              , rel pointsTo [ vb, vd ]
  --                              ]
  queryDatabase pointsTo [ a, b ]

extractFacts :: (Relation, Relation, Relation) -> Module -> Datalog LogicValue ()
extractFacts rels m = do
  mapM_ (extractFunctionFacts rels) (moduleFunctions m)



functionInstructions :: Value -> [Value]
functionInstructions Value { valueContent = f@Function {} } =
  concatMap blockInstructions (functionBody f)
functionInstructions _ = error "Not a function"


extractFunctionFacts :: (Relation, Relation, Relation) -> Value -> Datalog LogicValue ()
extractFunctionFacts rels v =
  mapM_ (extractValueFacts rels) (functionInstructions v)

extractValueFacts :: (Relation, Relation, Relation) -> Value -> Datalog LogicValue ()
extractValueFacts (storeValToDst, loadValAtLoc, _) v =
  case valueContent v of
    -- Stores must have the value be of a pointer type, otherwise it
    -- is a store of a scalar and not interesting
    StoreInst _ val dst@Value { valueType = TypePointer _ } _ -> do
      assertFact storeValToDst [ LLVMValue val, LLVMValue dst ]
      -- case valueContent val of
      --   LoadInst _ _ _ -> return ()
      --   _ -> do
      --     assertFact pointsToMemLoc [ LLVMValue val ] `debug` "Asserted a memLoc fact"
    -- Likewise, loads must be of type T** (at least), or it isn't
    -- interesting.
    LoadInst _ loc@Value { valueType = TypePointer _ } _ -> do
      assertFact loadValAtLoc [ LLVMValue v, LLVMValue loc ]
    _ -> return ()

  -- A relation to mark formal parameters as such (so that they can be
  -- matched up to actual arguments during inference).
  --
  -- Function, Index, Formal
  --formalParam <- newRelation "formalParam" [ values, index, values ]

  -- The helper relation to mark actual arguments so that they can be
  -- easily unified with formals.
  --
  -- Function, Index, Actual
  --actualArg <- newRelation "actualArg" [ values, index, values ]

  -- Helper to wire up return edges in the points-to graph.
  --
  -- Function, Value
  --calleeReturn <- newRelation "calleeReturn" [ values, values ]

  -- The other helper to wire up return edges (caller side).
  --
  -- Function, Value (result of call)
  --callerReturn <- newRelation "callerReturn" [ values, values ]

  -- -- Case 1: p = &a
  -- directEdge <- newRelation "directEdge" [ values, values ]

  -- -- Case 2: p = q
  -- copyPtr <- newRelation "copyPointer" [ values, values ]

  -- -- Case 3: p = *r
  -- storeDeref <- newRelation "storeDereference" [ values, values ]

  -- -- Case 4: *p = &a
  -- derefStoreDirect <- newRelation "dereferenceStoreDirect" [ values, values ]

  -- -- Case 5: *p = q
  -- derefCopyPtr <- newRelation "derefereceCopyPointer" [ values, values ]

  -- -- Case 6: *p = *q
  -- derefCopyDeref <- newRelation "dereferenceCopyDereference" [ values, values ]
