module Data.LLVM.Analysis.PointsTo.Andersen ( analysis ) where

import Data.Hashable
import Language.Datalog

import Data.LLVM
import Data.LLVM.Types

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

  -- A unary relation to mark some values (globals, the results of
  -- allocators) as memory locations.
  -- memLoc <- newRelation "memoryLocations" [ values ]

  -- The value noted points to a memory location.  This works well
  -- since all memory operations in LLVM work on pointers.
  pointsToMemLoc <- newRelation "pointsToMemLoc" [ values ]

  -- pointsToEdge(a, b) is a fact that means that everything that a
  -- points to gets an edge to everything b points to.  This is equivalent
  -- to: Store b a
  pointsToEdge <- newRelation "pointsToEdge" [ values, values ]

  -- followEdges(dst, val) is a fact that means that the points-to set
  -- of dst is the set of values that can be pointed to by anything in
  -- the points-to set of val.
  followEdges <- newRelation "followEdges" [ values, values ]

  pointsTo <- newRelation "pointsTo" [ values, values ]

  [ a, b, c, d ] <- mapM newLogicVar [ "a", "b", "c", "d" ]

  extractFacts [ pointsToMemLoc, pointsToEdge, followEdges ] m

  -- Simple case of p = &a
  assertRule pointsTo [ a, b ] [ [rel pointsToMemLoc [ b ]]
                               , [rel pointsToEdge [ a, b ]]
                               ]
  assertRule pointsTo [ a, b ] [ [rel followEdges [ b, c ]]
                               , [rel ] -- need to use an intermediate
                                        -- relation to compute the
                                        -- full set of objects that we
                                        -- need to add edges to
                               ]
  queryDatabase pointsTo [ a, b ]

extractFacts :: [Relation] -> Module -> Datalog LogicValue ()
extractFacts rels m =
  mapM_ (extractFunctionFacts rels) (moduleFunctions m)


functionInstructions :: Value -> [Value]
functionInstructions Value { valueContent = f@Function {} } =
  concatMap blockInstructions (functionBody f)
functionInstructions _ = error "Not a function"


extractFunctionFacts :: [Relation] -> Value -> Datalog LogicValue ()
extractFunctionFacts rels v =
  mapM_ (extractValueFacts rels) (functionInstructions v)

extractValueFacts :: [Relation] -> Value -> Datalog LogicValue ()
extractValueFacts [ directEdge, copyPtr, storeDeref, derefStoreDirect
                  , derefCopyPtr, derefCopyDeref ] v =
  case valueContent v of
    StoreInst _ val dst _ -> undefined
    LoadInst _ val _ -> undefined
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
