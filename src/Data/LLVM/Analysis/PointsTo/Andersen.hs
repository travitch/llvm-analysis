module Data.LLVM.Analysis.PointsTo.Andersen ( analysis, toLLVMValue ) where

import Data.Hashable
import Language.Datalog

import Data.LLVM
import Data.LLVM.Types

import Debug.Trace
debug = flip trace

data LogicValue = LLVMValue !Value
                | MemLoc !Value
                | IndexValue !Int
                deriving (Show, Eq, Ord)

instance Hashable LogicValue where
  hash (LLVMValue v) = hash v
  hash (MemLoc v) = hash v
  hash (IndexValue i) = hash i

toLLVMValue :: LogicValue -> Value
toLLVMValue (LLVMValue v) = v
toLLVMValue (MemLoc v) = v
toLLVMValue _ = error "Non-llvm value, corrupt domains"

analysis :: Module -> Datalog LogicValue (QueryResult LogicValue)
analysis m = do
  values <- newDomain "values"
  locations <- newDomain "locations"
  index <- newDomain "integer"

  initialRef <- newRelation "initialRef" [ values, locations ]

  storeValToDst <- newRelation "storeValToDst" [ values, values ]
  loadValAtLoc <- newRelation "loadValAtLoc" [ values, values ]
  pointsTo <- newRelation "pointsTo" [ values, locations ]
  memLoc <- newRelation "memLoc" [ locations, locations ]


  [ v1, v2, h1, h2 ] <- mapM newLogicVar [ "v1", "v2", "h1", "h2" ]

  extractFacts (storeValToDst, loadValAtLoc, initialRef) m

  assertRule pointsTo [ v1, h1 ] [ rel initialRef [ v1, h1 ] ]
  assertRule pointsTo [ v2, h2 ] [ rel loadValAtLoc [ v2, v1 ]
                                 , rel pointsTo [ v1, h1 ]
                                 , rel memLoc [ h1, h2 ]
                                 ]
  assertRule memLoc [ h1, h2 ] [ rel storeValToDst [ v2, v1 ]
                               , rel pointsTo [ v1, h1 ]
                               , rel pointsTo [ v2, h2 ]
                               ]
  assertRule pointsTo [ v1, h2 ] [ rel storeValToDst [ v2, v1 ]
                                 , rel pointsTo [ v2, h2 ]
                                 ]


  queryDatabase pointsTo [ v1, h1 ]

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
extractValueFacts (storeValToDst, loadValAtLoc, initialRef) v =
  case valueContent v of
    -- Stores must have the value be of a pointer type, otherwise it
    -- is a store of a scalar and not interesting
    StoreInst _ val dst@Value { valueType = TypePointer _ } _ -> do
      assertFact storeValToDst [ LLVMValue val, LLVMValue dst ]
      case valueContent val of
        Argument _ -> assertFact initialRef [ LLVMValue val, MemLoc val ]
        GlobalDeclaration {} -> assertFact initialRef [ LLVMValue val, MemLoc val ]
        Function {} -> assertFact initialRef [ LLVMValue val, MemLoc val ]
        _ -> return ()
    -- Likewise, loads must be of type T** (at least), or it isn't
    -- interesting.
    LoadInst _ loc@Value { valueType = TypePointer _ } _ -> do
      assertFact loadValAtLoc [ LLVMValue v, LLVMValue loc ]
      case valueContent loc of
        Argument _ -> assertFact initialRef [ LLVMValue loc, MemLoc loc ]
        GlobalDeclaration {} -> assertFact initialRef [ LLVMValue loc, MemLoc loc ]
        Function {} -> assertFact initialRef [ LLVMValue loc, MemLoc loc ]
        _ -> return ()
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
