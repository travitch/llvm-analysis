module Data.LLVM.Analysis.PointsTo.Andersen (
  -- * Types
  AndersenAnalysis,
  -- * Constructor
  runAndersenAnalysis
  ) where
--  analysis, toLLVMValue ) where

import Data.Hashable
import Data.Set ( Set )
import qualified Data.Set as S
import Language.Datalog

import Data.LLVM
import Data.LLVM.Analysis.PointsTo
import Data.LLVM.Types

-- | The wrapper around the result of an Andersen's analysis.  It is
-- an instance of the 'PointsToAnalysis' typeclass and is intended to
-- be used through that interface.
data AndersenAnalysis = AndersenAnalysis (QueryResult LogicValue)

instance PointsToAnalysis AndersenAnalysis where
  mayAlias = andersenMayAlias
  pointsTo = andersenPointsTo

-- | A wrapper type to shove values of different types into the
-- Datalog engine.  MemLoc serves to mark certain values as pointing
-- to memory locations.  It basically seeds the analysis with initial
-- points-to information.
data LogicValue = LLVMValue !Value
                | MemLoc !Value
                | IndexValue !Int
                deriving (Show, Eq, Ord)

instance Hashable LogicValue where
  hash (LLVMValue v) = hash v
  hash (MemLoc v) = hash v
  hash (IndexValue i) = hash i

-- | Extract an LLVM 'Value' from a LogicValue
toLLVMValue :: LogicValue -> Value
toLLVMValue (LLVMValue v) = v
toLLVMValue (MemLoc v) = v
toLLVMValue _ = error "Non-llvm value, corrupt domains"

-- | The actual definition of Andersen's points-to analysis.  It takes
-- a whole 'Module' from which it extracts facts.
andersen :: Module -> Datalog LogicValue (QueryResult LogicValue)
andersen m = do
  values <- newDomain "values"
  locations <- newDomain "locations"
  index <- newDomain "integer"

  initialRef <- newRelation "initialRef" [ values, locations ]

  storeValToDst <- newRelation "storeValToDst" [ values, values ]
  loadValAtLoc <- newRelation "loadValAtLoc" [ values, values ]
  pointsToR <- newRelation "pointsTo" [ values, locations ]
  memLoc <- newRelation "memLoc" [ locations, locations ]


  [ v1, v2, h1, h2 ] <- mapM newLogicVar [ "v1", "v2", "h1", "h2" ]

  extractFacts (storeValToDst, loadValAtLoc, initialRef) m

  assertRule pointsToR [ v1, h1 ] [ rel initialRef [ v1, h1 ] ]
  assertRule pointsToR [ v2, h2 ] [ rel loadValAtLoc [ v2, v1 ]
                                 , rel pointsToR [ v1, h1 ]
                                 , rel memLoc [ h1, h2 ]
                                 ]
  assertRule memLoc [ h1, h2 ] [ rel storeValToDst [ v2, v1 ]
                               , rel pointsToR [ v1, h1 ]
                               , rel pointsToR [ v2, h2 ]
                               ]
  assertRule pointsToR [ v1, h2 ] [ rel storeValToDst [ v2, v1 ]
                                 , rel pointsToR [ v2, h2 ]
                                 ]


  queryDatabase pointsToR [ v1, h1 ]

-- | Run the points-to analysis and return an object that is an
-- instance of PointsToAnalysis, which can be used to query the
-- results.
runAndersenAnalysis :: Module -> AndersenAnalysis
runAndersenAnalysis m = AndersenAnalysis $ evalDatalog (andersen m)

andersenMayAlias :: AndersenAnalysis -> Value -> Value -> Bool
andersenMayAlias a v1 v2 =
  (andersenPointsTo a v1 `S.intersection` andersenPointsTo a v2) /= S.empty

andersenPointsTo :: AndersenAnalysis -> Value -> Set Value
andersenPointsTo (AndersenAnalysis res) v = foldr buildRes S.empty rawVals
  where
    vres = restrictResults res [ (0, LLVMValue v) ]
    -- ^ Restrict the results to just those we are interested in
    -- (things v points to)
    rawVals = allResults vres
    -- | Due to some quirks (that may be worked out eventually),
    -- values have a phantom points-to edge to themselves.  Just
    -- ignore those when returning results (case 2 below).
    buildRes [ _, target ] s = case v == toLLVMValue target of
      False -> S.insert (toLLVMValue target) s
      True -> s
    buildRes _ _ = error "Invalid points-to relation arity"

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

