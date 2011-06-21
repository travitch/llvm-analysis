module Data.LLVM.Analysis.PointsTo.Andersen (
  -- * Types
  AndersenAnalysis,
  -- * Constructor
  runPointsToAnalysis
  ) where

import Data.Hashable
import Data.List ( sort )
import Data.Maybe ( fromJust )
import Data.Set ( Set )
import qualified Data.Set as S
import Language.Datalog

import Data.LLVM.Analysis.PointsTo
import Data.LLVM.Types

-- | The wrapper around the result of an Andersen's analysis.  It is
-- an instance of the 'PointsToAnalysis' typeclass and is intended to
-- be used through that interface.
data AndersenAnalysis = AndersenAnalysis (QueryResult LogicValue)

instance PointsToAnalysis AndersenAnalysis where
  mayAlias = andersenMayAlias
  pointsTo = andersenPointsTo

instance Show AndersenAnalysis where
  show = showAllResults

-- | A wrapper type to shove values of different types into the
-- Datalog engine.  MemLoc serves to mark certain values as pointing
-- to memory locations.  It basically seeds the analysis with initial
-- points-to information.
data LogicValue = LLVMValue !Value
                | MemLoc !Value
                | DefaultField
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
  fields <- newDomain "fields"
  index <- newDomain "integer"

  -- Facts
  initialRef <- newRelation "initialRef" [ values, locations ]
  storeValToDst <- newRelation "storeValToDst" [ values, values ]
  loadValFromLoc <- newRelation "loadValFromLoc" [ values, values ]

  varPointsTo <- newRelation "varPointsTo" [ values, locations ]
  fieldPointsTo <- newRelation "fieldPointsTo" [ locations, locations ]
--  memLoc <- newRelation "memLoc" [ locations, locations ]
  pointsToR <- newRelation "pointsToR" [ values, locations ]


  [ val, dst, loc ] <- mapM newLogicVar [ "val", "dst", "loc" ]
  [ v1, v2, h1, h2 ] <- mapM newLogicVar [ "v1", "v2", "h1", "h2" ]

  extractFacts (storeValToDst, loadValFromLoc, initialRef) m



  assertRule varPointsTo [ v1, h1 ] [ rel initialRef [ v1, h1 ] ]

  assertRule fieldPointsTo [ h1, h2 ] [ rel storeValToDst [ val, dst ]
                                      , rel varPointsTo [ val, h2 ]
                                      , rel varPointsTo [ dst, h1 ]
                                      ]

  assertRule varPointsTo [ val, h1 ] [ rel loadValFromLoc [ val, loc ]
                                    , rel varPointsTo [ loc, h2 ]
                                    , rel fieldPointsTo [ h2, h1 ]
                                    ]




  assertRule pointsToR [ v1, h1 ] [ rel varPointsTo [ v1, h1 ] ]
  assertRule pointsToR [ v1, h1 ] [ rel initialRef [ v1, h2 ]
                                  , rel fieldPointsTo [ h2, h1 ]
                                  ]

    -- rel storeValToDst [ val, dst ]
    --                               , rel pointsTo [ val, h1 ]
    --                               , rel pointsTo [ dst, h2 ]
    --                               ]

  -- FIXME: This triggers a bug in datalog
  -- assertRule pointsToR [ v1, h1 ] [ rel storeValToDst [ v2, v1 ]
  --                                 , rel pointsToR [ v2, h1 ]
  --                                 , negRel initialRef [ v2, h1 ]
  --                                 ]

  queryDatabase pointsToR {-fieldPointsTo-} [ v1, h1 ]

-- | Run the points-to analysis and return an object that is an
-- instance of PointsToAnalysis, which can be used to query the
-- results.
runPointsToAnalysis :: Module -> AndersenAnalysis
runPointsToAnalysis m = AndersenAnalysis $ evalDatalog (andersen m)

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
    buildRes [ _, target ] s = S.insert (toLLVMValue target) s
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
extractValueFacts (storeValToDst, loadValFromLoc, initialRef) v =
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
      assertFact loadValFromLoc [ LLVMValue v, LLVMValue loc ]
      case valueContent loc of
        Argument _ -> assertFact initialRef [ LLVMValue loc, MemLoc loc ]
        GlobalDeclaration {} -> assertFact initialRef [ LLVMValue loc, MemLoc loc ]
        Function {} -> assertFact initialRef [ LLVMValue loc, MemLoc loc ]
        _ -> return ()
    AllocaInst _ _ _ -> do
      assertFact initialRef [ LLVMValue v, MemLoc v ]
    _ -> return ()

showAllResults :: AndersenAnalysis -> String
showAllResults (AndersenAnalysis res) =
  unlines $ map toS results
  where
    results = sort $ allResults res
    unval = show . fromJust . valueName . toLLVMValue
    toS [ v1, v2 ] = concat [unval v1, " -> ", unval v2 ]