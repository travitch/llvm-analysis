{-# LANGUAGE ViewPatterns, DeriveDataTypeable #-}
-- | This is a simple implementation of Andersen's points-to analysis.
--
-- TODO:
--
-- * Add initial edges for struct/array constant initializers
--
-- * Add a PHI node test and a NULL pointer test
--
-- * Handle external functions (w/ hooks)
--
-- * Handle SelectInsts (of pointer type)
--
-- * Be more robust against type/memory-unsafe programs.
--
-- * Variable-length argument list functions
--
-- * Add field sensitivity eventually. See http://delivery.acm.org/10.1145/1300000/1290524/a4-pearce.pdf?ip=128.105.181.27&acc=ACTIVE%20SERVICE&CFID=52054919&CFTOKEN=71981976&__acm__=1320350342_65be4c25a6fba7e32d7b4cd60f13fe97
--
-- * On-the-fly allocator discovery
module LLVM.Analysis.PointsTo.Andersen (
  -- * Types
  Andersen,
  -- * Constructor
  runPointsToAnalysis,
  -- * Debugging aids
  -- savePointsToGraph,
  -- viewPointsToGraph,
  ) where

import Control.Exception
import Control.Monad.State.Strict
import Data.Typeable

import LLVM.Analysis
import LLVM.Analysis.PointsTo

import Constraints.Set.Solver

import Debug.Trace
debug = flip trace

-- A monad to manage fresh variable generation
data ConstraintState = ConstraintState { freshIdSrc :: !Int  }
type ConstraintGen = State ConstraintState

freshVariable :: ConstraintGen SetExp
freshVariable = do
  s <- get
  let vid = freshIdSrc s
      v = Fresh vid
  put $ s { freshIdSrc = vid + 1 }
  return $! setVariable v


data Constructor = Ref
                 | Atom !Value
                 deriving (Eq, Ord, Show, Typeable)
data Var = Fresh !Int
         | LocationSet !Value -- The X_{l_i} variables
         | LoadedLocation !Instruction
         deriving (Eq, Ord, Show, Typeable)

type SetExp = SetExpression Var Constructor
data Andersen = Andersen (SolvedSystem Var Constructor)

instance PointsToAnalysis Andersen where
  mayAlias = andersenMayAlias
  pointsTo = andersenPointsTo

andersenMayAlias :: Andersen -> Value -> Value -> Bool
andersenMayAlias _ _ _ = True

andersenPointsTo :: Andersen -> Value -> [Value]
andersenPointsTo (Andersen ss) v =
  either fromError (map fromLocation) (leastSolution ss var)
  where
    var = LocationSet v
    fromError :: ConstraintError Var Constructor -> [Value]
    fromError = const []
    fromLocation :: SetExp -> Value
    fromLocation (ConstructedTerm Ref _ [ConstructedTerm (Atom val) _ _, _, _]) = val

runPointsToAnalysis :: Module -> Andersen
runPointsToAnalysis m = evalState (pta m) (ConstraintState 0)

-- | Generate constraints and solve the system.  Any system we
-- generate should be solvable if we are generating the correct
-- constraints, so convert solving failures into an IO exception.
pta :: Module -> ConstraintGen Andersen
pta m = do
  initConstraints <- foldM globalInitializerConstraints [] (moduleGlobalVariables m)
  funcConstraints <- foldM functionConstraints [] (moduleDefinedFunctions m)
  let is = initConstraints ++ funcConstraints
      cs = constraintSystem is -- `debug` (unlines $ map show is)
      sol = either throwErr id (solveSystem cs)
  return $! Andersen sol
  where
    loadVar ldInst = setVariable (LoadedLocation ldInst)
    ref = term Ref [Covariant, Covariant, Contravariant]
    loc val =
      let var = case valueContent' val of
            InstructionC i@LoadInst {} -> loadVar i
            _ -> setVariable (LocationSet val)
      in ref [ atom (Atom val), var, var ]

    -- Have to be careful handling phi nodes - those will actually need to
    -- generate many constraints, and the rule for each one can generate a
    -- new set of assignments.
    setExpFor v = case valueContent' v of
      InstructionC i@LoadInst {} -> loadVar i
      _ -> loc v

    -- FIXME This probably needs to use the type of the initializer to
    -- determine if the initializer is a copy of another global or an
    -- address to a specific location
    globalInitializerConstraints acc global =
      case globalVariableInitializer global of
        Nothing -> return acc
        Just (valueContent -> ConstantC _) -> return acc
        Just i -> do
          f1 <- freshVariable
          f2 <- freshVariable
          let c1 = loc (toValue global) <=! ref [ universalSet, universalSet, f1 ]
              c2 = ref [ emptySet, loc i, emptySet ] <=! ref [ universalSet, f2, emptySet ]
              c3 = f2 <=! f1
          return $ c1 : c2 : c3 : acc

    functionConstraints acc = foldM instructionConstraints acc . functionInstructions
    instructionConstraints acc i =
      case i of
        LoadInst { loadAddress = la } -> do
          let c = loc la <=! ref [ universalSet, loadVar i, emptySet ] -- `debug`
                    -- ("Load constraint: " ++ show ((loadVar i) :: SetExpression Var Constructor))
          return $ c : acc
        StoreInst { storeAddress = sa, storeValue = sv } -> do
          f1 <- freshVariable
          f2 <- freshVariable
          let c1 = setExpFor sa <=! ref [ universalSet, universalSet, f1 ]
              c2 = ref [ emptySet, setExpFor sv, emptySet ] <=! ref [ universalSet, f2, emptySet ] -- `debug` show (loc sv)
              c3 = f2 <=! f1
          return $ c1 : c2 : c3 : acc
        _ -> return acc

          -- let c1 = setExpFor sa <=! ref [ universalSet, universalSet, f1 ]
          --     c2 = ref [ emptySet, loc sv, emptySet ] <=! ref [ universalSet, f2, emptySet ] -- `debug` show (loc sv)
          --     c3 = f2 <=! f1


-- Helpers


throwErr :: ConstraintError Var Constructor -> SolvedSystem Var Constructor
throwErr = throw
