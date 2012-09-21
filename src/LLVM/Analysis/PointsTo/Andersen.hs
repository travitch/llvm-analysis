{-# LANGUAGE ViewPatterns, DeriveDataTypeable, CPP #-}
-- | This is a simple implementation of Andersen's points-to analysis.
--
-- TODO:
--
-- * Add field sensitivity eventually. See http://delivery.acm.org/10.1145/1300000/1290524/a4-pearce.pdf?ip=128.105.181.27&acc=ACTIVE%20SERVICE&CFID=52054919&CFTOKEN=71981976&__acm__=1320350342_65be4c25a6fba7e32d7b4cd60f13fe97
module LLVM.Analysis.PointsTo.Andersen (
  -- * Types
  Andersen,
  -- * Constructor
  runPointsToAnalysis,
  -- * Debugging aids (note, subject to change and unstable)
  andersenConstraintGraph
  ) where

import Control.Exception
import Control.Monad.State.Strict
import Data.GraphViz
import Data.Maybe ( mapMaybe )
import Data.Typeable

import LLVM.Analysis
import LLVM.Analysis.PointsTo

import Constraints.Set.Solver

#if defined(DEBUGCONSTRAINTS)
import Debug.Trace
debug :: a -> String -> a
debug = flip trace
#endif

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
         | ArgLocation !Argument
         | VirtualArg !Value !Int
         | RetLocation !Instruction
         deriving (Eq, Ord, Show, Typeable)

type SetExp = SetExpression Var Constructor
data Andersen = Andersen !(SolvedSystem Var Constructor)

instance PointsToAnalysis Andersen where
  mayAlias = andersenMayAlias
  pointsTo = andersenPointsTo

andersenMayAlias :: Andersen -> Value -> Value -> Bool
andersenMayAlias _ _ _ = True

andersenPointsTo :: Andersen -> Value -> [Value]
andersenPointsTo (Andersen ss) v =
  either fromError (map fromLocation) (leastSolution ss var)
  where
    var = case valueContent' v of
      ArgumentC a -> ArgLocation a
      InstructionC i@CallInst {} -> RetLocation i
      _ -> LocationSet v
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
      cs = constraintSystem is
      sol = either throwErr id (solveSystem cs)
  return $! Andersen sol
  where
    loadVar ldInst = setVariable (LoadedLocation ldInst)
    argVar a = setVariable (ArgLocation a)
    virtArgVar sa ix = setVariable (VirtualArg sa ix)
    returnVar i = setVariable (RetLocation i)
    ref = term Ref [Covariant, Covariant, Contravariant]
    virtArgLoc sa ix =
      let var = virtArgVar sa ix
      in ref [ atom (Atom sa), var, var ]
    loc val =
      let var = case valueContent' val of
            InstructionC i@LoadInst {} -> loadVar i
            InstructionC i@CallInst {} -> returnVar i
            ArgumentC a -> argVar a
            _ -> setVariable (LocationSet val)
      in ref [ atom (Atom val), var, var ]

    -- Have to be careful handling phi nodes - those will actually need to
    -- generate many constraints, and the rule for each one can generate a
    -- new set of assignments.
    setExpFor v = case valueContent' v of
      InstructionC i@LoadInst {} -> loadVar i
      InstructionC i@CallInst {} -> returnVar i
      ArgumentC a -> argVar a
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
          let c = setExpFor la <=! ref [ universalSet, loadVar i, emptySet ]
          return $ c : acc
#if defined(DEBUGCONSTRAINTS)
            `debug` ("Inst: " ++ show i ++ "\n" ++ show c ++ "\n")
#endif

        -- If you store the stored address is a function type, add
        -- inclusion edges between the virtual arguments.  If sv is a
        -- Function, add virtual args linked to formals.
        StoreInst { storeAddress = sa, storeValue = sv } -> do
          f1 <- freshVariable
          f2 <- freshVariable
          let c1 = setExpFor sa <=! ref [ universalSet, universalSet, f1 ]
              c2 = ref [ emptySet, setExpFor sv, emptySet ] <=! ref [ universalSet, f2, emptySet ]
              c3 = f2 <=! f1
          acc' <- addVirtualArgConstraints acc sa sv
          return $ c1 : c2 : c3 : acc'
#if defined(DEBUGCONSTRAINTS)
            `debug` ("Inst: " ++ show i ++ "\n" ++ (unlines $ map show [c1,c2,c3]))
#endif
        CallInst { callFunction = (valueContent' -> FunctionC f)
                 , callArguments = args
                 } -> directCallConstraints acc i f (map fst args)
        InvokeInst { invokeFunction = (valueContent' -> FunctionC f)
                   , invokeArguments = args
                   } -> directCallConstraints acc i f (map fst args)
        CallInst { callFunction = (valueContent' -> InstructionC LoadInst { loadAddress = la })
                 , callArguments = args
                 } -> indirectCallConstraints acc i la (map fst args)
        _ -> return acc

    directCallConstraints acc i f actuals = do
      let formals = functionParameters f
      acc' <- foldM copyActualsToFormals acc (zip actuals formals)
      case valueType i of
        TypePointer _ _ ->
          let rvs = mapMaybe extractRetVal (functionExitInstructions f)
              cs = foldr (retConstraint i) [] rvs
          in return $ cs ++ acc'
        _ -> return acc'

    addVirtualArgConstraints acc sa sv
      | not (isFuncPtrType (valueType sv)) = return acc
      | otherwise =
        case valueContent' sv of
          -- Connect the virtuals for sa to the actuals of f
          FunctionC f -> do
            let formals = functionParameters f
            foldM (constrainVirtualArg sa) acc (zip [0..] formals)
          _ -> return acc

    constrainVirtualArg sa acc (ix, frml) = do
      let c = virtArgVar sa ix <=! argVar frml
      return $ c : acc

    -- The idea here will be that we equate the actuals with virtual
    -- nodes for this function pointer.  For function pointer will
    -- always be a load node so we can treat it somewhat uniformly.
    -- This may get complicated for loads of fields, but we should be
    -- able to take care of that outside of this rule.  Globals and
    -- locals are easy.
    indirectCallConstraints acc i la actuals = do
      let addIndirectConstraint (ix, act) a =
            (setExpFor act <=! virtArgVar la ix) : a
      let acc' = foldr addIndirectConstraint acc (zip [0..] actuals)
      return acc'

    retConstraint i rv acc =
      let c = setExpFor rv <=! setExpFor (toValue i)
      in c : acc
#if defined(DEBUGCONSTRAINTS)
             `debug` ("RetVal " ++ show i ++ "\n" ++ (unlines $ map show [c]))
#endif

    -- Note the rule has to be a bit strange because the formal is an
    -- r-value (and not an l-value like everything else).  We can
    -- actually do the really simple thing from other formulations
    -- here because of this.
    copyActualsToFormals acc (act, frml) = do
      let c = setExpFor act <=! argVar frml
      return $ c : acc
#if defined(DEBUGCONSTRAINTS)
        `debug` ("Args " ++ show act ++ " -> " ++ show frml ++ "\n" ++ (unlines $ map show [c]))
#endif

-- Helpers

isFuncPtrType :: Type -> Bool
isFuncPtrType t =
  case t of
    TypeFunction _ _ _ -> True
    TypePointer t' _ -> isFuncPtrType t'
    _ -> False

extractRetVal :: Instruction -> Maybe Value
extractRetVal RetInst { retInstValue = rv } = rv
extractRetVal _ = Nothing

throwErr :: ConstraintError Var Constructor -> SolvedSystem Var Constructor
throwErr = throw

-- Debugging

andersenConstraintGraph :: Andersen -> DotGraph Int
andersenConstraintGraph (Andersen s) =
  let (ns, es) = solvedSystemGraphElems s
  in graphElemsToDot andersenParams ns es

andersenParams :: GraphvizParams Int (SetExpression Var Constructor) ConstraintEdge () (SetExpression Var Constructor)
andersenParams = defaultParams { isDirected = True
                               , fmtNode = fmtAndersenNode
                               , fmtEdge = fmtAndersenEdge
                               }

fmtAndersenNode :: (a, SetExpression Var Constructor) -> [Attribute]
fmtAndersenNode (_, l) =
  case l of
    EmptySet -> [toLabel (show l)]
    UniversalSet -> [toLabel (show l)]
    SetVariable (Fresh i) -> [toLabel ("F" ++ show i)]
    SetVariable (VirtualArg sa ix) ->
      [toLabel ("VA_" ++ show ix ++ "[" ++ show (valueName sa) ++ "]")]
    SetVariable (LocationSet v) ->
      case valueName v of
        Nothing -> [toLabel ("X_" ++ show v)]
        Just vn -> [toLabel ("X_" ++ identifierAsString vn)]
    SetVariable (ArgLocation a) ->
      [toLabel ("AL_" ++ show (argumentName a))]
    SetVariable (RetLocation i) ->
      [toLabel ("RV_" ++ show (valueName (callFunction i)))]
    SetVariable (LoadedLocation i) ->
      case valueName i of
        Nothing -> error "Loads should have names"
        Just ln -> [toLabel ("LL_" ++ identifierAsString ln)]
    ConstructedTerm Ref _ [ConstructedTerm (Atom v) _ _, _, _] ->
      let vn = maybe (show v) identifierAsString (valueName v)
      in [toLabel $ concat [ "Ref( l_", vn, ", X_", vn, ", X_", vn ]]
    ConstructedTerm (Atom a) _ _ ->
      [toLabel (show a)]
    ConstructedTerm _ _ _ -> [toLabel (show l)]

fmtAndersenEdge :: (a, a, ConstraintEdge) -> [Attribute]
fmtAndersenEdge (_, _, lbl) =
  case lbl of
    Succ -> [style solid]
    Pred -> [style dashed]