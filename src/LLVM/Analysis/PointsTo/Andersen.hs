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
import Data.Maybe ( fromMaybe, mapMaybe )
import Data.Typeable

import LLVM.Analysis
import LLVM.Analysis.PointsTo

import Constraints.Set.Solver
import Constraints.Set.Internal

#if defined(DEBUGCONSTRAINTS)
import Debug.Trace
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
         | VirtualFieldArg !Type !Int !Int
         | RetLocation !Instruction
         | GEPLocation !Value
         | PhiCopy !Instruction
         | FieldLoc !Type !Int
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
    fromLocation se = error ("Unexpected set expression in result " ++ show se)

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
    phiVar i = setVariable (PhiCopy i)
    gepVar v = setVariable (GEPLocation v)
    -- If the location the function pointer is being stored to is a
    -- struct field, we need a special virtual argument that
    -- references the struct field instead of the value (because
    -- struct fields are treated differently from other values - every
    -- instance of a struct field maps to the same struct field slot
    -- indexed by type/position).
    virtArgVar sa ix =
      case valueContent' sa of
        InstructionC GetElementPtrInst { getElementPtrValue = base
                                       , getElementPtrIndices = ixs
                                       } ->
          case fieldDescriptor base ixs of
            Just (t, fldno) -> setVariable (VirtualFieldArg t fldno ix)
            Nothing -> setVariable (VirtualArg sa ix)
        _ -> setVariable (VirtualArg sa ix)
    returnVar i = setVariable (RetLocation i)
    ref = term Ref [Covariant, Covariant, Contravariant]
    loc val =
      let svar = fromMaybe (setVariable (LocationSet val)) (setVarFor val)
      in ref [ atom (Atom val), svar, svar ]

    -- FIXME Test taking and storing the address of a field (and then
    -- using it)
    --
    -- Also test embedded structs
    --
    -- Test funcptrs stored in an array (all should match on call)

    setVarFor v =
      case valueContent' v of
        InstructionC i@LoadInst {} -> return $ loadVar i
        InstructionC i@CallInst {} -> return $ returnVar i
        InstructionC i@InvokeInst {} -> return $ returnVar i
        InstructionC i@PhiNode {} -> return $ phiVar i
        InstructionC i@SelectInst {} -> return $ phiVar i
        InstructionC GetElementPtrInst { getElementPtrValue = base } ->
          return $ gepVar (getTargetIfLoad base)
        ArgumentC a -> return $ argVar a
        _ -> Nothing

    -- Have to be careful handling phi nodes - those will actually need to
    -- generate many constraints, and the rule for each one can generate a
    -- new set of assignments.
    setExpFor v =
      case valueContent' v of
        InstructionC GetElementPtrInst { getElementPtrValue = base
                                       , getElementPtrIndices = ixs
                                       } ->
          case fieldDescriptor base ixs of
            -- If we couldn't compute a field descriptor, this is an
            -- array.
            Nothing -> gepVar (getTargetIfLoad base)
            -- If this is a field access, treat it as the location for
            -- all slots of the given index in that type (one slot for
            -- all instances).
            Just (t, ix) ->
              let var = setVariable (FieldLoc t ix)
              in ref [ atom (Atom v), var, var ]
        -- This case is a bit of a hack to deal with the conversion from
        -- an array type to a pointer to the first element (using a
        -- constant GEP with all zero indices).
        ConstantC ConstantValue { constantInstruction = (valueContent' ->
          InstructionC GetElementPtrInst { getElementPtrValue = base
                                         , getElementPtrIndices = is
                                         })} ->
          case valueType base of
            TypePointer (TypeArray _ _) _ ->
              case all isConstantZero is of
                True -> setVariable (LocationSet base)
                False -> loc v
            _ -> loc v
        _ -> fromMaybe (loc v) (setVarFor v)

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
          -- If we load a function pointer, add new virtual nodes and
          -- link them up
          let c = setExpFor la <=! ref [ universalSet, loadVar i, emptySet ]
          acc' <- addVirtualConstraints acc (toValue i) la
          return $ c : acc' `traceConstraints` ("Inst: " ++ show i, [c])

        -- If you store the stored address is a function type, add
        -- inclusion edges between the virtual arguments.  If sv is a
        -- Function, add virtual args linked to formals.
        StoreInst { storeAddress = sa, storeValue = sv } -> do
          f1 <- freshVariable
          f2 <- freshVariable
          let c1 = setExpFor sa <=! ref [ universalSet, universalSet, f1 ]
              c2 = ref [ emptySet, setExpFor sv, emptySet ] <=! ref [ universalSet, f2, emptySet ]
              c3 = f2 <=! f1
          acc' <- addVirtualConstraints acc sa sv
          return $ c1 : c2 : c3 : acc' `traceConstraints` ("Inst: " ++ show i, [c1,c2,c3])

        CallInst { callFunction = (valueContent' -> FunctionC f)
                 , callArguments = args
                 } -> directCallConstraints acc i f (map fst args)
        InvokeInst { invokeFunction = (valueContent' -> FunctionC f)
                   , invokeArguments = args
                   } -> directCallConstraints acc i f (map fst args)
        -- For now, don't model calls to external functions
        CallInst { callFunction = (valueContent' -> ExternalFunctionC _) } -> return acc
        InvokeInst { invokeFunction = (valueContent' -> ExternalFunctionC _) } -> return acc
        CallInst { callFunction = callee, callArguments = args } ->
          indirectCallConstraints acc callee (map fst args)
        InvokeInst { invokeFunction = callee, invokeArguments = args } ->
          indirectCallConstraints acc callee (map fst args)

        SelectInst { selectTrueValue = tv, selectFalseValue = fv } ->
          foldM (valueAliasingChoise i) acc [ tv, fv ]
        PhiNode { phiIncomingValues = ivs } ->
          foldM (valueAliasingChoise i) acc (map fst ivs)

        -- FIXME: Add a case handling bitcasts.  If one type is
        -- bitcast to a related type, add equivalences between all of
        -- their respective fields.  Relation is by structural
        -- subtyping.

        -- Array rule.  Equate the base of the GEP and the GEP,
        -- effectively treating every array element as one location.
        -- This particular rule deals with pointers that are treated
        -- as arrays (the GEP has only one index).
        --
        -- Note that this rule looks into the base of the GEP deeply
        -- and is not totally local.  This seemed necessary to hook
        -- the constraints into the proper place in the constraint
        -- graph.  It may be possible to keep it entirely local with
        -- extra variables as is done for function pointers.
        GetElementPtrInst { getElementPtrValue = (valueContent' ->
          InstructionC LoadInst { loadAddress = la })
                          , getElementPtrIndices = [_]
                          } -> do
          f1 <- freshVariable
          f2 <- freshVariable

          let c1 = loc (toValue i) <=! ref [ universalSet, universalSet, f1 ]
              c2 = ref [ emptySet, setExpFor la, emptySet ] <=! ref [ universalSet, f2, emptySet ]
              c3 = f2 <=! f1

          acc' <- addVirtualConstraints acc (toValue i) la
          return $ c1 : c2 : c3 : acc' `traceConstraints` (concat ["GEP: " ++ show i], [c1,c2,c3])

        GetElementPtrInst { getElementPtrValue = base
                          , getElementPtrIndices = [_]
                          } -> do
          f1 <- freshVariable
          f2 <- freshVariable

          let c1 = loc (toValue i) <=! ref [ universalSet, universalSet, f1 ]
              c2 = ref [ emptySet, loc base, emptySet ] <=! ref [ universalSet, f2, emptySet ]
              c3 = f2 <=! f1

          acc' <- addVirtualConstraints acc (toValue i) base
          return $ c1 : c2 : c3 : acc' `traceConstraints` (concat ["GEP: " ++ show i], [c1,c2,c3])

        GetElementPtrInst { getElementPtrValue = base,
                            getElementPtrIndices = [(valueContent -> ConstantC ConstantInt { constantIntValue = 0 })
                                                   , _
                                                   ]
                          } ->
          case valueType base of
            TypePointer (TypeArray _ _) _ -> do
              f1 <- freshVariable
              f2 <- freshVariable

              let c1 = loc (toValue i) <=! ref [ universalSet, universalSet, f1 ]
                  c2 = ref [ emptySet, setExpFor base, emptySet ] <=! ref [ universalSet, f2, emptySet ]
                  c3 = f2 <=! f1

              acc' <- addVirtualConstraints acc (toValue i) base
              return $ c1 : c2 : c3 : acc' `traceConstraints` (concat ["GEP: " ++ show i], [c1,c2,c3])

            -- This case is actually a struct field reference, so fill
            -- that in later
            _ -> return acc

        _ -> return acc

    directCallConstraints acc i f actuals = do
      let formals = functionParameters f
      acc' <- foldM copyActualToFormal acc (zip actuals formals)
      case valueType i of
        TypePointer _ _ -> do
          let rvs = mapMaybe extractRetVal (functionExitInstructions f)
          cs <- foldM (retConstraint i) [] rvs
          return $ cs ++ acc'
        _ -> return acc'

    -- FIXME try adding virtual array constraints that are propagated
    -- everywhere; the base one should probably refer to the thing
    -- that is an array.  This might let us avoid extra cases and
    -- treat GEP instructions individually again.  If it works, it should
    -- also fix test case store-ptr-to-arg-array.c

    addVirtualConstraints acc0 dst src = do
      acc1 <- addVirtualArgConstraints acc0 dst src
--      acc2 <- addArrayConstraints acc1 dst src
      return acc1

    -- addArrayConstraints acc dst src = do
    --   let c = arrayVar dst <=! arrayVar src
    --   return $ c : acc `traceConstraints` (concat ["ArrayVar: ", show src, " -> ", show dst], [c])

    addVirtualArgConstraints acc sa sv
      | not (isFuncPtrType (valueType sv)) = return acc
      | otherwise =
        case valueContent' sv of
          -- Connect the virtuals for sa to the actuals of f
          FunctionC f -> do
            let formals = functionParameters f
            foldM (constrainVirtualArg sa) acc (zip [0..] formals)
          -- Otherwise, copy virtuals from old ref to new ref
          _ -> do
            let nparams = functionTypeParameters (valueType sv)
            foldM (virtVirtArg sa sv) acc [0..(nparams - 1)]

    virtVirtArg sa sv acc ix = do
      let c1 = virtArgVar sa ix <=! virtArgVar sv ix
          c2 = virtArgVar sv ix <=! virtArgVar sa ix
      return $ c1 : c2 : acc `traceConstraints` (concat ["VirtVirt: ", show ix, "(", show sa, " -> ", show sv, ")"], [c1, c2])

    constrainVirtualArg sa acc (ix, frml) = do
      let c = virtArgVar sa ix <=! argVar frml
      return $ c : acc `traceConstraints` (concat ["VirtArg: ", show ix, "(", show sa, ")"], [c])


    -- The idea here will be that we equate the actuals with virtual
    -- nodes for this function pointer.  For function pointer will
    -- always be a load node so we can treat it somewhat uniformly.
    -- This may get complicated for loads of fields, but we should be
    -- able to take care of that outside of this rule.  Globals and
    -- locals are easy.
    indirectCallConstraints acc callee actuals = do
      let addIndirectConstraint (ix, act) a =
            let c = setExpFor act <=! virtArgVar callee ix
            in c : a `traceConstraints` (concat ["IndirectCall ", show ix, "(", show act, ")" ], [c])
          acc' = foldr addIndirectConstraint acc (zip [0..] actuals)
      return acc'

    -- Set up constraints to propagate return values to caller
    -- contexts (including function argument virtuals for function
    -- pointer types).
    retConstraint i acc rv = do
      let c = setExpFor rv <=! setExpFor (toValue i)
      acc' <- addVirtualConstraints acc (toValue i) rv
      return $ c : acc' `traceConstraints` (concat [ "RetVal ", show i ], [c])

    -- Note the rule has to be a bit strange because the formal is an
    -- r-value (and not an l-value like everything else).  We can
    -- actually do the really simple thing from other formulations
    -- here because of this.
    --
    -- If the actual is a function (pointer) type, also add new
    -- virtual arg nodes for the formal
    copyActualToFormal acc (act, frml) = do
      let c = setExpFor act <=! argVar frml
      acc' <- addVirtualConstraints acc (toValue frml) act
      return $ c : acc' `traceConstraints` (concat [ "Args ", show act, " -> ", show frml ], [c])

    valueAliasingChoise i acc vfrom = do
      let c = setExpFor vfrom <=! setExpFor (toValue i)
      acc' <- addVirtualConstraints acc (toValue i) vfrom
      return $ c : acc' `traceConstraints` (concat [ "MultCopy ", show (valueName vfrom), " -> ", show (valueName i)], [c])

-- | Return the innermost type and the index into that type accessed
-- by the GEP instruction with the given base and indices.
fieldDescriptor :: Value -> [Value] -> Maybe (Type, Int)
fieldDescriptor base ixs =
  case (valueType base, ixs) of
    -- A pointer being accessed as an array
    (_, [_]) -> Nothing
    -- An actual array type (first index should be zero here)
    (TypePointer (TypeArray _ _) _, (valueContent' -> ConstantC ConstantInt { constantIntValue = 0 }):_) ->
      Nothing
    -- It doesn't matter what the first index is; even if it isn't
    -- zero (as in it *is* an array access), we only care about the
    -- ultimate field access and not the array.  Raw arrays are taken
    -- care of above.
    (TypePointer t _, _:rest) -> return $ walkType t rest
    _ -> Nothing

walkType :: Type -> [Value] -> (Type, Int)
walkType t [] = error ("LLVM.Analysis.PointsTo.Andersen.walkType: expected non-empty index list for " ++ show t)
walkType t [(valueContent -> ConstantC ConstantInt { constantIntValue = iv })] =
  (t, fromIntegral iv)
walkType t (ix:ixs) =
  case t of
    -- We can ignore inner array indices since we only care about the
    -- terminal struct index.  Note that if there are no further
    -- struct types (e.g., this is an array member of a struct), we
    -- need to return the index of the array... FIXME
    TypeArray _ t' -> walkType t' ixs
    TypeStruct _ ts _ ->
      case valueContent ix of
        ConstantC ConstantInt { constantIntValue = (fromIntegral -> iv) } ->
          case iv < length ts of
            True -> walkType (ts !! iv) ixs
            False -> error ("LLVM.Analysis.PointsTo.Andersen.walkType: index out of range " ++ show iv ++ " in " ++ show t)
        _ -> error ("LLVM.Analysis.PointsTo.Andersen.walkType: non-constant index " ++ show ix ++ " in " ++ show t)
    _ -> error ("LLVM.Analysis.PointsTo.Andersen.walkType: unexpected type " ++ show ix ++ " in " ++ show t)

isConstantZero :: Value -> Bool
isConstantZero v =
  case valueContent' v of
    ConstantC ConstantInt { constantIntValue = 0 } -> True
    _ -> False

getTargetIfLoad :: Value -> Value
getTargetIfLoad v =
  case valueContent' v of
    InstructionC LoadInst { loadAddress = la } -> la
    _ -> v

-- TODO:
--
-- * extra function pointer indirections

-- Helpers

{-# INLINE traceConstraints #-}
-- | This is a debugging helper to trace the constraints that are
-- generated.  When debugging is disabled via cabal, it is a no-op.
traceConstraints :: a -> (String, [Inclusion Var Constructor]) -> a
#if defined(DEBUGCONSTRAINTS)
traceConstraints a (msg, cs) = trace (msg ++ "\n" ++ (unlines $ map ((" "++) . show) cs)) a
#else
traceConstraints = const
#endif

isFuncPtrType :: Type -> Bool
isFuncPtrType t =
  case t of
    TypeFunction _ _ _ -> True
    TypePointer t' _ -> isFuncPtrType t'
    _ -> False

functionTypeParameters :: Type -> Int
functionTypeParameters t =
  case t of
    TypeFunction _ ts _ -> length ts
    TypePointer t' _ -> functionTypeParameters t'
    _ -> -1

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
    SetVariable (FieldLoc t ix) ->
      [toLabel ("Field_" ++ show t ++ "<" ++ show ix ++ ">")]
    SetVariable (Fresh i) -> [toLabel ("F" ++ show i)]
    SetVariable (PhiCopy i) -> [toLabel ("PhiCopy " ++ show i)]
    SetVariable (GEPLocation i) -> [toLabel ("GEPLoc " ++ show i)]
    SetVariable (VirtualArg sa ix) ->
      [toLabel ("VA_" ++ show ix ++ "[" ++ show (valueName sa) ++ "]")]
    SetVariable (VirtualFieldArg t fld ix) ->
      [toLabel ("VAField_" ++ show ix ++ "[" ++ show t ++ ".<" ++ show fld ++ ">]")]
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