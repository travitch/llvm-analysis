{-# LANGUAGE MultiParamTypeClasses #-}
-- | An interprocedural may-be-null pointer analysis.
module Main ( main ) where

import Data.List ( elemIndex, foldl' )
import Data.Maybe ( fromJust )
import qualified Data.Set as S

import Test.HUnit

import Data.LLVM
import Data.LLVM.ICFG
import Data.LLVM.Analysis.IFDS
import Data.LLVM.Analysis.PointsTo.TrivialFunction

import Data.LLVM.ParseBitcode
import Data.LLVM.Testing

import Text.Printf
import Debug.Trace

debug = flip trace

data INullPtr = INullPtr

instance IFDSAnalysis INullPtr Value where
  flow = inullFlow
  callFlow = inullCallFlow
  passArgs = inullPassArgs
  externPassArgs = inullExternPassArgs
  returnVal = inullReturnVal
  externReturnVal = inullExternReturnVal
  entrySetup = inullEntrySetup

-- Strategy: add entry nodes (id: negation of the Function id)?  Entry
-- points need to be treated specially so that their initial parameter
-- states can be set up.  Alternative to entry nodes: just have a
-- special "entryFlow" function that is called explicitly on entry
-- points.

inullFlow :: INullPtr -> Maybe Value -> Instruction -> [CFGEdge] -> [Maybe Value]
inullFlow _ Nothing i@AllocaInst {} _
  -- Allocas are always pointers - we need a T** to have a
  -- stack-allocated pointer.
  | isPointerPointerType (Value i) = [Nothing, Just $ Value i]
  | otherwise = [Nothing]
-- FIXME: Need to handle edges here to see when we are on a non-null
-- branch
{-
inullFlow _ v@(Just v') i@LoadInst { loadAddress = la } edges =
  -- Everything in LLVM is a pointer; the first "level" of load
  -- doesn't tell us anything since that is just LLVM loading the
  -- address in the pointer.  Subsequent loads (nested) represent
  -- actual pointer dereferences.
  case valueContent la of
    InstructionC LoadInst { loadAddress = la2 } ->
      case (Value la2) == v' of
        True -> [] `debug` printf "Dropping %s" (show v')
        False -> [v]
    _ -> [v] `debug` printf "Load (%s) pass through %s" (show i) (show v')
-}
-- In this case, nothing is NULL here but we might be NULLing out the
-- address being stored to.
inullFlow _ Nothing i@StoreInst { storeAddress = sa, storeValue = sv } edges =
  case valueContent sv of
    ConstantC (ConstantPointerNull {}) -> [Just sa]
    _ -> [Nothing]
inullFlow _ v@(Just v') StoreInst { storeAddress = sa, storeValue = sv } edges
  | sa == v' = case valueContent sv of
    ConstantC (ConstantPointerNull {}) -> [Just sa]
    _ -> [] -- assigning something that isn't NULL, so the value is no
           -- longer null.  The case where the assigned value may be
           -- null is handled in the sv == v' alternative.
  | sv == v' = [Just sa]
  | otherwise = case valueContent sv of
    ConstantC (ConstantPointerNull {}) -> [Just sa]
    InstructionC (LoadInst { loadAddress = la }) ->
      case la == v' of
        False -> [v]
        True -> [v, Just sa]
    _ -> [v]
inullFlow _ v@(Just _) _ _ = [v]
inullFlow _ Nothing _ _ = [Nothing]

-- | Only propagate information about locals across call->return edges
inullCallFlow :: INullPtr -> Maybe Value -> Instruction -> [CFGEdge] -> [Maybe Value]
inullCallFlow _ Nothing _ _ = [Nothing]
inullCallFlow _ v@(Just v') _ _ =
  case isGlobal v' of
    True -> []
    _ -> [v]
  -- case valueContent v' of
  --   InstructionC (AllocaInst {}) -> [v]
  --   _ -> []

{- Important note: Only arguments, constantpointernull, and loads can
be NULL -}

-- | If the Value is an argument to the Call (or Invoke) instruction
-- _AND_ it is a pointer type (just one level of pointer, T*), put the
-- corresponding formal parameter of the Function into the set instead
-- of the input value.
--
-- Also, just pass through information about globals.
inullPassArgs :: INullPtr -> Maybe Value -> Instruction -> Function -> [Maybe Value]
-- Handle the case where NULL is passed as an argument to a function.
-- Figure out which formal parameters correspond to the NULLs and
-- propagate those edges.  Also always propagate Λ.
--
-- FIXME: Extend to handle InvokeInst
inullPassArgs _ Nothing ci@(CallInst {}) f =
  let argMap = zip (map fst (callArguments ci)) (functionParameters f)
      isNullArg (actual,_) = case valueContent actual of
        ConstantC (ConstantPointerNull {}) -> True
        _ -> False
      nullArgs = filter isNullArg argMap
  in Nothing : map (Just . Value . snd) nullArgs `debug` printf "Nulls: %s" (show nullArgs)
-- Standard case to propagate Λ.
inullPassArgs _ Nothing _ _ = [Nothing]
-- This case handles propagating globals and non-constant pointer
-- information across function calls.  FIXME: Extend to handle InvokeInst.
inullPassArgs _ v@(Just v') ci@(CallInst {}) f =
  let nullFrmls = nullFormals v'
  in case isGlobal v' of
    True -> v : map (Just . Value) nullFrmls
    False -> map (Just . Value) nullFrmls
  where
    argMap = zip (map fst (callArguments ci)) (functionParameters f)
    nullFormals targetVal = foldl' (matchFormal targetVal) [] argMap
    matchFormal targetVal acc (arg, frml)
      | arg == targetVal = frml : acc
      | otherwise = case valueContent arg of
        InstructionC (LoadInst { loadAddress = la }) ->
          case la == targetVal of
            True -> frml : acc
            False -> acc
        _ -> acc


-- | Just pass information about globals.  In some other cases we
-- could look up a summary of the external function, but that isn't
-- necessary here (yet)
inullExternPassArgs :: INullPtr -> Maybe Value -> Instruction -> Maybe ExternalFunction -> [Maybe Value]
inullExternPassArgs _ Nothing _ _ = [Nothing]
inullExternPassArgs _ v@(Just v') _ _ = case isGlobal v' of
  True -> [v]
  False -> []

-- | Pass through information about globals.  If the return
-- instruction is returning a pointer value (and the value is equal to
-- Just v here), put the _call_ instruction that we are about to
-- return to into the set.
inullReturnVal :: INullPtr -> Maybe Value -> Instruction -> Instruction -> [Maybe Value]
inullReturnVal _ Nothing _ _ = [Nothing]
inullReturnVal _ v@(Just v') (RetInst { retInstValue = Just rv }) ci
  | isGlobal v' = [v]
  | otherwise = case v' == rv of
    True -> [Just (Value ci)]
    False -> []
inullReturnVal _ v@(Just v') (RetInst { retInstValue = Nothing}) ci
  | isGlobal v' = [v]
  | otherwise = []
inullReturnVal _ _ _ _ = []

-- | Just be conservative for now and return globals and the call
-- instruction.  Normally, look up summaries for the function
inullExternReturnVal :: INullPtr -> Maybe Value -> Maybe ExternalFunction -> Instruction -> [Maybe Value]
inullExternReturnVal _ Nothing _ _ = [Nothing]
inullExternReturnVal _ v@(Just v') _ ci
  | isGlobal v' = [v]
  | isPointerType (Value ci) = [v]
  | otherwise = []
inullExternReturnVal _ _ _ _ = []

-- | All globals of type T** _may_ be NULL at the start of a program.
-- We need to use T** here because all LLVM globals are pointers to
-- locations, so a global of type T* is a value that points to storage
-- that is a value of type T, which can never be NULL.
inullEntrySetup :: INullPtr -> Module -> Function -> [Maybe Value]
inullEntrySetup _ m entry = Nothing : map Just (gvs ++ evs)
  where
    globalsWithoutInits = filter isNullInitialized (moduleGlobalVariables m)
    gvs = filter isPtrToPtr $ map Value globalsWithoutInits
    evs = filter isPtrToPtr $ map Value (moduleExternalValues m)
    isNullInitialized gv = case globalVariableInitializer gv of
      Nothing -> True -- We'll treat uninintialized as null
      Just i -> case valueContent i of
        ConstantC (UndefValue {}) -> True
        ConstantC (ConstantPointerNull {}) -> True
        _ -> False
    isPtrToPtr v = case valueType v of
      TypePointer (TypePointer _ _) _ -> True
      _ -> False

expectedMapper :: FilePath -> FilePath
expectedMapper = (++ ".expected")

-- | Test values reachable at the return statement of a function
reachReturnTest :: Module -> [String]
reachReturnTest m = map (show . fromJust . valueName) endVals
  where
    pta = runPointsToAnalysis m
    Just progMain = findMain m
    icfg = mkICFG m pta [progMain]
    analysis = INullPtr
    res :: IFDSResult Value
    res = ifds analysis icfg
    retInst = functionExitInstruction progMain
    endVals = maybe [] S.toList (ifdsInstructionResult res retInst)


main :: IO ()
main = do
  let reachPattern = "tests/ifds/reach/*.c"
      testDescriptors = [ TestDescriptor { testPattern = reachPattern
                                         , testExpectedMapping = expectedMapper
                                         , testResultBuilder = reachReturnTest
                                         , testResultComparator = assertEqual
                                         }
                        ]
  testAgainstExpected (parseLLVMBitcodeFile defaultParserOptions) testDescriptors

isGlobal :: Value -> Bool
isGlobal v = case valueContent v of
  GlobalVariableC _ -> True
  ExternalValueC _ -> True
  _ -> False

isPointerType :: Value -> Bool
isPointerType v = case valueType v of
  TypePointer _ _ -> True
  _ -> False

isPointerPointerType :: Value -> Bool
isPointerPointerType v = case valueType v of
  TypePointer (TypePointer _ _) _ -> True
  _ -> False
