{-# LANGUAGE MultiParamTypeClasses, ViewPatterns #-}
-- | An interprocedural may-be-null pointer analysis.
module Main ( main ) where

import Data.List ( foldl' )
import Data.Maybe ( fromJust )
import Data.Set ( Set )
import qualified Data.Set as S
import Text.Regex.TDFA

import Test.HUnit

import Data.LLVM
import Data.LLVM.Analysis.ICFG
import Data.LLVM.Analysis.IFDS
import Data.LLVM.Analysis.PointsTo.TrivialFunction

import Data.LLVM.Parse
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
-- In this case, nothing is NULL here but we might be NULLing out the
-- address being stored to.
inullFlow _ Nothing i@StoreInst { storeAddress = sa, storeValue = sv } edges =
  case valueInjectsNull sv of
    True -> [Nothing, Just sa]
    False -> [Nothing]
inullFlow _ v@(Just v') StoreInst { storeAddress = sa, storeValue = sv } edges
  -- Storing a pointer value that may be NULL; the possibly-null
  -- pointer is still possibly null, and the address stored to is also
  -- possibly null now.
  | sv == v' = [Just sa, v]
  -- If the address being stored to contained NULL before, assume it
  -- doesn't know (if the value being stored *could* be NULL,
  -- subsequent rules will handle it).
  | sa == v' = []
  | otherwise = case valueContent sv of
    InstructionC LoadInst { loadAddress = la } ->
      case la == v' of
        True -> [Just sa, v]
        False -> [v]
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
inullPassArgs _ Nothing (CallInst { callArguments = cargs }) f =
  let argMap = zip (map fst cargs) (functionParameters f)

      nullArgs = filter (valueInjectsNull . fst) argMap
  in Nothing : map (Just . Value . snd) nullArgs
-- Standard case to propagate Λ.
inullPassArgs _ Nothing _ _ = [Nothing]
-- This case handles propagating globals and non-constant pointer
-- information across function calls.  FIXME: Extend to handle InvokeInst.
inullPassArgs _ v@(Just v') (CallInst { callArguments = cargs}) f =
  let nullFrmls = nullFormals v'
  in case isGlobal v' of
    True -> v : map (Just . Value) nullFrmls
    False -> map (Just . Value) nullFrmls
  where
    argMap = zip (map fst cargs) (functionParameters f)
    nullFormals targetVal = foldl' (matchFormal targetVal) [] argMap
    matchFormal targetVal acc (arg, frml)
      | arg == targetVal = frml : acc
      | otherwise = case valueContent arg of
        InstructionC LoadInst { loadAddress =
          (valueContent -> InstructionC GetElementPtrInst {})} -> frml : acc
        InstructionC LoadInst { loadAddress = la } -> case la == targetVal of
          -- The thing being loaded is known to be NULL
          True -> frml : acc
          -- Might not be NULL... there are some more cases to handle, I'd say
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

-- | This is a filter predicate for @inullPassArgs@, and only for the
-- case where the current domain value being considered is Λ
-- (Nothing).  This rule defines the function arguments that introduce
-- *new* NULL edges.
--
-- * Values loaded from fields of aggregates (structs, arrays) could be NULL
--
-- * Nested indirections are not tracked here (that is the job of
--   points-to analysis) so be conservative
--
-- * The NULL pointer
valueInjectsNull :: Value -> Bool
valueInjectsNull v = isPointerType v && case valueContent v of
  InstructionC LoadInst { loadAddress =
    (valueContent -> InstructionC GetElementPtrInst {}) } -> True
  InstructionC LoadInst { loadAddress =
    (valueContent -> InstructionC LoadInst {}) } -> True
  ConstantC ConstantPointerNull {} -> True
  InstructionC BitcastInst { castedValue = cv } -> valueInjectsNull cv
  InstructionC PhiNode { phiIncomingValues = ivs } -> any valueInjectsNull (map fst ivs)
  -- FIXME: What about a load of a PHI?
  _ -> False

valueMatchesNull :: Value -> Value -> Bool
valueMatchesNull v target = case valueContent v of
  ArgumentC _ -> v == target
  InstructionC LoadInst { loadAddress = la } -> target == la
  InstructionC PhiNode { phiIncomingValues = ivs } -> any (==target) (map fst ivs)
  _ -> False

-- | Pass through information about globals.  If the return
-- instruction is returning a pointer value (and the value is equal to
-- Just v here), put the _call_ instruction that we are about to
-- return to into the set.
inullReturnVal :: INullPtr -> Maybe Value -> Instruction -> Instruction -> [Maybe Value]
-- This case lets us introduce new NULLs from things like constants
-- and untracked loads.
inullReturnVal _ Nothing (RetInst { retInstValue = Just rv }) ci =
  case valueInjectsNull rv of
    True -> [Nothing, Just (Value ci)]
    False -> [Nothing]
inullReturnVal _ v@(Just v') (RetInst { retInstValue = Just rv }) ci =
  case valueMatchesNull rv v' of
    True -> Just (Value ci) : globalProp
    False -> globalProp
  where
    globalProp = case isGlobal v' of
      False -> []
      True -> [v]
inullReturnVal _ v@(Just v') (RetInst { retInstValue = Nothing}) ci
  | isGlobal v' = [v]
  | otherwise = []
inullReturnVal _ _ _ _ = []

-- | Just be conservative for now and return globals and the call
-- instruction.  Normally, look up summaries for the function
inullExternReturnVal :: INullPtr -> Maybe Value -> Maybe ExternalFunction -> Instruction -> [Maybe Value]
inullExternReturnVal _ Nothing _ ci =
  case isPointerType (Value ci) of
    False -> [Nothing]
    True -> [Just (Value ci), Nothing]
inullExternReturnVal _ v@(Just v') _ ci
  | isGlobal v' = [v]
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
reachReturnTest :: Module -> Set String
reachReturnTest m = S.filter isNotNumericName $ S.map (show . fromJust . valueName) endVals
  where
    pta = runPointsToAnalysis m
    Just progMain = findMain m
    icfg = mkICFG m pta [progMain]
    analysis = INullPtr
    res :: IFDSResult Value
    res = ifds analysis icfg
    retInst = functionExitInstruction progMain
    endVals = maybe S.empty id (ifdsInstructionResult res retInst)
    isNotNumericName = not . (=~"%[[:digit:]]+")

main :: IO ()
main = do
  let reachPattern = "tests/ifds/reach/*.c"
      testDescriptors = [ TestDescriptor { testPattern = reachPattern
                                         , testExpectedMapping = expectedMapper
                                         , testResultBuilder = reachReturnTest
                                         , testResultComparator = assertEqual
                                         }
                        ]
  testAgainstExpected [] (parseLLVMFile defaultParserOptions) testDescriptors

isArgument :: Value -> Bool
isArgument v = case valueContent v of
  ArgumentC _ -> True
  _ -> False

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
