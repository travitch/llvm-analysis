{-# LANGUAGE MultiParamTypeClasses #-}
-- | An interprocedural may-be-null pointer analysis.
module Main ( main ) where

import Data.List ( elemIndex )
import Data.Maybe ( fromJust )
import qualified Data.Set as S

import Test.HUnit

import Data.LLVM
import Data.LLVM.ICFG
import Data.LLVM.Analysis.IFDS
import Data.LLVM.Analysis.PointsTo.TrivialFunction

import Data.LLVM.Testing

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
inullFlow _ Nothing _ _ = [Nothing]
-- FIXME: Need to handle edges here to see when we are on a non-null
-- branch
inullFlow _ v@(Just v') i@LoadInst { loadAddress = la } edges =
  -- Everything in LLVM is a pointer; the first "level" of load
  -- doesn't tell us anything since that is just LLVM loading the
  -- address in the pointer.  Subsequent loads (nested) represent
  -- actual pointer dereferences.
  case valueContent la of
    InstructionC LoadInst { loadAddress = la2 } ->
      case (Value la2) == v' of
        True -> []
        False -> [v]
    _ -> [v]
inullFlow _ v@(Just v') StoreInst { storeAddress = sa, storeValue = sv } edges
  | sa == v' = []
  | sv == v' = [Just sa]
  | otherwise = [v]
inullFlow _ v@(Just _) _ _ = [v]

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

-- | If the Value is an argument to the Call (or Invoke) instruction
-- _AND_ it is a pointer type (just one level of pointer, T*), put the
-- corresponding formal parameter of the Function into the set instead
-- of the input value.
--
-- Also, just pass through information about globals.
inullPassArgs :: INullPtr -> Maybe Value -> Instruction -> Function -> [Maybe Value]
inullPassArgs _ Nothing _ _ = [Nothing]
inullPassArgs _ v@(Just v') ci@(CallInst {}) f =
  case (isPointerType v', isGlobal v', argumentIndex v' (callArguments ci)) of
    (True, False, Just ix) -> [(Just . Value) (functionParameters f !! ix)]
    (_, True, _) -> [v]
    _ -> []

isGlobal :: Value -> Bool
isGlobal v = case valueContent v of
  GlobalVariableC _ -> True
  ExternalValueC _ -> True
  _ -> False

argumentIndex :: Value -> [(Value, [ParamAttribute])] -> Maybe Int
argumentIndex v args = elemIndex v (map fst args)

isPointerType :: Value -> Bool
isPointerType v = case valueType v of
  TypePointer _ _ -> True
  _ -> False

isPointerPointerType :: Value -> Bool
isPointerPointerType v = case valueType v of
  TypePointer (TypePointer _ _) _ -> True
  _ -> False

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
    gvs = filter isPtrToPtr $ map Value (moduleGlobalVariables m)
    evs = filter isPtrToPtr $ map Value (moduleExternalValues m)
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
  testAgainstExpected [ TestDescriptor { testPattern = reachPattern
                                       , testExpectedMapping = expectedMapper
                                       , testOptimized = False
                                       , testResultBuilder = reachReturnTest
                                       , testResultComparator = assertEqual
                                       }
                      ]
  {-
  [fname] <- getArgs
  Right m <- parseLLVMBitcodeFile defaultParserOptions fname
  let pta = runPointsToAnalysis m
      icfg = mkICFG m pta (maybe [] (:[]) (findMain m))
      analysis = INullPtr
      res :: IFDSResult Value
      res = ifds analysis icfg
  print res
-}


