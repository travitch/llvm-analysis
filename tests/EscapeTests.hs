module Main ( main ) where

import Control.Monad.Identity
import Data.List ( find )
import Data.Map ( Map )
import Data.Maybe ( isJust )
import Data.Set ( Set )
import System.FilePath
import Test.HUnit ( assertEqual )

import Data.LLVM
import Data.LLVM.Analysis.CallGraph
import Data.LLVM.Analysis.Escape
import Data.LLVM.Analysis.PointsTo.TrivialFunction
import Data.LLVM.Parse
import Data.LLVM.Testing

main :: IO ()
main = testAgainstExpected ["-mem2reg", "-basicaa"] bcParser testDescriptors
  where
    bcParser = parseLLVMFile defaultParserOptions

testDescriptors :: [TestDescriptor]
testDescriptors = [ TestDescriptor { testPattern = "tests/escape/proper-escapes/*.c"
                                   , testExpectedMapping = (<.> "expected")
                                   , testResultBuilder = properEscapeSummary
                                   , testResultComparator = assertEqual
                                   }
                  , TestDescriptor { testPattern = "tests/escape/will-escape/*.c"
                                   , testExpectedMapping = (<.> "expected")
                                   , testResultBuilder = willEscapeSummary
                                   , testResultComparator = assertEqual
                                   }
                  , TestDescriptor { testPattern = "tests/escape/instruction-escape/*.c"
                                   , testExpectedMapping = (<.> "expected")
                                   , testResultBuilder = callInstructionEscapeSummary
                                   , testResultComparator = assertEqual
                                   }
                  ]

-- These tests assume that any external function allows all of its
-- arguments to escape.
properEscapeSummary :: Module -> Map String (Set String)
properEscapeSummary m = escapeResultToTestFormat er
  where
    pta = runPointsToAnalysis m
    cg = mkCallGraph m pta []
    er = runIdentity $ escapeAnalysis cg extSumm
    extSumm _ _ = return True

willEscapeSummary :: Module -> Map String (Set String)
willEscapeSummary m = willEscapeResultToTestFormat er
  where
    pta = runPointsToAnalysis m
    cg = mkCallGraph m pta []
    er = runIdentity $ escapeAnalysis cg extSumm
    extSumm _ _ = return True

callInstructionEscapeSummary :: Module -> Bool
callInstructionEscapeSummary m = isJust $ instructionEscapes er i
  where
    pta = runPointsToAnalysis m
    cg = mkCallGraph m pta []
    er = runIdentity $ escapeAnalysis cg extSumm
    extSumm _ _ = return True
    Just i = find isCallInst (moduleInstructions m)

moduleInstructions =
  concatMap basicBlockInstructions . concatMap functionBody . moduleDefinedFunctions

isCallInst :: Instruction -> Bool
isCallInst i =
  case i of
    CallInst {} -> True
    _ -> False
