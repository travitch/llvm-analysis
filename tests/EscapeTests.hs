module Main ( main ) where

import qualified Data.ByteString.Char8 as BS
import Data.Char ( isDigit )
import Data.Map ( Map )
import qualified Data.Map as M
import Data.Set ( Set )
import qualified Data.Set as S
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
                  ]

properEscapeSummary :: Module -> Map String (Set String)
properEscapeSummary m = escapeResultToTestFormat er
  where
    pta = runPointsToAnalysis m
    cg = mkCallGraph m pta []
    er = escapeAnalysis cg extSumm
    extSumm _ _ = True

willEscapeSummary :: Module -> Map String (Set String)
willEscapeSummary m = willEscapeResultToTestFormat er
  where
    globalVars = map Value $ moduleGlobalVariables m
    pta = runPointsToAnalysis m
    cg = mkCallGraph m pta []
    er = escapeAnalysis cg extSumm
    extSumm _ _ = True
