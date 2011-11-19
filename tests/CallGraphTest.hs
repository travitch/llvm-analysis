module Main ( main ) where

import Data.Set ( Set )
import qualified Data.Set as S
import System.FilePath
import Test.HUnit ( assertEqual )

import Data.LLVM
import Data.LLVM.CallGraph
import Data.LLVM.Analysis.PointsTo.TrivialFunction
import Data.LLVM.Analysis.CallGraphSCCTraversal

import Data.LLVM.ParseBitcode
import Data.LLVM.Testing

main :: IO ()
main = testAgainstExpected bcParser testDescriptors
  where
    bcParser = parseLLVMBitcodeFile defaultParserOptions

testDescriptors = [ TestDescriptor { testPattern = cgPattern
                                   , testExpectedMapping = expectedMapper
                                   , testResultBuilder = extractTraversalOrder
                                   , testResultComparator = assertEqual
                                   }
                  ]

cgPattern = "tests/callgraph/order/*.c"
expectedMapper = (<.> "expected")

extractTraversalOrder m =
  callGraphSCCTraversal cg buildSummary []
  where
    Just main = findMain m
    pta = runPointsToAnalysis m
    cg = mkCallGraph m pta [main]

buildSummary scc summ = S.fromList fnames : summ
  where
    fnames = map (identifierContent . functionName) scc