module Main ( main ) where

import Data.Set ( Set )
import qualified Data.Set as S
import System.FilePath
import Test.HUnit ( assertEqual )

import LLVM.Analysis
import LLVM.Analysis.CallGraph
import LLVM.Analysis.PointsTo.TrivialFunction
import LLVM.Analysis.CallGraphSCCTraversal
import LLVM.Analysis.Util.Testing
import LLVM.Parse

main :: IO ()
main = testAgainstExpected ["-mem2reg", "-basicaa"] bcParser testDescriptors
  where
    bcParser = parseLLVMFile defaultParserOptions

testDescriptors :: [TestDescriptor]
testDescriptors = [ TestDescriptor { testPattern = cgPattern
                                   , testExpectedMapping = expectedMapper
                                   , testResultBuilder = extractTraversalOrder
                                   , testResultComparator = assertEqual
                                   }
                  ]

cgPattern :: String
cgPattern = "tests/callgraph/order/*.c"

expectedMapper :: FilePath -> FilePath
expectedMapper = (<.> "expected")

extractTraversalOrder :: Module -> [Set String]
extractTraversalOrder m =
  case res == pres of
    True -> res
    False -> error "Mismatch between serial and parallel result"
  where
    Just mainFunc = findMain m
    pta = runPointsToAnalysis m
    cg = callGraph m pta [mainFunc]

    res = callGraphSCCTraversal cg buildSummary []
    pres = parallelCallGraphSCCTraversal cg buildSummary []

buildSummary :: [Function] -> [Set String] -> [Set String]
buildSummary scc summ = S.fromList fnames : summ
  where
    fnames = map (identifierAsString . functionName) scc