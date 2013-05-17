module Main ( main ) where

import Data.Functor.Identity
import Data.Foldable ( toList )
import Data.HashSet ( HashSet )
import Data.Monoid
import Data.Set ( Set )
import qualified Data.Set as S
import System.FilePath ( (<.>) )
import System.Environment ( getArgs, withArgs )
import Test.HUnit ( assertEqual )

import LLVM.Analysis
import LLVM.Analysis.CFG
import LLVM.Analysis.CallGraph
import LLVM.Analysis.CallGraphSCCTraversal
import LLVM.Analysis.PointsTo.TrivialFunction
import LLVM.Analysis.NoReturn
import LLVM.Analysis.Util.Testing
import LLVM.Parse

main :: IO ()
main = do
  args <- getArgs
  let pattern = case args of
        [] -> "tests/noreturn/*.c"
        [infile] -> infile
  let testDescriptors = [ TestDescriptor { testPattern = pattern
                                         , testExpectedMapping = (<.> "expected")
                                         , testResultBuilder = analyzeReturns
                                         , testResultComparator = assertEqual
                                         }
                        ]

  withArgs [] $ testAgainstExpected opts parser testDescriptors
  where
    opts = ["-mem2reg", "-basicaa", "-gvn"]
    parser = parseLLVMFile defaultParserOptions

exitTest :: (Monad m) => ExternalFunction -> m Bool
exitTest ef = return $ "@exit" == efname
  where
    efname = show (externalFunctionName ef)

nameToString :: Function -> String
nameToString = show . functionName

runNoReturnAnalysis :: CallGraph -> (ExternalFunction -> Identity Bool) -> [Function]
runNoReturnAnalysis cg extSummary =
  let analysis :: [CFG] -> HashSet Function -> HashSet Function
      analysis = callGraphAnalysisM runIdentity (noReturnAnalysis extSummary)
      res = callGraphSCCTraversal cg analysis mempty
  in toList res


analyzeReturns :: Module -> Set String
analyzeReturns m = S.fromList $ map nameToString nrs
  where
    nrs = runNoReturnAnalysis cg exitTest -- runIdentity (noReturnAnalysis cg exitTest)
    pta = runPointsToAnalysis m
    cg = callGraph m pta []
