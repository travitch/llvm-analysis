module Main ( main ) where

import Control.Monad.Identity
import Data.Set ( Set )
import qualified Data.Set as S
import System.FilePath ( (<.>) )
import System.Environment ( getArgs, withArgs )
import Test.HUnit ( assertEqual )

import Data.LLVM
import Data.LLVM.Analysis.CallGraph
import Data.LLVM.Analysis.PointsTo.TrivialFunction
import Data.LLVM.Analysis.NoReturn
import Data.LLVM.Parse
import Data.LLVM.Testing

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

analyzeReturns :: Module -> Set String
analyzeReturns m = S.fromList $ map nameToString nrs
  where
    nrs = runIdentity (noReturnAnalysis cg exitTest)
    pta = runPointsToAnalysis m
    cg = mkCallGraph m pta []
