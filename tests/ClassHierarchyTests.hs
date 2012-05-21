module Main ( main ) where

import Data.Map ( Map )
import Data.Set ( Set )
import System.Environment ( getArgs, withArgs )
import System.FilePath ( (<.>) )
import Test.HUnit ( assertEqual )

import LLVM.Analysis
import LLVM.Analysis.ClassHierarchy
import LLVM.Analysis.Util.Testing
import LLVM.Parse

main :: IO ()
main = do
  args <- getArgs
  let pattern = case args of
        [] -> "tests/class-hierarchy/*.cpp"
        [infile] -> infile
        _ -> error "Only one argument allowed"
      testDescriptors = [ TestDescriptor { testPattern = pattern
                                         , testExpectedMapping = (<.> "expected")
                                         , testResultBuilder = analyzeHierarchy
                                         , testResultComparator = assertEqual
                                         }
                        ]
  withArgs [] $ testAgainstExpected opts parser testDescriptors
  where
    opts = [ "-mem2reg", "-basicaa", "-gvn" ]
    parser = parseLLVMFile defaultParserOptions

analyzeHierarchy :: Module -> Map String (Set String)
analyzeHierarchy = classHierarchyToTestFormat . runCHA
