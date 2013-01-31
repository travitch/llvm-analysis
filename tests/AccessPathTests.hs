module Main ( main ) where

import Data.List ( find )
import Data.Map ( Map )
import qualified Data.Map as M
import System.Environment ( getArgs, withArgs )
import System.FilePath ( (<.>) )
import Test.HUnit ( assertEqual )

import LLVM.Analysis
import LLVM.Analysis.AccessPath
import LLVM.Analysis.Util.Testing
import LLVM.Parse

main :: IO ()
main = do
  args <- getArgs
  let pattern = case args of
        [] -> "tests/accesspath/*.c"
        [infile] -> infile
        _ -> error "Only one argument allowed"
      testDescriptors = [ TestDescriptor { testPattern = pattern
                                         , testExpectedMapping = (<.> "expected")
                                         , testResultBuilder = extractFirstPath
                                         , testResultComparator = assertEqual
                                         }
                        ]
  withArgs [] $ testAgainstExpected opts parser testDescriptors
  where
    opts = [ "-mem2reg", "-basicaa", "-gvn" ]
    parser = parseLLVMFile defaultParserOptions

type Summary = (String, [AccessType])

-- Feed the first store instruction in each function to accessPath and
-- map each function to its path.
extractFirstPath :: Module -> Map String Summary
extractFirstPath m = M.fromList $ map extractFirstFuncPath funcs
  where
    funcs = moduleDefinedFunctions m

extractFirstFuncPath :: Function -> (String, Summary)
extractFirstFuncPath f = (show (functionName f), summ)
  where
    allInsts = concatMap basicBlockInstructions (functionBody f)
    Just firstStore = find isStore allInsts
    Just p = accessPath firstStore
    p' = abstractAccessPath p
    summ = (show (abstractAccessPathBaseType p'),
            abstractAccessPathComponents p')

isStore :: Instruction -> Bool
isStore StoreInst {} = True
isStore _ = False
