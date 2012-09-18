{-# LANGUAGE ViewPatterns #-}
module Main ( main ) where

import Data.Map ( Map )
import qualified Data.Map as M
import Data.Monoid
import System.Environment ( getArgs, withArgs )
import System.FilePath
import Test.HUnit ( assertEqual )

import LLVM.Analysis
import LLVM.Analysis.BlockReturnValue
import LLVM.Analysis.Util.Testing
import LLVM.Parse

main :: IO ()
main = do
  args <- getArgs
  let pattern = case args of
        [] -> "tests/block-return/*.c"
        [infile] -> infile
        _ -> error "Only one argument allowed"
      testDescriptors = [ TestDescriptor { testPattern = pattern
                                         , testExpectedMapping = (<.> "expected")
                                         , testResultBuilder = blockRetMap
                                         , testResultComparator = assertEqual
                                         }
                        ]
  withArgs [] $ testAgainstExpected opts parser testDescriptors
  where
    opts = [ "-mem2reg", "-basicaa", "-gvn" ]
    parser = parseLLVMFile defaultParserOptions

-- Take the first function in the module and summarize it (map of
-- block names to return values that are constant ints)
blockRetMap :: Module -> Map String Int
blockRetMap m = foldr (recordConstIntReturn brs) mempty blocks
  where
    f1 : _ = moduleDefinedFunctions m
    blocks = functionBody f1
    brs = hoistReturns f1

recordConstIntReturn :: BlockReturns -> BasicBlock -> Map String Int -> Map String Int
recordConstIntReturn brs bb m =
  case blockReturn brs bb of
    Just (valueContent' -> ConstantC ConstantInt { constantIntValue = iv }) ->
      M.insert (show (basicBlockName bb)) (fromIntegral iv) m
    _ -> m