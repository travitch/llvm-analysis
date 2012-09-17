module Main ( main ) where

import Data.Map ( Map )
import Data.Set ( Set )
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Monoid
import System.Environment ( getArgs, withArgs )
import System.FilePath
import Test.HUnit ( assertEqual )

import LLVM.Analysis
-- import LLVM.Analysis.PointsTo.AllocatorProfile
import LLVM.Analysis.PointsTo.Andersen
import LLVM.Analysis.PointsTo
import LLVM.Analysis.Util.Testing
import LLVM.Parse

import Debug.Trace
debug = flip trace

type ExpectedResult = Map String (Set String)

extractSummary :: Module -> ExpectedResult
extractSummary m = foldr addInfo mempty ptrs
  where
    pta = runPointsToAnalysis m
    ptrs = map toValue (globalPointerVariables m) -- ++ map Value (functionPointerParameters m)
    addInfo v r =
      let vals = pointsTo pta v
          name = maybe "???" show (valueName v)
      in case null vals of
        True -> r
        False ->
          let targets = map (maybe "??" show . valueName) vals -- `debug` show vals
          in M.insert name (S.fromList targets) r

isPointerType t = case t of
  TypePointer _ _ -> True
  _ -> False

isPointer :: (IsValue a) => a -> Bool
isPointer = isPointerType . valueType

globalPointerVariables :: Module -> [GlobalVariable]
globalPointerVariables m = filter isPointer (moduleGlobalVariables m)

functionPointerParameters :: Module -> [Argument]
functionPointerParameters m = concatMap pointerParams (moduleDefinedFunctions m)
  where
    pointerParams = filter isPointer . functionParameters

main :: IO ()
main = do
  args <- getArgs
  let pattern = case args of
        [] -> "tests/points-to-inputs/*.c"
        [infile] -> infile
        _ -> error "Only one argument allowed"
      testDescriptors = [ TestDescriptor { testPattern = pattern
                                         , testExpectedMapping = expectedMapper
                                         , testResultBuilder = extractSummary
                                         , testResultComparator = assertEqual
                                         }
                        ]
  withArgs [] $ testAgainstExpected opts parser testDescriptors
  where
    opts = [ "-mem2reg", "-basicaa", "-gvn" ]
    parser = parseLLVMFile defaultParserOptions
    expectedMapper = (<.> "expected-andersen")
