import Data.Map ( Map )
import Data.Set ( Set )
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe ( fromJust )
import System.FilePath
import Test.HUnit ( assertEqual )

import LLVM.Analysis
import LLVM.Analysis.PointsTo.AllocatorProfile
import LLVM.Analysis.PointsTo.Andersen
import LLVM.Analysis.PointsTo
import LLVM.Analysis.Util.Testing
import LLVM.Parse

ptPattern = "tests/points-to-inputs/*.c"
expectedMapper = flip replaceExtension ".expected-andersen"
bcParser = parseLLVMFile defaultParserOptions

-- extractSummary :: Module -> ExpectedResult
extractSummary m = foldr addInfo M.empty ptrs
  where
    pta = runPointsToAnalysis [standardCProfile] m
    ptrs = map Value (globalPointerVariables m) ++ map Value (functionPointerParameters m)
    addInfo v r = case S.null vals of
      True -> r
      False -> M.insert (show $ fromJust $ valueName v) vals r
      where
        vals = case pointsTo pta v of
          PTSet s -> S.map show s
          UniversalSet s -> "unknown" `S.insert` S.map show s

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

testDescriptors = [ TestDescriptor { testPattern = ptPattern
                                   , testExpectedMapping = expectedMapper
                                   , testResultBuilder = extractSummary
                                   , testResultComparator = assertEqual
                                   }
                  ]

main :: IO ()
main = testAgainstExpected [] bcParser testDescriptors
