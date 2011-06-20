import Control.Exception ( bracket )
import Control.Monad ( when )
import Data.Map ( Map )
import Data.Set ( Set )
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe ( fromJust )

import System.Directory
import System.Exit ( ExitCode(ExitSuccess) )
import System.FilePath
import System.FilePath.Glob
import System.IO
import System.Process

import Test.Framework ( defaultMain, buildTest, Test )
import Test.Framework.Providers.HUnit
import Test.HUnit hiding ( Test, test )

import Text.Printf

import Data.LLVM
import Data.LLVM.Types
import Data.LLVM.Analysis.PointsTo
import Data.LLVM.Analysis.PointsTo.Andersen

buildModule :: FilePath -> IO Module
buildModule inputFile =
  bracket openTempBitcodeFile disposeTempBitcode buildModule'
  where
    disposeTempBitcode (fp, h) = do
      hClose h
      removeFile fp
    openTempBitcodeFile = do
      let fname = replaceExtension inputFile ".bc"
      tmpDir <- getTemporaryDirectory
      -- fname has some leading directories on it - drop them when opening the temp file.
      openBinaryTempFile tmpDir (takeFileName fname)
    buildModule' (fp, h) = do
      -- Run clang over the input file and have it output the bitcode
      -- file to STDOUT.  We'll redirect this stdout to the temporary
      -- file (through handle h)
      let baseCmd = proc "clang" [ "-emit-llvm", "-o", "-", "-c", inputFile ]
          clangCmd = baseCmd { std_out = UseHandle h }
      (_, _, _, procHandle) <- createProcess clangCmd
      exitCode <- waitForProcess procHandle
      when (exitCode /= ExitSuccess) (assertFailure $ printf "Failed to compile %s" inputFile)

      parseResult <- parseLLVMBitcodeFile defaultParserOptions fp
      either error return parseResult

type ExpectedResult = Map String (Set String)

readInputAndExpected :: FilePath -> IO (Module, ExpectedResult)
readInputAndExpected inputFile = do
  let exFile = replaceExtension inputFile ".expected-andersen"
  exContent <- readFile exFile
  let expected = seq exContent (read exContent)
  m <- buildModule inputFile
  return (m, expected)

extractSummary :: Module -> AndersenAnalysis -> ExpectedResult
extractSummary m a = foldr addInfo M.empty ptrs
  where
    ptrs = globalPointerVariables m ++ functionPointerParameters m
    addInfo v r = M.insert (show $ fromJust $ valueName v) vals r
      where
        vals = S.map (show . fromJust . valueName) (pointsTo a v)

globalPointerVariables :: Module -> [Value]
globalPointerVariables m = filter isGlobalVar (moduleGlobals m)
  where
    isGlobalVar Value { valueContent = GlobalDeclaration {}
                      , valueType = TypePointer _
                      } = True
    isGlobalVar _ = False

functionPointerParameters :: Module -> [Value]
functionPointerParameters m = concatMap pointerParams (moduleFunctions m)
  where
    pointerParams Value { valueContent = f } = filter isPointerType (functionParameters f)
    isPointerType Value { valueType = TypePointer _ } = True
    isPointerType _ = False

-- Read the input file and the expected results.  Run the analysis
-- over the input file and then compare the results to the expected
-- values.
--
-- The difficult part is extracting the results into a form that can
-- be compared with.
mkTest :: FilePath -> IO Test
mkTest fp = do
  (m, ex) <- readInputAndExpected fp
  let res = runAndersenAnalysis m
      summary = extractSummary m res
  return $ testCase fp $ assertEqual fp ex summary

main :: IO ()
main = do
  -- Get all .c and .cpp input files
  testInputs <- namesMatching "tests/points-to-inputs/*.c*"
  defaultMain $ map (buildTest . mkTest) testInputs
