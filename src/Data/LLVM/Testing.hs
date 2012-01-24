{-# LANGUAGE ExistentialQuantification, TemplateHaskell #-}
-- | Various functions to help test this library and analyses based on
-- it.
--
-- The idea behind the test framework is that each 'TestDescriptor'
-- describes inputs for a test suite and automatically converts the inputs
-- to a summary value, which it compares against an expected value.  It
-- reports how many such tests pass/fail.
--
-- More concretely, each test suite specifies:
--
-- * The test input files (via a shell glob)
--
-- * A function to conver a test input file name to a filename
--   containing the expected outut.
--
-- * A summary function to reduce a Module to a summary value
--
-- * A comparison function (usually an assertion from HUnit)
--
-- With these components, the framework reads each input file and
-- converts it to bitcode.  It uses the summary function to reduce the
-- Module to a summary value and reads the expected output (using the
-- 'read' function).  These two types (the summary and expected
-- output) must be identical.  The comparison function is then
-- applied.  If it throws an exception, the test is considered to have
-- failed.
--
-- NOTE 1: The result type of the summary function MUST be an instance
-- of 'Read' AND the same as the type found in the expected results
-- file.
--
-- NOTE 2: The test inputs can be C, C++, bitcode, or LLVM assembly
-- files.
module Data.LLVM.Testing (
  -- * Types
  TestDescriptor(..),
  -- * Actions
  testAgainstExpected,
  -- * Helpers
  buildModule,
  readInputAndExpected
  ) where

import Control.Monad ( when )

import FileLocation

import System.Exit ( ExitCode(ExitSuccess) )
import System.FilePath
import System.FilePath.Glob
import System.IO.Temp
import System.Process

import Test.Framework ( defaultMain, Test )
import Test.Framework.Providers.HUnit

import Data.LLVM
import Data.LLVM.Environment

-- | A description of a set of tests.
data TestDescriptor =
  forall a. (Read a) => TestDescriptor {
    testPattern :: String, -- ^ A shell glob pattern (relative to the project root) that collects all test inputs
    testExpectedMapping :: FilePath -> FilePath, -- ^ A function to apply to an input file name to find the file containing its expected results
    testResultBuilder :: Module -> a, -- ^ A function to turn a Module into a summary value of any type
    testResultComparator :: String -> a -> a -> IO () -- ^ A function to compare two summary values (throws on failure)
    }

-- | An intermediate helper to turn input files into modules and
-- return the expected output.
readInputAndExpected :: (Read a)
                        => [String] -- ^ Arguments for opt
                        -> (FilePath -> IO (Either String Module)) -- ^ A function to turn a bitcode file bytestring into a Module
                        -> (FilePath -> FilePath) -- ^ The function to map an input file name to the expected output file
                        -> FilePath -- ^ The input file
                        -> IO (FilePath, Module, a)
readInputAndExpected optOpts parseFile expectedFunc inputFile = do
  let exFile = expectedFunc inputFile
  exContent <- readFile exFile
  -- use seq here to force the full evaluation of the read file.
  let expected = length exContent `seq` read exContent
  m <- buildModule optOpts parseFile inputFile
  return (inputFile, m, expected)

-- | This is the main test suite entry point.  It takes a bitcode
-- parser and a list of test suites.
--
-- The bitcode parser is taken as an input so that this library does
-- not have a direct dependency on any FFI code.
testAgainstExpected :: [String] -- ^ Options for opt
                       -> (FilePath -> IO (Either String Module)) -- ^ A function to turn a bitcode file bytestring into a Module
                       -> [TestDescriptor] -- ^ The list of test suites to run
                       -> IO ()
testAgainstExpected optOpts parseFile testDescriptors = do
  caseSets <- mapM mkDescriptorSet testDescriptors
  defaultMain $ concat caseSets
  where
    mkDescriptorSet :: TestDescriptor -> IO [Test]
    mkDescriptorSet TestDescriptor { testPattern = pat
                                   , testExpectedMapping = mapping
                                   , testResultBuilder = br
                                   , testResultComparator = cmp
                                   } = do

      -- Glob up all of the files in the test directory with the target extension
      testInputFiles <- namesMatching pat
      -- Read in the expected results and corresponding modules
      inputsAndExpecteds <- mapM (readInputAndExpected optOpts parseFile mapping) testInputFiles
      -- Build actual test cases by applying the result builder
      mapM (mkTest br cmp) inputsAndExpecteds

    mkTest br cmp (file, m, expected) = do
      let actual = br m
      return $ testCase file $ cmp file expected actual

-- | Optimize the bitcode in the given bytestring using opt with the provided options
optify :: [String] -> FilePath -> FilePath -> IO ()
optify args inp optFile = do
  opt <- findOpt
  let cmd = proc opt ("-o" : optFile : inp : args)
  (_, _, _, p) <- createProcess cmd
  rc <- waitForProcess p
  when (rc /= ExitSuccess) ($err' ("Could not optimize " ++ inp))

buildModule ::  [String] -- ^ Optimization options (passed to opt) for the module.  opt is not run if the list is empty
                -> (FilePath -> IO (Either String Module)) -- ^ A function to turn a bitcode file into a Module
                -> FilePath -- ^ The input file (either bitcode or C/C++)
                -> IO Module
buildModule optOpts parseFile inputFilePath =
  case takeExtension inputFilePath of
    ".ll" -> simpleBuilder inputFilePath
    ".bc" -> simpleBuilder inputFilePath
    ".c" -> clangBuilder inputFilePath "clang"
    ".C" -> clangBuilder inputFilePath "clang++"
    ".cxx" -> clangBuilder inputFilePath "clang++"
    ".cpp" -> clangBuilder inputFilePath "clang++"
    _ -> $err' ("No build method for test input " ++ inputFilePath)
  where
    simpleBuilder inp =
      case null optOpts of
        True -> do
          parseResult <- parseFile inp
          either $err' return parseResult
        False ->
          withSystemTempFile ("opt_" ++ takeFileName inp) $ \optFname _ -> do
            optify optOpts inp optFname
            parseResult <- parseFile optFname
            either $err' return parseResult

    clangBuilder inp driver =
      withSystemTempFile ("base_" ++ takeFileName inp) $ \baseFname _ -> do
        let baseCmd = proc driver ["-emit-llvm", "-o" , baseFname, "-c", inp]
        (_, _, _, p) <- createProcess baseCmd
        rc <- waitForProcess p
        when (rc /= ExitSuccess) ($err' ("Could not compile input to bitcode: " ++ inp))
        case null optOpts of
          True -> do
            parseResult <- parseFile baseFname
            either $err' return parseResult
          False ->
            withSystemTempFile ("opt_" ++ takeFileName inp) $ \optFname _ -> do
              optify optOpts baseFname optFname
              parseResult <- parseFile optFname
              either $err' return parseResult
