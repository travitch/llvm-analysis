{-# LANGUAGE ExistentialQuantification #-}
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

import Control.Exception ( bracket )
import Control.Monad ( when )

import System.Directory
import System.Exit ( ExitCode(ExitSuccess) )
import System.FilePath
import System.FilePath.Glob
import System.IO
import System.Process

import Text.Printf
import Test.Framework ( defaultMain, Test )
import Test.Framework.Providers.HUnit

import Data.LLVM

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
                        => (FilePath -> IO (Either String Module)) -- ^ A function to parse bitcode files
                        -> (FilePath -> FilePath) -- ^ The function to map an input file name to the expected output file
                        -> FilePath -- ^ The input file
                        -> IO (FilePath, Module, a)
readInputAndExpected parseBitcode expectedFunc inputFile = do
  let exFile = expectedFunc inputFile
  exContent <- readFile exFile
  -- use seq here to force the full evaluation of the read file.
  let expected = length exContent `seq` read exContent
  m <- buildModule parseBitcode inputFile
  return (inputFile, m, expected)

-- | This is the main test suite entry point.  It takes a bitcode
-- parser and a list of test suites.
--
-- The bitcode parser is taken as an input so that this library does
-- not have a direct dependency on any FFI code.
testAgainstExpected :: (FilePath -> IO (Either String Module)) -- ^ A bitcode parsing function
                        -> [TestDescriptor] -- ^ The list of test suites to run
                        -> IO ()
testAgainstExpected parseBitcode testDescriptors = do
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
      inputsAndExpecteds <- mapM (readInputAndExpected parseBitcode mapping) testInputFiles
      -- Build actual test cases by applying the result builder
      mapM (mkTest br cmp) inputsAndExpecteds

    mkTest br cmp (file, m, expected) = do
      let actual = br m
      return $ testCase file $ cmp file expected actual

-- | Build a 'Module' from a C or C++ file using clang.  The binary
-- must be in PATH.  This function also supports LLVM bitcode and LLVM
-- assembly files.
--
-- It will raise an error if passed an unrecognized input file type.
buildModule :: (FilePath -> IO (Either String Module)) -> FilePath -> IO Module
buildModule parseFile inputFilePath =
  case takeExtension inputFilePath of
    ".ll" -> simpleBuilder inputFilePath
    ".bc" -> simpleBuilder inputFilePath
    ".c" -> bracket (openTempBitcodeFile inputFilePath) disposeTempBitcode (buildModule' "clang")
    ".C" -> bracket (openTempBitcodeFile inputFilePath) disposeTempBitcode (buildModule' "clang++")
    ".cxx" -> bracket (openTempBitcodeFile inputFilePath) disposeTempBitcode (buildModule' "clang++")
    ".cpp" -> bracket (openTempBitcodeFile inputFilePath) disposeTempBitcode (buildModule' "clang++")
    _ -> error ("No build method for " ++ inputFilePath)
  where
    -- | Parse a bitcode or llvm assembly file into a Module.
    simpleBuilder infile = do
      parseResult <- parseFile infile
      either error return parseResult
    -- | Turn a source file into a bitcode file with clang and then
    -- parse the result into a Module.
    buildModule' compileDriver (fp, h) = do
      -- If we are optimizing, wire opt into the process pipeline.
      -- Otherwise, just have clang write directly to the output file.
      (clangHandle, mOptProc) <- return (h, Nothing)
      let baseCmd = proc compileDriver [ "-emit-llvm", "-o", "-", "-c", inputFilePath ]
          clangCmd = baseCmd { std_out = UseHandle clangHandle }
      (_, _, _, clangProc) <- createProcess clangCmd
      clangrc <- waitForProcess clangProc
      optrc <- maybe (return ExitSuccess) waitForProcess mOptProc
      when (clangrc /= ExitSuccess) (error $ printf "Failed to compile %s" inputFilePath)
      when (optrc /= ExitSuccess) (error $ printf "Failed to optimize %s" inputFilePath)

      parseResult <- parseFile fp
      either error return parseResult

-- | Clean up after a temporary bitcode file
disposeTempBitcode :: (FilePath, Handle) -> IO ()
disposeTempBitcode (fp, h) = do
  hClose h
  removeFile fp

-- | Create a temporary bitcode file
openTempBitcodeFile :: FilePath -> IO (FilePath, Handle)
openTempBitcodeFile inputFilePath = do
  let fname = addExtension inputFilePath ".bc"
  tmpDir <- getTemporaryDirectory
  -- The filename has leading directory components (or can) - drop
  -- them when opening the temp file
  openBinaryTempFile tmpDir (takeFileName fname)
