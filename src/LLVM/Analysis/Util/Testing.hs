{-# LANGUAGE ExistentialQuantification, DeriveDataTypeable #-}
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
module LLVM.Analysis.Util.Testing (
  -- * Types
  TestDescriptor(..),
  BuildException(..),
  -- * Actions
  testAgainstExpected,
  -- * Helpers
  buildModule,
  readInputAndExpected
  ) where

import Control.Exception as E
import Control.Monad ( when )
import Data.Typeable ( Typeable )
import System.Directory ( findExecutable )
import System.Environment ( getEnv )
import System.Exit ( ExitCode(ExitSuccess) )
import System.FilePath
import System.FilePath.Glob
import System.IO.Error
import System.IO.Temp
import System.Process as P

import Test.Framework ( defaultMain, Test )
import Test.Framework.Providers.HUnit

import LLVM.Analysis

data BuildException = ClangFailed FilePath ExitCode
                    | NoBuildMethodForInput FilePath
                    | OptFailed FilePath ExitCode
                    | NoOptBinaryFound
                    deriving (Typeable, Show)

instance Exception BuildException

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
                        -> (FilePath -> IO Module) -- ^ A function to turn a bitcode file bytestring into a Module
                        -> (FilePath -> FilePath) -- ^ The function to map an input file name to the expected output file
                        -> FilePath -- ^ The input file
                        -> IO (FilePath, Module, a)
readInputAndExpected optOpts parseFile expectedFunc inputFile = do
  let exFile = expectedFunc inputFile
  exContent <- readFile exFile
  -- use seq here to force the full evaluation of the read file.
  let expected = length exContent `seq` read exContent
  m <- buildModule [] optOpts parseFile inputFile
  return (inputFile, m, expected)

-- | This is the main test suite entry point.  It takes a bitcode
-- parser and a list of test suites.
--
-- The bitcode parser is taken as an input so that this library does
-- not have a direct dependency on any FFI code.
testAgainstExpected :: [String] -- ^ Options for opt
                       -> (FilePath -> IO Module) -- ^ A function to turn a bitcode file bytestring into a Module
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

-- | Optimize the bitcode in the given bytestring using opt with the
-- provided options
optify :: [String] -> FilePath -> FilePath -> IO ()
optify args inp optFile = do
  opt <- findOpt
  let cmd = P.proc opt ("-o" : optFile : inp : args)
  (_, _, _, p) <- createProcess cmd
  rc <- waitForProcess p
  when (rc /= ExitSuccess) $ E.throwIO $ OptFailed inp rc

-- | Given an input file, bitcode parsing function, and options to
-- pass to opt, return a Module.  The input file can be C, C++, or
-- LLVM bitcode.
--
-- Note that this function returns an Either value to report some
-- kinds of errors.  It can also raise IOErrors.
buildModule :: [String]                 -- ^ Front-end options (passed to clang) for the module.
            -> [String]                 -- ^ Optimization options (passed to opt) for the module.  opt is not run if the list is empty
            -> (FilePath -> IO Module)  -- ^ A function to turn a bitcode file into a Module
            -> FilePath                 -- ^ The input file (either bitcode or C/C++)
            -> IO Module
buildModule clangOpts optOpts parseFile inputFilePath = do
  clang <- catchIOError (getEnv "LLVM_CLANG") (const (return "clang"))
  clangxx <- catchIOError (getEnv "LLVM_CLANGXX") (const (return "clang++"))
  case takeExtension inputFilePath of
    ".ll"  -> simpleBuilder inputFilePath
    ".bc"  -> simpleBuilder inputFilePath
    ".c"   -> clangBuilder inputFilePath clang
    ".C"   -> clangBuilder inputFilePath clangxx
    ".cxx" -> clangBuilder inputFilePath clangxx
    ".cpp" -> clangBuilder inputFilePath clangxx
    _ -> E.throwIO $ NoBuildMethodForInput inputFilePath
  where
    simpleBuilder inp
      | null optOpts = parseFile inp
      | otherwise =
        withSystemTempFile ("opt_" ++ takeFileName inp) $ \optFname _ -> do
          optify optOpts inp optFname
          parseFile optFname

    clangBuilder inp driver =
      withSystemTempFile ("base_" ++ takeFileName inp) $ \baseFname _ -> do
        let cOpts     = clangOpts ++ ["-emit-llvm", "-o" , baseFname, "-c", inp]
        (_, _, _, p) <- createProcess $ proc driver cOpts
        rc <- waitForProcess p
        when (rc /= ExitSuccess) $ E.throwIO $ ClangFailed inputFilePath rc
        case null optOpts of
          True  -> parseFile baseFname
          False ->
            withSystemTempFile ("opt_" ++ takeFileName inp) $ \optFname _ -> do
              optify optOpts baseFname optFname
              parseFile optFname

-- | Find a suitable @opt@ binary in the user's PATH
--
-- First consult the LLVM_OPT environment variable.  If that is not
-- set, try a few common opt aliases.
findOpt :: IO FilePath
findOpt = do
  let fbin = findBin [ "opt", "opt-3.2", "opt-3.1", "opt-3.0" ]
  catchIOError (getEnv "LLVM_OPT") (const fbin)
  where
    findBin [] = E.throwIO NoOptBinaryFound
    findBin (bin:bins) = do
      b <- findExecutable bin
      case b of
        Just e -> return e
        Nothing -> findBin bins

{-# ANN module "HLint: ignore Use if" #-}
