{-# LANGUAGE ExistentialQuantification #-}
-- | Various functions to help test this library and analyses based on
-- it.
module Data.LLVM.Testing (
  -- * Types
  TestDescriptor(..),
  -- * Actions
  buildModule,
  readInputAndExpected,
  testAgainstExpected
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

readInputAndExpected :: (Read a)
                        => (FilePath -> IO (Either String Module))
                        -> (FilePath -> FilePath)
                        -> FilePath
                        -> IO (FilePath, Module, a)
readInputAndExpected parseBitcode expectedFunc inputFile = do
  let exFile = expectedFunc inputFile
  exContent <- readFile exFile
  -- use seq here to force the full evaluation of the read file.
  let expected = length exContent `seq` read exContent
  m <- buildModule parseBitcode inputFile
  return (inputFile, m, expected)

data TestDescriptor =
  forall a. (Read a) => TestDescriptor { testPattern :: String
                                       , testExpectedMapping :: FilePath -> FilePath
                                       , testResultBuilder :: Module -> a
                                       , testResultComparator :: String -> a -> a -> IO ()
                                       }

testAgainstExpected :: (FilePath -> IO (Either String Module))
                        -> [TestDescriptor]
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
-- must be in PATH
buildModule :: (FilePath -> IO (Either String Module)) -> FilePath -> IO Module
buildModule parseBitcode inputFilePath =
  bracket (openTempBitcodeFile inputFilePath) disposeTempBitcode buildModule'
  where
    compileDriver = case takeExtension inputFilePath of
      ".c" -> "clang"
      ".cpp" -> "clang++"
      ".cxx" -> "clang++"
      ".C" -> "clang++"
    buildModule' (fp, h) = do
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

      parseResult <- parseBitcode fp
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
