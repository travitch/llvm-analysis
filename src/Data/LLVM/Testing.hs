-- | Various functions to help test this library and analyses based on
-- it.
module Data.LLVM.Testing (
  readInputAndExpected,
  testAgainstExpected,
  module Data.LLVM.Testing.BuildModule
  ) where

import System.FilePath.Glob

import Data.LLVM.Types
import Data.LLVM.Testing.BuildModule

readInputAndExpected :: (Read a) => (FilePath -> FilePath) -> Bool -> FilePath ->
                        IO (FilePath, Module, a)
readInputAndExpected expectedFunc optimize inputFile = do
  let exFile = expectedFunc inputFile
  exContent <- readFile exFile
  -- use seq here to force the full evaluation of the read file.
  let expected = length exContent `seq` read exContent
  m <- buildModule inputFile optimize
  return (inputFile, m, expected)

testAgainstExpected :: (Read a) => String -> (FilePath -> FilePath) -> Bool ->
                       (Module -> a) -> (String -> a -> a -> IO ()) -> IO ()
testAgainstExpected testPattern expectedMap optimize buildResult compareResults = do
  -- Glob up all of the files in the test directory with the target extension
  testInputFiles <- namesMatching testPattern
  inputsAndExpecteds <- mapM (readInputAndExpected expectedMap optimize) testInputFiles
  mapM_ runAndCompare inputsAndExpecteds
  where
    runAndCompare (file, m, expected) = do
      let actual = buildResult m
      compareResults file actual expected