-- | Various functions to help test this library and analyses based on
-- it.
module Data.LLVM.Testing (
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

import Data.LLVM
import Data.LLVM.Types

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

-- | Build a 'Module' from a C or C++ file using clang.  Optionally,
-- apply light optimizations (-O1, -mem2reg) using opt.  Both binaries
-- must be in your path.
buildModule :: FilePath -> Bool -> IO Module
buildModule inputFilePath optimize =
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
      (clangHandle, mOptProc) <- case optimize of
        True -> do
          let optimizeCmd = proc "opt" [ "-O1", "-mem2reg", "-o", "-" ]
              optCmd = optimizeCmd { std_out = UseHandle h
                                   , std_in = CreatePipe }
          (Just optH, _, _, optProc) <- createProcess optCmd
          return (optH, Just optProc)
        False -> return (h, Nothing)
      let baseCmd = proc compileDriver [ "-emit-llvm", "-o", "-", "-c", inputFilePath ]
          clangCmd = baseCmd { std_out = UseHandle clangHandle }
      (_, _, _, clangProc) <- createProcess clangCmd
      clangrc <- waitForProcess clangProc
      optrc <- maybe (return ExitSuccess) waitForProcess mOptProc
      when (clangrc /= ExitSuccess) (error $ printf "Failed to compile %s" inputFilePath)
      when (optrc /= ExitSuccess) (error $ printf "Failed to optimize %s" inputFilePath)

      parseResult <- parseLLVMBitcodeFile defaultParserOptions fp
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
