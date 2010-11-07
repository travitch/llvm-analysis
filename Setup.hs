import Control.Monad (when, unless, mplus)

import Data.Maybe (listToMaybe, fromMaybe)
import Distribution.PackageDescription
    (PackageDescription, buildable, exeName, buildInfo, executables, customFieldsBI, BuildInfo)
import Distribution.Verbosity              (normal)
import Distribution.Simple.Build           (build)
import Distribution.Simple.LocalBuildInfo  (LocalBuildInfo(..))
import Distribution.Simple.PreProcess      (knownSuffixHandlers)
import Distribution.Simple.Program         (programFindLocation, lookupKnownProgram )
import Distribution.Simple.Setup           (defaultBuildFlags)
import Distribution.Simple
    ( Args, defaultMainWithHooks, UserHooks(..), simpleUserHooks)

import System.Exit       (ExitCode(..))
import System.FilePath   ( (</>), splitDirectories, isAbsolute )
import System.IO         (openFile, IOMode (..))
import System.Process
import System.Directory
    ( getCurrentDirectory, createDirectoryIfMissing
    , setCurrentDirectory, findExecutable, canonicalizePath
    , removeFile, doesDirectoryExist
    )

main = defaultMainWithHooks hooks

hooks :: UserHooks
hooks = simpleUserHooks { runTests = testHook }

testAssemblyParser :: a -> (BuildInfo -> a) -> PackageDescription -> a
testAssemblyParser dflt f pd =
  fromMaybe dflt $ listToMaybe
    [ f (buildInfo exe) | exe <- executables pd, exeName exe == "test-assembly-parser" ]

testHook :: Args -> Bool -> PackageDescription -> LocalBuildInfo -> IO ()
testHook args0 _unknown pd lbi = do
    let args = if null args0 then [] else "-t" : args0
    -- dir <- getWorkingDirectory
    let testDir = buildDir lbi </> "test-assembly-parser"
    baseDir <- getCurrentDirectory
    canonicalBuildDir <- canonicalizePath (buildDir lbi)
    t <- doesDirectoryExist testDir
    unless t $ do
        unless (testAssemblyParser False buildable pd) $ do
          fail "Reconfigure with 'cabal configure -ftests' or 'cabal install -ftests' and try again."
        putStrLn "building tests"
        build pd lbi defaultBuildFlags knownSuffixHandlers
        putStrLn "tests built"

    setCurrentDirectory testDir
    let customFields = testAssemblyParser [] customFieldsBI pd

    exitcode <- system $ unwords $ "test-assembly-parser" : args
    unless (exitcode == ExitSuccess) $
        fail "test failed"