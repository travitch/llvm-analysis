module Main ( main ) where

import Data.Generics.Uniplate.Data
import Data.List ( find )
import Data.Map ( Map )
import qualified Data.Map as M
import Data.Maybe ( mapMaybe )
import Data.Set ( Set )
import qualified Data.Set as S
import System.Environment ( getArgs, withArgs )
import System.FilePath ( (<.>) )
import Test.HUnit ( assertEqual )

import qualified ABI.Itanium as ABI

import LLVM.Analysis
import LLVM.Analysis.ClassHierarchy
import LLVM.Analysis.Util.Names
import LLVM.Analysis.Util.Testing
import LLVM.Parse

main :: IO ()
main = do
  args <- getArgs
  let pattern1 = case args of
        [] -> "tests/class-hierarchy/*.cpp"
        [infile] -> infile
        _ -> error "Only one argument allowed"
      pattern2 = case args of
        [] -> "tests/virtual-dispatch/*.cpp"
        [infile] -> infile
        _ -> error "Only one argument allowed"
      testDescriptors = [ TestDescriptor { testPattern = pattern1
                                         , testExpectedMapping = (<.> "expected")
                                         , testResultBuilder = analyzeHierarchy
                                         , testResultComparator = assertEqual
                                         }
                        , TestDescriptor { testPattern = pattern2
                                         , testExpectedMapping = (<.> "expected")
                                         , testResultBuilder = findCallees
                                         , testResultComparator = assertEqual
                                         }
                        ]
  withArgs [] $ testAgainstExpected opts parser testDescriptors
  where
    opts = [ "-mem2reg", "-basicaa", "-gvn" ]
    parser = parseLLVMFile defaultParserOptions

analyzeHierarchy :: Module -> Map String (Set String)
analyzeHierarchy = classHierarchyToTestFormat . runCHA

findCallees :: Module -> Map String (Set String)
findCallees m = M.fromList $ mapMaybe (firstCalleeTargets cha) funcs
  where
    cha = runCHA m
    funcs = moduleDefinedFunctions m

functionToDemangledName :: Function -> String
functionToDemangledName f =
  case parseFunctionName f of
    Left e -> error e
    Right sname ->
      case unparseFunctionName sname of
        Nothing -> error ("Unable to unparse function name: " ++ show sname)
        Just n -> n

firstCalleeTargets :: CHA -> Function -> Maybe (String, Set String)
firstCalleeTargets cha f = do
  case isConstructor f of
    True -> Nothing
    False -> do
      firstCall <- find isCallInst insts
      callees <- resolveVirtualCallee cha firstCall
      return (fname, S.fromList (map functionToDemangledName callees))
  where
    insts = functionInstructions f
    fname = functionToDemangledName f

isConstructor :: Function -> Bool
isConstructor f =
  case dname of
    Left _ -> False
    Right structuredName ->
      case universeBi structuredName of
        [ABI.C2] -> True
        [ABI.C1] -> True
        [ABI.C3] -> True
        _ -> False
  where
    n = identifierAsString (functionName f)
    dname = ABI.demangleName n

isCallInst :: Instruction -> Bool
isCallInst i =
  case i of
    CallInst {} -> True
    _ -> False