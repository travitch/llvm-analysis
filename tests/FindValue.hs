-- | This program takes a Module and a list of unique Value ids.  It
-- prints out the details of each named value that exists in the
-- module.
--
-- It currently does not dump constants
module Main ( main ) where

import Data.HashSet ( HashSet )
import qualified Data.HashSet as HS
import System.Environment ( getArgs )
import Text.Printf

import Data.LLVM
import Data.LLVM.Parse

main :: IO ()
main = do
  fname : nodes <- getArgs
  let nodeSet = HS.fromList (map read nodes)
  mm <- parseLLVMFile defaultParserOptions fname
  let vals = either error (findNodes nodeSet) mm
  mapM_ showValue vals

findNodes :: HashSet Int -> Module -> [Value]
findNodes nodes m = filter ((`HS.member` nodes) . valueUniqueId) allVals
  where
    fs = moduleDefinedFunctions m
    allArgs = concatMap functionParameters fs
    allBlocks = concatMap functionBody fs
    allInsts = concatMap basicBlockInstructions allBlocks
    allVals = concat [ map Value fs
                     , map Value (moduleGlobalVariables m)
                     , map Value (moduleExternalValues m)
                     , map Value (moduleExternalFunctions m)
                     , map Value (moduleAliases m)
                     , map Value allBlocks
                     , map Value allInsts
                     , map Value allArgs
                     ]

showValue :: Value -> IO ()
showValue v = do
  _ <- printf "%d\n" (valueUniqueId v)
  _ <- case valueContent v of
    FunctionC f -> printf "  Function: %s\n" (show (functionName f))
    ArgumentC a -> do
      let f = argumentFunction a
      printf "  Argument: <%s:%s>\n" (show (functionName f)) (show (argumentName a))
    BasicBlockC b -> do
      let f = basicBlockFunction b
      printf "  Block: <%s:%s>\n" (show (functionName f)) (show (basicBlockName b))
    GlobalVariableC g -> printf "  GlobalVar: %s\n" (show (globalVariableName g))
    GlobalAliasC a -> printf "  GlobalAlias: %s\n" (show (globalAliasName a))
    ExternalValueC e -> printf "  ExternalValue: %s\n" (show (externalValueName e))
    ExternalFunctionC e -> printf "  ExternalFunction: %s\n" (show (externalFunctionName e))
    InstructionC i -> do
      let Just b = instructionBasicBlock i
          f = basicBlockFunction b
      printf "  Instruction: <%s:%s:%s>\n" (show (functionName f)) (show (basicBlockName b)) (show v)
    ConstantC _ -> printf "  Constants not supported\n"
  return ()