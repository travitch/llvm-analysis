module Main ( main ) where

import qualified Data.ByteString.Char8 as BS
import Data.Char ( isDigit )
import Data.Map ( Map )
import qualified Data.Map as M
import Data.Set ( Set )
import qualified Data.Set as S
import System.FilePath
import Test.HUnit ( assertEqual )

import Data.LLVM
import Data.LLVM.Analysis.Escape

import Data.LLVM.ParseBitcode
import Data.LLVM.Testing

main :: IO ()
main = testAgainstExpected bcParser testDescriptors
  where
    bcParser = parseLLVMBitcodeFile defaultParserOptions

testDescriptors = [ TestDescriptor { testPattern = "tests/escape/proper-escapes/*.c"
                                   , testExpectedMapping = (<.> "expected")
                                   , testResultBuilder = properEscapeSummary
                                   , testResultComparator = assertEqual
                                   }
                  , TestDescriptor { testPattern = "tests/escape/points-to/*.c"
                                   , testExpectedMapping = (<.> "expected")
                                   , testResultBuilder = pointsToSummary
                                   , testResultComparator = assertEqual
                                   }
                  ]

properEscapeSummary :: Module -> Set String
properEscapeSummary m =
  foldr (findProperEscapes er) S.empty (moduleDefinedFunctions m)
  where
    er = runEscapeAnalysis m

-- | check all instructions and arguments
findProperEscapes :: EscapeResult -> Function -> Set String -> Set String
findProperEscapes er f acc = acc `S.union` S.fromList peNames
  where
    exitInst = functionExitInstruction f
    eg = escapeGraphAtLocation er exitInst
    allInstructions = map Value $ concatMap basicBlockInstructions (functionBody f)
    args = map Value $ functionParameters f
    properEscapes = filter (valueProperlyEscaped eg) (args ++ allInstructions)
    peNames = map (show . fromJust' "Escapee should have name" . valueName) properEscapes

pointsToSummary :: Module -> Map String (Set String)
pointsToSummary m =
  foldr (collectPointsToRelations globalVars er) M.empty (moduleDefinedFunctions m)
  where
    globalVars = map Value $ moduleGlobalVariables m
    er = runEscapeAnalysis m

-- | Starting at all of the locals (allocas with real names) and
-- globals, record each of the things they point to.  If they point to
-- a virtual node, work backwards (via preds) to real location nodes?
collectPointsToRelations :: [Value]
                            -> EscapeResult
                            -> Function
                            -> Map String (Set String)
                            -> Map String (Set String)
collectPointsToRelations globals er f acc =
  M.fromList $ filter (not . S.null . snd) (zip vnames vtargets)
  where
    exitInst = functionExitInstruction f
    eg = escapeGraphAtLocation er exitInst
    funcInsts = concatMap basicBlockInstructions (functionBody f)
    localVars = map Value $ filter isLocal funcInsts
    allVals = globals ++ localVars
    vnames = map (show . fromJust' "escVarPT" . valueName) allVals
    vtargets = map (S.map escNodeToString . localPointsTo eg) allVals

-- | Is this instruction an alloca representing a local variable?
-- Returns False for anonymous locals.
isLocal :: Instruction -> Bool
isLocal i = case i of
  AllocaInst { instructionName = Just n } -> not (beginsWithDigit n)
  _ -> False

beginsWithDigit n = isDigit (BS.head content)
  where
    content = identifierContent n


fromJust' msg v = case v of
  Nothing -> error msg
  Just v' -> v'

escNodeToString :: EscapeNode -> String
escNodeToString (IVirtual _) = "virtual"
escNodeToString (OReturnNode _) = "ret"
escNodeToString (VariableNode _) = error "Variable node to string?"
escNodeToString (OParameterNode v) =
  show (fromJust' "Parameters need names" (valueName v))
escNodeToString (OGlobalNode v) =
  show (fromJust' "Globals need names" (valueName v))
escNodeToString (INode v) =
  show (fromJust' "Locals have names" (valueName v))