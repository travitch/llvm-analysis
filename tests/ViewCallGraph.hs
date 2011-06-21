import System.Environment ( getArgs )

import Data.LLVM
import Data.LLVM.CallGraph
import Data.LLVM.Analysis.PointsTo.TrivialFunction
import Data.LLVM.Visualization

main :: IO ()
main = do
  [ fname ] <- getArgs
  Right m <- parseLLVMBitcodeFile defaultParserOptions fname
  let aa = runPointsToAnalysis m
      cg = mkCallGraph m aa ""
  viewCG cg
  _ <- getChar
  putStrLn "foo"
