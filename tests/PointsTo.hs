import System.Environment ( getArgs )

import Data.LLVM.Analysis.PointsTo.Andersen
import Data.LLVM

main :: IO ()
main = do
  [ fname ] <- getArgs
  Right m <- parseLLVMBitcodeFile defaultParserOptions fname
  let a = runPointsToAnalysis m
  putStrLn $ show a
