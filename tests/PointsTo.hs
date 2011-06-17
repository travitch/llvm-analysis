import System.Environment ( getArgs )
import Language.Datalog
import Data.LLVM.Analysis.PointsTo.Andersen
import Data.LLVM

main :: IO ()
main = do
  [ fname ] <- getArgs
  Right m <- parseLLVMBitcodeFile defaultParserOptions fname
  let res = evalDatalog (analysis m)
  putStrLn $ show $ allResults res
