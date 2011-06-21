import Data.Maybe ( fromJust )
-- import Language.Datalog
import System.Environment ( getArgs )
import Text.Printf

import Data.LLVM.Analysis.PointsTo.Andersen
import Data.LLVM
import Data.LLVM.Types

main :: IO ()
main = do
  [ fname ] <- getArgs
  Right m <- parseLLVMBitcodeFile defaultParserOptions fname
  let a = runAndersenAnalysis m-- res = evalDatalog (analysis m)
  --     allres = allResults res
  -- mapM_ showPtPair allres
  putStrLn $ show a

-- tos = show . fromJust . valueName . toLLVMValue

-- showPtPair [ v1, v2 ] = do
--   let v1' = tos v1
--       v2' = tos v2
--   printf "%s -> %s\n" v1' v2'
