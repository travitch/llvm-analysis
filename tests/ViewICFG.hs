import System.Environment ( getArgs )

import Data.GraphViz

import Data.LLVM
import Data.LLVM.ICFG
import Data.LLVM.Analysis.PointsTo.TrivialFunction
import Data.LLVM.Visualization

main :: IO ()
main = do
  [ fname ] <- getArgs
  Right m <- parseLLVMBitcodeFile defaultParserOptions fname
  let aa = runPointsToAnalysis m
      icfg = mkICFG m aa (moduleDefinedFunctions m) []
  viewICFG icfg
  _ <- getChar
  return ()
  -- let g = callGraphRepr cg
  --     params = nonClusteredParams { fmtNode = \(_,l) -> [toLabel l]
  --                                 , fmtEdge = \(_,_,l) -> [toLabel l]
  --                                 }

  --     dg = graphToDot params g
  -- dotrepr <- prettyPrint dg
  -- putStrLn dotrepr
