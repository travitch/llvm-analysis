import System.Environment ( getArgs )

import Data.GraphViz

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

  let g = callGraphRepr cg
      params = nonClusteredParams { fmtNode = \(_,l) -> [toLabel l]
                                  , fmtEdge = \(_,_,l) -> [toLabel l]
                                  }

      dg = graphToDot params g
  dotrepr <- prettyPrint dg
  putStrLn dotrepr
