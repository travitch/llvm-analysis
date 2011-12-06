import System.Environment ( getArgs )
import Data.LLVM
import Data.LLVM.CallGraph
import Data.LLVM.Parse
import Data.LLVM.Analysis.Escape
import Data.LLVM.Analysis.PointsTo.TrivialFunction

main :: IO ()
main = do
  [ fname ] <- getArgs
  mm <- parseLLVMFile defaultParserOptions fname
  case mm of
    Left err -> putStrLn err
    Right m -> do
      let pt = runPointsToAnalysis m
          cg = mkCallGraph m pt []
          e = runEscapeAnalysis m cg
      mapM_ (viewEscapeGraph e) (moduleDefinedFunctions m)
      return ()
