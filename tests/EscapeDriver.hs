import System.Environment ( getArgs )
import Data.LLVM
import Data.LLVM.Parse
import Data.LLVM.Analysis.Escape

main :: IO ()
main = do
  [ fname ] <- getArgs
  mm <- parseLLVMFile defaultParserOptions fname
  case mm of
    Left err -> putStrLn err
    Right m -> do
      let e = runEscapeAnalysis m
      mapM_ (viewEscapeGraph e) (moduleDefinedFunctions m)
      return ()
