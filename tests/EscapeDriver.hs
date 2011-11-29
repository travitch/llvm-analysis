import System.Environment ( getArgs )
import Data.LLVM
import Data.LLVM.ParseBitcode
import Data.LLVM.Analysis.Escape

main :: IO ()
main = do
  [ fname ] <- getArgs
  mm <- parseLLVMBitcodeFile defaultParserOptions fname
  case mm of
    Left err -> putStrLn err
    Right m -> do
      let e = runEscapeAnalysis m
      mapM_ (viewEscapeGraph e) (moduleDefinedFunctions m)
      return ()
