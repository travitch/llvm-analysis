import System.Environment ( getArgs )

import Data.LLVM
import Data.LLVM.Parse

main :: IO ()
main = do
  [ fname ] <- getArgs
  let opts = defaultParserOptions { metaPositionPrecision = PositionNone }
  llvmModule <- parseLLVMFile opts fname
  either putStrLn dumpModule llvmModule

dumpModule :: Module -> IO ()
dumpModule m = do
  putStrLn $ show m
