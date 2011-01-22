import qualified Data.Text.IO as T
import System ( getArgs )

import Data.LLVM

main = do
  [ fname ] <- getArgs
  content <- T.readFile fname
  let llvmModule = parseLLVMAsm content
  putStrLn $ show llvmModule
