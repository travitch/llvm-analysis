import qualified Data.Text.IO as T
import System ( getArgs )

import Data.Either
import qualified Data.Map as M

import Data.LLVM
import Data.LLVM.Types
import Data.LLVM.Visualization

main = do
  [ fname ] <- getArgs
  llvmModule <- parseLLVMBitcodeFile fname
  -- content <- T.readFile fname
  -- let llvmModule = parseLLVMAsm content
  either putStrLn dumpModule llvmModule

dumpModule :: Module -> IO ()
dumpModule m = do
  putStrLn $ show m
  let cfgs = M.elems $ moduleCFGs m
  putStrLn $ "CFGs: " ++ (show $ length cfgs)
  mapM_ viewCFG cfgs

printG0 (Right m) = do
  let cfgs = M.elems $ moduleCFGs m
      g0 = cfgs !! 0
  print g0
