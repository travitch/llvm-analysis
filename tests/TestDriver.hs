import System.Environment ( getArgs )

import qualified Data.HashMap.Strict as M

import Data.LLVM
import Data.LLVM.Types
import Data.LLVM.Visualization

main = do
  [ fname ] <- getArgs
  let opts = defaultParserOptions { metaPositionPrecision = PositionNone }
  llvmModule <- parseLLVMBitcodeFile opts fname
  -- content <- T.readFile fname
  -- let llvmModule = parseLLVMAsm content
  either putStrLn dumpModule llvmModule

dumpModule :: Module -> IO ()
dumpModule m = do
  putStrLn $ show m
  -- let cfgs = M.elems $ moduleCFGs m
  -- putStrLn $ "CFGs: " ++ (show $ length cfgs)
  -- mapM_ viewCFG cfgs

-- printG0 (Right m) = do
--   let cfgs = M.elems $ moduleCFGs m
--       g0 = cfgs !! 0
--   print g0
