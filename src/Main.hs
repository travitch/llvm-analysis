-- import LLVM.Core
import Data.LLVM

import Data.Maybe
import System.Environment ( getArgs )

-- getVal (name, mv) = do
--   let val :: Function (IO T)
--       val = castModuleValue mv
--       func :: Function (T -> IO T)
--       func = castModuleValue mv
--   if isJust val
--     then putStrLn $ show $ fromJust val
--     else putStrLn $ show $ fromJust func


main = do
  [ filename ] <- getArgs
  inputModule <- readBitcodeFromFile filename
  putStrLn $ show inputModule
  -- vals <- getModuleValues inputModule
  -- mapM getVal vals