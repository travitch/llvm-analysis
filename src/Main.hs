-- import LLVM.Core
import Data.LLVM.Lexer
import qualified Data.ByteString.Lazy as BS

-- import Data.Maybe
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
  contents <- BS.readFile filename
  let tokens = lexer contents
  -- inputModule <- readBitcodeFromFile filename
  -- putStrLn $ show inputModule
  -- vals <- getModuleValues inputModule
  -- mapM getVal vals
  putStrLn $ show tokens