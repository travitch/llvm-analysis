import System.Environment ( getArgs )

import Data.Either
import Text.Printf

import Data.LLVM
import Data.LLVM.Types

main :: IO ()
main = do
  [ fname ] <- getArgs
  llvmModule <- parseLLVMBitcodeFile fname
  either putStrLn printAllFuncArgs llvmModule

printAllFuncArgs :: Module -> IO ()
printAllFuncArgs = (mapM_ printFuncArgs) . moduleFunctions

printFuncArgs :: Value -> IO ()
printFuncArgs Value { valueContent = fc } = do
  printf "Function [%s]:\n" (show (functionName fc))
  mapM_ printArgType args
  where
    args = functionParameters fc

metaType :: Metadata -> String
metaType md = show $ metaLocalType $ metaValueContent md
metaType md = show md

printArgType :: Value -> IO ()
printArgType a = do
  let n = maybe "" show (valueName a)
      mdType = maybe "none" metaType (valueMetadata a)
      vType = show (valueType a)
  printf "%s :: %s (stated type %s)\n" n mdType vType
