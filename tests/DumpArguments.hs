import System.Environment ( getArgs )

import Text.Printf

import Data.LLVM
import Data.LLVM.Types

main :: IO ()
main = do
  [ fname ] <- getArgs
  let opts = defaultParserOptions -- { metaPositionPrecision = PositionNone }
  llvmModule <- parseLLVMBitcodeFile opts fname
  either putStrLn printAllFuncArgs llvmModule

printAllFuncArgs :: Module -> IO ()
printAllFuncArgs m = mapM_ printFuncArgs $ moduleDefinedFunctions m

printFuncArgs :: Function -> IO ()
printFuncArgs f = do
  _ <- printf "Function [%s]:\n" (show $ functionName f)
  mapM_ printArgType args
  where
    args = functionParameters f

metaType :: [Metadata] -> String
metaType [] = "none"
metaType (md:_) = show $ metaLocalType $ metaValueContent md

printArgType :: Argument -> IO ()
printArgType a = do
  let n = maybe "" show (valueName a)
      mdType = metaType (valueMetadata a)
      vType = show (valueType a)
  printf "%s :: %s (stated type %s)\n" n mdType vType
