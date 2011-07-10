import System.Environment ( getArgs )

import Data.Either
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
printAllFuncArgs m = mapM_ printFuncArgs $ moduleFunctions m

printFuncArgs :: Value -> IO ()
printFuncArgs Value { valueContent = fc
                    , valueName = Just n
                    } = do
  printf "Function [%s]:\n" (show n)
--  mapM_ printArgType args
  -- where
  --   args = functionParameters fc

metaType :: [Metadata] -> String
metaType [] = "none"
metaType (md:_) = show $ metaLocalType $ metaValueContent md
-- metaType md = show md

printArgType :: Value -> IO ()
printArgType a = do
  let n = maybe "" show (valueName a)
      mdType = metaType (valueMetadata a)
      vType = show (valueType a)
  printf "%s :: %s (stated type %s)\n" n mdType vType
