module Data.LLVM ( parseLLVMAsm
                 , maybeParseLLVMAsm
                 ) where

import Data.Text (Text)

import Data.LLVM.Types
import Data.LLVM.Private.AssemblyParser
import Data.LLVM.Private.ParsingMonad
import Data.LLVM.Private.TieKnot

parseLLVMAsm :: Text -> Either String Module
parseLLVMAsm t = case parseTree of
    Right llvmModule -> Right $ tieKnot llvmModule
    Left err -> Left err
  where parseTree = runLLVMParser parser t

maybeParseLLVMAsm :: Text -> Maybe Module
maybeParseLLVMAsm t = do
  parseTree <- maybeRunLLVMParser parser t
  return $ tieKnot parseTree


