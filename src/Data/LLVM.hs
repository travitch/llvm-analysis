-- |
module Data.LLVM ( parseLLVMAsm
                 , maybeParseLLVMAsm
                 , parseLLVMAsmFile
                 , parseLLVMBitcodeFile
                 ) where

import Data.Text (Text)
import qualified Data.Text.IO as T
import System.Process

import Data.LLVM.Types
import Data.LLVM.Private.AssemblyParser
import Data.LLVM.Private.ParsingMonad
import Data.LLVM.Private.TieKnot

-- | Parse a Text string containing an LLVM Assembly file into the
-- Haskell form of the IR.  This will either return an LLVM Module
-- or a String describing the reason for a parser failure.
parseLLVMAsm :: Text -> Either String Module
parseLLVMAsm t = case parseTree of
    Right llvmModule -> Right $ tieKnot llvmModule
    Left err -> Left err
  where parseTree = runLLVMParser parser t

-- | This is a wrapper for those who do not care about the reason for
-- a parse error.  This will probably be removed at some point.
maybeParseLLVMAsm :: Text -> Maybe Module
maybeParseLLVMAsm t = do
  parseTree <- maybeRunLLVMParser parser t
  return $ tieKnot parseTree

-- | Parse an LLVM Assembly file into the Haskell form of the IR.
-- This version reads the file directly.  While no special steps need
-- to be taken (the parser can read any LLVM Assembly file), this
-- function is most useful if the assembly has been run through 'opt'
-- with at least -gvn and -mem2reg.
parseLLVMAsmFile :: FilePath -> IO (Either String Module)
parseLLVMAsmFile llfile = do
  content <- T.readFile llfile
  return $ parseLLVMAsm content

-- | Parse an LLVM Bitcode file into the Haskell form of the IR.  This
-- invokes the 'opt' program from the LLVM distribution to convert the
-- bitcode into the textual assembly form and then parses that.  This
-- function requires that 'opt' be visible in the PATH of the calling
-- process.
parseLLVMBitcodeFile :: FilePath -> IO (Either String Module)
parseLLVMBitcodeFile bcfile = do
  let llvmOpts = [ "-mem2reg", "-gvn", "-S", bcfile ]
  (_, Just hOut, _, _) <- createProcess (proc "opt" llvmOpts) { std_out = CreatePipe }
  content <- T.hGetContents hOut
  return $ parseLLVMAsm content





