-- |
module Data.LLVM (--  parseLLVMAsm
                 -- , maybeParseLLVMAsm
                 -- , parseLLVMAsmFile
  parseLLVMBitcodeFile
  , defaultParserOptions
  , ParserOptions(..)
  , PositionPrecision(..)
  ) where

import Data.ByteString.Lazy.Char8 ( ByteString )
import qualified Data.ByteString.Lazy.Char8 as B
import System.Process

import Data.LLVM.Types
import Data.LLVM.Private.Parser.Options
import Data.LLVM.Private.Parser.Unmarshal

-- | Parse a Text string containing an LLVM Assembly file into the
-- Haskell form of the IR.  This will either return an LLVM Module
-- or a String describing the reason for a parser failure.
-- parseLLVMAsm :: ParserOptions -> ByteString -> Either String Module
-- parseLLVMAsm opts t = case parseTree of
--     Right llvmModule -> Right $ tieKnot opts llvmModule
--     Left err -> Left (show err)
--   where parseTree = runLLVMParser parser t

-- | This is a wrapper for those who do not care about the reason for
-- a parse error.  This will probably be removed at some point.
-- maybeParseLLVMAsm :: ParserOptions -> ByteString -> Maybe Module
-- maybeParseLLVMAsm opts t = do
--   parseTree <- maybeRunLLVMParser parser t
--   return $ tieKnot opts parseTree

-- | Parse an LLVM Assembly file into the Haskell form of the IR.
-- This version reads the file directly.  While no special steps need
-- to be taken (the parser can read any LLVM Assembly file), this
-- function is most useful if the assembly has been run through 'opt'
-- with at least -gvn and -mem2reg.
-- parseLLVMAsmFile :: ParserOptions -> FilePath -> IO (Either String Module)
-- parseLLVMAsmFile opts llfile = do
--   content <- B.readFile llfile
--   return $ parseLLVMAsm opts content

-- | Parse an LLVM Bitcode file into the Haskell form of the IR.  This
-- invokes the 'opt' program from the LLVM distribution to convert the
-- bitcode into the textual assembly form and then parses that.  This
-- function requires that 'opt' be visible in the PATH of the calling
-- process.
parseLLVMBitcodeFile :: ParserOptions -> FilePath -> IO (Either String Module)
parseLLVMBitcodeFile = parseBitcode
  -- do
  -- let llvmOpts = [ "-mem2reg", "-gvn", "-S", bcfile ]
  -- (_, Just hOut, _, _) <- createProcess (proc "opt" llvmOpts) { std_out = CreatePipe }
  -- content <- B.hGetContents hOut
  -- return $ parseLLVMAsm opts content





