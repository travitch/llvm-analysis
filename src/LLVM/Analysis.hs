-- | This top-level module exports the LLVM IR definitions and some
-- basic functions to inspect the IR.  The sub-modules under
-- LLVM.Analysis provide higher-level tools for analyzing the IR.
module LLVM.Analysis (
  -- * Parsing LLVM Bitcode

  -- $parsing
  module Data.LLVM.Types
  ) where

import Data.LLVM.Types

-- $parsing
--
-- The functions to parse LLVM Bitcode into a Haskell ADT are in the
-- llvm-data-interop package (in the "LLVM.Parse" module).  The first
-- is 'parseLLVMFile':
--
-- > import LLVM.Parse
-- > main = do
-- >   m <- parseLLVMFile defaultParserOptions filePath
-- >   either error analyzeModule
-- >
-- > analyzeModule :: Module -> IO ()
--
-- The 'defaultParserOptions' direct the parser to keep all metadata.
-- This behavior can be changed to discard the location metadata
-- normally attached to each instruction, saving a great deal of
-- space.  Metadata describing the source-level types of functions,
-- arguments, and local variables (among other things) is preserved.
-- If the module was compiled without debug information, no metadata
-- will be parsed at all.
--
-- There are two variants of 'parseLLVMFile':
--
--  * 'hParseLLVMFile' parses its input from a 'Handle' instead of
--    a named file.
--
--  * 'parseLLVM' parses its input from a (strict) 'ByteString'.
--
-- There is also a higher-level wrapper in
-- "LLVM.Analysis.Util.Testing":
--
-- > import LLVM.Analysis.Util.Testing
-- > import LLVM.Parse
-- > main = do
-- >   m <- buildModule ["-mem2reg", "-gvn"] (parseLLVMFile defaultParserOptions) filePath
-- >   either error analyzeModule
--
-- This wrapper function accepts both LLVM Bitcode and C/C++ source
-- files.  Source files are compiled with clang into bitcode; the
-- resulting bitcode is fed to the @opt@ binary, which is passed the
-- options in the first argument to 'buildModule'.
