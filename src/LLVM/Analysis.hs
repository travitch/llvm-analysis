-- | This top-level module exports the LLVM IR definitions and some
-- basic functions to inspect the IR.  The sub-modules under
-- LLVM.Analysis provide higher-level tools for analyzing the IR.
module LLVM.Analysis (
  -- * Parsing LLVM Bitcode
  -- $parsing

  -- * Types
  module Data.LLVM.Types,

  -- * Extra helpers
  FuncLike(..),
  ToGraphviz(..)
  ) where

import Data.GraphViz ( DotGraph )
import Data.LLVM.Types

-- | A class for types that can be derived from a Function.
class FuncLike a where
  fromFunction :: Function -> a

instance FuncLike Function where
  fromFunction = id

-- | A class for things that can be converted to graphviz graphs
class ToGraphviz a where
  toGraphviz :: a -> DotGraph Int

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
--
-- By default, this helper calls binaries named @clang@, @clang++@,
-- and @opt@, which are expected to be in your @PATH@.  To accommodate
-- distro packages, additional names are searched for @opt@:
-- @opt-3.2@, @opt-3.1@, and @opt-3.0@.
--
-- If you cannot place these binaries in your @PATH@, or if your
-- binaries have different names, you can specify them (either using
-- absolute or relative paths) with the environment variables
-- @LLVM_CLANG@, @LLVM_CLANGXX@, and @LLVM_OPT@.  These environment
-- variables override any default searching.
