module Data.LLVM.Private.KnotHelpers ( SymbolTable
                                     , IdStream
                                     , extract
                                     , initialStream
                                     , split
                                     , split2
                                     , split3
                                     , split4
                                     ) where

import Control.Comonad
import Data.HashMap.Strict ( HashMap )
import Data.Stream.Supply hiding ( split )
import Data.LLVM.Types
import System.IO.Unsafe


type SymbolTable = HashMap Identifier Value

type IdStream = Supply Integer

-- The documentation recommends forcing the compiler to not inline
-- calls involving unsafePerformIO.  It shouldn't be an issue for this
-- use, but it also doesn't hurt.
{-# NOINLINE initialStream #-}
initialStream :: IdStream
initialStream = unsafePerformIO newNumSupply

split :: Supply a -> Supply a
split = leftSupply