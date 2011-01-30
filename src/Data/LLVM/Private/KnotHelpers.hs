module Data.LLVM.Private.KnotHelpers ( SymbolTable
                                     , IdStream
                                     , initialStream
                                     , splitStream
                                     ) where

import Data.HamtMap (HamtMap)

import Data.LLVM.Types


type SymbolTable = HamtMap Identifier Value

type IdStream = [Integer]

splitStream :: IdStream -> (IdStream, IdStream)
splitStream (x:xs) = (x:bs, as)
  where (as, bs) = splitStream xs

initialStream :: IdStream
initialStream = [ 0 .. ]
