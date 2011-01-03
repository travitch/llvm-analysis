module Data.LLVM.Private.PlaceholderTypeExtractors ( getInt
                                                   , getBool
                                                   , getMDString
                                                   ) where

import Data.LLVM.Private.PlaceholderTypes

getInt (ConstValue (ConstantInt i) (TypeInteger 32)) = i
getInt c = error ("Constant is not an int: " ++ show c)

getBool (ConstValue (ConstantInt i) (TypeInteger 1)) = i == 1
getBool c = error ("Constant is not a bool: " ++ show c)

getMDString (ConstValue (MDString txt) TypeMetadata) = txt
getMDString c = error ("Not a constant metadata string: " ++ show c)
