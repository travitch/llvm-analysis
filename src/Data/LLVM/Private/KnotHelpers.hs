module Data.LLVM.Private.KnotHelpers ( IdentDict
                                     , addGlobal
                                     , addLocal
                                     , emptyDict
                                     , getFunctionLocals
                                     , getGlobals
                                     , nextSequenceNumber
                                     ) where

import Data.Map (Map, (!))
import qualified Data.Map as M

import Data.LLVM.Types
import Debug.Trace
debug = flip trace
-- GlobalMap, Func->Locals Map
data IdentDict = IdentDict { globalValues :: Map Identifier Value
                           , functionLocals :: Map Identifier (Map Identifier Value)
                           , sequenceNumber :: Integer
                           }
               deriving (Show)

emptyDict :: IdentDict
emptyDict = IdentDict { globalValues = M.empty
                      , functionLocals = M.empty
                      , sequenceNumber = 0
                      }

addGlobal :: Identifier -> Value -> IdentDict -> IdentDict
addGlobal name val d@IdentDict { globalValues = gvs } =
  d { globalValues = M.insert name val gvs }

addLocal :: Identifier -> Identifier -> Value -> IdentDict -> IdentDict
addLocal func name val d@IdentDict { functionLocals = locals } =
  d { functionLocals = updatedData }
  where funcMap = locals ! func
        updatedFuncMap = M.insert name val funcMap
        updatedData = M.insert func updatedFuncMap locals `debug` ("Map Sizes: " ++ show (M.size funcMap) ++ " -> " ++ show (M.size updatedFuncMap))

getFunctionLocals :: Identifier -> IdentDict -> Map Identifier Value
getFunctionLocals func IdentDict { functionLocals = locals } = locals ! func

getGlobals :: IdentDict -> Map Identifier Value
getGlobals IdentDict { globalValues = gvs } = gvs

nextSequenceNumber :: IdentDict -> (Integer, IdentDict)
nextSequenceNumber d@IdentDict { sequenceNumber = n } = (n, nxt `debug` ("ID: " ++ show n))
  where nxt = d { sequenceNumber = n + 1 }
