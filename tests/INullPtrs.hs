{-# LANGUAGE MultiParamTypeClasses #-}
-- | An interprocedural may-be-null pointer analysis.
module Main ( main ) where

import System.Environment ( getArgs )
import Data.LLVM
import Data.LLVM.ICFG
import Data.LLVM.Analysis.IFDS
import Data.LLVM.Analysis.PointsTo.TrivialFunction

data INullPtr = INullPtr

instance IFDSAnalysis INullPtr Value where
  flow = inullFlow
  callFlow = inullCallFlow
  passArgs = inullPassArgs
  externPassArgs = inullExternPassArgs
  returnVal = inullReturnVal
  externReturnVal = inullExternReturnVal

inullFlow :: INullPtr -> Maybe Value -> Instruction -> [CFGEdge] -> [Maybe Value]
inullFlow _ v i edges = undefined

inullCallFlow :: INullPtr -> Maybe Value -> Instruction -> [CFGEdge] -> [Maybe Value]
inullCallFlow _ v i edges = undefined

inullPassArgs :: INullPtr -> Maybe Value -> Instruction -> Function -> [Maybe Value]
inullPassArgs _ v i f = undefined

inullExternPassArgs :: INullPtr -> Maybe Value -> Instruction -> Maybe ExternalFunction -> [Maybe Value]
inullExternPassArgs _ v i ef = undefined

inullReturnVal :: INullPtr -> Maybe Value -> Instruction -> [Maybe Value]
inullReturnVal _ v i = undefined

inullExternReturnVal :: INullPtr -> Maybe Value -> Maybe ExternalFunction -> [Maybe Value]
inullExternReturnVal _ v ef = undefined

main :: IO ()
main = do
  [fname] <- getArgs
  Right m <- parseLLVMBitcodeFile defaultParserOptions fname
  let pta = runPointsToAnalysis m
      icfg = mkICFG m pta (maybe [] (:[]) (findMain m))
      analysis = INullPtr
      res :: IFDSResult Value
      res = ifds analysis icfg
  putStrLn ""


