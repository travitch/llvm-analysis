module Main ( main ) where

import Data.Hashable
import Data.TransitiveClosure
import Data.HashSet ( HashSet )
import qualified Data.HashSet as S

import Test.Framework ( defaultMain, testGroup )
import Test.Framework.Providers.HUnit
import Test.HUnit

type Set = HashSet

main :: IO ()
main = defaultMain tests

tests = [ testGroup "TClosChecks" [
             testCase "test1" test1,
             testCase "test2" test2,
             testCase "test3" test3,
             testCase "test4" test4
             ]
        ]

data Value = Global Int
           | Phi Int [Value]
           deriving (Ord)

valueId :: Value -> Int
valueId (Global i) = i
valueId (Phi i _) = i

instance Eq Value where
  v1 == v2 = valueId v1 == valueId v2

instance Show Value where
  show (Global i) = "Global " ++ show i
  show (Phi i _) = "Phi " ++ show i

instance Hashable Value where
  hash (Global i) = i
  hash (Phi i _) = i

tester :: Value -> Set Int
tester = S.fromList . map valueId . markVisited tester' . (:[])
  where
    tester' v =
      case v of
        Global _ -> [v] -- S.singleton v
        Phi _ vs -> vs -- S.fromList vs

test1 :: Assertion
test1 = assertEqual "test1" expected (tester v)
  where
    v = Global 1
    expected = S.singleton 1

test2 :: Assertion
test2 = assertEqual "test2" expected (tester v)
  where
    v = Phi 1 [Global 2, Global 3]
    expected = S.fromList [2, 3, 1]

test3 :: Assertion
test3 = assertEqual "test3" expected (tester v)
  where
    p1 = Phi 1 [Global 2, Global 3, p2]
    p2 = Phi 4 [Global 5, Global 6]
    v = Phi 7 [p1, p2]
    expected = S.fromList [1,2,3,4,5,6,7]

test4 :: Assertion
test4 = assertEqual "test4" expected (tester v)
  where
    p1 = Phi 1 [Global 2, Global 3, p2]
    p2 = Phi 4 [Global 5, Global 6, p1]
    v = Phi 7 [p1, p2]
    expected = S.fromList [1,2,3,4,5,6,7]