module Main ( main ) where

import Data.TransitiveClosure
import Data.Set ( Set )
import qualified Data.Set as S

import Test.Framework ( defaultMain, testGroup )
import Test.Framework.Providers.HUnit
import Test.HUnit

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
           deriving (Show, Ord)

valueId :: Value -> Int
valueId (Global i) = i
valueId (Phi i _) = i

instance Eq Value where
  v1 == v2 = valueId v1 == valueId v2

tester :: Value -> Set Int
tester = S.map valueId . markVisited tester'
  where
    tester' v =
      case v of
        Global _ -> S.singleton v
        Phi _ vs -> S.fromList vs

test1 :: Assertion
test1 = assertEqual "test1" (tester v) expected
  where
    v = Global 1
    expected = S.singleton 1

test2 :: Assertion
test2 = assertEqual "test2" (tester v) expected
  where
    v = Phi 1 [Global 2, Global 3]
    expected = S.fromList [2, 3]

test3 :: Assertion
test3 = assertEqual "test3" (tester v) expected
  where
    p1 = Phi 1 [Global 2, Global 3, p2]
    p2 = Phi 4 [Global 5, Global 6]
    v = Phi 7 [p1, p2]
    expected = S.fromList [1,2,3,4,5,6]

test4 :: Assertion
test4 = assertEqual "test4" (tester v) expected
  where
    p1 = Phi 1 [Global 2, Global 3, p2]
    p2 = Phi 4 [Global 5, Global 6, p1]
    v = Phi 7 [p1, p2]
    expected = S.fromList [1,2,3,4,5,6]