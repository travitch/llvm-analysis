import Data.List (unzip5, zip4)
import Data.Monoid (mconcat)
import Test.HUnit

import Data.LLVM
import Data.LLVM.AssemblyParser
import Data.LLVM.Private.AttributeTypes
import Data.LLVM.Private.PlaceholderTypes

identParser = runLLVMParser identifierParser
identTests = [ ("localIdentNamed", assertEqual, "parse %local", LocalIdentifier "local", identParser "%local")
             , ("localIdentUnnamed", assertEqual, "parse %123", LocalIdentifier "123", identParser "%123")
             , ("localIdentWithDot", assertEqual, "parse %local.ident", LocalIdentifier "local.ident", identParser "%local.ident")
             , ("localIdentWithUnder", assertEqual, "parse %local_ident", LocalIdentifier "local_ident", identParser "%local_ident")
             , ("localIdentWithDollar", assertEqual, "parse %local$ident", LocalIdentifier "local$ident", identParser "%local$ident")
             , ("localIdentQuoted", assertEqual, "parse %\"local ident\"", LocalIdentifier "local ident", identParser "%\"local ident\"")
               -- Global Identifiers
             , ("globalIdentNamed", assertEqual, "parse @global", GlobalIdentifier "global", identParser "@global")
             , ("globalIdentUnnamed", assertEqual, "parse @123", GlobalIdentifier "123", identParser "@123")
             , ("globalIdentWithDot", assertEqual, "parse @global.ident", GlobalIdentifier "global.ident", identParser "@global.ident")
             , ("globalIdentWithUnder", assertEqual, "parse @global_ident", GlobalIdentifier "global_ident", identParser "@global_ident")
             , ("globalIdentWithDollar", assertEqual, "parse @global$ident", GlobalIdentifier "global$ident", identParser "@global$ident")
             , ("globalIdentQuoted", assertEqual, "parse @\"global ident\"", GlobalIdentifier "global ident", identParser "@\"global ident\"")
             ]


ccParser = runLLVMParser callingConventionParser
ccTests = [ ("ccCCC", assertEqual, "parse ccc", CCC, ccParser "ccc")
          , ("ccFastCC", assertEqual, "parse fastcc", CCFastCC, ccParser "fastcc")
          , ("ccColdCC", assertEqual, "parse coldcc", CCColdCC, ccParser "coldcc")
          , ("ccGHC", assertEqual, "parse cc 10 (ghc)", CCGHC, ccParser "cc 10")
          , ("ccN100", assertEqual, "parse cc 100", CCN 100, ccParser "cc 100")
          ]

makeTestList tests = zipWith TestLabel names tests'
  where (names, assertions, msgs, results, actions) = unzip5 tests
        expected = map Just results
        tests' = map toTestCase $ zip4 assertions msgs expected actions
        toTestCase (assertion, msg, expected, actual) =
          TestCase $ assertion msg expected actual

main = runTestTT theTests
  where theTests = TestList listOfTests
        listOfTests = mconcat [ makeTestList identTests
                              , makeTestList ccTests
                              ]