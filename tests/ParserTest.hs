{-# LANGUAGE OverloadedStrings, ExplicitForAll, NoMonomorphismRestriction #-}
import Data.List ( unzip5, zip4 )
import Data.Monoid ( mconcat )
import Test.Framework ( defaultMain, testGroup, Test )
import Test.Framework.Providers.HUnit
import Test.HUnit hiding ( Test, test )

import Data.LLVM
import Data.LLVM.Types hiding ( Type(..) )
import Data.LLVM.Private.Testing.Parser
import Data.LLVM.Private.Testing.Types

identParser = maybeRunLLVMParser identifierP
identTests = [ ("localIdentNamed", "parse %local", makeLocalIdentifier "local", identParser "%local")
             , ("localIdentUnnamed", "parse %123", makeLocalIdentifier "123", identParser "%123")
             , ("localIdentWithDot", "parse %local.ident", makeLocalIdentifier "local.ident", identParser "%local.ident")
             , ("localIdentWithUnder", "parse %local_ident", makeLocalIdentifier "local_ident", identParser "%local_ident")
             , ("localIdentWithDollar", "parse %local$ident", makeLocalIdentifier "local$ident", identParser "%local$ident")
             , ("localIdentQuoted", "parse %\"local ident\"", makeLocalIdentifier "local ident", identParser "%\"local ident\"")
               -- Global Identifiers
             , ("globalIdentNamed", "parse @global", makeGlobalIdentifier "global", identParser "@global")
             , ("globalIdentUnnamed", "parse @123", makeGlobalIdentifier "123", identParser "@123")
             , ("globalIdentWithDot", "parse @global.ident", makeGlobalIdentifier "global.ident", identParser "@global.ident")
             , ("globalIdentWithUnder", "parse @global_ident", makeGlobalIdentifier "global_ident", identParser "@global_ident")
             , ("globalIdentWithDollar", "parse @global$ident", makeGlobalIdentifier "global$ident", identParser "@global$ident")
             , ("globalIdentQuoted", "parse @\"global ident\"", makeGlobalIdentifier "global ident", identParser "@\"global ident\"")
             ]


ccParser = maybeRunLLVMParser callingConventionP
ccTests = [ ("ccCCC", "parse ccc", CCC, ccParser "ccc")
          , ("ccFastCC", "parse fastcc", CCFastCC, ccParser "fastcc")
          , ("ccColdCC", "parse coldcc", CCColdCC, ccParser "coldcc")
          , ("ccGHC", "parse cc 10 (ghc)", CCGHC, ccParser "cc 10")
          , ("ccN100", "parse cc 100", CCN 100, ccParser "cc 100")
          ]

gcParser = maybeRunLLVMParser gcNameP
gcTests = [ ("gcName", "parse gc \"FooBar\"", GCName "FooBar", gcParser "gc \"FooBar\"") ]

typeParser = maybeRunLLVMParser typeP
typeTests = [ ("typeBool", "parse i1", TypeInteger 1, typeParser "i1")
            , ("typeInt8", "parse i8", TypeInteger 8, typeParser "i8")
            , ("typeInt64", "parse i64", TypeInteger 64, typeParser "i64")
            , ("typeIntHuge", "parse i8192", TypeInteger 8192, typeParser "i8192")
            , ("typeFloat", "parse float", TypeFloat, typeParser "float")
            , ("typeDouble", "parse double", TypeDouble, typeParser "double")
            , ("typex86_fp80", "parse x86_fp80", TypeX86FP80, typeParser "x86_fp80")
            , ("typefp128", "parse fp128", TypeFP128, typeParser "fp128")
            , ("typeppc_fp128", "parse ppc_fp128", TypePPCFP128, typeParser "ppc_fp128")
            , ("typemmx", "parse x86mmx", TypeX86MMX, typeParser "x86mmx")
            , ("typevoid", "parse void", TypeVoid, typeParser "void")
            , ("typelabel", "parse label", TypeLabel, typeParser "label")
            , ("typemetadata", "parse metadata", TypeMetadata, typeParser "metadata")
            , ("typeOpaque", "parse opaque", TypeOpaque, typeParser "opaque")
            , ("typeArray1", "parse [ 1 x i1 ]", TypeArray 1 (TypeInteger 1), typeParser "[1 x i1]")
            , ("typeArray2", "parse [ 100 x i128 ]", TypeArray 100 (TypeInteger 128), typeParser "[100 x i128]")
            , ("typeArray3", "parse [ 66 x double ]", TypeArray 66 TypeDouble, typeParser "[66 x double]")
            , ("typeArray4", "parse [ 15 x [ 10 x float ] ]", TypeArray 15 (TypeArray 10 TypeFloat), typeParser "[15 x [10 x float]]")
            , ("typeArray5", "parse [ 5 x [4 x [3 x i32]] ]", TypeArray 5 (TypeArray 4 (TypeArray 3 (TypeInteger 32))), typeParser "[ 5 x [4 x [3 x i32]] ]")
            , ("typeVector1", "parse < 1 x i1 >", TypeVector 1 (TypeInteger 1), typeParser "<1 x i1>")
            , ("typeVector2", "parse < 100 x i128 >", TypeVector 100 (TypeInteger 128), typeParser "<100 x i128>")
            , ("typeVector3", "parse < 66 x double >", TypeVector 66 TypeDouble, typeParser "<66 x double>")
            , ("typeVector4", "parse < 15 x < 10 x float > >", TypeVector 15 (TypeVector 10 TypeFloat), typeParser "<15 x <10 x float>>")
            , ("typeVector5", "parse < 5 x <4 x <3 x i32>> >", TypeVector 5 (TypeVector 4 (TypeVector 3 (TypeInteger 32))), typeParser "< 5 x <4 x <3 x i32>> >")
            , ("typeFuncAbs", "parse i32(i32)", TypeFunction (TypeInteger 32) [TypeInteger 32] False, typeParser "i32(i32)")
            , ("typePrintf", "parse i32(i8*,...)", TypeFunction (TypeInteger 32) [TypePointer (TypeInteger 8)] True, typeParser "i32(i8*,...)")
            , ("typeFuncFunc", "parse i32(i32)(i32)", TypeFunction (TypeFunction (TypeInteger 32) [TypeInteger 32] False) [TypeInteger 32] False, typeParser "i32(i32)(i32)")
            , ("typeFuncFunc2", "parse i32(i32)(double)", TypeFunction (TypeFunction (TypeInteger 32) [TypeInteger 32] False) [TypeDouble] False, typeParser "i32(i32)(double)")
            , ("typeFuncPtr", "parse i32(i32)*", TypePointer (TypeFunction (TypeInteger 32) [TypeInteger 32] False), typeParser "i32(i32)*")
            , ("typeStruct1", "parse {i32}", TypeStruct [TypeInteger 32], typeParser "{i32}")
            , ("typeStruct2", "parse {i32, double}", TypeStruct [TypeInteger 32, TypeDouble], typeParser "{i32, double}")
            , ("typeStruct3", "parse { { double} }", TypeStruct [TypeStruct [TypeDouble]], typeParser "{{double}}")
            , ("typePackedstruct1", "parse <{i32}>", TypePackedStruct [TypeInteger 32], typeParser "<{i32}>")
            , ("typePackedstruct2", "parse <{i32, double}>", TypePackedStruct [TypeInteger 32, TypeDouble], typeParser "<{i32, double}>")
            , ("typePackedstruct3", "parse <{ <{ double}> }>", TypePackedStruct [TypePackedStruct [TypeDouble]], typeParser "<{<{double}>}>")
            , ("typePointer1", "parse i32*", TypePointer (TypeInteger 32), typeParser "i32*")
              -- FIXME: Need some type upref tests.  Also, want to eliminate uprefs while tying the knot
            ]

makeTestList :: (Show a, Eq a) => [(String, String, a, Maybe a)] -> [Test]
makeTestList tests = map mkTest tests
  where
    mkTest (name, msg, expected, action) =
      testCase name $ assertEqual msg (Just expected) action

main :: IO ()
main = defaultMain listOfTests
  where
    listOfTests = mconcat [ makeTestList identTests
                          , makeTestList ccTests
                          , makeTestList gcTests
                          , makeTestList typeTests
                          ]
