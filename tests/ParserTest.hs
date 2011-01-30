import Data.List (unzip5, zip4)
import Data.Monoid (mconcat)
import Test.HUnit

import Data.LLVM
import Data.LLVM.Private.AssemblyParser
import Data.LLVM.Private.AttributeTypes
import Data.LLVM.Private.ParsingMonad
import Data.LLVM.Private.PlaceholderTypes

identParser = maybeRunLLVMParser parseIdentifier
identTests = [ ("localIdentNamed", assertEqual, "parse %local", makeLocalIdentifier "local", identParser "%local")
             , ("localIdentUnnamed", assertEqual, "parse %123", makeLocalIdentifier "123", identParser "%123")
             , ("localIdentWithDot", assertEqual, "parse %local.ident", makeLocalIdentifier "local.ident", identParser "%local.ident")
             , ("localIdentWithUnder", assertEqual, "parse %local_ident", makeLocalIdentifier "local_ident", identParser "%local_ident")
             , ("localIdentWithDollar", assertEqual, "parse %local$ident", makeLocalIdentifier "local$ident", identParser "%local$ident")
             , ("localIdentQuoted", assertEqual, "parse %\"local ident\"", makeLocalIdentifier "local ident", identParser "%\"local ident\"")
               -- Global Identifiers
             , ("globalIdentNamed", assertEqual, "parse @global", makeGlobalIdentifier "global", identParser "@global")
             , ("globalIdentUnnamed", assertEqual, "parse @123", makeGlobalIdentifier "123", identParser "@123")
             , ("globalIdentWithDot", assertEqual, "parse @global.ident", makeGlobalIdentifier "global.ident", identParser "@global.ident")
             , ("globalIdentWithUnder", assertEqual, "parse @global_ident", makeGlobalIdentifier "global_ident", identParser "@global_ident")
             , ("globalIdentWithDollar", assertEqual, "parse @global$ident", makeGlobalIdentifier "global$ident", identParser "@global$ident")
             , ("globalIdentQuoted", assertEqual, "parse @\"global ident\"", makeGlobalIdentifier "global ident", identParser "@\"global ident\"")
             ]


ccParser = maybeRunLLVMParser parseCallingConvention
ccTests = [ ("ccCCC", assertEqual, "parse ccc", CCC, ccParser "ccc")
          , ("ccFastCC", assertEqual, "parse fastcc", CCFastCC, ccParser "fastcc")
          , ("ccColdCC", assertEqual, "parse coldcc", CCColdCC, ccParser "coldcc")
          , ("ccGHC", assertEqual, "parse cc 10 (ghc)", CCGHC, ccParser "cc 10")
          , ("ccN100", assertEqual, "parse cc 100", CCN 100, ccParser "cc 100")
          ]

gcParser = maybeRunLLVMParser parseGCName
gcTests = [ ("gcName", assertEqual, "parse gc \"FooBar\"", GCName "FooBar", gcParser "gc \"FooBar\"") ]

typeParser = maybeRunLLVMParser parseType
typeTests = [ ("typeBool", assertEqual, "parse i1", TypeInteger 1, typeParser "i1")
            , ("typeInt8", assertEqual, "parse i8", TypeInteger 8, typeParser "i8")
            , ("typeInt64", assertEqual, "parse i64", TypeInteger 64, typeParser "i64")
            , ("typeIntHuge", assertEqual, "parse i8192", TypeInteger 8192, typeParser "i8192")
            , ("typeFloat", assertEqual, "parse float", TypeFloat, typeParser "float")
            , ("typeDouble", assertEqual, "parse double", TypeDouble, typeParser "double")
            , ("typex86_fp80", assertEqual, "parse x86_fp80", TypeX86FP80, typeParser "x86_fp80")
            , ("typefp128", assertEqual, "parse fp128", TypeFP128, typeParser "fp128")
            , ("typeppc_fp128", assertEqual, "parse ppc_fp128", TypePPCFP128, typeParser "ppc_fp128")
            , ("typemmx", assertEqual, "parse x86mmx", TypeX86MMX, typeParser "x86mmx")
            , ("typevoid", assertEqual, "parse void", TypeVoid, typeParser "void")
            , ("typelabel", assertEqual, "parse label", TypeLabel, typeParser "label")
            , ("typemetadata", assertEqual, "parse metadata", TypeMetadata, typeParser "metadata")
            , ("typeOpaque", assertEqual, "parse opaque", TypeOpaque, typeParser "opaque")
            , ("typeArray1", assertEqual, "parse [ 1 x i1 ]", TypeArray 1 (TypeInteger 1), typeParser "[1 x i1]")
            , ("typeArray2", assertEqual, "parse [ 100 x i128 ]", TypeArray 100 (TypeInteger 128), typeParser "[100 x i128]")
            , ("typeArray3", assertEqual, "parse [ 66 x double ]", TypeArray 66 TypeDouble, typeParser "[66 x double]")
            , ("typeArray4", assertEqual, "parse [ 15 x [ 10 x float ] ]", TypeArray 15 (TypeArray 10 TypeFloat), typeParser "[15 x [10 x float]]")
            , ("typeArray5", assertEqual, "parse [ 5 x [4 x [3 x i32]] ]", TypeArray 5 (TypeArray 4 (TypeArray 3 (TypeInteger 32))), typeParser "[ 5 x [4 x [3 x i32]] ]")
            , ("typeVector1", assertEqual, "parse < 1 x i1 >", TypeVector 1 (TypeInteger 1), typeParser "<1 x i1>")
            , ("typeVector2", assertEqual, "parse < 100 x i128 >", TypeVector 100 (TypeInteger 128), typeParser "<100 x i128>")
            , ("typeVector3", assertEqual, "parse < 66 x double >", TypeVector 66 TypeDouble, typeParser "<66 x double>")
            , ("typeVector4", assertEqual, "parse < 15 x < 10 x float > >", TypeVector 15 (TypeVector 10 TypeFloat), typeParser "<15 x <10 x float>>")
            , ("typeVector5", assertEqual, "parse < 5 x <4 x <3 x i32>> >", TypeVector 5 (TypeVector 4 (TypeVector 3 (TypeInteger 32))), typeParser "< 5 x <4 x <3 x i32>> >")
            , ("typeFuncAbs", assertEqual, "parse i32(i32)", TypeFunction (TypeInteger 32) [TypeInteger 32] False, typeParser "i32(i32)")
            , ("typePrintf", assertEqual, "parse i32(i8*,...)", TypeFunction (TypeInteger 32) [TypePointer (TypeInteger 8)] True, typeParser "i32(i8*,...)")
            , ("typeFuncFunc", assertEqual, "parse i32(i32)(i32)", TypeFunction (TypeFunction (TypeInteger 32) [TypeInteger 32] False) [TypeInteger 32] False, typeParser "i32(i32)(i32)")
            , ("typeFuncPtr", assertEqual, "parse i32(i32)*", TypePointer (TypeFunction (TypeInteger 32) [TypeInteger 32] False), typeParser "i32(i32)*")
            , ("typeStruct1", assertEqual, "parse {i32}", TypeStruct [TypeInteger 32], typeParser "{i32}")
            , ("typeStruct2", assertEqual, "parse {i32, double}", TypeStruct [TypeInteger 32, TypeDouble], typeParser "{i32, double}")
            , ("typeStruct3", assertEqual, "parse { { double} }", TypeStruct [TypeStruct [TypeDouble]], typeParser "{{double}}")
            , ("typePackedstruct1", assertEqual, "parse <{i32}>", TypePackedStruct [TypeInteger 32], typeParser "<{i32}>")
            , ("typePackedstruct2", assertEqual, "parse <{i32, double}>", TypePackedStruct [TypeInteger 32, TypeDouble], typeParser "<{i32, double}>")
            , ("typePackedstruct3", assertEqual, "parse <{ <{ double}> }>", TypePackedStruct [TypePackedStruct [TypeDouble]], typeParser "<{<{double}>}>")
            , ("typePointer1", assertEqual, "parse i32*", TypePointer (TypeInteger 32), typeParser "i32*")
              -- FIXME: Need some type upref tests.  Also, want to eliminate uprefs while tying the knot
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
                              , makeTestList gcTests
                              , makeTestList typeTests
                              ]