module Data.LLVM.Private.Printers ( printMetadata
                                  , printModule
                                  , printType
                                  , printValue
                                  ) where

import Data.List (intercalate)
import Data.Maybe (maybe)
import Data.Monoid
import Data.Text (unpack)

import Data.LLVM.Private.AttributeTypes
import Data.LLVM.Private.ReferentialTypes

-- FIXME: implement this large thing
printMetadata :: Metadata -> String
printMetadata _ = "metadata"

printModule :: Module -> String
printModule Module { moduleDataLayout = layout
                   , moduleTarget = triple
                   , moduleAssembly = asm
                   , moduleGlobals = vals
                   } =
  mconcat [ layoutS, "\n", tripleS, "\n", asmS, "\n", valS, "\n" ]
  where layoutS = mconcat [ "target datalayout = \"", show layout, "\"" ]
        tripleS = mconcat [ "target triple = \"", show triple, "\"" ]
        asmS = printAsm asm
        valS = mconcat $ map printValue vals

-- Take all of the asm chunks, break their contents into lines,
-- then wrap each of those lines in the 'module asm' wrapper.
-- Combine them into a single string with newlines.
printAsm :: [Assembly] -> String
printAsm asm = mconcat asmLines
  where chunks = map show asm
        block = mconcat chunks
        asmLines = map adorn (lines block)
        adorn s = "module asm \"" ++ s ++ "\"\n"

-- When referencing a non-constant value during printing, just use
-- this instead of printValue to avoid problems printing cyclic data.
printName :: Value -> String
printName Value { valueName = Just ident
                , valueType = t
                } =
  mconcat [ printType t, " ", show ident ]
printName other = error $ "Cannot show the name of " ++ printValue other


compose :: [String] -> String
compose = intercalate " " . filter (not . null)

quote :: String -> String
quote s = mconcat [ "\"", s, "\"" ]

printValue :: Value -> String
printValue Value { valueContent =
                      Function { functionType = t
                               , functionParameters = args
                               , functionBody = blockList
                               , functionLinkage = linkage
                               , functionVisibility = visStyle
                               , functionCC = cc
                               , functionRetAttrs = retAttrs
                               , functionName = name
                               , functionSection = section
                               , functionAlign = align
                               , functionGCName = gcname
                               , functionIsVararg = isVararg
                               }
                 } =
  compose [ "define", show linkage, show visStyle, show cc,
            retAttrS, printType rtype, show name, "(",
            argS, ")", fAttrS, maybe "" unpack section,
            "align", show align, maybe "" show gcname, "{\n",
            bodyS, "}" ]
  where retAttrS = intercalate " " $ map show retAttrs
        argS = intercalate ", " $ map printValue args
        vaTag = if isVararg then ", ..." else ""
        fAttrS = intercalate " " $ map show fattrs
        bodyS = unlines $ map printValue blockList
        (TypeFunction rtype _ _ fattrs) = t

printValue Value { valueContent =
                      GlobalDeclaration { globalVariableAddressSpace = addrSpace
                                        , globalVariableAnnotations = annots
                                        , globalVariableInitializer = initializer
                                        , globalVariableAlignment = align
                                        , globalVariableSection = section
                                        }
                 , valueType = t
                 , valueName = name
                 , valueMetadata = md
                 } =
  compose [ show name, addrSpaceS, annotsS, -- Don't show t here since
                                            -- showing the initializer
                                            -- will handle it
            printValue initializer, sectionS, alignS ]
  where addrSpaceS = case addrSpace of
          0 -> ""
          _ -> "addrspace(" ++ show addrSpace ++ ")"
        annotsS = intercalate " " $ map show annots
        sectionS = maybe "" ((", section "++) . quote . unpack) section
        alignS = case align of
          0 -> ""
          _ -> ", align " ++ show align

printType :: Type -> String
printType (TypeInteger bits) = "i" ++ show bits
printType TypeFloat = "float"
printType TypeDouble = "double"
printType TypeFP128 = "fp128"
printType TypeX86FP80 = "x86_fp80"
printType TypePPCFP128 = "ppc_fp128"
printType TypeX86MMX = "x86mmx"
printType TypeVoid = "void"
printType TypeLabel = "label"
printType TypeMetadata = "metadata"
printType (TypeArray n ty) = mconcat [ "[", show n, " x ", printType ty, "]" ]
printType (TypeVector n ty) = mconcat [ "<", show n, " x ", printType ty, ">" ]
-- FIXME: Put attrs in here somewhere
printType (TypeFunction retT argTs isVa attrs) =
  mconcat [ printType retT, "(", argVals, vaTag, ")" ]
  where argVals :: String
        argVals = intercalate ", " $ map printType argTs
        vaTag :: String
        vaTag = if isVa then ", ..." else ""
printType TypeOpaque = "opaque"
printType (TypePointer ty) = mconcat [ printType ty, "*" ]
printType (TypeStruct ts) = mconcat [ "{", fieldVals, "}" ]
  where fieldVals = intercalate ", " $ map printType ts
printType (TypePackedStruct ts) = mconcat [ "{", fieldVals, "}" ]
  where fieldVals = intercalate ", " $ map printType ts

