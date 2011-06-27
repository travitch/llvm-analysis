{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | Define the unparser for our LLVM IR
module Data.LLVM.Private.Printers ( printMetadata
                                  , printAsm
                                  , printType
                                  , printValue
                                  ) where

import Data.Int
import Data.List ( intercalate )
import Data.Monoid
import Data.ByteString.Char8 ( ByteString, unpack )

import Data.LLVM.Private.Types.Attributes
import Data.LLVM.Private.Types.Identifiers
import Data.LLVM.Private.Types.Referential

-- TODO List
--
-- * Pretty up the DataLayout
-- * Print out named type definitions
-- * Make the function type printing as flexible as the official
--   version

showUntypedMDName :: Metadata -> String
showUntypedMDName = (show . fj . metaValueName)
  where
    fj (Just v) = v
    fj Nothing = error "No metadata value name in showUntypedMDName"

showMDName :: Metadata -> String
showMDName md = "metadata " ++ (show . fj . metaValueName) md
  where
    fj (Just v) = v
    fj Nothing = error "No Metadata value name in showMDName"

showMDString :: ByteString -> String
showMDString bs = "metadata !" ++ show bs

showBool :: Bool -> String
showBool True = "i1 true"
showBool False = "i1 false"

maybeShowMDName :: Maybe Metadata -> String
maybeShowMDName Nothing = "null"
maybeShowMDName (Just m) = showMDName m

dbgTag :: Int -> String
dbgTag i = show (i + fromIntegral llvmDebugVersion)

printMetadata :: Metadata -> String
printMetadata md@Metadata { metaValueContent = sl@MetaSourceLocation { } } =
  mconcat [ showUntypedMDName md, " = metadata !{i32 ", show (metaSourceRow sl)
          , ", i32 ", show (metaSourceCol sl)
          , ", ", showMDName (metaSourceScope sl)
          , " null}"
          ]
printMetadata md@Metadata { metaValueContent = lb@MetaDWLexicalBlock { } } =
  mconcat [ showUntypedMDName md, " = metadata !{i32 ", dbgTag 11
          , ", i32 ", show (metaLexicalBlockRow lb)
          , ", i32 ", show (metaLexicalBlockCol lb)
          , ", ", showMDName (metaLexicalBlockContext lb)
          , ", ", showMDName (metaLexicalBlockFile lb)
          , ", i32 ", show (metaLexicalBlockDepth lb), "}"
          ]
printMetadata md@Metadata { metaValueContent = cu@MetaDWCompileUnit {} } =
  mconcat [ showUntypedMDName md, " = metadata !{i32 ", dbgTag 17
          , ", i32 ", show (metaCompileUnitLanguage cu)
          , ", ", showMDString (metaCompileUnitSourceFile cu)
          , ", ", showMDString (metaCompileUnitCompileDir cu)
          , ", ", showMDString (metaCompileUnitProducer cu)
          , ", ", showBool (metaCompileUnitIsMain cu)
          , ", ", showBool (metaCompileUnitIsOpt cu)
          , ", i32 ", show (metaCompileUnitVersion cu), "}"
          ]
printMetadata md@Metadata { metaValueContent = f@MetaDWFile {} } =
  mconcat [ showUntypedMDName md, " = metadata !{i32 ", dbgTag 41
          , ", ", showMDString (metaFileSourceFile f)
          , ", ", showMDString (metaFileSourceDir f)
          , ", ", showMDName (metaFileCompileUnit f), "}"
          ]
printMetadata md@Metadata { metaValueContent = v@MetaDWVariable {} } =
  mconcat [ showUntypedMDName md, " = metadata !{i32 ", dbgTag 52
          , ", ", showMDName (metaGlobalVarContext v)
          , ", ", showMDString (metaGlobalVarName v)
          , ", ", showMDString (metaGlobalVarDisplayName v)
          , ", ", showMDString (metaGlobalVarLinkageName v)
          , ", ", showMDName (metaGlobalVarFile v)
          , ", i32 ", show (metaGlobalVarLine v)
          , ", ", showMDName (metaGlobalVarType v)
          , ", ", showBool (metaGlobalVarStatic v)
          , ", ", showBool (metaGlobalVarNotExtern v), "}"
          ]
printMetadata md@Metadata { metaValueContent = sp@MetaDWSubprogram {} } =
  mconcat [ showUntypedMDName md, " = metadata !{i32 ", dbgTag 46
          , ", ", showMDName (metaSubprogramContext sp)
          , ", ", showMDString (metaSubprogramName sp)
          , ", ", showMDString (metaSubprogramDisplayName sp)
          , ", ", showMDString (metaSubprogramLinkageName sp)
          , ", ", showMDName (metaSubprogramFile sp)
          , ", i32 ", show (metaSubprogramLine sp)
          , ", ", showMDName (metaSubprogramType sp)
          , ", ", showBool (metaSubprogramStatic sp)
          , ", ", showBool (metaSubprogramNotExtern sp)
          , ", i32 ", show (metaSubprogramVirtuality sp)
          , ", i32 ", show (metaSubprogramVirtIndex sp)
          , ", ", maybeShowMDName (metaSubprogramBaseType sp)
          , ", ", showBool (metaSubprogramArtificial sp)
          , ", ", showBool (metaSubprogramOptimized sp), "}"
          ]
printMetadata md@Metadata { metaValueContent = bt@MetaDWBaseType {} } =
  mconcat [ showUntypedMDName md, " = metadata !{i32 ", dbgTag 36
          , ", ", showMDName (metaBaseTypeContext bt)
          , ", ", showMDString (metaBaseTypeName bt)
          , ", ", maybeShowMDName (metaBaseTypeFile bt)
          , ", i32 ", show (metaBaseTypeLine bt)
          , ", i32 ", show (metaBaseTypeSize bt)
          , ", i32 ", show (metaBaseTypeAlign bt)
          , ", i64 ", show (metaBaseTypeOffset bt)
          , ", i32 ", show (metaBaseTypeFlags bt)
          , ", i32 ", show (metaBaseTypeEncoding bt), "}"
          ]
printMetadata md@Metadata { metaValueContent = dt@MetaDWDerivedType {} } =
  mconcat [ showUntypedMDName md, " = metadata !{i32 ", show (metaDerivedTypeTag dt)
          , ", ", showMDName (metaDerivedTypeContext dt)
          , ", ", showMDString (metaDerivedTypeName dt)
          , ", ", maybeShowMDName (metaDerivedTypeFile dt)
          , ", i32 ", show (metaDerivedTypeLine dt)
          , ", i32 ", show (metaDerivedTypeSize dt)
          , ", i32 ", show (metaDerivedTypeAlign dt)
          , ", i64 ", show (metaDerivedTypeOffset dt)
          , ", ", maybeShowMDName (metaDerivedTypeParent dt), "}"
          ]
printMetadata md@Metadata { metaValueContent = ct@MetaDWCompositeType {} } =
  mconcat [ showUntypedMDName md, " = metadata !{i32 ", show (metaCompositeTypeTag ct)
          , ", ", showMDName (metaCompositeTypeContext ct)
          , ", ", showMDString (metaCompositeTypeName ct)
          , ", ", maybeShowMDName (metaCompositeTypeFile ct)
          , ", i32 ", show (metaCompositeTypeLine ct)
          , ", i32 ", show (metaCompositeTypeSize ct)
          , ", i32 ", show (metaCompositeTypeAlign ct)
          , ", i64 ", show (metaCompositeTypeOffset ct)
          , ", i32 ", show (metaCompositeTypeFlags ct)
          , ", ", maybeShowMDName (metaCompositeTypeParent ct)
          , ", ", maybeShowMDName (metaCompositeTypeMembers ct)
          , ", i32 ", show (metaCompositeTypeRuntime ct), "}"
          ]
printMetadata md@Metadata { metaValueContent = sr@MetaDWSubrange {} } =
  mconcat [ showUntypedMDName md, " = metadata !{i32 ", dbgTag 33
          , ", i32 ", show (metaSubrangeLow sr)
          , ", i32 ", show (metaSubrangeHigh sr), "}"
          ]
printMetadata md@Metadata { metaValueContent = en@MetaDWEnumerator {} } =
  mconcat [ showUntypedMDName md, " = metadata !{i32 ", dbgTag 40
          , ", ", showMDString (metaEnumeratorName en)
          , ", i32 ", show (metaEnumeratorValue en), "}"
          ]
printMetadata md@Metadata { metaValueContent = l@MetaDWLocal {} } =
  mconcat [ showUntypedMDName md, " = metadata !{i32 ", show (metaLocalTag l)
          , ", ", showMDName (metaLocalContext l)
          , ", ", showMDString (metaLocalName l)
          , ", ", showMDName (metaLocalFile l)
          , ", i32 ", show (metaLocalLine l)
          , ", ", showMDName (metaLocalType l), "}"
          ]
printMetadata md@Metadata { metaValueContent = MetadataList vals } =
  mconcat [ showUntypedMDName md, " = metadata !{"
          , intercalate ", " (map showMDName vals)
          , "}"
          ]
printMetadata md@Metadata { metaValueContent = MetadataValueConstant v } =
  mconcat [ showUntypedMDName md, " = metadata !{", printValue v, "}" ]
printMetadata md@Metadata { metaValueContent = MetadataDiscarded } =
  mconcat [ showUntypedMDName md, " = metadata !{ }" ]
printMetadata md@Metadata { metaValueContent = MetadataUnknown } =
  mconcat [ showUntypedMDName md, " = metadata unknown" ]

-- Take all of the asm chunks, break their contents into lines,
-- then wrap each of those lines in the 'module asm' wrapper.
-- Combine them into a single string with newlines.
printAsm :: Assembly -> String
printAsm asm = mconcat asmLines
  where
    asmLines = map adorn (lines (show asm))
    adorn s = "module asm \"" ++ s ++ "\"\n"

-- When referencing a non-constant value during printing, just use
-- this instead of printValue to avoid problems printing cyclic data.
-- If the value doesn't have a name, just print it (it should be a
-- constant).
printConstOrName :: Value -> String
printConstOrName Value { valueName = Just ident
                       , valueType = t
                       } =
  mconcat [ printType t, " ", show ident ]
printConstOrName v@Value { valueType = t } =
  mconcat [ printType t, " ", printValue v ]

printConstOrNameNoType :: Value -> String
printConstOrNameNoType Value { valueName = Just ident } = show ident
printConstOrNameNoType v@Value { valueName = Nothing } = printValue v

compose :: [String] -> String
compose = unwords . filter (not . null)

quote :: String -> String
quote s = mconcat [ "\"", s, "\"" ]

isInteger :: String -> Bool
isInteger s = case reads s :: [(Integer, String)] of
  [(_, "")] -> True
  _ -> False

printValue :: Value -> String
printValue Value { valueContent =
                      Function { -- functionType = t
                                functionParameters = args
                               , functionBody = blockList
                               , functionLinkage = linkage
                               , functionVisibility = visStyle
                               , functionCC = cc
                               , functionRetAttrs = retAttrs
--                               , functionName = name
                               , functionSection = section
                               , functionAlign = align
                               , functionGCName = gcname
                               , functionIsVararg = isVararg
                               , functionAttrs = fattrs
                               }
                 , valueName = Just name
                 , valueType = t
                 } =
  compose [ "define", show linkage, show visStyle, show cc,
            retAttrS, printType rtype, show name, "(",
            argS, vaTag, ")", fAttrS, maybe "" unpack section,
            printAlignment align, maybe "" show gcname, "{\n",
            bodyS, "}" ]
  where retAttrS = unwords $ map show retAttrs
        argS = intercalate ", " $ map printValue args
        vaTag = if isVararg then ", ..." else ""
        fAttrS = unwords $ map show fattrs
        bodyS = unlines $ map printValue blockList
        (TypeFunction rtype _ _) = t

printValue Value { valueContent =
                      GlobalDeclaration { globalVariableLinkage = linkage
                                        , globalVariableVisibility = vis
                                        , globalVariableInitializer = initializer
                                        , globalVariableAlignment = align
                                        , globalVariableSection = section
                                        , globalVariableIsConstant = isConst
                                        }
                 , valueType = TypePointer _ addrSpace
                 , valueName = Just name
                 , valueMetadata = _
                 } =
   -- Don't show t here since showing the initializer will handle it
  compose [ show name, "=", addrSpaceS, linkageS, visS, annotsS
          , initS, sectionS
          , printAlignment align
          ]
  where addrSpaceS = case addrSpace of
          0 -> ""
          _ -> "addrspace(" ++ show addrSpace ++ ")"
        linkageS = show linkage
        visS = show vis
        annotsS = if isConst then "constant" else "global"
        sectionS = maybe "" ((", section "++) . quote . unpack) section
        initS = maybe "" printConstOrName initializer

printValue Value { valueContent = GlobalDeclaration {}
                 , valueName = Nothing
                 } =
  error "Global value has no name"

printValue Value { valueContent =
                      GlobalAlias { globalAliasLinkage = linkage
                                  , globalAliasVisibility = vis
                                  , globalAliasValue = val
                                  }
                 , valueType = _
                 , valueName = name
                 , valueMetadata = _
                 } =
  compose [ show name, "alias", show linkage, show vis, printConstOrName val ]

printValue Value { valueContent = ExternalValue
                 , valueType = t
                 , valueName = Just name
                 , valueMetadata = _
                 } = case t of
  TypeFunction rtype argTypes isva ->
    compose [ "declare", printType rtype, show name, "(",
              intercalate ", " $ map printType argTypes,
              if isva then ", ..." else "", ")" ]
  _ -> compose [ "declare", printType t, show name ]

printValue Value { valueContent = ExternalFunction attrs
                 , valueType = t
                 , valueName = Just name
                 , valueMetadata = _
                 } = case t of
  TypeFunction rtype argTypes isva ->
    compose [ "declare", printType rtype, show name, "("
            ,  intercalate ", " $ map printType argTypes
            , if isva then ", ..." else "", ")"
            , unwords $ map show attrs ]
  _ -> compose [ "declare", printType t, show name ]

printValue Value { valueContent = ExternalValue
                 , valueName = Nothing
                 } =
  error "External values must have names"

printValue Value { valueContent = ExternalFunction _
                 , valueName = Nothing
                 } =
  error "External functions must have names"

printValue Value { valueContent = BasicBlock instructions
                 , valueName = Just identifier
                 } = mconcat [ label, "\n", instS'' ]
  where
    instS'' = unlines $ map indent instS'
    instS = map printValue instructions
    dbgS = map (printDebugTag . valueMetadata) instructions
    instS' = map (uncurry (++)) $ zip instS dbgS
    identS = identifierAsString identifier
    indent = ("  "++)
    label = if isInteger identS
            then "; <label>:" ++ identS
            else identS ++ ":"

printValue Value { valueContent = BasicBlock instructions
                 , valueName = _
                 } = instS
  where instS = unlines $ map (indent . printValue) instructions
        indent = ("  "++)

printValue Value { valueContent = Argument paramAttrs
                 , valueName = Just paramName
                 , valueType = paramType
                 , valueMetadata = _
                 } =
  compose [ printType paramType
          , unwords $ map show paramAttrs
          , show paramName
          ]

printValue Value { valueContent = Argument _
                 , valueName = Nothing
                 } =
  error "Arguments must have names"

printValue Value { valueContent = RetInst val
                 , valueMetadata = _
                 } =
  compose [ "ret", arg ]
  where arg = case val of
          Nothing -> "void"
          Just aVal -> printConstOrName aVal

printValue Value { valueContent = UnconditionalBranchInst dest
                 , valueMetadata = _
                 } =
  compose [ "br", printConstOrName dest ]

printValue Value { valueContent =
                      BranchInst { branchCondition = cond
                                 , branchTrueTarget = tTarget
                                 , branchFalseTarget = fTarget
                                 }
                 , valueMetadata = _
                 } =
  compose [ "br", printConstOrName cond, ","
          , printConstOrName tTarget, ","
          , printConstOrName fTarget
          ]

printValue Value { valueContent =
                      SwitchInst { switchValue = val
                                 , switchDefaultTarget = defTarget
                                 , switchCases = cases
                                 }
                 , valueMetadata = _
                 } =
  compose [ "switch", printConstOrName val, ","
          , printConstOrName defTarget, "["
          , caseDests, "]" ]
  where caseDests = unwords $ map printPair cases
        printPair (caseVal, caseDest) = mconcat [ printConstOrName caseVal
                                                , ", "
                                                , printConstOrName caseDest
                                                ]

printValue Value { valueContent =
                      IndirectBranchInst { indirectBranchAddress = addr
                                         , indirectBranchTargets = targets
                                         }
                 , valueMetadata = _
                 } =
  compose [ "indirectbr", printConstOrName addr
          , "[", targetS , "]"
          ]
  where targetS = intercalate ", " $ map printConstOrName targets

printValue Value { valueContent = UnwindInst } = "unwind"
printValue Value { valueContent = UnreachableInst } = "unreachable"

printValue Value { valueContent = AddInst flags v1 v2
                 , valueName = name
                 , valueType = t
                 , valueMetadata = md
                 } =
  printFlaggedBinaryOp "add" name flags t v1 v2 md

printValue Value { valueContent = SubInst flags v1 v2
                 , valueName = name
                 , valueType = t
                 , valueMetadata = md
                 } =
  printFlaggedBinaryOp "sub" name flags t v1 v2 md

printValue Value { valueContent = MulInst flags v1 v2
                 , valueName = name
                 , valueType = t
                 , valueMetadata = md
                 } =
  printFlaggedBinaryOp "mul" name flags t v1 v2 md

printValue Value { valueContent = DivInst v1 v2
                 , valueName = name
                 , valueType = t
                 , valueMetadata = md
                 } =
  printBinaryOp "div" name t v1 v2 md

printValue Value { valueContent = RemInst v1 v2
                 , valueName = name
                 , valueType = t
                 , valueMetadata = md
                 } =
  printBinaryOp "rem" name t v1 v2 md

printValue Value { valueContent = ShlInst v1 v2
                 , valueName = name
                 , valueType = t
                 , valueMetadata = md
                 } =
  printBinaryOp "shl" name t v1 v2 md

printValue Value { valueContent = LshrInst v1 v2
                 , valueName = name
                 , valueType = t
                 , valueMetadata = md
                 } =
  printBinaryOp "lshr" name t v1 v2 md

printValue Value { valueContent = AshrInst v1 v2
                 , valueName = name
                 , valueType = t
                 , valueMetadata = md
                 } =
  printBinaryOp "ashr" name t v1 v2 md

printValue Value { valueContent = AndInst v1 v2
                 , valueName = name
                 , valueType = t
                 , valueMetadata = md
                 } =
  printBinaryOp "and" name t v1 v2 md

printValue Value { valueContent = OrInst v1 v2
                 , valueName = name
                 , valueType = t
                 , valueMetadata = md
                 } =
  printBinaryOp "or" name t v1 v2 md

printValue Value { valueContent = XorInst v1 v2
                 , valueName = name
                 , valueType = t
                 , valueMetadata = md
                 } =
  printBinaryOp "xor" name t v1 v2 md

printValue Value { valueContent =
                      ExtractElementInst { extractElementVector = vec
                                         , extractElementIndex = idx
                                         }
                 , valueName = name
                 , valueType = _
                 , valueMetadata = _
                 } =
  compose [ printInstNamePrefix name
          , "extractelement"
          , printConstOrName vec, ","
          , printConstOrName idx
          ]

printValue Value { valueContent =
                      InsertElementInst { insertElementVector = vec
                                        , insertElementValue = val
                                        , insertElementIndex = idx
                                        }
                 , valueName = name
                 , valueType = _
                 , valueMetadata = _
                 } =
  compose [ printInstNamePrefix name
          , "insertelement"
          , printConstOrName vec, ","
          , printConstOrName val, ","
          , printConstOrName idx
          ]

printValue Value { valueContent =
                      ShuffleVectorInst { shuffleVectorV1 = v1
                                        , shuffleVectorV2 = v2
                                        , shuffleVectorMask = mask
                                        }
                 , valueName = name
                 , valueType = _
                 , valueMetadata = _
                 } =
  compose [ printInstNamePrefix name
          , "shufflevector"
          , printConstOrName v1, ","
          , printConstOrName v2, ","
          , printConstOrName mask
          ]

printValue Value { valueContent =
                      ExtractValueInst { extractValueAggregate = agg
                                       , extractValueIndices = indices
                                       }
                 , valueType = _
                 , valueName = name
                 , valueMetadata = _
                 } =
  compose [ printInstNamePrefix name
          , "extractvalue"
          , printConstOrName agg
          , intercalate ", " $ map show indices
          ]

printValue Value { valueContent =
                      InsertValueInst { insertValueAggregate = agg
                                      , insertValueValue = val
                                      , insertValueIndices = indices
                                      }
                 , valueType = _
                 , valueName = name
                 , valueMetadata = _
                 } =
  compose [ printInstNamePrefix name
          , "insertvalue"
          , printConstOrName agg, ","
          , printConstOrName val, ","
          , intercalate ", " $ map show indices
          ]

printValue Value { valueContent = AllocaInst elems align
                 , valueName = name
                 , valueType = TypePointer ty _
                 , valueMetadata = _
                 } =
  compose [ printInstNamePrefix name
          , "alloca"
          , printType ty
          , count
          , printAlignment align
          ]
  where count = case elems of
          Value { valueContent = ConstantInt 1 } -> ""
          _ -> ", " ++ printConstOrName elems

printValue Value { valueContent =
                      LoadInst { loadIsVolatile = volatile
                               , loadAddress = src
                               , loadAlignment = align
                               }
                 , valueName = name
                 , valueType = _
                 , valueMetadata = _
                 } =
  compose [ printInstNamePrefix name
          , printVolatileFlag volatile
          , "load"
          , printConstOrName src
          , printAlignment align
          ]

printValue Value { valueContent =
                      StoreInst { storeIsVolatile = volatile
                                , storeValue = val
                                , storeAddress = dest
                                , storeAlignment = align
                                }
                 , valueName = _
                 , valueType = _
                 , valueMetadata = _
                 } =
  compose [ printVolatileFlag volatile
          , "store"
          , printConstOrName val, ","
          , printConstOrName dest
          , printAlignment align
          ]

printValue Value { valueContent = TruncInst v@Value { valueType = ty }
                 , valueName = name
                 , valueMetadata = md
                 } =
  printTypecast "trunc" name v ty md

printValue Value { valueContent = ZExtInst v@Value { valueType =  ty }
                 , valueName = name
                 , valueMetadata = md
                 } =
  printTypecast "zext" name v ty md

printValue Value { valueContent = SExtInst v@Value { valueType =  ty }
                 , valueName = name
                 , valueMetadata = md
                 } =
  printTypecast "sext" name v ty md

printValue Value { valueContent = FPTruncInst v@Value { valueType =  ty }
                 , valueName = name
                 , valueMetadata = md
                 } =
  printTypecast "fptrunc" name v ty md

printValue Value { valueContent = FPExtInst v@Value { valueType = ty }
                 , valueName = name
                 , valueMetadata = md
                 } =
  printTypecast "fpext" name v ty md

printValue Value { valueContent = FPToUIInst v@Value { valueType = ty }
                 , valueName = name
                 , valueMetadata = md
                 } =
  printTypecast "fptoui" name v ty md

printValue Value { valueContent = FPToSIInst v@Value { valueType = ty }
                 , valueName = name
                 , valueMetadata = md
                 } =
  printTypecast "fptosi" name v ty md

printValue Value { valueContent = UIToFPInst v@Value { valueType = ty }
                 , valueName = name
                 , valueMetadata = md
                 } =
  printTypecast "uitofp" name v ty md

printValue Value { valueContent = SIToFPInst v@Value { valueType = ty }
                 , valueName = name
                 , valueMetadata = md
                 } =
  printTypecast "sitofp" name v ty md

printValue Value { valueContent = PtrToIntInst v@Value { valueType = ty }
                 , valueName = name
                 , valueMetadata = md
                 } =
  printTypecast "ptrtoint" name v ty md

printValue Value { valueContent = IntToPtrInst v@Value { valueType = ty }
                 , valueName = name
                 , valueMetadata = md
                 } =
  printTypecast "inttoptr" name v ty md

printValue Value { valueContent = BitcastInst v@Value { valueType = ty }
                 , valueName = name
                 , valueMetadata = md
                 } =
  printTypecast "bitcast" name v ty md

printValue Value { valueContent = ICmpInst cond v1 v2
                 , valueName = name
                 , valueType = _
                 , valueMetadata = _
                 } =
  compose [ printInstNamePrefix name
          , "icmp"
          , show cond
          , printConstOrName v1, ","
          , printConstOrNameNoType v2
          ]

printValue Value { valueContent = FCmpInst cond v1 v2
                 , valueName = name
                 , valueType = _
                 , valueMetadata = _
                 } =
  compose [ printInstNamePrefix name
          , "fcmp"
          , show cond
          , printConstOrName v1, ","
          , printConstOrNameNoType v2
          ]

printValue Value { valueContent = PhiNode vals
                 , valueName = name
                 , valueType = t
                 , valueMetadata = _
                 } =
  compose [ printInstNamePrefix name
          , "phi"
          , printType t
          , "["
          , valS
          ,"]"
          ]
  where printPair (v,lab) =
          mconcat [ "["
                  , printConstOrNameNoType v
                  , ", "
                  , printConstOrNameNoType lab
                  , "]"
                  ]
        valS = intercalate ", " $ map printPair vals

printValue Value { valueContent = SelectInst cond v1 v2
                 , valueType = _
                 , valueName = name
                 , valueMetadata = _
                 } =
  compose [ printInstNamePrefix name
          , "select"
          , printConstOrName cond
          , ","
          , printConstOrName v1
          , ","
          , printConstOrName v2
          ]

printValue Value { valueContent =
                      GetElementPtrInst { getElementPtrInBounds = inBounds
                                        , getElementPtrValue = v
                                        , getElementPtrIndices = indices
                                        }
                 , valueType = _
                 , valueName = name
                 , valueMetadata = _
                 } =
  compose [ printInstNamePrefix name
          , "getelementptr"
          , printInBounds inBounds
          , printConstOrName v
          , ","
          , indicesS
          ]
  where indicesS = intercalate ", " $ map printConstOrName indices

printValue Value { valueContent =
                      CallInst { callIsTail = isTail
                               , callConvention = cc
                               , callParamAttrs = pattrs
                               , callFunction = f
                               , callArguments = args
                               , callAttrs = cattrs
                               , callHasSRet = _
                               }
                 , valueName = name
                 , valueType = _
                 , valueMetadata = _
                 } =
  compose [ printInstNamePrefix name
          , printTailTag isTail
          , "call"
          , show cc
          , unwords $ map show pattrs
          , printType rtype
          , printConstOrNameNoType f
          , "("
          , intercalate ", " $ map printArgument args
          , ")"
          , unwords $ map show cattrs
          ]
  where
    TypeFunction rtype _ _ = valueType f

printValue Value { valueContent =
                      InvokeInst { invokeConvention = cc
                                 , invokeParamAttrs = pattrs
                                 , invokeFunction = f
                                 , invokeArguments = args
                                 , invokeAttrs = attrs
                                 , invokeNormalLabel = nlabel
                                 , invokeUnwindLabel = ulabel
                                 , invokeHasSRet = _
                                 }
                 , valueName = name
                 , valueType = _
                 , valueMetadata = _
                 } =
  compose [ printInstNamePrefix name
          , "invoke"
          , show cc
          , unwords $ map show pattrs
          , printConstOrName f
          , "("
          , intercalate ", " $ map printArgument args
          , ")"
          , unwords $ map show attrs
          , "to"
          , printConstOrName nlabel
          , "unwind"
          , printConstOrName ulabel
          ]

printValue Value { valueContent = VaArgInst v
                 , valueName = name
                 , valueType = ty
                 , valueMetadata = _
                 } =
  compose [ printInstNamePrefix name
          , "va_arg"
          , printConstOrName v, ","
          , printType ty
          ]

printValue Value { valueContent = UndefValue } = "undef"
printValue Value { valueContent = BlockAddress f block } =
  mconcat [ "blockaddress("
          , printConstOrNameNoType f, ", "
          , printConstOrNameNoType block
          , ")"
          ]

printValue Value { valueContent = ConstantAggregateZero } = "zeroinitializer"
printValue Value { valueContent = ConstantArray vals } =
  mconcat [ "[ ", vstring, " ]" ]
  where vstring = intercalate ", " $ map printConstOrName vals

printValue Value { valueContent = ConstantFP d } = show d
printValue Value { valueContent = ConstantInt i } = show i
printValue Value { valueContent = ConstantString s } =
  mconcat [ "c\"", unpack s, "\"" ]
printValue Value { valueContent = ConstantPointerNull } = "null"
printValue Value { valueContent = ConstantStruct vals } =
  mconcat [ "{ ", vstring, " }" ]
  where vstring = intercalate ", " $ map printConstOrName vals
printValue Value { valueContent = ConstantVector vals } =
  mconcat [ "< ", vstring, " >" ]
  where vstring = intercalate ", " $ map printConstOrName vals
printValue Value { valueContent = ConstantValue valT
                 , valueType = t
                 } =
  mconcat [ printType t, " ", printConstExp valT ]
printValue Value { valueContent = InlineAsm asm constraints } =
  mconcat [ "asm \"", unpack asm, "\", \"", unpack constraints, "\"" ]

printArgument :: (Value, [ParamAttribute]) -> String
printArgument (v, atts) =
  compose [ printType $ valueType v
          , unwords $ map show atts
          , printConstOrNameNoType v
          ]

printConstExp :: ValueT -> String
printConstExp valT = case valT of
  TruncInst v@Value { valueType = t } -> printTypecastConst "trunc" v t
  ZExtInst v@Value { valueType =  t } -> printTypecastConst "zext" v t
  SExtInst v@Value { valueType =  t } -> printTypecastConst "sext" v t
  FPTruncInst v@Value { valueType =  t } -> printTypecastConst "fptrunc" v t
  FPExtInst v@Value { valueType =  t } -> printTypecastConst "fpext" v t
  FPToUIInst v@Value { valueType =  t } -> printTypecastConst "fptoui" v t
  FPToSIInst v@Value { valueType =  t } -> printTypecastConst "fptosi" v t
  UIToFPInst v@Value { valueType =  t } -> printTypecastConst "uitofp" v t
  SIToFPInst v@Value { valueType =  t } -> printTypecastConst "sitofp" v t
  PtrToIntInst v@Value { valueType =  t } -> printTypecastConst "ptrtoint" v t
  IntToPtrInst v@Value { valueType =  t } -> printTypecastConst "inttoptr" v t
  BitcastInst v@Value { valueType =  t } -> printTypecastConst "bitcast" v t
  GetElementPtrInst { getElementPtrInBounds = inBounds
                    , getElementPtrValue = val
                    , getElementPtrIndices = indices
                    } ->
    compose [ "getelementptr"
            , printInBounds inBounds
            , "("
            , printConstOrName val, ", "
            , intercalate ", " $ map printConstOrName indices
            , ")"
            ]
  SelectInst cond v1 v2 ->
    mconcat [ "select ("
            , printConstOrName cond, ", "
            , printConstOrName v1, ", "
            , printConstOrName v2, ")"
            ]
  ICmpInst cond v1 v2 ->
    mconcat [ "icmp ", show cond, " ("
            , printConstOrName v1, ", "
            , printConstOrName v2, ")"
            ]
  FCmpInst cond v1 v2 ->
    mconcat [ "fcmp ", show cond, " ("
            , printConstOrName v1, ", "
            , printConstOrName v2, ")"
            ]
  ExtractElementInst { extractElementVector = v
                     , extractElementIndex = idx
                     } ->
    mconcat [ "extractelement ("
            , printConstOrName v, ", "
            , printConstOrName idx, ")"
            ]
  InsertElementInst { insertElementVector = vec
                    , insertElementValue = val
                    , insertElementIndex = idx
                    } ->
    mconcat [ "insertelement ("
            , printConstOrName vec, ", "
            , printConstOrName val, ", "
            , printConstOrName idx, ")"
            ]
  ShuffleVectorInst { shuffleVectorV1 = v1
                    , shuffleVectorV2 = v2
                    , shuffleVectorMask = mask
                    } ->
    mconcat [ "shufflevector ("
            , printConstOrName v1, ", "
            , printConstOrName v2, ", "
            , printConstOrName mask, ")"
            ]
  ExtractValueInst { extractValueAggregate = agg
                   , extractValueIndices = indices
                   } ->
    mconcat [ "extractvalue ("
            , printConstOrName agg, ", "
            , intercalate ", " $ map show indices, ")"
            ]
  InsertValueInst { insertValueAggregate = agg
                  , insertValueValue = val
                  , insertValueIndices = indices
                  } ->
    mconcat [ "insertvalue ("
            , printConstOrName agg, ", "
            , printConstOrName val, ", "
            , intercalate ", " $ map show indices, ")"
            ]
  AddInst _ v1 v2 -> printBinaryConst "add" v1 v2
  SubInst _ v1 v2 -> printBinaryConst "sub" v1 v2
  MulInst _ v1 v2 -> printBinaryConst "mul" v1 v2
  DivInst v1 v2 -> printBinaryConst "div" v1 v2
  RemInst v1 v2 -> printBinaryConst "rem" v1 v2
  ShlInst v1 v2 -> printBinaryConst "shl" v1 v2
  LshrInst v1 v2 -> printBinaryConst "lshr" v1 v2
  AshrInst v1 v2 -> printBinaryConst "ashr" v1 v2
  AndInst v1 v2 -> printBinaryConst "and" v1 v2
  OrInst v1 v2 -> printBinaryConst "or" v1 v2
  XorInst v1 v2 -> printBinaryConst "xor" v1 v2
  _ -> error "Non-constant ValueT"

printBinaryConst :: String -> Value -> Value -> String
printBinaryConst name v1 v2 =
  mconcat [ name, " (", printConstOrName v1, ", "
          , printConstOrName v2, ")"
          ]

printTypecastConst :: String -> Value -> Type -> String
printTypecastConst n v t =
  mconcat [ n, " (", printConstOrName v, " to ", printType t, ")" ]

printTailTag :: Bool -> String
printTailTag isTail = if isTail then "tail" else ""

printVolatileFlag :: Bool -> String
printVolatileFlag f = if f then "volatile" else ""

printAlignment :: Int64 -> String
printAlignment align = case align of
  0 -> ""
  _ -> ", align " ++ show align

printTypecast :: String -> Maybe Identifier -> Value -> Type -> Maybe Metadata -> String
printTypecast inst name val newType _ =
  compose [ printInstNamePrefix name
          , inst
          , printConstOrName val
          , "to"
          , printType newType
          ]

printInBounds :: Bool -> String
printInBounds inBounds = if inBounds then "inbounds" else ""

printFlaggedBinaryOp :: String -> Maybe Identifier -> ArithFlags ->
                        Type -> Value -> Value -> Maybe Metadata -> String
printFlaggedBinaryOp inst name flags t v1 v2 _ =
  compose [ printInstNamePrefix name, inst
          , show flags
          , printType t
          , printConstOrNameNoType v1, ","
          , printConstOrNameNoType v2
          ]

printBinaryOp :: String -> Maybe Identifier -> Type ->
                 Value -> Value -> Maybe Metadata -> String
printBinaryOp inst name t v1 v2 _ =
  compose [ printInstNamePrefix name, inst
          , printType t
          , printConstOrNameNoType v1, ","
          , printConstOrNameNoType v2
          ]

printInstNamePrefix :: Maybe Identifier -> String
printInstNamePrefix Nothing = ""
printInstNamePrefix (Just n) = mconcat [ show n, " =" ]

printDebugTag :: Maybe Metadata -> String
printDebugTag Nothing = ""
printDebugTag (Just (Metadata { metaValueName = Just n })) = ", !dbg " ++ show n
printDebugTag (Just e) = error $ "Not metadata: " ++ printMetadata e

printType :: Type -> String
printType (TypeInteger bits) = 'i' : show bits
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
printType (TypeFunction retT argTs isVa) =
  mconcat [ printType retT, "(", argVals, vaTag, ")" ]
  where argVals :: String
        argVals = intercalate ", " $ map printType argTs
        vaTag :: String
        vaTag = if isVa then ", ..." else ""
printType TypeOpaque = "opaque"
printType (TypePointer ty _) = mconcat [ printType ty, "*" ]
printType (TypeStruct ts p) =
  case p of
    True -> mconcat [ "<", fieldVals, ">" ]
    False -> mconcat [ "{", fieldVals, "}" ]
  where fieldVals = intercalate ", " $ map printType ts
printType (TypeNamed name _) = name

instance Show Metadata where
  show = printMetadata

instance Show Type where
  show = printType

instance Show Value where
  show = printValue
