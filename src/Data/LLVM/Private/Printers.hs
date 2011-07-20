{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | Define the unparser for our LLVM IR
module Data.LLVM.Private.Printers (
  printMetadata
  , printAsm
  , printType
  , printValue
  ) where

import Data.GraphViz
import Data.Int
import Data.List ( intercalate )
import Data.Monoid
import Data.ByteString.Char8 ( ByteString, unpack )

import Data.LLVM.Attributes
import Data.LLVM.Identifiers
import Data.LLVM.Private.Types.Referential

-- TODO List
--
-- * Pretty up the DataLayout
-- * Print out named type definitions
-- * Make the function type printing as flexible as the official
--   version

showUntypedMDName :: Metadata -> String
showUntypedMDName = ("!"++) . show . metaValueUniqueId

showMDName :: Metadata -> String
showMDName = ("metadata !"++) . show . metaValueUniqueId

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
          -- , ", ", showMDName (metaLexicalBlockFile lb)
          -- , ", i32 ", show (metaLexicalBlockDepth lb)
          , "}"
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
printMetadata md@Metadata { metaValueContent = n@MetaDWNamespace {} } =
  mconcat [ showUntypedMDName md, " = metadata !{i32 ", dbgTag 57
          , ", ", showMDString (metaNamespaceName n)
          , ", ", showMDName (metaNamespaceContext n)
          , ", ", showMDName (metaNamespaceCompileUnit n)
          , ", i32 ", show (metaNamespaceLine n)
          , "}"
          ]
printMetadata md@Metadata { metaValueContent = t@MetaDWTemplateTypeParameter {} } =
  mconcat [ showUntypedMDName md, " = metadata !{i32 ", dbgTag 0x2f
          , ", ", showMDString (metaTemplateTypeParameterName t)
          , ", i32 ", show (metaTemplateTypeParameterLine t)
          , ", i32 ", show (metaTemplateTypeParameterCol t)
          , ", ", showMDName (metaTemplateTypeParameterContext t)
          , ", ", showMDName (metaTemplateTypeParameterType t)
          , "}"
          ]
printMetadata md@Metadata { metaValueContent = t@MetaDWTemplateValueParameter {} } =
  mconcat [ showUntypedMDName md, " = metadata !{i32 ", dbgTag 0x30
          , ", ", showMDString (metaTemplateValueParameterName t)
          , ", i32 ", show (metaTemplateValueParameterLine t)
          , ", i32 ", show (metaTemplateValueParameterCol t)
          , ", ", showMDName (metaTemplateValueParameterContext t)
          , ", ", showMDName (metaTemplateValueParameterType t)
          , ", i64 ", show (metaTemplateValueParameterValue t)
          , "}"
          ]

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
printConstOrName v = case valueName v of
  Nothing -> mconcat [ printType (valueType v), " ", printValue v ]
  Just ident -> mconcat [ printType (valueType v), " ", show ident ]

printConstOrNameNoType :: Value -> String
printConstOrNameNoType v = case valueName v of
  Nothing -> printValue v
  Just ident -> show ident

compose :: [String] -> String
compose = unwords . filter (not . null)

quote :: String -> String
quote s = mconcat [ "\"", s, "\"" ]

isInteger :: String -> Bool
isInteger s = case reads s :: [(Integer, String)] of
  [(_, "")] -> True
  _ -> False

printValue :: Value -> String
printValue v = case valueContent v of
  FunctionC f ->
    let retAttrS = unwords $ map show (functionRetAttrs f)
        argS = intercalate ", " $ map (printValue . Value) (functionParameters f)
        fAttrS = unwords $ map show (functionAttrs f)
        bodyS = unlines $ map (printValue . Value) (functionBody f)
        vaTag = if functionIsVararg f then ", ..." else ""
        (TypeFunction rtype _ _) = functionType f
        name = functionName f
    in compose [ "define", show (functionLinkage f), show (functionVisibility f)
               , show (functionCC f), retAttrS, printType rtype, show name, "("
               , argS, vaTag, ")", fAttrS, maybe "" unpack (functionSection f)
               , printAlignment (functionAlign f), maybe "" show (functionGCName f)
               , "{\n", bodyS, "}"
               ]
  ArgumentC a ->
    compose [ printType (argumentType a)
            , unwords $ map show (argumentParamAttrs a)
            , show (argumentName a)
            ]
  BasicBlockC b ->
    let indent = ("  "++)
        dbgS = map (printDebugTag . valueMetadata) (basicBlockInstructions b)
        instS = map (printValue . Value) (basicBlockInstructions b)
        instS' = zipWith (++) instS dbgS
        instS'' = unlines $ map indent instS'
        identS = identifierAsString (basicBlockName b)
        label = case isInteger identS of
          True -> "; <label>:" ++ identS
          False -> identS ++ ":"
    in mconcat [ label, "\n", instS'' ]
  GlobalVariableC g ->
    let TypePointer _ addrSpace = globalVariableType g
        addrSpaceS = case addrSpace of
          0 -> ""
          _ -> mconcat [ "addrspace(", show addrSpace, ")" ]
        annotsS = if globalVariableIsConstant g then "constant" else ""
        initS = maybe "" printConstOrName (globalVariableInitializer g)
        sectionS = maybe "" ((", section"++) . quote . unpack) (globalVariableSection g)
    in compose [ show (globalVariableName g), "=", addrSpaceS
               , show (globalVariableLinkage g), show (globalVariableVisibility g)
               , annotsS, initS, sectionS, printAlignment (globalVariableAlignment g)
               ]
  GlobalAliasC a ->
    compose [ show (globalAliasName a), "= alias", show (globalAliasLinkage a)
            , show (globalAliasVisibility a), printConstOrName (globalAliasTarget a)
            ]
  ExternalValueC e ->
    compose [ "declare", printType (valueType e), show (externalValueName e) ]
  ExternalFunctionC e ->
    let TypeFunction rtype argTypes isva = externalFunctionType e
    in compose [ "declare", printType rtype, show (externalFunctionName e)
               , "(", intercalate ", " $ map printType argTypes
               , if isva then ", ..." else "", ")"
               ]
  InstructionC i ->
    case i of
      RetInst { retInstValue = Just rv } -> compose [ "ret", printConstOrName rv ]
      RetInst { } -> "ret void"
      UnconditionalBranchInst { unconditionalBranchTarget = dest } ->
        compose [ "br", (printConstOrName . Value) dest ]
      BranchInst { branchCondition = cond
                 , branchTrueTarget = tTarget
                 , branchFalseTarget = fTarget
                 } ->
        compose [ "br", printConstOrName cond
                , ",", printConstOrName (Value tTarget)
                , ",", printConstOrName (Value fTarget)
                ]
      SwitchInst { switchValue = val
                 , switchDefaultTarget = defTarget
                 , switchCases = cases
                 } ->
        let caseDests = unwords $ map printPair cases
            printPair (caseVal, caseDest) =
              mconcat [ printConstOrName caseVal, ", ", printConstOrName (Value caseDest) ]
        in compose [ "switch", printConstOrName val, ",", printConstOrName (Value defTarget)
                   , "[", caseDests, "]"
                   ]
      IndirectBranchInst { indirectBranchAddress = addr
                         , indirectBranchTargets = targets
                         } ->
        compose [ "indirectbr", printConstOrName addr
                , "[", intercalate ", " $ map (printConstOrName . Value) targets, "]"
                ]
      UnwindInst { } -> "unwind"
      UnreachableInst { } -> "unreachable"
      AddInst { } -> printFlaggedBinaryOp "add" i
      SubInst { } -> printFlaggedBinaryOp "sub" i
      MulInst { } -> printFlaggedBinaryOp "mul" i
      DivInst { } -> printBinaryOp "div" i
      RemInst { } -> printBinaryOp "rem" i
      ShlInst { } -> printBinaryOp "shl" i
      LshrInst { } -> printBinaryOp "lshr" i
      AshrInst { } -> printBinaryOp "ashr" i
      AndInst { } -> printBinaryOp "and" i
      OrInst { } -> printBinaryOp "or" i
      XorInst { } -> printBinaryOp "xor" i
      ExtractElementInst { extractElementVector = vec
                         , extractElementIndex = idx
                         } ->
        compose [ printInstNamePrefix i
                , "extractelement"
                , printConstOrName vec, ","
                , printConstOrName idx
                ]
      InsertElementInst { insertElementVector = vec
                        , insertElementValue = val
                        , insertElementIndex = idx
                        } ->
        compose [ printInstNamePrefix i
                , "insertelement"
                , printConstOrName vec, ","
                , printConstOrName val, ","
                , printConstOrName idx
                ]
      ShuffleVectorInst { shuffleVectorV1 = v1
                        , shuffleVectorV2 = v2
                        , shuffleVectorMask = mask
                        } ->
        compose [ printInstNamePrefix i
                , "shufflevector"
                , printConstOrName v1, ","
                , printConstOrName v2, ","
                , printConstOrName mask
                ]
      ExtractValueInst { extractValueAggregate = agg
                       , extractValueIndices = indices
                       } ->
        compose [ printInstNamePrefix i
                , "extractvalue"
                , printConstOrName agg
                , intercalate ", " $ map show indices
                ]
      InsertValueInst { insertValueAggregate = agg
                      , insertValueValue = val
                      , insertValueIndices = indices
                      } ->
        compose [ printInstNamePrefix i
                , "insertvalue"
                , printConstOrName agg, ","
                , printConstOrName val, ","
                , intercalate ", " $ map show indices
                ]
      AllocaInst { allocaNumElements = elems
                 , allocaAlign = align
                 , instructionType = t -- TypePointer ty _
                 } ->
        let count = case valueContent elems of
              ConstantC ConstantInt { constantIntValue = 1 } -> ""
              _ -> ", " ++ printConstOrName elems
            TypePointer ty _ = t
        in   compose [ printInstNamePrefix i
                     , "alloca"
                     , printType ty
                     , count
                     , printAlignment align
                     ]
      LoadInst { loadIsVolatile = volatile
               , loadAddress = src
               , loadAlignment = align
               } ->
        compose [ printInstNamePrefix i
                , printVolatileFlag volatile
                , "load"
                , printConstOrName src
                , printAlignment align
                ]
      StoreInst { storeIsVolatile = volatile
                , storeValue = val
                , storeAddress = dest
                , storeAlignment = align
                } ->
        compose [ printVolatileFlag volatile
                , "store"
                , printConstOrName val, ","
                , printConstOrName dest
                , printAlignment align
                ]
      TruncInst { } -> printTypecast "trunc" i
      ZExtInst { } -> printTypecast "zext" i
      SExtInst { } -> printTypecast "sext" i
      FPTruncInst { } -> printTypecast "fptrunc" i
      FPExtInst { } -> printTypecast "fpext" i
      FPToUIInst { } -> printTypecast "fptoui" i
      FPToSIInst { } -> printTypecast "fptosi" i
      UIToFPInst { } -> printTypecast "uitofp" i
      SIToFPInst { } -> printTypecast "sitofp" i
      PtrToIntInst { } -> printTypecast "ptrtoint" i
      IntToPtrInst { } -> printTypecast "inttoptr" i
      BitcastInst { } -> printTypecast "bitcast" i
      ICmpInst { cmpPredicate = cond
               , cmpV1 = v1
               , cmpV2 = v2
               } ->
        compose [ printInstNamePrefix i
                , "icmp"
                , show cond
                , printConstOrName v1, ","
                , printConstOrNameNoType v2
                ]
      FCmpInst { cmpPredicate = cond
               , cmpV1 = v1
               , cmpV2 = v2
               } ->
        compose [ printInstNamePrefix i
                , "fcmp"
                , show cond
                , printConstOrName v1, ","
                , printConstOrNameNoType v2
                ]
      PhiNode { phiIncomingValues = vals
              } ->
        let printPair (val, lab) =
              mconcat [ "[", printConstOrNameNoType val
                      , ", ", printConstOrNameNoType lab, "]"
                      ]
            valS = intercalate ", " $ map printPair vals
        in compose [ printInstNamePrefix i
                   , "phi"
                   , printType (instructionType i)
                   , "[", valS, "]"
                   ]
      SelectInst { selectCondition = cond
                 , selectTrueValue = v1
                 , selectFalseValue = v2
                 } ->
        compose [ printInstNamePrefix i
                , "select"
                , printConstOrName cond, ","
                , printConstOrName v1, ","
                , printConstOrName v2
                ]
      GetElementPtrInst { getElementPtrInBounds = inBounds
                        , getElementPtrValue = val
                        , getElementPtrIndices = indices
                        } ->
        compose [ printInstNamePrefix i
                , "getelementptr"
                , printInBounds inBounds
                , printConstOrName val, ","
                , intercalate ", " $ map printConstOrName indices
                ]
      CallInst { callIsTail = isTail
               , callConvention = cc
               , callParamAttrs = pattrs
               , callFunction = f
               , callArguments = args
               , callAttrs = cattrs
               , callHasSRet = _
               } ->
        let rtype = case valueType f of
              TypeFunction r _ _ -> r
              TypePointer (TypeFunction r _ _) _ -> r
        in compose [ printInstNamePrefix i
                   , printTailTag isTail
                   , "call"
                   , show cc
                   , unwords $ map show pattrs
                   , printType rtype
                   , printConstOrNameNoType f
                   , "(", intercalate ", " $ map printArgument args, ")"
                   , unwords $ map show cattrs
                   ]
      InvokeInst { invokeConvention = cc
                 , invokeParamAttrs = pattrs
                 , invokeFunction = f
                 , invokeArguments = args
                 , invokeAttrs = attrs
                 , invokeNormalLabel = nlabel
                 , invokeUnwindLabel = ulabel
                 , invokeHasSRet = _
                 } ->
        compose [ printInstNamePrefix i
                , "invoke"
                , show cc
                , unwords $ map show pattrs
                , printConstOrName f
                , "(", intercalate ", " $ map printArgument args, ")"
                , unwords $ map show attrs
                , "to", printConstOrName nlabel
                , "unwind", printConstOrName ulabel
                ]
      VaArgInst { vaArgValue = va } ->
        compose [ printInstNamePrefix i
                , "va_arg"
                , printConstOrName va, ","
                , printType (instructionType i)
                ]
  ConstantC c -> printConstant c

printConstant :: Constant -> String
printConstant c = case c of
  UndefValue { } -> "undef"
  ConstantAggregateZero { } -> "zeroinitializer"
  ConstantPointerNull { } -> "null"
  BlockAddress { blockAddressFunction = f
               , blockAddressBlock = b
               } ->
    mconcat [ "blockaddress("
            , printConstOrNameNoType (Value f), ", "
            , printConstOrNameNoType (Value b), ")"
            ]
  ConstantArray { constantArrayValues = vs } ->
    mconcat [ "[", intercalate ", " $ map printConstOrName vs, "]" ]
  ConstantFP { constantFPValue = d } -> show d
  ConstantInt { constantIntValue = i } -> show i
  ConstantString { constantStringValue = s } -> mconcat [ "c\"", unpack s, "\"" ]
  ConstantStruct { constantStructValues = vs } ->
    mconcat [ "{", intercalate ", " $ map printConstOrName vs, "}" ]
  ConstantVector { constantVectorValues = vs } ->
    mconcat [ "<", intercalate ", " $ map printConstOrName vs, ">" ]
  ConstantValue { constantInstruction = i } ->
    mconcat [ printType (constantType c), " ", printConstInst i ]
  InlineAsm { inlineAsmString = asm
            , inlineAsmConstraints = constraints
            } ->
    mconcat [ "asm \"", unpack asm, "\", \"", unpack constraints, "\"" ]

printArgument :: (Value, [ParamAttribute]) -> String
printArgument (v, atts) =
  compose [ printType $ valueType v
          , unwords $ map show atts
          , printConstOrNameNoType v
          ]

printConstInst :: Instruction -> String
printConstInst valT = case valT of
  TruncInst { } -> printTypecastConst "trunc" valT
  ZExtInst { } -> printTypecastConst "zext" valT
  SExtInst { } -> printTypecastConst "sext" valT
  FPTruncInst { } -> printTypecastConst "fptrunc" valT
  FPExtInst { } -> printTypecastConst "fpext" valT
  FPToUIInst { } -> printTypecastConst "fptoui" valT
  FPToSIInst { } -> printTypecastConst "fptosi" valT
  UIToFPInst { } -> printTypecastConst "uitofp" valT
  SIToFPInst { } -> printTypecastConst "sitofp" valT
  PtrToIntInst { } -> printTypecastConst "ptrtoint" valT
  IntToPtrInst { } -> printTypecastConst "inttoptr" valT
  BitcastInst { } -> printTypecastConst "bitcast" valT
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
  SelectInst { selectCondition = cond
             , selectTrueValue = v1
             , selectFalseValue = v2
             } ->
    mconcat [ "select ("
            , printConstOrName cond, ", "
            , printConstOrName v1, ", "
            , printConstOrName v2, ")"
            ]
  ICmpInst { cmpPredicate = cond
           , cmpV1 = v1
           , cmpV2 = v2
           } ->
    mconcat [ "icmp ", show cond, " ("
            , printConstOrName v1, ", "
            , printConstOrName v2, ")"
            ]
  FCmpInst { cmpPredicate = cond
           , cmpV1 = v1
           , cmpV2 = v2
           } ->
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
  AddInst { } -> printBinaryConst "add" valT
  SubInst { } -> printBinaryConst "sub" valT
  MulInst { } -> printBinaryConst "mul" valT
  DivInst { } -> printBinaryConst "div" valT
  RemInst { } -> printBinaryConst "rem" valT
  ShlInst { } -> printBinaryConst "shl" valT
  LshrInst { } -> printBinaryConst "lshr" valT
  AshrInst { } -> printBinaryConst "ashr" valT
  AndInst { } -> printBinaryConst "and" valT
  OrInst { } -> printBinaryConst "or" valT
  XorInst { } -> printBinaryConst "xor" valT
  _ -> error "Non-constant ValueT"

printBinaryConst :: String -> Instruction -> String
printBinaryConst name inst =
  mconcat [ name, " (", printConstOrName (binaryLhs inst), ", "
          , printConstOrName (binaryRhs inst), ")"
          ]

printTypecastConst :: String -> Instruction -> String
printTypecastConst n inst =
  mconcat [ n, " (", printConstOrName (castedValue inst)
          , " to ", printType (instructionType inst), ")"
          ]

printTailTag :: Bool -> String
printTailTag isTail = if isTail then "tail" else ""

printVolatileFlag :: Bool -> String
printVolatileFlag f = if f then "volatile" else ""

printAlignment :: Int64 -> String
printAlignment align = case align of
  0 -> ""
  _ -> ", align " ++ show align

printTypecast :: String -> Instruction -> String
printTypecast str inst =
  compose [ printInstNamePrefix inst
          , str
          , printConstOrName (castedValue inst)
          , "to"
          , printType (valueType inst) -- newType
          ]

printInBounds :: Bool -> String
printInBounds inBounds = if inBounds then "inbounds" else ""

printFlaggedBinaryOp :: String -> Instruction -> String
printFlaggedBinaryOp str inst =
  compose [ printInstNamePrefix inst
          , str
          , show (binaryArithFlags inst)
          , printType (instructionType inst)
          , printConstOrNameNoType (binaryLhs inst), ","
          , printConstOrNameNoType (binaryRhs inst)
          ]

printBinaryOp :: String -> Instruction -> String
printBinaryOp str inst =
  compose [ printInstNamePrefix inst
          , str
          , printType (instructionType inst)
          , printConstOrNameNoType (binaryLhs inst), ","
          , printConstOrNameNoType (binaryRhs inst)
          ]

printInstNamePrefix :: Instruction -> String
printInstNamePrefix i = case instructionName i of
  Nothing -> ""
  Just n -> mconcat [ show n, " =" ]

-- | This is kind of gross - it only prints out the first piece of
-- metadata.
printDebugTag :: [Metadata] -> String
printDebugTag [] = ""
printDebugTag ((Metadata { metaValueUniqueId = uid }):_) = ", !dbg !" ++ show uid

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
printType (TypeNamed name _) = '%' : name

instance Show Metadata where
  show = printMetadata

instance Show Type where
  show = printType

instance Show Value where
  show = printValue

instance Labellable Value where
  toLabel = (Label . StrLabel) . show
