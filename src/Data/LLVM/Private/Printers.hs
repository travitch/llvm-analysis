module Data.LLVM.Private.Printers ( printMetadata
                                  , printAsm
                                  , printType
                                  , printValue
                                  ) where

import Data.List (intercalate)
import Data.Monoid
import Data.ByteString.Lazy.Char8 (unpack)

import Data.LLVM.Private.AttributeTypes
import Data.LLVM.Private.ReferentialTypes

-- TODO List
--
-- * Pretty up the DataLayout
-- * Print out named type definitions
-- * Print metadata
-- * Make the function type printing as flexible as the official
--   version

-- FIXME: implement this large thing
printMetadata :: Metadata -> String
printMetadata Metadata { metaValueName = Just metaValName
                       , metaValueContent = MetaSourceLocation { metaSourceRow = row
                                                               , metaSourceCol = col
                                                               , metaSourceScope = scope
                                                               }
                       } =
  mconcat [ show metaValName, " = metadata !{i32 "
          , show row, ", i32 ", show col
          , ", metadata ", show (metaValueName scope), " null}"
          ]


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
                               , functionAttrs = fattrs
                               }
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
                      GlobalDeclaration { globalVariableAddressSpace = addrSpace
                                        , globalVariableLinkage = linkage
                                        , globalVariableAnnotation = annot
                                        , globalVariableInitializer = initializer
                                        , globalVariableAlignment = align
                                        , globalVariableSection = section
                                        }
                 , valueType = _
                 , valueName = Just name
                 , valueMetadata = _
                 } =
   -- Don't show t here since showing the initializer will handle it
  compose [ show name, "=", addrSpaceS, linkageS, annotsS
          , initS, sectionS
          , printAlignment align
          ]
  where addrSpaceS = case addrSpace of
          0 -> ""
          _ -> "addrspace(" ++ show addrSpace ++ ")"
        linkageS = show linkage
        annotsS = show annot
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
                 } = mconcat [ label, "\n", instS ]
  where instS = unlines $ map (indent . printValue) instructions
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

printValue Value { valueContent = AllocaInst ty elems align
                 , valueName = name
                 , valueType = _
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

printValue Value { valueContent = LoadInst volatile src align
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

printValue Value { valueContent = StoreInst volatile val dest align
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

printValue Value { valueContent = TruncInst v ty
                 , valueName = name
                 , valueMetadata = md
                 } =
  printTypecast "trunc" name v ty md

printValue Value { valueContent = ZExtInst v ty
                 , valueName = name
                 , valueMetadata = md
                 } =
  printTypecast "zext" name v ty md

printValue Value { valueContent = SExtInst v ty
                 , valueName = name
                 , valueMetadata = md
                 } =
  printTypecast "sext" name v ty md

printValue Value { valueContent = FPTruncInst v ty
                 , valueName = name
                 , valueMetadata = md
                 } =
  printTypecast "fptrunc" name v ty md

printValue Value { valueContent = FPExtInst v ty
                 , valueName = name
                 , valueMetadata = md
                 } =
  printTypecast "fpext" name v ty md

printValue Value { valueContent = FPToUIInst v ty
                 , valueName = name
                 , valueMetadata = md
                 } =
  printTypecast "fptoui" name v ty md

printValue Value { valueContent = FPToSIInst v ty
                 , valueName = name
                 , valueMetadata = md
                 } =
  printTypecast "fptosi" name v ty md

printValue Value { valueContent = UIToFPInst v ty
                 , valueName = name
                 , valueMetadata = md
                 } =
  printTypecast "uitofp" name v ty md

printValue Value { valueContent = SIToFPInst v ty
                 , valueName = name
                 , valueMetadata = md
                 } =
  printTypecast "sitofp" name v ty md

printValue Value { valueContent = PtrToIntInst v ty
                 , valueName = name
                 , valueMetadata = md
                 } =
  printTypecast "ptrtoint" name v ty md

printValue Value { valueContent = IntToPtrInst v ty
                 , valueName = name
                 , valueMetadata = md
                 } =
  printTypecast "inttoptr" name v ty md

printValue Value { valueContent = BitcastInst v ty
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
                               , callRetType = rtype
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

printValue Value { valueContent =
                      InvokeInst { invokeConvention = cc
                                 , invokeParamAttrs = pattrs
                                 , invokeRetType = _
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

printValue Value { valueContent = VaArgInst v ty
                 , valueName = name
                 , valueType = _
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

printValue Value { valueContent = MetadataValue _ } = error "Can't print metadata yet"

printArgument :: (Value, [ParamAttribute]) -> String
printArgument (v, atts) =
  compose [ printType $ valueType v
          , unwords $ map show atts
          , printConstOrNameNoType v
          ]

printConstExp :: ValueT -> String
printConstExp valT = case valT of
  TruncInst v t -> printTypecastConst "trunc" v t
  ZExtInst v t -> printTypecastConst "zext" v t
  SExtInst v t -> printTypecastConst "sext" v t
  FPTruncInst v t -> printTypecastConst "fptrunc" v t
  FPExtInst v t -> printTypecastConst "fpext" v t
  FPToUIInst v t -> printTypecastConst "fptoui" v t
  FPToSIInst v t -> printTypecastConst "fptosi" v t
  UIToFPInst v t -> printTypecastConst "uitofp" v t
  SIToFPInst v t -> printTypecastConst "sitofp" v t
  PtrToIntInst v t -> printTypecastConst "ptrtoint" v t
  IntToPtrInst v t -> printTypecastConst "inttoptr" v t
  BitcastInst v t -> printTypecastConst "bitcast" v t
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

printAlignment :: Integer -> String
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

printFlaggedBinaryOp :: String -> Maybe Identifier -> [ArithFlag] ->
                        Type -> Value -> Value -> Maybe Metadata -> String
printFlaggedBinaryOp inst name flags t v1 v2 _ =
  compose [ printInstNamePrefix name, inst
          , unwords $ map show flags
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

printDebugTag :: Maybe Value -> String
printDebugTag Nothing = ""
printDebugTag (Just (Value { valueName = Just n })) = ", !dbg " ++ show n
printDebugTag (Just e) = error $ "Not metadata: " ++ printValue e

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
printType (TypePointer ty) = mconcat [ printType ty, "*" ]
printType (TypeStruct ts) = mconcat [ "{", fieldVals, "}" ]
  where fieldVals = intercalate ", " $ map printType ts
printType (TypePackedStruct ts) = mconcat [ "<{", fieldVals, "}>" ]
  where fieldVals = intercalate ", " $ map printType ts
printType (TypeNamed name _) = name

instance Show Metadata where
  show = printMetadata

instance Show Type where
  show = printType

instance Show Value where
  show = printValue
