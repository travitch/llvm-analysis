{-# LANGUAGE OverloadedStrings #-}
module Data.LLVM.Private.Translators.Functions ( translateFunctionDefinition ) where

import Data.Map (Map, (!))
import qualified Data.Map as M

import Data.LLVM.Types
import Data.LLVM.Private.KnotHelpers
import qualified Data.LLVM.Private.PlaceholderTypes as O
import Data.LLVM.Private.Translators.Instructions

import Debug.Trace
debug = flip trace

mkFuncType :: (O.Type -> Type) -> O.GlobalDeclaration -> Type
mkFuncType typeMapper O.FunctionDefinition { O.funcRetType = fret
                                           , O.funcParams = params
                                           , O.funcIsVararg = isVararg
                                           } = llvmType
  where rtype = typeMapper fret
        argTypes = map (typeMapper . xtype) params
        xtype (O.FormalParameter t _ _) = t
        llvmType = TypeFunction rtype argTypes isVararg
mkFuncType _ _ = error "Non-func decl in mkFuncType"

translateFunctionDefinition :: (O.Type -> Type) ->
                               (O.Constant -> IdentDict -> (Value, IdentDict)) ->
                               (Map Identifier Metadata) ->
                               IdentDict ->
                               O.GlobalDeclaration ->
                               IdentDict
translateFunctionDefinition typeMapper trConst globalMetadata vals decl =
  addGlobal fName v finalDict
  where v = Value { valueType = ftype
                  , valueName = Just functionIdentifier
                  , valueMetadata = getMetadata functionIdentifier
                  , valueContent =
                    Function { functionType = ftype
                             , functionParameters = parameterVals
                             , functionBody = translatedBody
                             , functionLinkage = O.funcLinkage decl
                             , functionVisibility = O.funcVisibility decl
                             , functionCC = O.funcCC decl
                             , functionRetAttrs = O.funcRetAttrs decl
                             , functionName = fName
                             , functionSection = O.funcSection decl
                             , functionAlign = O.funcAlign decl
                             , functionGCName = O.funcGCName decl
                             , functionIsVararg = O.funcIsVararg decl
                             , functionAttrs = O.funcAttrs decl
                             }
                  , valueUniqueId = funcUID
                  }
        (funcUID, finalDict) = nextSequenceNumber dictWithLocals

        addFuncLocal = addLocal fName

        -- Trivial metadata lookup function
        getMetadata i = M.lookup i globalMetadata

        -- Helper to translate the placeholder constants from the parser
        -- into real values
        -- trConst (O.ValueRef localIdent@(LocalIdentifier _)) =
        --   (localVals ! localIdent)
        -- trConst c = transValOrConst c
        fName = O.funcName decl
        -- Translated type of the function
        ftype = mkFuncType typeMapper decl
        -- Map from parameter names to their translated values
        (parameterVals, dictWithParams) =
          foldr translateParameter ([], vals) (O.funcParams decl)
        -- Name of the function
        functionIdentifier = O.funcName decl
        -- Remove @llvm.dbg.* calls from the body after extracting the
        -- information they provide.  Some of the debug information
        -- (e.g. changes in variable values) are ignored.  That
        -- information is implicit in phi nodes.  There is actually
        -- some information in this metadata map for local variables,
        -- too.  I'm not attaching that for now since it doesn't seem
        -- all that useful and is somewhat inconvenient.  It could be
        -- fixed if necessary.
        (localMetadata, noDebugBody) = stripDebugCalls (O.funcBody decl)
        -- Tie the knot on the function body, converting all
        -- instructions to values
        (translatedBody, dictWithLocals) = translateBody noDebugBody
        translateParameter (O.FormalParameter ty attrs paramIdent) (acc, d) =
          (param : acc, addFuncLocal paramIdent param d')
          where param = Value { valueType = typeMapper ty
                              , valueName = Just paramIdent
                              -- Note: Parameter metadata is mapped in an odd way -
                              -- via pseudo-calls to llvm.dbg.declare (or .value).
                              -- We construct this map by preprocessing the function
                              -- body so that we can map metadata to parameters here.
                              , valueMetadata = M.lookup paramIdent localMetadata
                              , valueContent = Argument attrs
                              , valueUniqueId = uid
                              }
                (uid, d') = nextSequenceNumber d
        -- Note, the set of locals is initialized with the parameters
        -- of the function.
        translateBody = foldr translateBlock ([], dictWithParams)
        translateBlock (O.BasicBlock blockName placeholderInsts) (blocks, locals) =
          (bb : blocks, updatedMap)
          where bb = Value { valueType = TypeLabel
                           , valueName = blockName
                           , valueMetadata = Nothing -- can BBs have metadata?
                           , valueContent = BasicBlock insts
                           , valueUniqueId = blockid
                           }
                (insts, blocksWithLocals) = translateInsts locals placeholderInsts
                (blockid, localsDict) = nextSequenceNumber blocksWithLocals
                updatedMap = case blockName of
                  Nothing -> localsDict
                  Just aBlockName -> addFuncLocal aBlockName bb localsDict

        translateInsts locals = foldr trInst ([], locals)
        trInst = transInst addFuncLocal typeMapper trConst getMetadata
        -- Returns a new body and a map of identifiers (vals) to
        -- metadata identifiers
        stripDebugCalls = foldr undebugBlock (M.empty, [])
        undebugBlock (O.BasicBlock blockName is) (md, blocks) = (md', newBlock:blocks)
          where (md', is') = foldr undebugInst (md, []) is
                newBlock = O.BasicBlock blockName is'
        undebugInst :: O.Instruction -> (Map Identifier Metadata, [O.Instruction]) -> (Map Identifier Metadata, [O.Instruction])
        undebugInst i acc@(md, insts) = case i of
          O.Instruction { O.instContent =
                             O.CallInst { O.callFunction =
                                             O.ValueRef (GlobalIdentifier "llvm.dbg.declare")
                                        , O.callArguments = args
                                        }
                        } -> destructureDebugCall args acc
          O.Instruction { O.instContent =
                             O.CallInst { O.callFunction =
                                             O.ValueRef (GlobalIdentifier "llvm.dbg.value")
                                        , O.callArguments = args
                                        }
                        } -> destructureDebugCall args acc
          -- Not a debug call - just include it
          _ -> (md, i : insts)

        -- Always discard the instruction, but update the metadata map
        -- when possible
        destructureDebugCall [ ((O.ConstValue (O.MDNode [Just (O.ValueRef varRef)]) _), [])
                             , ((O.ValueRef i@(MetaIdentifier _)), []) ] acc@(md, insts) =
          case getMetadata i of
            Nothing -> acc
            Just metadata -> (M.insert varRef metadata md, insts)
        destructureDebugCall [ ((O.ConstValue (O.MDNode [Just (O.ValueRef varRef)]) _), [])
                             , _
                             , ((O.ValueRef i@(MetaIdentifier _)), []) ] acc@(md, insts) =
          case getMetadata i of
            Nothing -> acc
            Just metadata -> (M.insert varRef metadata md, insts)
        destructureDebugCall _ acc = acc


transInst :: (Identifier -> Value -> IdentDict -> IdentDict) ->
             (O.Type -> Type) ->
             (O.Constant -> IdentDict -> (Value, IdentDict)) ->
             (Identifier -> Maybe Metadata) ->
             O.Instruction ->
             ([Value], IdentDict) ->
             ([Value], IdentDict)
transInst addFuncLocal typeMapper trConst getMetadata i (insts, dict) =
  case instName of
    Nothing -> (newValue : insts, valueDict)
    Just valName -> (newValue : insts, addFuncLocal valName newValue valueDict)
  where (newValue, valueDict) = repackInst i content instDict
        oldContent = case i of
          O.Instruction { O.instContent = oc } -> oc
          O.UnresolvedInst { O.unresInstContent = oc } -> oc
        instName = case i of
          O.Instruction { O.instName = iname } -> iname
          O.UnresolvedInst { O.unresInstName = iname } -> iname
        (content, instDict) = translateInstruction typeMapper trConst oldContent dict

        -- This helper converts the non-content fields of an instruction to
        -- the equivalent fields in a Value.  It performs the necessary
        -- translations to fetch metadata and handle type differences If we
        -- want to attach metadata to alloca nodes, we could pass in the local
        -- metadata map here and search for iname - if there is an entry in
        -- that map then it is actually a local and we have extra metadata
        repackInst O.Instruction { O.instType = itype
                                 , O.instName = iname
                                 , O.instMetadata = md
                                 } newContent d = (v, d')
          where v = Value { valueType = typeMapper itype
                          , valueName = iname
                          , valueMetadata = maybe Nothing getMetadata md
                          , valueContent = newContent
                          , valueUniqueId = uid
                          }
                (uid, d') = nextSequenceNumber d
        repackInst ui@O.UnresolvedInst { O.unresInstName = iname
                                       , O.unresInstMetadata = md
                                       } newContent d = (v, d')
          where v = Value { valueType = t
                          , valueName = iname
                          , valueMetadata = maybe Nothing getMetadata md
                          , valueContent = newContent
                          , valueUniqueId = uid
                          }
                (uid, d') = nextSequenceNumber d
                t = case newContent of
                  ExtractValueInst { extractValueAggregate = agg
                                   , extractValueIndices = indices
                                   } -> traceTypeIndicesEV (valueType agg) indices
                  GetElementPtrInst { getElementPtrValue = agg
                                    , getElementPtrIndices = indices
                                    } -> traceTypeIndicesGEP (valueType agg) indices
                  _ -> error ("Unpexected UnresolvedInst: " ++ show ui)

-- Vectors are not allowed as arguments to GetElementPtr.  They are
-- probably also not allowed for ExtractValue because ExtractElement
-- is the vector version.  The ExtractValue version only takes a list
-- of Int indices.  GEP is more general and accepts values at runtime.
--
-- For pointer and array types, we don't need the index for the type
-- computation - it can only be the inner type since those are
-- heterogeneous structures.  For the struct types, the value must be
-- a constant, otherwise we don't have a statically-typed language
-- anymore.
traceTypeIndicesEV :: Type -> [Integer] -> Type
traceTypeIndicesEV t indices@(idx:rest) = case t of
  TypePointer innerType -> traceTypeIndicesEV innerType rest
  TypeArray _ innerType -> traceTypeIndicesEV innerType rest
  TypeStruct ts -> traceTypeIndicesEV (ts !! fromIntegral idx) rest
  TypePackedStruct ts -> traceTypeIndicesEV (ts !! fromIntegral idx) rest
  TypeNamed _ realType -> traceTypeIndicesEV realType indices
  _ -> error ("Invalid type for ExtractValue: " ++ show t)
traceTypeIndicesEV t [] = t

traceTypeIndicesGEP :: Type -> [Value] -> Type
traceTypeIndicesGEP t indices@(idx:rest) = case t of
  TypePointer innerType -> traceTypeIndicesGEP innerType rest
  TypeArray _ innerType -> traceTypeIndicesGEP innerType rest
  TypeStruct ts -> traceTypeIndicesGEP (ts !! fromIntegral (getConstIntVal idx)) rest
  TypePackedStruct ts -> traceTypeIndicesGEP (ts !! fromIntegral (getConstIntVal idx)) rest
  -- Follow the named type, but don't consume an index (since we
  -- haven't checked it yet)
  TypeNamed _ realType -> traceTypeIndicesGEP realType indices
  _ -> error ("Invalid type for GetElementPtr: " ++ show t)
traceTypeIndicesGEP t [] = t

getConstIntVal :: Value -> Integer
getConstIntVal Value { valueContent = ConstantInt i } = i
getConstIntVal v = error ("Value is not a constant integer: " ++ show v)
