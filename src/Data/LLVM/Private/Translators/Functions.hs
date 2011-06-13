{-# LANGUAGE OverloadedStrings #-}
module Data.LLVM.Private.Translators.Functions (
  SymbolTable,
  translateFunctionDefinition
  ) where

import Data.List ( mapAccumR )
import Data.HashMap.Strict ( HashMap )
import qualified Data.HashMap.Strict as M

import Data.LLVM.Types
import Data.LLVM.Private.UniqueId
import Data.LLVM.Private.Types.Identifiers
import qualified Data.LLVM.Private.Types.Placeholder as O
import Data.LLVM.Private.Translators.Instructions

-- | The type of a 'Function's local symbol table.
type SymbolTable = HashMap Identifier Value

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
                               (O.Constant -> IdStream -> Value) ->
                               Map Identifier Metadata ->
                               IdStream ->
                               O.GlobalDeclaration ->
                               (SymbolTable, Value)
translateFunctionDefinition typeMapper trConst globalMetadata idstream decl =
  (localVals, v)
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
                             , functionName = functionIdentifier
                             , functionSection = O.funcSection decl
                             , functionAlign = fromIntegral $ O.funcAlign decl
                             , functionGCName = O.funcGCName decl
                             , functionIsVararg = O.funcIsVararg decl
                             , functionAttrs = O.funcAttrs decl
                             }
                  , valueUniqueId = thisId
                  }
        thisId = extract idstream
        restIds = split idstream

        -- Trivial metadata lookup function
        getMetadata i = M.lookup i globalMetadata

        -- Name of the function
        functionIdentifier = O.funcName decl
        -- Translated type of the function
        ftype = mkFuncType typeMapper decl
        placeholderParams = O.funcParams decl

        -- Reserve unique IDs for parameters without splitting the
        -- stream.  We don't need to split the stream since parameter
        -- creation doesn't involve recursive translation.  The rest
        -- of the IDs are for the translation of the function body.
        -- (paramIds, bodyIds) = splitAt (length placeholderParams) restIds
        (paramIds, bodyIds) = split2 restIds
        -- Map from parameter names to their translated values
        (_, parameterVals) = mapAccumR translateParameter paramIds placeholderParams
        translateParameter pidsrc (O.FormalParameter ty attrs paramIdent) =
          (split pidsrc,
           Value { valueType = typeMapper ty
                 , valueName = Just paramIdent
                 -- Note: Parameter metadata is mapped in an odd way -
                 -- via pseudo-calls to llvm.dbg.declare (or .value).
                 -- We construct this map by preprocessing the function
                 -- body so that we can map metadata to parameters here.
                 , valueMetadata = M.lookup paramIdent localMetadata
                 , valueContent = Argument attrs
                 , valueUniqueId = uid
                 })
          where uid = extract pidsrc
        -- This is the initial list of local values available to the function
        -- Parameters must have names
        paramMap = M.fromList $ zip (map xtract parameterVals) parameterVals
          where xtract = (maybe (error "Parameters must have identifiers") id) . valueName

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
        -- instructions to values.  The ignored return value is the
        -- rest of the ID stream.
        ((_, localVals), translatedBody) = translateBody noDebugBody


        -- Note, the set of locals is initialized with the parameters
        -- of the function.
        translateBody = mapAccumR translateBlock (bodyIds, paramMap)

        translateBlock (blockIdSrc, locals) (O.BasicBlock blockName placeholderInsts) =
          ((restStream, updatedMap), bb)
          where bb = Value { valueType = TypeLabel
                           , valueName = blockName
                           , valueMetadata = Nothing -- can BBs have metadata?
                           , valueContent = BasicBlock insts
                           , valueUniqueId = extract blockIdSrc
                           }
                (blockStream, restStream) = split2 blockIdSrc
                ((_,blocksWithLocals), insts) = translateInsts locals blockStream placeholderInsts
                updatedMap = case blockName of
                  Nothing -> blocksWithLocals
                  Just aBlockName -> M.insert aBlockName bb blocksWithLocals

        translateInsts locals inststream =
          mapAccumR trInst (inststream, locals)
        trInst = transInst typeMapper trConst getMetadata
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
                                             O.ValueRef GlobalIdentifier { globalIdentifier = "llvm.dbg.declare" }
                                        , O.callArguments = args
                                        }
                        } -> destructureDebugCall args acc
          O.Instruction { O.instContent =
                             O.CallInst { O.callFunction =
                                             O.ValueRef GlobalIdentifier { globalIdentifier = "llvm.dbg.value" }
                                        , O.callArguments = args
                                        }
                        } -> destructureDebugCall args acc
          -- Not a debug call - just include it
          _ -> (md, i : insts)

        -- Always discard the instruction, but update the metadata map
        -- when possible
        destructureDebugCall [ (O.ConstValue (O.MDNode [Just (O.ValueRef varRef)]) _, [])
                             , (O.ValueRef i@(MetaIdentifier {}), []) ] acc@(md, insts) =
          case getMetadata i of
            Nothing -> acc
            Just metadata -> (M.insert varRef metadata md, insts)
        destructureDebugCall [ (O.ConstValue (O.MDNode [Just (O.ValueRef varRef)]) _, [])
                             , _
                             , (O.ValueRef i@(MetaIdentifier {}), []) ] acc@(md, insts) =
          case getMetadata i of
            Nothing -> acc
            Just metadata -> (M.insert varRef metadata md, insts)
        destructureDebugCall _ acc = acc


transInst :: (O.Type -> Type) ->
             (O.Constant -> IdStream -> Value) ->
             (Identifier -> Maybe Metadata) ->
             (IdStream, Map Identifier Value) ->
             O.Instruction ->
             ((IdStream, Map Identifier Value), Value)
transInst typeMapper trConst getMetadata (idStream, dict) i =
  case instName of
    Nothing -> ((restStream, dict), newValue)
    Just valName -> ((restStream, M.insert valName newValue dict), newValue)
  where newValue = repackInst i content
        thisId = extract idStream
        oldContent = case i of
          O.Instruction { O.instContent = oc } -> oc
          O.UnresolvedInst { O.unresInstContent = oc } -> oc
        instName = case i of
          O.Instruction { O.instName = iname } -> iname
          O.UnresolvedInst { O.unresInstName = iname } -> iname
        content = translateInstruction typeMapper trConst oldContent instStream

        (instStream, restStream) = split2 idStream

        -- This helper converts the non-content fields of an instruction to
        -- the equivalent fields in a Value.  It performs the necessary
        -- translations to fetch metadata and handle type differences If we
        -- want to attach metadata to alloca nodes, we could pass in the local
        -- metadata map here and search for iname - if there is an entry in
        -- that map then it is actually a local and we have extra metadata
        repackInst O.Instruction { O.instType = itype
                                 , O.instName = iname
                                 , O.instMetadata = md
                                 } newContent = v
          where v = Value { valueType = typeMapper itype
                          , valueName = iname
                          , valueMetadata = maybe Nothing getMetadata md
                          , valueContent = newContent
                          , valueUniqueId = thisId
                          }
        repackInst ui@O.UnresolvedInst { O.unresInstName = iname
                                       , O.unresInstMetadata = md
                                       } newContent = v
          where v = Value { valueType = t
                          , valueName = iname
                          , valueMetadata = maybe Nothing getMetadata md
                          , valueContent = newContent
                          , valueUniqueId = thisId
                          }
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
-- All values in LLVM refer to pointers
traceTypeIndicesEV t [] = TypePointer t

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
-- All values in LLVM refer to pointers.  As we walk through the
-- structure in the GEP instruction, these are real values, but the
-- final type of the value will be a pointer to whatever the indices
-- indicate.
traceTypeIndicesGEP t [] = TypePointer t

getConstIntVal :: Value -> Integer
getConstIntVal Value { valueContent = ConstantInt i } = i
getConstIntVal v = error ("Value is not a constant integer: " ++ show v)
