module Data.LLVM.Private.Translators.Functions ( translateFunctionDefinition ) where

import Data.Map (Map, (!))
import qualified Data.Map as M

import Data.LLVM.Types
import Data.LLVM.Private.AttributeTypes
import qualified Data.LLVM.Private.PlaceholderTypes as O
import Data.LLVM.Private.Translators.Instructions

mkFuncType :: (O.Type -> Type) -> O.GlobalDeclaration -> Type
mkFuncType typeMapper O.FunctionDefinition { O.funcRetType = fret
                                           , O.funcParams = params
                                           , O.funcIsVararg = isVararg
                                           , O.funcAttrs = attrs
                                           } = llvmType
  where rtype = typeMapper fret
        argTypes = map (typeMapper . xtype) params
        xtype (O.FormalParameter t _ _) = t
        llvmType = TypeFunction rtype argTypes isVararg attrs
mkFuncType _ _ = error "Non-func decl in mkFuncType"

translateFunctionDefinition :: (O.Type -> Type) ->
                               ((Map Identifier Value) -> O.Constant -> Value) ->
                               (Map Identifier Metadata) ->
                               (Map Identifier Value) ->
                               O.GlobalDeclaration ->
                               Map Identifier Value
translateFunctionDefinition typeMapper pTransValOrConst globalMetadata vals decl =
  M.insert (O.funcName decl) v vals
  where v = Value { valueType = ftype
                  , valueName = Just ident
                  , valueMetadata = getMetadata ident
                  , valueContent =
                    Function { functionType = ftype
                             , functionParameters = map translateParameter $ O.funcParams decl
                             , functionBody = translatedBody
                             , functionLinkage = O.funcLinkage decl
                             , functionVisibility = O.funcVisibility decl
                             , functionCC = O.funcCC decl
                             , functionRetAttrs = O.funcRetAttrs decl
                             , functionName = O.funcName decl
                             , functionSection = O.funcSection decl
                             , functionAlign = O.funcAlign decl
                             , functionGCName = O.funcGCName decl
                             , functionIsVararg = O.funcIsVararg decl
                             }
                  }
        transValOrConst = pTransValOrConst localVals

        -- Trivial metadata lookup function
        getMetadata i = M.lookup i globalMetadata

        -- Helper to translate the placeholder constants from the parser
        -- into real values
        trConst (O.ValueRef localIdent@(LocalIdentifier _)) = localVals ! localIdent
        trConst c = transValOrConst c

        -- Translated type of the function
        ftype = mkFuncType typeMapper decl
        -- Name of the function
        ident = O.funcName decl
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
        (localVals, translatedBody) = translateBody noDebugBody
        translateParameter (O.FormalParameter ty attrs paramIdent) =
          Value { valueType = typeMapper ty
                , valueName = Just paramIdent
                -- Note: Parameter metadata is mapped in an odd way -
                -- via pseudo-calls to llvm.dbg.declare (or .value).
                -- We construct this map by preprocessing the function
                -- body so that we can map metadata to parameters here.
                , valueMetadata = M.lookup paramIdent localMetadata
                , valueContent = Argument attrs
                }
        translateBody = foldr translateBlock (M.empty, [])
        translateBlock (O.BasicBlock blockName placeholderInsts) (locals, blocks) =
          (M.insert ident bb blocksWithLocals, bb : blocks)
          where bb = Value { valueType = TypeVoid
                           , valueName = Just blockName
                           , valueMetadata = Nothing -- can BBs have metadata?
                           , valueContent = BasicBlock insts
                           }
                (blocksWithLocals, insts) = translateInsts locals placeholderInsts
        translateInsts locals = foldr trInst (locals, [])
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
        destructureDebugCall [ (O.ConstValue (O.MDNode [Just (O.ValueRef varRef)]) _)
                             , (O.ValueRef i@(MetaIdentifier _)) ] acc@(md, insts) =
          case getMetadata i of
            Nothing -> acc
            Just metadata -> (M.insert varRef metadata md, insts)
        destructureDebugCall [ (O.ConstValue (O.MDNode [Just (O.ValueRef varRef)]) _)
                             , _
                             , (O.ValueRef i@(MetaIdentifier _)) ] acc@(md, insts) =
          case getMetadata i of
            Nothing -> acc
            Just metadata -> (M.insert varRef metadata md, insts)
        destructureDebugCall _ acc = acc


-- This handles updating the accumulator in the big
-- translateInstruction fold.  Always add the new value to the
-- instruction list.  Add it to the local variable map only if it has
-- a name.
foldResult :: (Map Identifier Value, [Value]) -> Value ->
              (Map Identifier Value, [Value])
foldResult (locals, insts) val = case valueName val of
  Just i -> (M.insert i val locals, val : insts)
  Nothing -> (locals, val : insts)

transInst :: (O.Type -> Type) ->
             (O.Constant -> Value) ->
             (Identifier -> Maybe Metadata) ->
             O.Instruction ->
             (Map Identifier Value, [Value]) ->
             (Map Identifier Value, [Value])
transInst typeMapper trConst getMetadata i acc =
  foldResult acc newValue
  where newValue = repackInst i content
        oldContent = case i of
          O.Instruction { O.instContent = oc } -> oc
          O.UnresolvedInst { O.unresInstContent = oc } -> oc
        content = translateInstruction typeMapper trConst oldContent

        -- This helper converts the non-content fields of an instruction to
        -- the equivalent fields in a Value.  It performs the necessary
        -- translations to fetch metadata and handle type differences If we
        -- want to attach metadata to alloca nodes, we could pass in the local
        -- metadata map here and search for iname - if there is an entry in
        -- that map then it is actually a local and we have extra metadata
        -- FIXME: Here we need to give types to getelementptr and extractvalue
        -- instructions ; they couldn't be determined earlier since the type
        -- graph wasn't yet constructed.
        repackInst O.Instruction { O.instType = itype
                                 , O.instName = iname
                                 , O.instMetadata = md
                                 } newContent = v
          where v = Value { valueType = typeMapper itype
                          , valueName = iname
                          , valueMetadata = maybe Nothing getMetadata md
                          , valueContent = newContent
                          }
        repackInst ui@O.UnresolvedInst { O.unresInstName = iname
                                       , O.unresInstMetadata = md
                                       } newContent = v
          where v = Value { valueType = t
                          , valueName = iname
                          , valueMetadata = maybe Nothing getMetadata md
                          , valueContent = newContent
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
traceTypeIndicesEV t (idx:rest) = case t of
  TypePointer innerType -> traceTypeIndicesEV innerType rest
  TypeArray _ innerType -> traceTypeIndicesEV innerType rest
  TypeStruct ts -> traceTypeIndicesEV (ts !! fromIntegral idx) rest
  TypePackedStruct ts -> traceTypeIndicesEV (ts !! fromIntegral idx) rest
  _ -> error ("Invalid type for ExtractValue: " ++ show t)
traceTypeIndicesEV t [] = t

traceTypeIndicesGEP :: Type -> [Value] -> Type
traceTypeIndicesGEP t (idx:rest) = case t of
  TypePointer innerType -> traceTypeIndicesGEP innerType rest
  TypeArray _ innerType -> traceTypeIndicesGEP innerType rest
  TypeStruct ts -> traceTypeIndicesGEP (ts !! fromIntegral (getConstIntVal idx)) rest
  TypePackedStruct ts -> traceTypeIndicesGEP (ts !! fromIntegral (getConstIntVal idx)) rest
  _ -> error ("Invalid type for GetElementPtr: " ++ show t)
traceTypeIndicesGEP t [] = t

getConstIntVal :: Value -> Integer
getConstIntVal Value { valueContent = ConstantInt i } = i
getConstIntVal v = error ("Value is not a constant integer: " ++ show v)
