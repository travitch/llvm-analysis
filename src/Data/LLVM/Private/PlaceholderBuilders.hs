module Data.LLVM.Private.PlaceholderBuilders ( mkExtractElementInst
                                             , mkInsertElementInst
                                             , mkDataLayout
                                             , mkTriple
                                             , mkShuffleVectorInst
                                             , mkInsertValueInst
                                             , mkAllocaInst
                                             , mkLoadInst
                                             , mkStoreInst
                                             , mkConversionInst
                                             , mkIcmpInst
                                             , mkFcmpInst
                                             , mkPhiNode
                                             , mkSelectInst
                                             , mkFlaggedArithInst
                                             , mkArithInst
                                             , mkCallInst
                                             , mkInvokeInst
                                             , mkVaArgInst
                                             , mkExtractValueInst
                                             , mkGetElementPtrInst
                                             , mkExternalFuncDecl
                                             , mkGlobalDecl
                                             , mkBasicBlock
                                             , mkFunctionDef
                                             ) where

import Data.ByteString.Lazy (ByteString)

import Data.LLVM.Private.AttributeTypes
import Data.LLVM.Private.PlaceholderTypes

mkExtractElementInst :: (Monad m) => Identifier -> Type -> PartialConstant -> Constant -> m Instruction
mkExtractElementInst name ty1 v1 v2 =
  case ty1 of
    TypeVector _ t -> return $ namedInst name t $ ExtractElementInst (v1 ty1) v2
    _ -> fail "Non-vector type in extractelement"

mkInsertElementInst :: Identifier -> Type -> PartialConstant -> Constant -> Constant -> Instruction
mkInsertElementInst name tyr val sclr idx =
  namedInst name tyr $ InsertElementInst (val tyr) sclr idx

-- FIXME: Parse the bytestring - have the code for this already in the
-- old attoparsec-based parser
mkDataLayout :: a -> DataLayout
mkDataLayout s = defaultDataLayout

mkTriple :: ByteString -> TargetTriple
mkTriple = TargetTriple

mkShuffleVectorInst :: (Monad m) => Identifier -> Type -> PartialConstant -> Type -> PartialConstant -> Type -> PartialConstant -> m Instruction
mkShuffleVectorInst name t1 val1 t2 val2 t3 mask
  | t1 == t2 =
    case (t1, t3) of
      (TypeVector _ t, TypeVector n _) ->
        return $ namedInst name (TypeVector n t) (ShuffleVectorInst (val1 t1) (val2 t2) (mask t3))
      _ -> fail "Non-vector type for vec or mask in shufflevector"

  | otherwise = fail "Input vector types do not match"

-- FIXME: Add checks to ensure the type of the inserted element is correct
-- based on the index
mkInsertValueInst :: Identifier -> Type -> PartialConstant -> Type -> PartialConstant -> Integer -> Instruction
mkInsertValueInst name t1 v1 t2 v2 idx =
  namedInst name t1 $ InsertValueInst (v1 t1) (v2 t2) idx

mkExtractValueInst :: (Monad m) => Identifier -> Type -> PartialConstant -> [Integer] -> m Instruction
mkExtractValueInst ident inType val indices =
  return UnresolvedInst { unresInstName = Just ident
                        , unresInstContent = ExtractValueInst (val inType) indices
                        }

mkAllocaInst :: Identifier -> Type -> Constant -> Integer -> Instruction
mkAllocaInst name ty num align =
  namedInst name (TypePointer ty) $ AllocaInst ty num align

-- The type of a load is ty with a layer of pointer type unwrapped.
-- The input *must* be a pointer type
mkLoadInst :: Identifier -> Bool -> Type -> PartialConstant -> Integer -> Instruction
mkLoadInst ident volatile ty val align =
  namedInst ident ty' $ LoadInst volatile (val ty) align
  where (TypePointer ty') = ty

mkStoreInst :: (Monad m) => Bool -> Type -> PartialConstant -> Type -> PartialConstant -> Integer -> m Instruction
mkStoreInst volatile t1 val t2 dest align
  | t2 == t1 = return $ voidInst $ StoreInst volatile (val t1) (dest t2) align
  | otherwise = fail "Store type mismatch"
  where (TypePointer t2') = t2

mkFlaggedArithInst :: (Monad m) => (a -> Constant -> Constant -> InstructionT) -> Identifier -> Type -> a -> PartialConstant -> PartialConstant -> m Instruction
mkFlaggedArithInst inst ident ty flags v1 v2 =
  return $ namedInst ident ty $ inst flags (v1 ty) (v2 ty)

mkArithInst :: (Monad m) => (Constant -> Constant -> InstructionT) -> Identifier -> Type -> PartialConstant -> PartialConstant -> m Instruction
mkArithInst inst ident ty v1 v2 =
  return $ namedInst ident ty $ inst (v1 ty) (v2 ty)

-- Build a conversion instruction using the provided type constructor.
-- Examples: TruncInst, ZextInst
-- These all follow the same pattern: convert a value of t1 to t2.
mkConversionInst :: (Monad m) => (Constant -> Type -> InstructionT) -> Identifier -> Constant -> Type -> m Instruction
mkConversionInst inst ident val t =
  return $ namedInst ident t $ inst val t

mkIcmpInst :: (Monad m) => Identifier -> ICmpCondition -> Type -> PartialConstant -> PartialConstant -> m Instruction
mkIcmpInst ident cond t v1 v2 =
  return $ namedInst ident t' $ ICmpInst cond (v1 t) (v2 t)
  -- The result type is i1 for scalars, or a vector of i1 with one
  -- entry per element in the input vectors
  where t' = case t of
          TypeVector n innerType -> TypeVector n (TypeInteger 1)
          _ -> TypeInteger 1


mkFcmpInst :: (Monad m) => Identifier -> FCmpCondition -> Type -> PartialConstant -> PartialConstant -> m Instruction
mkFcmpInst ident cond t v1 v2 =
  return $ namedInst ident t' $ FCmpInst cond (v1 t) (v2 t)
  -- The result type is i1 for scalars, or a vector of i1 with one
  -- entry per element in the input vectors
  where t' = case t of
          TypeVector n innerType -> TypeVector n (TypeInteger 1)
          _ -> TypeInteger 1

mkPhiNode :: (Monad m) => Identifier -> Type -> [(PartialConstant, Identifier)] -> m Instruction
mkPhiNode ident ty vals =
  return $ namedInst ident ty $ PhiNode (map applicator vals)
  where applicator (pc, id) = (pc ty, id)

-- this doesn't encode the semantics very well (though they are
-- represented).  If selty is a vector i1, then the selection is
-- performed element-wise in the vectors.  That said, this behavior
-- isn't implemented in LLVM yet so it is a moot point, for now.
mkSelectInst :: (Monad m) => Identifier -> Type -> PartialConstant -> Type -> PartialConstant -> Type -> PartialConstant -> m Instruction
mkSelectInst ident selty sel t1 v1 t2 v2 = do
  if t1 /= t2
    then fail "Vectors must be of the same type"
    else mk'
  where mk' = return $ namedInst ident t1 $ SelectInst (sel selty) (v1 t1) (v2 t2)

mkCallInst :: (Monad m) => Maybe Identifier -> Bool -> CallingConvention -> [ParamAttribute] -> Type -> Maybe Type -> PartialConstant -> [Constant] -> [FunctionAttribute] -> m Instruction
mkCallInst mident isTail cc pattrs rtype mftype func params funcAttrs =
  return $ maybeNamedInst mident rtype i
  where i = CallInst { callIsTail = isTail
                     , callConvention = cc
                     , callParamAttrs = pattrs
                     , callRetType = rtype
                     , callFunction = realFunc
                     , callArguments = params
                     , callAttrs = funcAttrs
                     }
        realFunc = case (func rtype, mftype) of
          (ValueRef _, _) -> func rtype
          (_, Just t) -> func t
          _ -> error "Should not have a constant function without full functype"

mkInvokeInst :: (Monad m) => Maybe Identifier -> CallingConvention -> [ParamAttribute] -> Type -> PartialConstant -> [Constant] -> [FunctionAttribute] -> Constant -> Constant -> m Instruction
mkInvokeInst mident cc pattrs rtype func params fattrs normal unwind =
  return $ maybeNamedInst mident rtype i
  where i = InvokeInst { invokeConvention = cc
                       , invokeParamAttrs = pattrs
                       , invokeRetType = rtype
                       , invokeFunction = realFunc
                       , invokeArguments = params
                       , invokeAttrs = fattrs
                       , invokeNormalLabel = normal
                       , invokeUnwindLabel = unwind
                       }
        realFunc = case func rtype of
          ValueRef _ -> func rtype
          _ -> error "Cannot invoke anything besides a named constant..."

-- Ident, va_arg type, the va_list arg, the type being extracted
mkVaArgInst :: (Monad m) => Identifier -> Type -> PartialConstant -> Type -> m Instruction
mkVaArgInst ident ty1 val ty2 =
  return $ namedInst ident ty2 $ VaArgInst (val ty1) ty2

mkGetElementPtrInst :: (Monad m) => Identifier -> Bool -> Type -> PartialConstant -> [Constant] -> m Instruction
mkGetElementPtrInst ident inBounds ty val indices =
  return UnresolvedInst { unresInstName = Just ident
                        , unresInstContent = GetElementPtrInst inBounds (val ty) indices
                        }

mkExternalFuncDecl :: Type -> Identifier -> ([Type], Bool) -> [FunctionAttribute] -> ExternalDecl
mkExternalFuncDecl retType ident (argTypes, isVararg) attrs = ExternalDecl t ident
  where t = TypeFunction retType argTypes isVararg attrs

mkGlobalDecl :: Identifier -> Int -> [GlobalAnnotation] -> Type -> PartialConstant -> Integer -> GlobalDeclaration
mkGlobalDecl ident addrSpace annots initType init align =
  GlobalDeclaration ident addrSpace annots t i align
  where t = TypePointer initType
        i = init initType

mkBasicBlock :: ByteString -> [Instruction] -> BasicBlock
mkBasicBlock = BasicBlock

mkFunctionDef :: LinkageType -> VisibilityStyle -> CallingConvention ->
                 [ParamAttribute] -> Type -> Identifier ->
                 ([FormalParameter], Bool) -> [FunctionAttribute] ->
                 Maybe ByteString -> Integer -> GCName -> [BasicBlock] ->
                 GlobalDeclaration
mkFunctionDef linkage vis cc retAttr retTy name (args, isVararg) fAttrs section align gcname body =
  FunctionDefinition { funcLinkage = linkage
                     , funcVisibility = vis
                     , funcCC = cc
                     , funcRetAttrs = retAttr
                     , funcRetType = retTy
                     , funcName = name
                     , funcParams = args
                     , funcAttrs = fAttrs
                     , funcSection = section
                     , funcAlign = align
                     , funcGCName = gcname
                     , funcBody = body
                     , funcIsVararg = isVararg
                     }