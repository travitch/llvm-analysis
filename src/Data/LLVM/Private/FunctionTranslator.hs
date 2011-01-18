module Data.LLVM.Private.FunctionTranslator ( transFuncDef ) where

import Data.Map (Map, (!))
import qualified Data.Map as M

import Data.LLVM.Types
import Data.LLVM.Private.AttributeTypes
import qualified Data.LLVM.Private.PlaceholderTypes as O

mkFuncType typeMapper O.FunctionDefinition { O.funcRetType = fret
                                           , O.funcParams = params
                                           , O.funcIsVararg = isVararg
                                           , O.funcAttrs = attrs
                                           } =
  TypeFunction rtype argTypes isVararg attrs
  where rtype = typeMapper fret
        argTypes = map (typeMapper . xtype) params
        xtype (O.FormalParameter t _ _) = t

mkFuncType _ _ = error "Non-func decl in mkFuncType"

getFuncIdent O.FunctionDefinition { O.funcName = ident } = ident

transFuncDef :: (O.Type -> Type) ->
                (O.Constant -> Value) ->
                (Identifier -> Maybe Metadata) ->
                (Map Identifier Value) ->
                O.GlobalDeclaration ->
                Map Identifier Value
transFuncDef typeMapper transValOrConst getMetadata vals decl =
  M.insert (O.funcName decl) v vals
  where v = Value { valueType = mkFuncType typeMapper decl
                  , valueName = Just ident
                  , valueMetadata = getMetadata ident
                  , valueContent = ExternalValue -- changeme
                  }
        ident = getFuncIdent decl
        (localVals, body) = translateBody (O.funcBody decl)
        trConst (O.ValueRef ident@(LocalIdentifier _)) = localVals ! ident
        trConst c = transValOrConst c
        translateBody = foldr translateBlock (M.empty, [])
        translateBlock (O.BasicBlock ident placeholderInsts) (locals, blocks) =
          (M.insert ident bb blocksWithLocals, bb : blocks)
          where bb = Value { valueType = TypeVoid
                           , valueName = Just ident
                           , valueMetadata = Nothing -- can BBs have metadata?
                           , valueContent = BasicBlock insts
                           }
                (blocksWithLocals, insts) = translateInsts locals placeholderInsts
        translateInsts locals = foldr translateInstruction (locals, [])

        -- This helper converts the non-content fields of an instruction
        -- to the equivalent fields in a Value.  It performs the necessary
        -- translations to fetch metadata and handle type differences
        repackInst O.Instruction { O.instType = itype
                                 , O.instName = iname
                                 , O.instMetadata = md
                                 } ni = v
          where v = Value { valueType = typeMapper itype
                          , valueName = iname
                          , valueMetadata = maybe Nothing getMetadata md
                          , valueContent = ni
                          }

        foldResult (locals, insts) val = case valueName v of
          Just i -> (M.insert i val locals, val : insts)
          Nothing -> (locals, val : insts)

        -- Translate the control flow instructions first.  These are simpler
        -- and don't require any updates to the local variable map since these
        -- instructions are never referenced by name
        translateInstruction i@O.Instruction { O.instContent = O.RetInst mc } acc =
          foldResult acc v
          where v = repackInst i $ RetInst mc'
                mc' = maybe Nothing (Just . trConst) mc

        translateInstruction i@O.Instruction { O.instContent = O.UnconditionalBranchInst target } acc =
          foldResult acc v
          where v = repackInst i $ UnconditionalBranchInst $ trConst target

        translateInstruction i@O.Instruction { O.instContent = O.BranchInst cond tTarget fTarget } acc =
          foldResult acc v
          where v = repackInst i ni
                ni = BranchInst { branchCondition = trConst cond
                                , branchTrueTarget = trConst tTarget
                                , branchFalseTarget = trConst fTarget
                                }

        translateInstruction i@O.Instruction { O.instContent = O.SwitchInst val defTarget dests } acc =
          foldResult acc v
          where v = repackInst i ni
                trTargetPair (v, t) = (trConst v, trConst t)
                ni = SwitchInst { switchValue = trConst val
                                , switchDefaultTarget = trConst defTarget
                                , switchCases = map trTargetPair dests
                                }

        translateInstruction i@O.Instruction { O.instContent = O.IndirectBranchInst val possibleDests } acc =
          foldResult acc v
          where v = repackInst i ni
                ni = IndirectBranchInst { indirectBranchAddress = trConst val
                                        , indirectBranchTargets = map trConst possibleDests
                                        }

        translateInstruction i@O.Instruction { O.instContent = O.UnwindInst } acc =
          foldResult acc v
          where v = repackInst i UnwindInst

        translateInstruction i@O.Instruction { O.instContent = O.UnreachableInst } acc =
          foldResult acc v
          where v = repackInst i UnreachableInst

        -- Translate the trivial instructions
        translateInstruction i@O.Instruction { O.instContent = O.AddInst flags lhs rhs } acc =
          foldResult acc v
          where v = repackInst i $ AddInst flags (trConst lhs) (trConst rhs)

        translateInstruction i@O.Instruction { O.instContent = O.SubInst flags lhs rhs } acc =
          foldResult acc v
          where v = repackInst i $ SubInst flags (trConst lhs) (trConst rhs)

        translateInstruction i@O.Instruction { O.instContent = O.MulInst flags lhs rhs } acc =
          foldResult acc v
          where v = repackInst i $ SubInst flags (trConst lhs) (trConst rhs)

        translateInstruction i@O.Instruction { O.instContent = O.DivInst lhs rhs } acc =
          foldResult acc v
          where v = repackInst i $ DivInst (trConst lhs) (trConst rhs)

        translateInstruction i@O.Instruction { O.instContent = O.RemInst lhs rhs } acc =
          foldResult acc v
          where v = repackInst i $ RemInst (trConst lhs) (trConst rhs)

        translateInstruction i@O.Instruction { O.instContent = O.ShlInst lhs rhs } acc =
          foldResult acc v
          where v = repackInst i $ ShlInst (trConst lhs) (trConst rhs)

        translateInstruction i@O.Instruction { O.instContent = O.LshrInst lhs rhs } acc =
          foldResult acc v
          where v = repackInst i $ LshrInst (trConst lhs) (trConst rhs)

        translateInstruction i@O.Instruction { O.instContent = O.AshrInst lhs rhs } acc =
          foldResult acc v
          where v = repackInst i $ AshrInst (trConst lhs) (trConst rhs)

        translateInstruction i@O.Instruction { O.instContent = O.AndInst lhs rhs } acc =
          foldResult acc v
          where v = repackInst i $ AndInst (trConst lhs) (trConst rhs)

        translateInstruction i@O.Instruction { O.instContent = O.OrInst lhs rhs } acc =
          foldResult acc v
          where v = repackInst i $ OrInst (trConst lhs) (trConst rhs)

        translateInstruction i@O.Instruction { O.instContent = O.XorInst lhs rhs } acc =
          foldResult acc v
          where v = repackInst i $ XorInst (trConst lhs) (trConst rhs)

