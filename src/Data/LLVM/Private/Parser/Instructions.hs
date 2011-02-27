module Data.LLVM.Private.Parser.Instructions ( instructionP ) where


import Control.Applicative hiding ((<|>), many)
import Control.Monad (when)
import Text.Parsec

import Data.LLVM.Private.AttributeTypes
import Data.LLVM.Private.Lexer
import Data.LLVM.Private.PlaceholderTypes
import Data.LLVM.Private.Parser.Attributes
import Data.LLVM.Private.Parser.Constants
import Data.LLVM.Private.Parser.Primitive
import Data.LLVM.Private.Parser.Types

instructionP :: AssemblyParser Instruction
instructionP = instructionNoMDP

instructionNoMDP :: AssemblyParser Instruction
instructionNoMDP = do
  realParser <- lookAhead dispatcher
  anInst <- realParser
  return anInst
  where dispatcher = tokenAs matcher
        matcher x =
          case x of
            TLocalIdent _ -> Just namedInstP
            TRet -> Just retInstP
            TBr -> Just brInstP
            TSwitch -> Just switchInstP
            TIndirectBr -> Just indirectBrInstP
            -- TInvoke -> Just bareInvokeInstP
            -- TCall -> Just bareCallInstP
            -- TTail -> Just tailCallInstP -- variant of call
            TUnwind -> Just unwindInstP
            TUnreachable -> Just unreachableInstP
            TStore -> Just (storeInstP False)
            TVolatile -> Just (storeInstP True)
            _ -> Just (parserFail "Expected an Instruction")

namedInstP :: AssemblyParser Instruction
namedInstP = do
  -- First, slurp up the identifier and then discard the equals sign.
  -- Pass the identifier on to the real parser.
  name <- localIdentifierP
  consumeToken TAssign
  restParser <- lookAhead dispatcher
  anInst <- restParser name
  return anInst
  where dispatcher = tokenAs matcher
        matcher x =
          case x of
            TAdd -> Just (binaryFlagInstP TAdd AddInst)
            TFadd -> Just (binaryFlagInstP TFadd AddInst)
            TSub -> Just (binaryFlagInstP TSub SubInst)
            TFsub -> Just (binaryFlagInstP TFsub SubInst)
            TMul -> Just (binaryFlagInstP TMul MulInst)
            TFmul -> Just (binaryFlagInstP TFmul MulInst)
            TUdiv -> Just (binaryInstP TUdiv DivInst)
            TSdiv -> Just (binaryInstP TSdiv DivInst)
            TFdiv -> Just (binaryInstP TFdiv DivInst)
            TUrem -> Just (binaryInstP TUrem RemInst)
            TSrem -> Just (binaryInstP TSrem RemInst)
            TFrem -> Just (binaryInstP TFrem RemInst)
            TShl -> Just (binaryInstP TShl ShlInst)
            TLshr -> Just (binaryInstP TLshr LshrInst)
            TAshr -> Just (binaryInstP TAshr AshrInst)
            TAnd -> Just (binaryInstP TAnd AndInst)
            TOr -> Just (binaryInstP TOr OrInst)
            TXor -> Just (binaryInstP TXor XorInst)

binaryFlagInstP :: LexerToken ->
                   ([ArithFlag] -> Constant -> Constant -> InstructionT) ->
                   Identifier ->
                   AssemblyParser Instruction
binaryFlagInstP tok cons name = do
  consumeToken tok
  flags <- many arithFlagP
  t <- typeP
  lhs <- partialConstantP
  consumeToken TComma
  rhs <- partialConstantP
  return $ namedInst name t $ cons flags (lhs t) (rhs t)

binaryInstP :: LexerToken -> (Constant -> Constant -> InstructionT) ->
               Identifier -> AssemblyParser Instruction
binaryInstP tok cons name = do
  consumeToken tok
  t <- typeP
  lhs <- partialConstantP
  consumeToken TComma
  rhs <- partialConstantP
  return $ namedInst name t $ cons (lhs t) (rhs t)

storeInstP :: Bool -> AssemblyParser Instruction
storeInstP volatile = do
  when volatile (consumeToken TVolatile)
  consumeToken TStore
  t1 <- typeP
  val <- partialConstantP
  consumeToken TComma
  t2 <- typeP
  dest <- partialConstantP
  align <- alignmentSpecP
  let (TypePointer t2') = t2
  if t2' == t1
    then return $ voidInst $ StoreInst volatile (val t1) (dest t2) align
    else parserFail "Store type mismatch"

retInstP :: AssemblyParser Instruction
retInstP = do
  consumeToken TRet
  t <- typeP
  pc <- optionMaybe partialConstantP
  case (t, pc) of
    (TypeVoid, Nothing) -> return $ voidInst $ RetInst Nothing
    (_, Just pc') -> return $ voidInst $ RetInst $ Just (pc' t)
    _ -> parserFail "Non-void return without return value"

-- | Parse just a function-local label, but wrap it in a ValueRef
-- so that it can be used as a Constant
localLabelP :: AssemblyParser Constant
localLabelP = ValueRef <$> localIdentifierP

-- | Parse ', label <label>'
commaLabelP :: AssemblyParser Constant
commaLabelP = consumeTokens [TComma, TLabelT] *> localLabelP

-- | Parses both conditional and unconditional branches.  It does this
-- dispatch by checking the token after the initial 'br'; if it is the
-- 'label' keyword, this is an unconditional branch.  Otherwise, it is
-- a conditional branch.
brInstP :: AssemblyParser Instruction
brInstP = do
  consumeToken TBr
  realParser <- lookAhead dispatcher
  anInst <- realParser
  return anInst
  where dispatcher = tokenAs matcher
        matcher x =
          case x of
            TLabelT -> Just unconditionalBranchInstP
            _ -> Just conditionalBranchInstP
        unconditionalBranchInstP =
          voidInst <$> (UnconditionalBranchInst <$> localLabelP)
        conditionalBranchInstP =
          voidInst <$> (BranchInst <$> constantP <*> commaLabelP <*> commaLabelP)

switchInstP :: AssemblyParser Instruction
switchInstP =
  voidInst <$> (SwitchInst <$> constantP <*> defLabP <*> branches)
  where defLabP = commaLabelP
        branches = betweenTokens [TLSquare] [TRSquare] (many switchBranchP)
        switchBranchP = (,) <$> constantP <*> commaLabelP

indirectBrInstP :: AssemblyParser Instruction
indirectBrInstP =
  voidInst <$> (IndirectBranchInst <$> constantP <*> targetsP)
  where targetsP = consumeTokens [TComma, TLSquare] *> sepBy labelValP (consumeToken TComma) <* consumeToken TRSquare
        labelValP = mkLabelConst <$> (consumeToken TLabelT *> partialConstantP)
        mkLabelConst x = x TypeLabel

unwindInstP :: AssemblyParser Instruction
unwindInstP = (voidInst UnwindInst) <$ consumeToken TUnwind

unreachableInstP :: AssemblyParser Instruction
unreachableInstP = (voidInst UnreachableInst) <$ consumeToken TUnreachable
