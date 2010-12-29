{
module Data.LLVM.AssemblyParser ( parser
                                , parseIdentifier
                                , parseCallingConvention
                                , parseGCName
                                , parseType
                                , parseInstruction
                                ) where

import Data.LLVM.Lexer
import Data.LLVM.Private.AttributeTypes
import Data.LLVM.Private.PlaceholderTypes
import Data.LLVM.Private.PlaceholderBuilders
import Data.LLVM.Private.ParsingMonad
import Data.Monoid

}

%name parser
%name parseIdentifier Identifier
%name parseCallingConvention CallingConvention
%name parseGCName GCName
%name parseType Type
%name parseInstruction Instruction

%tokentype { Token }
%error { parseError }
%monad { ParsingMonad }

%token
  gident      { TGlobalIdent $$ }
  lident      { TLocalIdent $$ }
  mdname      { TMetadataName $$ }
  intlit      { TIntLit $$ }
  floatlit    { TFloatLit $$ }
  stringlit   { TStringLit $$ }
  mdstring    { TMetadataString $$ }
  label       { TLabel $$ }
  string      { TString $$ }
  "true"      { TTrueLit }
  "false"     { TFalseLit }
  "null"      { TNullLit }
  "undef"     { TUndefLit }
  newline     { TNewline }
  "zeroinitializer" { TZeroInitializer }

  ","         { TComma }
  "="         { TAssign }
  "*"         { TStar }
  "("         { TLParen }
  ")"         { TRParen }
  "["         { TLSquare }
  "]"         { TRSquare }
  "{"         { TLCurl }
  "}"         { TRCurl }
  "<"         { TLAngle }
  ">"         { TRAngle }
  "!"         { TBang }
  "x"         { TAggLen }
  "to"        { TTo }
  "..."       { TDotDotDot }

  "private"   { TPrivate }
  "linker_private" { TLinkerPrivate }
  "linker_private_weak" { TLinkerPrivateWeak }
  "linker_private_weak_def_auto" { TLinkerPrivateWeakDefAuto }
  "internal"  { TInternal }
  "available_externally" { TAvailableExternally }
  "link_once" { TLinkOnce }
  "weak"      { TWeak }
  "common"    { TCommon }
  "appending" { TAppending }
  "extern_weak" { TExternWeak }
  "link_once_odr" { TLinkOnceODR }
  "weak_odr"  { TWeakODR }
  "dllimport" { TDLLImport }
  "dllexport" { TDLLExport }

  "ccc"       { TCCCCC }
  "fastcc"    { TCCFastCC }
  "coldcc"    { TCCColdCC }
  "ghc"       { TCCGHC }
  ccN         { TCCN $$ }

  "default"   { TVisDefault }
  "hidden"    { TVisHidden }
  "protected" { TVisProtected }

  "zeroext"   { TPAZeroExt }
  "signext"   { TPASignExt }
  "inreg"     { TPAInReg }
  "byval"     { TPAByVal }
  "sret"      { TPASRet }
  "noalias"   { TPANoAlias }
  "nocapture" { TPANoCapture}
  "nest"      { TPANest }

  alignstackN   { TFAAlignStack $$ }
  "alwaysinline" { TFAAlwaysInline }
  "hotpatch"     { TFAHotPatch }
  "inlinehint"   { TFAInlineHint }
  "naked"        { TFANaked }
  "noimplicitfloat" { TFANoImplicitFloat }
  "noinline"     { TFANoInline }
  "noredzone"    { TFANoRedZone }
  "noreturn"     { TFANoReturn }
  "nounwind"     { TFANoUnwind }
  "optsize"      { TFAOptSize }
  "readnone"     { TFAReadNone }
  "readonly"     { TFAReadOnly }
  "ssp"          { TFASSP }
  "sspreq"       { TFASSPReq }

  iN             { TIntegralT $$ }
  "float"        { TFloatT }
  "double"       { TDoubleT }
  "x86_fp80"     { TX86_FP80T }
  "fp128"        { TFP128T }
  "ppc_fp128"    { TPPC_FP128T }
  "x86mmx"       { TX86mmxT }
  "void"         { TVoidT }
  "metadata"     { TMetadataT }
  "opaque"       { TOpaqueT }
  upref          { TUprefT $$ }
  "label"        { TLabelT }

  "type"         { TType }
  addrspace      { TAddrspace $$ }
  "constant"     { TConstant }
  "section"      { TSection }
  "align"        { TAlign }
  "alignstack"   { TAlignStack }
  "sideeffect"   { TSideEffect }
  "alias"        { TAlias }
  "declare"      { TDeclare }
  "define"       { TDefine }
  "gc"           { TGC }
  "module"       { TModule }
  "asm"          { TAsm }
  "target"       { TTarget }
  "datalayout"   { TDataLayout }
  "blockaddress" { TBlockAddress }
  "inbounds"     { TInbounds }
  "global"       { TGlobal }
  "tail"         { TTail }

  "nuw"          { TNUW }
  "nsw"          { TNSW }

  "exact"        { TExact }
  "volatile"     { TVolatile }

  "trunc"        { TTrunc }
  "zext"         { TZext }
  "sext"         { TSext }
  "fptrunc"      { TFpTrunc }
  "fpext"        { TFpExt }
  "fptoui"       { TFpToUI }
  "fptosi"       { TFpToSI }
  "uitofp"       { TUIToFp }
  "sitofp"       { TSIToFp }
  "ptrtoint"     { TPtrToInt }
  "inttoptr"     { TIntToPtr }
  "bitcast"      { TBitCast }
  "getelementptr"  { TGetElementPtr }
  "select"       { TSelect }
  "icmp"         { TIcmp }
  "fcmp"         { TFcmp }
  "extractelement" { TExtractElement }
  "insertelement"  { TInsertElement }
  "shufflevector"  { TShuffleVector }
  "extractvalue"   { TExtractValue }
  "insertvalue"    { TInsertValue }
  "call"           { TCall }
  "ret"            { TRet }
  "br"             { TBr }
  "switch"         { TSwitch }
  "indirectbr"     { TIndirectBr }
  "invoke"         { TInvoke }
  "unwind"         { TUnwind }
  "unreachable"    { TUnreachable }
  "add"            { TAdd }
  "fadd"           { TFadd }
  "sub"            { TSub }
  "fsub"           { TFsub }
  "mul"            { TMul }
  "fmul"           { TFmul }
  "udiv"           { TUdiv }
  "sdiv"           { TSdiv }
  "fdiv"           { TFdiv }
  "urem"           { TUrem }
  "srem"           { TSrem }
  "frem"           { TFrem }
  "shl"            { TShl }
  "lshr"           { TLshr }
  "ashr"           { TAshr }
  "and"            { TAnd }
  "or"             { TOr }
  "xor"            { TXor }
  "alloca"         { TAlloca }
  "load"           { TLoad }
  "store"          { TStore }
  "phi"            { TPhi }
  "va_arg"         { TVaArg }

  "eq"             { Teq }
  "ne"             { Tne }
  "ugt"            { Tugt }
  "uge"            { Tuge }
  "ult"            { Tult }
  "ule"            { Tule }
  "sgt"            { Tsgt }
  "sge"            { Tsge }
  "slt"            { Tslt }
  "sle"            { Tsle }
  "oeq"            { Toeq }
  "ogt"            { Togt }
  "oge"            { Toge }
  "olt"            { Tolt }
  "ole"            { Tole }
  "one"            { Tone }
  "ord"            { Tord }
  "ueq"            { Tueq }
  "une"            { Tune }
  "uno"            { Tuno }

%%

LinkageType:
    "private"   { LTPrivate }
  | "linker_private" { LTLinkerPrivate }
  | "linker_private_weak" { LTLinkerPrivateWeak }
  | "linker_private_weak_def_auto" { LTLinkerPrivateWeakDefAuto }
  | "internal"  { LTInternal }
  | "available_externally" { LTAvailableExternally }
  | "link_once" { LTLinkOnce }
  | "weak"      { LTWeak }
  | "common"    { LTCommon }
  | "appending" { LTAppending }
  | "extern_weak" { LTExternWeak }
  | "link_once_odr" { LTLinkOnceODR }
  | "weak_odr"  { LTWeakODR }
  | "dllimport" { LTDLLImport }
  | "dllexport" { LTDLLExport }
  |             { LTExtern } -- The default


CallingConvention:
    "ccc"     { CCC }
  | "fastcc"  { CCFastCC }
  | "coldcc"  { CCColdCC }
  | "ghc"     { CCGHC }
  | ccN       { CCN $1 }
  |           { CCC } -- C calling convention is the default

VisibilityStyle:
    "default"   { VisibilityDefault }
  | "hidden"    { VisibilityHidden }
  | "protected" { VisibilityProtected }


Identifier:
    gident { GlobalIdentifier $1 }
  | lident { LocalIdentifier $1 }


ParameterAttribute:
    "zeroext"   { PAZeroExt }
  | "signext"   { PASignExt }
  | "inreg"     { PAInReg }
  | "byval"     { PAByVal }
  | "sret"      { PASRet }
  | "noalias"   { PANoAlias }
  | "nocapture" { PANoCapture }
  | "nest"      { PANest }


GCName:
  "gc" string { GCName $2 }


FunctionAttribute:
    alignstackN       { FAAlignStack $1 }
  | "alwaysinline"    { FAAlwaysInline }
  | "hotpatch"        { FAHotPatch }
  | "inlinehint"      { FAInlineHint }
  | "naked"           { FANaked }
  | "noimplicitfloat" { FANoImplicitFloat }
  | "noinline"        { FANoInline }
  | "noredzone"       { FANoRedZone }
  | "noreturn"        { FANoReturn }
  | "nounwind"        { FANoUnwind }
  | "optsize"         { FAOptSize }
  | "readnone"        { FAReadNone }
  | "readonly"        { FAReadOnly }
  | "ssp"             { FASSP }
  | "sspreq"          { FASSPReq }


ModuleInlineAssembly:
  "module" "asm" string newline { ModuleAssembly $3 }


DataLayout:
  "target" "datalayout" "=" string newline { mkDataLayout $4 }

Type:
    iN          { TypeInteger $1 }
  | "float"     { TypeFloat }
  | "double"    { TypeDouble }
  | "x86_fp80"  { TypeX86FP80 }
  | "fp128"     { TypeFP128 }
  | "ppc_fp128" { TypePPCFP128 }
  | "x86mmx"    { TypeX86MMX }
  | "void"      { TypeVoid }
  | "metadata"  { TypeMetadata }
  | "opaque"    { TypeOpaque }
  | upref       { TypeUpref $1 }
  | "label"     { TypeLabel }
  | Type "*"    { TypePointer $1 } -- FIXME: Add support for address space annotations
  | "[" intlit "x" Type "]" { TypeArray $2 $4 }
  | "<" intlit "x" Type ">" { TypeVector $2 $4 }
  | Type "(" FuncTypeArgList ")" { TypeFunction $1 (fst $3) (snd $3) }
  | "{" sep(Type, ",") "}" { TypeStruct $2 }
  | "<" "{" sep(Type, ",") "}" ">" { TypePackedStruct $3 }

-- Can't use the simple sep1 parameterized rule here since
-- that generates a nasty shift/reduce conflict where the ,
-- before a ... is slurped into the list but sep1 doesn't
-- know what to do with ... since it isn't a type. Oh well.
FuncTypeArgList:
    Type MoreFuncTypeArgs { ($1 : fst $2, snd $2) }
  |                       { ([], False) }

MoreFuncTypeArgs:
    "," Type MoreFuncTypeArgs { ($2 : (fst $3), snd $3) }
  | "," "..."                 { ([], True) }
  |                           { ([], False) }

SimpleConstant:
    "true"     { ConstantInt 1 }
  | "false"    { ConstantInt 0 }
  | intlit     { ConstantInt $1 }
  | floatlit   { ConstantFP $1 }
  | "null"     { ConstantPointerNull }
  -- | Identifier { ConstantIdentifier $1 }

ComplexConstant:
    "{" sep(Constant, ",") "}"   { ConstantStruct $2 }
  | "[" sep(Constant, ",") "]"   { ConstantArray $2 }
  | "<" sep(Constant, ",") ">"   { ConstantVector $2 }
  | "zeroinitializer"                 { ConstantAggregateZero }
  | "undef"                           { UndefValue }
  | "blockaddress" "(" Identifier "," Identifier ")" { BlockAddress $3 $5 }

Constant:
  Type PartialConstant { $2 $1 }

AllConstants:
    SimpleConstant  { $1 }
  | ComplexConstant { $1 }

-- These "constants" are actually partial - they need to be applied to a type
-- before they are actually constants.
PartialConstant:
    AllConstants { ConstValue $1 }
  | Identifier   { valueRef $1 }

-- FIXME: Inline asm
-- FIXME: Handle metadata

Instruction:
    "ret" Type PartialConstant  { voidInst $ RetInst (Just ($3 $2)) }
  | "ret" "void"         { voidInst $ RetInst Nothing }
  | "br" "label" label   { voidInst $ UnconditionalBranchInst $3 }
  | "br" Type PartialConstant "," "label" label "," "label" label
    { voidInst $ BranchInst ($3 $2) $6 $9 }
  | "switch" Type PartialConstant "," "label" label "[" list(SwitchBranch) "]"
    { voidInst $ SwitchInst ($3 $2) $6 $8 }
  | "indirectbr" Type PartialConstant "," "[" sep(LabelVal, ",") "]"
    { voidInst $ IndirectBranchInst ($3 $2) $6 }
  -- FIXME: "invoke"
  | "unwind" { voidInst UnwindInst }
  | "unreachable" { voidInst UnreachableInst }
  | Identifier "=" AddInst list(ArithFlag) Type PartialConstant "," PartialConstant
    {% mkFlaggedArithInst AddInst $1 $5 $4 $6 $8 }
  | Identifier "=" SubInst list(ArithFlag) Type PartialConstant "," PartialConstant
    {% mkFlaggedArithInst SubInst $1 $5 $4 $6 $8 }
  | Identifier "=" MulInst list(ArithFlag) Type PartialConstant "," PartialConstant
    {% mkFlaggedArithInst MulInst $1 $5 $4 $6 $8 }
  | Identifier "=" DivInst Type PartialConstant "," PartialConstant
    {% mkArithInst DivInst $1 $4 $5 $7 }
  | Identifier "=" RemInst Type PartialConstant "," PartialConstant
    {% mkArithInst RemInst $1 $4 $5 $7 }
  | Identifier "=" "shl"  Type PartialConstant "," PartialConstant
    {% mkArithInst ShlInst $1 $4 $5 $7 }
  | Identifier "=" "lshr" Type PartialConstant "," PartialConstant
    {% mkArithInst LshrInst $1 $4 $5 $7 }
  | Identifier "=" "ashr" Type PartialConstant "," PartialConstant
    {% mkArithInst AshrInst $1 $4 $5 $7 }
  | Identifier "=" "and"  Type PartialConstant "," PartialConstant
    {% mkArithInst AndInst $1 $4 $5 $7 }
  | Identifier "=" "or"   Type PartialConstant "," PartialConstant
    {% mkArithInst OrInst $1 $4 $5 $7 }
  | Identifier "=" "xor"  Type PartialConstant "," PartialConstant
    {% mkArithInst XorInst $1 $4 $5 $7 }
  | Identifier "=" "extractelement" Type PartialConstant "," Constant
    {% mkExtractElementInst $1 $4 $5 $7 }
  | Identifier "=" "insertelement" Type PartialConstant "," Constant "," Constant
    { mkInsertElementInst $1 $4 $5 $7 $9 }
  | Identifier "=" "shufflevector" Type PartialConstant "," Type PartialConstant "," Type PartialConstant
    {% mkShuffleVectorInst $1 $4 $5 $7 $8 $10 $11 }
  -- FIXME: extractvalue
  | Identifier "=" "insertvalue" Type PartialConstant "," Type PartialConstant "," intlit
    { mkInsertValueInst $1 $4 $5 $7 $8 $10 }
  | Identifier "=" "alloca" Type AllocaNumElems AlignmentSpec
    { mkAllocaInst $1 $4 $5 $6 }
  -- FIXME: Add support for the !nontemporal metadata thing
  | Identifier "=" VolatileFlag "load" Type PartialConstant AlignmentSpec
    { mkLoadInst $1 $3 $5 $6 $7 }
  -- -- FIXME: Add support for !<index> = !{ <ty> <val> } form
  -- -- FIXME: There is also an optional nontemporal thing
  -- | VolatileFlag "store" Type Value "," Type Identifier AlignmentSpec
  --   {% mkStoreInst $1 $4 $6 $7 $8 }
  -- -- FIXME: Add GetElementPtr
  -- | Identifier "=" "trunc" Type Value "to" Type
  --   {% mkConversionInst TruncInst $1 $4 $5 $7 }
  -- | Identifier "=" "zext" Type Value "to" Type
  --   {% mkConversionInst ZExtInst $1 $4 $5 $7 }
  -- | Identifier "=" "sext" Type Value "to" Type
  --   {% mkConversionInst SExtInst $1 $4 $5 $7 }
  -- | Identifier "=" "fptrunc" Type Value "to" Type
  --   {% mkConversionInst FPTruncInst $1 $4 $5 $7 }
  -- | Identifier "=" "fpext" Type Value "to" Type
  --   {% mkConversionInst FPExtInst $1 $4 $5 $7 }
  -- | Identifier "=" "fptoui" Type Value "to" Type
  --   {% mkConversionInst FPToUIInst $1 $4 $5 $7 }
  -- | Identifier "=" "fptosi" Type Value "to" Type
  --   {% mkConversionInst FPToSIInst $1 $4 $5 $7 }
  -- | Identifier "=" "uitofp" Type Value "to" Type
  --   {% mkConversionInst UIToFPInst $1 $4 $5 $7 }
  -- | Identifier "=" "sitofp" Type Value "to" Type
  --   {% mkConversionInst SIToFPInst $1 $4 $5 $7 }
  -- | Identifier "=" "ptrtoint" Type Value "to" Type
  --   {% mkConversionInst PtrToIntInst $1 $4 $5 $7 }
  -- | Identifier "=" "inttoptr" Type Value "to" Type
  --   {% mkConversionInst IntToPtrInst $1 $4 $5 $7 }
  -- | Identifier "=" "bitcast" Type Value "to" Type
  --   {% mkConversionInst BitcastInst $1 $4 $5 $7 }
  -- | Identifier "=" "icmp" ICmpCondition Type Value "," Value
  --   {% mkIcmpInst $1 $4 $5 $6 $8 }
  -- | Identifier "=" "fcmp" FCmpCondition Type Value "," Value
  --   {% mkFcmpInst $1 $4 $5 $6 $8 }
  -- | Identifier "=" "phi" Type sep1(PhiPair, ",")
  --   {% mkPhiNode $1 $4 $5 }
  -- | Identifier "=" "select" Type Value "," Type Value "," Type Value
  --   {% mkSelectInst $1 $4 $5 $7 $8 $10 $11 }


--  | optional(CallIdentifier) TailMarker "call" CallingConvention list(ParameterAttribute) Type optional(Type) Value "(" sep(Value, ",") ")" list(FunctionAttribute)
--    {% mkCallInst $1 $2 $4 $5 $6 $7 $8 $10 $12 }

CallIdentifier:
  Identifier "=" { $1 }

TailMarker:
    "tail" { True }
  |        { False }

PhiPair:
  Identifier "," label { (ValueRef $1, $3) }

ICmpCondition:
    "eq"  { ICmpEq }
  | "ne"  { ICmpNe }
  | "ugt" { ICmpUgt }
  | "uge" { ICmpUge }
  | "ult" { ICmpUlt }
  | "ule" { ICmpUle }
  | "sgt" { ICmpSgt }
  | "sge" { ICmpSge }
  | "slt" { ICmpSlt }
  | "sle" { ICmpSle }

FCmpCondition:
    "false" { FCmpFalse }
  | "oeq"   { FCmpOeq }
  | "ogt"   { FCmpOgt }
  | "oge"   { FCmpOge }
  | "olt"   { FCmpOlt }
  | "ole"   { FCmpOle }
  | "one"   { FCmpOne }
  | "ord"   { FCmpOrd }
  | "ueq"   { FCmpUeq }
  | "ugt"   { FCmpUgt }
  | "uge"   { FCmpUge }
  | "ult"   { FCmpUlt }
  | "ule"   { FCmpUle }
  | "une"   { FCmpUne }
  | "uno"   { FCmpUno }
  | "true"  { FCmpTrue }

-- If unspecified, allocates 1 element
AllocaNumElems:
    "," Constant { $2 }
  |              { ConstValue (ConstantInt 1) (TypeInteger 32) }

VolatileFlag:
    "volatile" { True  }
  |            { False }
AlignmentSpec:
    "," "align" intlit { $3 }
  |                    { 0 }

AddInst:
    "add"  { $1 }
  | "fadd" { $1 }

SubInst:
    "sub"  { $1 }
  | "fsub" { $1 }

MulInst:
    "mul"  { $1 }
  | "fmul" { $1 }

DivInst:
    "udiv" { $1 }
  | "sdiv" { $1 }
  | "fdiv" { $1 }

RemInst:
    "urem" { $1 }
  | "srem" { $1 }
  | "frem" { $1 }

SwitchBranch:
  Type PartialConstant "," "label" label { ($2 $1, $5) }

LabelVal:
  "label" PartialConstant { $2 TypeLabel }

ArithFlag:
    "nsw" { AFNSW }
  | "nuw" { AFNUW }

-- Helper parameterized parsers

optional(p):
    p { Just $1 }
  |   { Nothing }

-- Possibly empty list of 'p' separated by 's'
sep(p,s):
    sep1(p,s) { $1 }
  |           { [] }

-- Non-empty list of 'p' separated by s
sep1(p,s):
  p list(snd(s, p)) { $1 : $2 }

-- Allow the list to be empty
list(p):
    list1(p) { $1 }
  |          { [] }

-- Reverse the list of rev_list1 to get a list in the correct
-- order
list1(p):
  rev_list1(p) { reverse $1 }

-- Build up a list of parsed 'p's in reverse order
rev_list1(p):
    p              { [$1] }
  | rev_list1(p) p { $2 : $1 }

-- Apply two parsers, returning the result of the second
snd(p,q): p q { $2 }

{

-- Use the underlying fail in the parse monad
parseError :: [Token] -> ParsingMonad a
parseError ts = fail $ show ts

}
