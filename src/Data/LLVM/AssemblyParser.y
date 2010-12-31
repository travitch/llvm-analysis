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
  "triple"       { TTriple }

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

Module:
  DataLayout Triple list(GlobalEntity)
  { Module $1 $2 $3 }

GlobalEntity:
    GlobalDecl          { $1 }
  | FunctionDefinition  { $1 }
  | ModuleLevelAssembly { $1 }
  | ExternalDecl        { $1 }
  | TypeDeclaration     { $1 }

TypeDeclaration:
  LocalIdentifier "=" "type" Type { NamedType $1 $4 }

GlobalDecl:
  GlobalIdentifier "=" AddrSpace list(GlobalAnnotation) Type PartialConstant AlignmentSpec
  { mkGlobalDecl $1 $3 $4 $5 $6 $7 }

FunctionDefinition:
  "define" LinkageType VisibilityStyle CallingConvention list(ParameterAttribute)
     Type GlobalIdentifier "(" FuncArgList ")" list(FunctionAttribute)
     SectionName FunctionAlignment GCName "{" FunctionBody "}"
  { mkFunctionDef $2 $3 $4 $5 $6 $7 $9 $11 $12 $13 $14 $16 }

ModuleLevelAssembly:
  "module" "asm" string newline { ModuleAssembly $3 }

DataLayout:
  "target" "datalayout" "=" string newline { mkDataLayout $4 }

Triple:
  "target" "triple" "=" string newline { mkTriple $4 }

ExternalDecl:
    "declare" Type GlobalIdentifier "(" FuncTypeArgList ")" list(FunctionAttribute)
    { mkExternalFuncDecl $2 $3 $5 $7 }
  | "declare" Type GlobalIdentifier
    { ExternalDecl $2 $3 }


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
  |             { VisibilityDefault }

GlobalIdentifier:
  gident { GlobalIdentifier $1 }

LocalIdentifier:
  lident { LocalIdentifier $1 }

Identifier:
    LocalIdentifier { $1 }
  | GlobalIdentifier { $1 }

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

SectionName:
    "section" string { Just $2 }
  |                  { Nothing }


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
--Might need to fix this to actually have attributes in the type
  | Type "(" FuncTypeArgList ")" { TypeFunction $1 (fst $3) (snd $3) [] }
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

AddrSpace:
    addrspace { $1 }
  |           { 0 }

GlobalAnnotation:
    "constant" { GAConstant }
  | "global"   { GAGlobal }
  | "common"   { GACommon }
  | "private"  { GAPrivate }

FuncArgList:
    Type list(ParameterAttribute) LocalIdentifier MoreFuncArgs
    { ((FormalParameter $1 $2 $3) : (fst $4), snd $4) }
  | { ([], False) }

MoreFuncArgs:
    "," Type list(ParameterAttribute) LocalIdentifier MoreFuncArgs
    { ((FormalParameter $2 $3 $4) : (fst $5), snd $5) }
  | "," "..."
    { ([], True) }
  | { ([], False) }

NormalArgument:
  Type LocalIdentifier { ($1, $2) }


FunctionBody:
    -- This is the simple case for a function with no control flow; there is no
    -- initial label and just a single basic block.
    list1(Instruction) { [mkBasicBlock "0" $1] }
  | list1(BasicBlock)  { $1 }

BasicBlock:
  label list(Instruction) { mkBasicBlock $1 $2 }

-- FIXME: Need the constant instructions

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

AllConstants:
    SimpleConstant  { $1 }
  | ComplexConstant { $1 }

-- These "constants" are actually partial - they need to be applied to a type
-- before they are actually constants.
PartialConstant:
    AllConstants { ConstValue $1 }
  | Identifier   { valueRef $1 }

-- These are full constants
Constant:
  Type PartialConstant { $2 $1 }

-- FIXME: Inline asm
-- FIXME: Handle metadata

Instruction:
    "ret" Type optional(PartialConstant)
    {% mkRetInst $2 $3 }
  | "br" "label" label   { voidInst $ UnconditionalBranchInst $3 }
  | "br" Type PartialConstant "," "label" label "," "label" label
    { voidInst $ BranchInst ($3 $2) $6 $9 }
  | "switch" Type PartialConstant "," "label" label "[" list(SwitchBranch) "]"
    { voidInst $ SwitchInst ($3 $2) $6 $8 }
  | "indirectbr" Type PartialConstant "," "[" sep(LabelVal, ",") "]"
    { voidInst $ IndirectBranchInst ($3 $2) $6 }
  | optional(CallIdentifier) "invoke" CallingConvention list(ParameterAttribute) Type PartialConstant "(" sep(Constant, ",") ")" list(FunctionAttribute) "to" Constant "unwind" Constant
    {% mkInvokeInst $1 $3 $4 $5 $6 $8 $10 $12 $14 }
  | "unwind" { voidInst UnwindInst }
  | "unreachable" { voidInst UnreachableInst }
  | LocalIdentifier "=" AddInst list(ArithFlag) Type PartialConstant "," PartialConstant
    {% mkFlaggedArithInst AddInst $1 $5 $4 $6 $8 }
  | LocalIdentifier "=" SubInst list(ArithFlag) Type PartialConstant "," PartialConstant
    {% mkFlaggedArithInst SubInst $1 $5 $4 $6 $8 }
  | LocalIdentifier "=" MulInst list(ArithFlag) Type PartialConstant "," PartialConstant
    {% mkFlaggedArithInst MulInst $1 $5 $4 $6 $8 }
  | LocalIdentifier "=" DivInst Type PartialConstant "," PartialConstant
    {% mkArithInst DivInst $1 $4 $5 $7 }
  | LocalIdentifier "=" RemInst Type PartialConstant "," PartialConstant
    {% mkArithInst RemInst $1 $4 $5 $7 }
  | LocalIdentifier "=" "shl"  Type PartialConstant "," PartialConstant
    {% mkArithInst ShlInst $1 $4 $5 $7 }
  | LocalIdentifier "=" "lshr" Type PartialConstant "," PartialConstant
    {% mkArithInst LshrInst $1 $4 $5 $7 }
  | LocalIdentifier "=" "ashr" Type PartialConstant "," PartialConstant
    {% mkArithInst AshrInst $1 $4 $5 $7 }
  | LocalIdentifier "=" "and"  Type PartialConstant "," PartialConstant
    {% mkArithInst AndInst $1 $4 $5 $7 }
  | LocalIdentifier "=" "or"   Type PartialConstant "," PartialConstant
    {% mkArithInst OrInst $1 $4 $5 $7 }
  | LocalIdentifier "=" "xor"  Type PartialConstant "," PartialConstant
    {% mkArithInst XorInst $1 $4 $5 $7 }
  | LocalIdentifier "=" "extractelement" Type PartialConstant "," Constant
    {% mkExtractElementInst $1 $4 $5 $7 }
  | LocalIdentifier "=" "insertelement" Type PartialConstant "," Constant "," Constant
    { mkInsertElementInst $1 $4 $5 $7 $9 }
  | LocalIdentifier "=" "shufflevector" Type PartialConstant "," Type PartialConstant "," Type PartialConstant
    {% mkShuffleVectorInst $1 $4 $5 $7 $8 $10 $11 }
  | LocalIdentifier "=" "extractvalue" Type PartialConstant "," intlit list(ExtraIntLitIndex)
    {% mkExtractValueInst $1 $4 $5 ($7 : $8) }
  | LocalIdentifier "=" "insertvalue" Type PartialConstant "," Type PartialConstant "," intlit
    { mkInsertValueInst $1 $4 $5 $7 $8 $10 }
  | LocalIdentifier "=" "alloca" Type AllocaNumElems AlignmentSpec
    { mkAllocaInst $1 $4 $5 $6 }
  -- FIXME: Add support for the !nontemporal metadata thing
  | LocalIdentifier "=" VolatileFlag "load" Type PartialConstant AlignmentSpec
    { mkLoadInst $1 $3 $5 $6 $7 }
  -- FIXME: Add support for !<index> = !{ <ty> <val> } form
  -- FIXME: There is also an optional nontemporal thing
  | VolatileFlag "store" Type PartialConstant "," Type PartialConstant AlignmentSpec
    {% mkStoreInst $1 $3 $4 $6 $7 $8 }
  | LocalIdentifier "=" "getelementptr" InBounds Type PartialConstant "," sep1(Constant, ",")
    {% mkGetElementPtrInst $1 $4 $5 $6 $8 }
  | LocalIdentifier "=" "trunc" Constant "to" Type
    {% mkConversionInst TruncInst $1 $4 $6 }
  | LocalIdentifier "=" "zext" Constant "to" Type
    {% mkConversionInst ZExtInst $1 $4 $6 }
  | LocalIdentifier "=" "sext" Constant "to" Type
    {% mkConversionInst SExtInst $1 $4 $6 }
  | LocalIdentifier "=" "fptrunc" Constant "to" Type
    {% mkConversionInst FPTruncInst $1 $4 $6 }
  | LocalIdentifier "=" "fpext" Constant "to" Type
    {% mkConversionInst FPExtInst $1 $4 $6 }
  | LocalIdentifier "=" "fptoui" Constant "to" Type
    {% mkConversionInst FPToUIInst $1 $4 $6 }
  | LocalIdentifier "=" "fptosi" Constant "to" Type
    {% mkConversionInst FPToSIInst $1 $4 $6 }
  | LocalIdentifier "=" "uitofp" Constant "to" Type
    {% mkConversionInst UIToFPInst $1 $4 $6 }
  | LocalIdentifier "=" "sitofp" Constant "to" Type
    {% mkConversionInst SIToFPInst $1 $4 $6 }
  | LocalIdentifier "=" "ptrtoint" Constant "to" Type
    {% mkConversionInst PtrToIntInst $1 $4 $6 }
  | LocalIdentifier "=" "inttoptr" Constant "to" Type
    {% mkConversionInst IntToPtrInst $1 $4 $6 }
  | LocalIdentifier "=" "bitcast" Constant "to" Type
    {% mkConversionInst BitcastInst $1 $4 $6 }
  | LocalIdentifier "=" "icmp" ICmpCondition Type PartialConstant "," PartialConstant
    {% mkIcmpInst $1 $4 $5 $6 $8 }
  | LocalIdentifier "=" "fcmp" FCmpCondition Type PartialConstant "," PartialConstant
    {% mkFcmpInst $1 $4 $5 $6 $8 }
  | LocalIdentifier "=" "phi" Type sep1(PhiPair, ",")
    {% mkPhiNode $1 $4 $5 }
  | LocalIdentifier "=" "select" Type PartialConstant "," Type PartialConstant "," Type PartialConstant
    {% mkSelectInst $1 $4 $5 $7 $8 $10 $11 }
  | optional(CallIdentifier) TailMarker "call" CallingConvention list(ParameterAttribute) Type optional(Type) PartialConstant "(" sep(Constant, ",") ")" list(FunctionAttribute)
    {% mkCallInst $1 $2 $4 $5 $6 $7 $8 $10 $12 }
  | LocalIdentifier "=" "va_arg" Type PartialConstant "," Type
    {% mkVaArgInst $1 $4 $5 $7 }

InBounds:
    "inbounds" { True  }
  |            { False }

ExtraIntLitIndex:
  "," intlit { $2 }

CallIdentifier:
  LocalIdentifier "=" { $1 }

TailMarker:
    "tail" { True }
  |        { False }

PhiPair:
  PartialConstant "," LocalIdentifier { ($1, $3) }

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

FunctionAlignment:
    "align" intlit { $2 }
  |                { 0  }

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
