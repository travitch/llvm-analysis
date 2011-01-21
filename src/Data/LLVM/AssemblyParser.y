{
{-# LANGUAGE OverloadedStrings #-}
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
  gident      { TGlobalIdent _ $$ }
  lident      { TLocalIdent _ $$ }
  mdname      { TMetadataName _ $$ }
  intlit      { TIntLit _ $$ }
  floatlit    { TFloatLit _ $$ }
  stringlit   { TStringLit _ $$ }
  mdstring    { TMetadataString _ $$ }
  label       { TLabel _ $$ }
  string      { TString _ $$ }
  "true"      { TTrueLit _ }
  "false"     { TFalseLit _ }
  "null"      { TNullLit _ }
  "undef"     { TUndefLit _ }
  "zeroinitializer" { TZeroInitializer _ }

  ","         { TComma _ }
  "="         { TAssign _ }
  "*"         { TStar _ }
  "("         { TLParen _ }
  ")"         { TRParen _ }
  "["         { TLSquare _ }
  "]"         { TRSquare _ }
  "{"         { TLCurl _ }
  "}"         { TRCurl _ }
  "<"         { TLAngle _ }
  ">"         { TRAngle _ }
  "!"         { TBang _ }
  "x"         { TAggLen _ }
  "to"        { TTo _ }
  "..."       { TDotDotDot _ }

  "private"   { TPrivate _ }
  "linker_private" { TLinkerPrivate _ }
  "linker_private_weak" { TLinkerPrivateWeak _ }
  "linker_private_weak_def_auto" { TLinkerPrivateWeakDefAuto _ }
  "internal"  { TInternal _ }
  "available_externally" { TAvailableExternally _ }
  "link_once" { TLinkOnce _ }
  "weak"      { TWeak _ }
  "common"    { TCommon _ }
  "appending" { TAppending _ }
  "extern_weak" { TExternWeak _ }
  "link_once_odr" { TLinkOnceODR _ }
  "weak_odr"  { TWeakODR _ }
  "dllimport" { TDLLImport _ }
  "dllexport" { TDLLExport _ }

  "ccc"       { TCCCCC _ }
  "fastcc"    { TCCFastCC _ }
  "coldcc"    { TCCColdCC _ }
  "ghc"       { TCCGHC _ }
  ccN         { TCCN _ $$ }

  "default"   { TVisDefault _ }
  "hidden"    { TVisHidden _ }
  "protected" { TVisProtected _ }

  "zeroext"   { TPAZeroExt _ }
  "signext"   { TPASignExt _ }
  "inreg"     { TPAInReg _ }
  "byval"     { TPAByVal _ }
  "sret"      { TPASRet _ }
  "noalias"   { TPANoAlias _ }
  "nocapture" { TPANoCapture _ }
  "nest"      { TPANest _ }

  alignstackN   { TFAAlignStack _ $$ }
  "alwaysinline" { TFAAlwaysInline _ }
  "hotpatch"     { TFAHotPatch _ }
  "inlinehint"   { TFAInlineHint _ }
  "naked"        { TFANaked _ }
  "noimplicitfloat" { TFANoImplicitFloat _ }
  "noinline"     { TFANoInline _ }
  "noredzone"    { TFANoRedZone _ }
  "noreturn"     { TFANoReturn _ }
  "nounwind"     { TFANoUnwind _ }
  "optsize"      { TFAOptSize _ }
  "readnone"     { TFAReadNone _ }
  "readonly"     { TFAReadOnly _ }
  "ssp"          { TFASSP _ }
  "sspreq"       { TFASSPReq _ }

  iN             { TIntegralT _ $$ }
  "float"        { TFloatT _ }
  "double"       { TDoubleT _ }
  "x86_fp80"     { TX86_FP80T _ }
  "fp128"        { TFP128T _ }
  "ppc_fp128"    { TPPC_FP128T _ }
  "x86mmx"       { TX86mmxT _ }
  "void"         { TVoidT _ }
  "metadata"     { TMetadataT _ }
  "opaque"       { TOpaqueT _ }
  upref          { TUprefT _ $$ }
  "label"        { TLabelT _ }

  "type"         { TType _ }
  addrspace      { TAddrspace _ $$ }
  "constant"     { TConstant _ }
  "section"      { TSection _ }
  "align"        { TAlign _ }
  "alignstack"   { TAlignStack _ }
  "sideeffect"   { TSideEffect _ }
  "alias"        { TAlias _ }
  "declare"      { TDeclare _ }
  "define"       { TDefine _ }
  "gc"           { TGC _ }
  "module"       { TModule _ }
  "asm"          { TAsm _ }
  "target"       { TTarget _ }
  "datalayout"   { TDataLayout _ }
  "blockaddress" { TBlockAddress _ }
  "inbounds"     { TInbounds _ }
  "global"       { TGlobal _ }
  "tail"         { TTail _ }
  "triple"       { TTriple _ }
  ",!dbg"          { TDbg _ }
  "external"     { TExternal _ }

  "nuw"          { TNUW _ }
  "nsw"          { TNSW _ }

  "exact"        { TExact _ }
  "volatile"     { TVolatile _ }

  "trunc"        { TTrunc _ }
  "zext"         { TZext _ }
  "sext"         { TSext _ }
  "fptrunc"      { TFpTrunc _ }
  "fpext"        { TFpExt _ }
  "fptoui"       { TFpToUI _ }
  "fptosi"       { TFpToSI _ }
  "uitofp"       { TUIToFp _ }
  "sitofp"       { TSIToFp _ }
  "ptrtoint"     { TPtrToInt _ }
  "inttoptr"     { TIntToPtr _ }
  "bitcast"      { TBitCast _ }
  "getelementptr"  { TGetElementPtr _ }
  "select"       { TSelect _ }
  "icmp"         { TIcmp _ }
  "fcmp"         { TFcmp _ }
  "extractelement" { TExtractElement _ }
  "insertelement"  { TInsertElement _ }
  "shufflevector"  { TShuffleVector _ }
  "extractvalue"   { TExtractValue _ }
  "insertvalue"    { TInsertValue _ }
  "call"           { TCall _ }
  "ret"            { TRet _ }
  "br"             { TBr _ }
  "switch"         { TSwitch _ }
  "indirectbr"     { TIndirectBr _ }
  "invoke"         { TInvoke _ }
  "unwind"         { TUnwind _ }
  "unreachable"    { TUnreachable _ }
  "add"            { TAdd _ }
  "fadd"           { TFadd _ }
  "sub"            { TSub _ }
  "fsub"           { TFsub _ }
  "mul"            { TMul _ }
  "fmul"           { TFmul _ }
  "udiv"           { TUdiv _ }
  "sdiv"           { TSdiv _ }
  "fdiv"           { TFdiv _ }
  "urem"           { TUrem _ }
  "srem"           { TSrem _ }
  "frem"           { TFrem _ }
  "shl"            { TShl _ }
  "lshr"           { TLshr _ }
  "ashr"           { TAshr _ }
  "and"            { TAnd _ }
  "or"             { TOr _ }
  "xor"            { TXor _ }
  "alloca"         { TAlloca _ }
  "load"           { TLoad _ }
  "store"          { TStore _ }
  "phi"            { TPhi _ }
  "va_arg"         { TVaArg _ }

  "eq"             { Teq _ }
  "ne"             { Tne _ }
  "ugt"            { Tugt _ }
  "uge"            { Tuge _ }
  "ult"            { Tult _ }
  "ule"            { Tule _ }
  "sgt"            { Tsgt _ }
  "sge"            { Tsge _ }
  "slt"            { Tslt _ }
  "sle"            { Tsle _ }
  "oeq"            { Toeq _ }
  "ogt"            { Togt _ }
  "oge"            { Toge _ }
  "olt"            { Tolt _ }
  "ole"            { Tole _ }
  "one"            { Tone _ }
  "ord"            { Tord _ }
  "ueq"            { Tueq _ }
  "une"            { Tune _ }
  "uno"            { Tuno _ }

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
  | UnnamedMetadata     { $1 }
  | NamedMetadata       { $1 }
  | GlobalAlias         { $1 }

-- Note: This can supposedly also take a bitcast of a global identifier...
GlobalAlias:
  GlobalIdentifier "=" "alias" LinkageType VisibilityStyle Type PartialConstant
  { mkGlobalAlias $1 $4 $5 $6 $7 }

TypeDeclaration:
  LocalIdentifier "=" "type" Type { NamedType $1 $4 }

GlobalDecl:
  GlobalIdentifier "=" AddrSpace list(GlobalAnnotation) Type PartialConstant AlignmentSpec
  { mkGlobalDecl $1 $3 $4 $5 $6 $7 }

FunctionDefinition:
  "define" LinkageType VisibilityStyle CallingConvention list(ParameterAttribute)
     Type GlobalIdentifier "(" FuncArgList ")" list(FunctionAttribute)
     SectionName FunctionAlignment optional(GCName) "{" FunctionBody "}"
  { mkFunctionDef $2 $3 $4 $5 $6 $7 $9 $11 $12 $13 $14 $16 }

ModuleLevelAssembly:
  "module" "asm" string { ModuleAssembly $ Assembly $3 }

DataLayout:
  "target" "datalayout" "=" string { mkDataLayout $4 }

Triple:
  "target" "triple" "=" string { mkTriple $4 }

-- FIXME: Add param attributes to extern decls
ExternalDecl:
    "declare" list(ParameterAttribute) Type GlobalIdentifier "(" FuncTypeArgList ")" list(FunctionAttribute)
    { mkExternalFuncDecl $3 $4 $6 $8 }
  | "declare" list(ParameterAttribute) Type GlobalIdentifier
    { ExternalDecl $3 $4 }

MetadataConstant:
    Constant { Just $1 }
  | "null"   { Nothing }

MDNode:
  "!" "{" sep(MetadataConstant, ",") "}"   { $3 }

UnnamedMetadata:
  MetaIdentifier "=" "metadata" MDNode
  { mkMDNode $1 $4 }

NamedMetadata:
  MetaIdentifier "=" "!" "{" sep1(MetaIdentifier, ",") "}"
  { mkNamedMetadata $1 $5 }

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

MetaIdentifier:
  mdname { MetaIdentifier $1 }

Identifier:
    LocalIdentifier  { $1 }
  | GlobalIdentifier { $1 }
  | MetaIdentifier   { $1 }

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
  | LocalIdentifier { TypeNamed $1 }

-- Can't use the simple sep1 parameterized rule here since
-- that generates a nasty shift/reduce conflict where the ,
-- before a ... is slurped into the list but sep1 doesn't
-- know what to do with ... since it isn't a type. Oh well.
-- FIXME: Need to save the parameter attribute list eventually.
FuncTypeArgList:
    Type list(ParameterAttribute) MoreFuncTypeArgs { ($1 : fst $3, snd $3) }
  |                                                { ([], False) }

MoreFuncTypeArgs:
    "," MoreFuncTypeArgsOrVararg { $2 }
  |                              { ([], False) }
  --   "," Type list(ParameterAttribute) MoreFuncTypeArgs { ($2 : (fst $4), snd $4) }
  -- | "," "..."                 { ([], True) }
  -- |                           { ([], False) }

MoreFuncTypeArgsOrVararg:
    Type list(ParameterAttribute) MoreFuncTypeArgs { ($1 : (fst $3), snd $3) }
  | "..."                 { ([], True) }

AddrSpace:
    addrspace { $1 }
  |           { 0 }

GlobalAnnotation:
    "constant" { GAConstant }
  | "global"   { GAGlobal }
  | "common"   { GACommon }
  | "private"  { GAPrivate }
  | "external" { GAExternal }

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

SimpleConstant:
    "true"     { ConstantInt 1 }
  | "false"    { ConstantInt 0 }
  | intlit     { ConstantInt $1 }
  | floatlit   { ConstantFP $1 }
  | stringlit  { ConstantString $1 }
  | "null"     { ConstantPointerNull }
  | mdstring   { MDString $1 }

ComplexConstant:
    "{" sep(Constant, ",") "}"   { ConstantStruct $2 }
  | "[" sep(Constant, ",") "]"   { ConstantArray $2 }
  | "<" sep(Constant, ",") ">"   { ConstantVector $2 }
  | "zeroinitializer"                 { ConstantAggregateZero }
  | "undef"                           { UndefValue }
  | "blockaddress" "(" Identifier "," Identifier ")" { BlockAddress $3 $5 }
  -- The case where the mdnode ends in a plain untyped null shouldn't ever occur here
  | MDNode                       { MDNode $1 }
  | "asm" string "," string { InlineAsm $2 $4 }
  | "trunc" "(" Constant "to" Type ")" { ConstantExpr $ TruncInst $3 $5 }
  | "zext" "(" Constant "to" Type ")"  { ConstantExpr $ ZExtInst $3 $5 }
  | "sext" "(" Constant "to" Type ")"  { ConstantExpr $ SExtInst $3 $5 }
  | "fptrunc" "(" Constant "to" Type ")" { ConstantExpr $ FPTruncInst $3 $5 }
  | "fpext" "(" Constant "to" Type ")" { ConstantExpr $ FPExtInst $3 $5 }
  | "fptoui" "(" Constant "to" Type ")" { ConstantExpr $ FPToUIInst $3 $5 }
  | "fptosi" "(" Constant "to" Type ")" { ConstantExpr $ FPToSIInst $3 $5 }
  | "uitofp" "(" Constant "to" Type ")" { ConstantExpr $ UIToFPInst $3 $5 }
  | "sitofp" "(" Constant "to" Type ")" { ConstantExpr $ SIToFPInst $3 $5 }
  | "ptrtoint" "(" Constant "to" Type ")" { ConstantExpr $ PtrToIntInst $3 $5 }
  | "inttoptr" "(" Constant "to" Type ")" { ConstantExpr $ IntToPtrInst $3 $5 }
  | "bitcast" "(" Constant "to" Type ")"  { ConstantExpr $ BitcastInst $3 $5 }
  | "getelementptr" InBounds "(" Constant "," sep1(Constant, ",") ")"
    { ConstantExpr $ GetElementPtrInst $2 $4 $6 }
  | "select" "(" Constant "," Constant "," Constant ")"
    { ConstantExpr $ SelectInst $3 $5 $7 }
  | "icmp" ICmpCondition "(" Constant "," Constant ")"
    { ConstantExpr $ ICmpInst $2 $4 $6 }
  | "fcmp" FCmpCondition "(" Constant "," Constant ")"
    { ConstantExpr $ FCmpInst $2 $4 $6 }
  | "extractelement" "(" Constant "," Constant ")"
    { ConstantExpr $ ExtractElementInst $3 $5 }
  | "insertelement" "(" Constant "," Constant "," Constant ")"
    { ConstantExpr $ InsertElementInst $3 $5 $7 }
  | "shufflevector" "(" Constant "," Constant "," Constant ")"
    { ConstantExpr $ ShuffleVectorInst $3 $5 $7 }
  | "extractvalue" "(" Constant "," sep1(intlit, ",") ")"
    { ConstantExpr $ ExtractValueInst $3 $5 }
  | "insertvalue" "(" Constant "," Constant "," sep1(intlit, ",") ")"
    { ConstantExpr $ InsertValueInst $3 $5 $7 }
  | AddInst "(" Constant "," Constant ")" { ConstantExpr $ AddInst [] $3 $5 }
  | SubInst "(" Constant "," Constant ")" { ConstantExpr $ SubInst [] $3 $5 }
  | MulInst "(" Constant "," Constant ")" { ConstantExpr $ MulInst [] $3 $5 }
  | DivInst "(" Constant "," Constant ")" { ConstantExpr $ DivInst $3 $5 }
  | RemInst "(" Constant "," Constant ")" { ConstantExpr $ RemInst $3 $5 }
  | "shl" "(" Constant "," Constant ")"   { ConstantExpr $ ShlInst $3 $5 }
  | "lshr" "(" Constant "," Constant ")"  { ConstantExpr $ LshrInst $3 $5 }
  | "ashr" "(" Constant "," Constant ")"  { ConstantExpr $ AshrInst $3 $5 }
  | "and" "(" Constant "," Constant ")"   { ConstantExpr $ AndInst $3 $5 }
  | "or" "(" Constant "," Constant ")"     { ConstantExpr $ OrInst $3 $5 }
  | "xor" "(" Constant "," Constant ")"   { ConstantExpr $ XorInst $3 $5 }

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

CallArgument:
  Type SRetTag PartialConstant { ($3 $1, $2) }

SRetTag:
    "sret" { True }
  |        { False }

-- The "!dbg" token has a comma preceeding it to ensure it doesn't get slurped up
-- into argument lists
InstMetadata:
  ",!dbg" MetaIdentifier { $2 }

Instruction:
  InstructionNoMD optional(InstMetadata) { mkMDInst $1 $2 }

LocalLabel:
  LocalIdentifier { ValueRef $1 }

InstructionNoMD:
    "ret" Type optional(PartialConstant)
    {% mkRetInst $2 $3 }
  | "br" "label" LocalLabel   { voidInst $ UnconditionalBranchInst $3 }
  | "br" Type PartialConstant "," "label" LocalLabel "," "label" LocalLabel
    { voidInst $ BranchInst ($3 $2) $6 $9 }
  | "switch" Type PartialConstant "," "label" LocalLabel "[" list(SwitchBranch) "]"
    { voidInst $ SwitchInst ($3 $2) $6 $8 }
  | "indirectbr" Type PartialConstant "," "[" sep(LabelVal, ",") "]"
    { voidInst $ IndirectBranchInst ($3 $2) $6 }
  | optional(CallIdentifier) "invoke" CallingConvention list(ParameterAttribute) Type PartialConstant "(" sep(CallArgument, ",") ")" list(FunctionAttribute) "to" Constant "unwind" Constant
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
  | LocalIdentifier "=" "extractvalue" Type PartialConstant "," sep1(intlit, ",")
    {% mkExtractValueInst $1 $4 $5 $7 }
  | LocalIdentifier "=" "insertvalue" Type PartialConstant "," Type PartialConstant "," sep1(intlit, ",")
    { mkInsertValueInst $1 $4 $5 $7 $8 $10 }
  | LocalIdentifier "=" "alloca" Type AllocaTail
    { mkAllocaInst $1 $4 (fst $5) (snd $5) }
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
  | optional(CallIdentifier) TailMarker "call" CallingConvention list(ParameterAttribute) Type optional(Type) PartialConstant "(" sep(CallArgument, ",") ")" list(FunctionAttribute)
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
  "[" PartialConstant "," LocalLabel "]" { ($2, $4) }

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


-- This is split up because having two optional arguments in a row
-- causes a problematic shift/reduce conflict.  The number of elements
-- defaults to one if not specified.
AllocaTail:
    "," AllocaNumElemsOrAlign { $2 }
  |   { (ConstValue (ConstantInt 1) (TypeInteger 32), 0) }

AllocaNumElemsOrAlign:
    Constant AlignmentSpec { ($1, $2) }
  | "align" intlit { (ConstValue (ConstantInt 1) (TypeInteger 32), $2) }

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
  Type PartialConstant "," "label" LocalLabel { ($2 $1, $5) }

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
parseError ts = fail $ show $ take 10 ts

}
