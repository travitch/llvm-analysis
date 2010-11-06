{
module Data.LLVM.AssemblyParser (llvmAssemblyParser) where

import Data.LLVM.AttributeTypes
import Data.LLVM.Lexer
import Data.LLVM.PlaceholderTypes
import Data.Monoid
}

%name llvmAssemblyParser
%tokentype { Token }
%error { parseError }

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

%%

Test: Type { $1 }

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

-- Helper parameterized parsers

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
parseError :: [Token] -> a
parseError ts = error ("Parse Error: " `mappend` show ts)

-- FIXME: Parse the bytestring - have the code for this already in the
-- old attoparsec-based parser
mkDataLayout s = defaultDataLayout

}
