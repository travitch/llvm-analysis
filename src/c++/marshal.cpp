#define __STDC_LIMIT_MACROS
#define __STDC_CONSTANT_MACROS

#include "marshal.h"

#include <algorithm>
#include <cstdlib>
#include <cstring>
#include <sstream>
#include <string>
#include <vector>
#include <tr1/unordered_map>

#include <llvm/CallingConv.h>
#include <llvm/InlineAsm.h>
#include <llvm/IntrinsicInst.h>
#include <llvm/Instructions.h>
#include <llvm/LLVMContext.h>
#include <llvm/Module.h>
#include <llvm/Type.h>
#include <llvm/DerivedTypes.h>
#include <llvm/ADT/OwningPtr.h>
#include <llvm/ADT/SmallVector.h>
#include <llvm/Analysis/DebugInfo.h>
#include <llvm/Bitcode/ReaderWriter.h>
#include <llvm/Support/MemoryBuffer.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/Support/system_error.h>

using namespace llvm;
using std::ostringstream;
using std::string;
using std::tr1::unordered_map;

struct PrivateData {
  LLVMContext ctxt;
  OwningPtr<MemoryBuffer> buffer;
  // Foreign callers do not need to access below this point.
  Module* original;
  int includeLocs;

  // This map is actually state only for this translation code.  Since
  // types have pointer equality in LLVM, every type will just be
  // translated once to a heap-allocated CType.  On the Haskell side,
  // each CType needs to be translated once (mapping the address of
  // the CType to the translated version).
  unordered_map<const Type*, CType*> typeMap;
  unordered_map<const Value*, CValue*> valueMap;
  unordered_map<const MDNode*, CMeta*> metaMap;
};

static ValueTag decodeOpcode(unsigned opcode) {
  switch(opcode) {
  case Instruction::Ret: return VAL_RETINST;
  case Instruction::Br: return VAL_BRANCHINST;
  case Instruction::Switch: return VAL_SWITCHINST;
  case Instruction::IndirectBr: return VAL_INDIRECTBRINST;
  case Instruction::Invoke: return VAL_INVOKEINST;
  case Instruction::Unwind: return VAL_UNWINDINST;
  case Instruction::Unreachable: return VAL_UNREACHABLEINST;
  case Instruction::Add: return VAL_ADDINST;
  case Instruction::FAdd: return VAL_FADDINST;
  case Instruction::Sub: return VAL_SUBINST;
  case Instruction::FSub: return VAL_FSUBINST;
  case Instruction::Mul: return VAL_MULINST;
  case Instruction::FMul: return VAL_FMULINST;
  case Instruction::UDiv: return VAL_UDIVINST;
  case Instruction::SDiv: return VAL_SDIVINST;
  case Instruction::FDiv: return VAL_FDIVINST;
  case Instruction::URem: return VAL_UREMINST;
  case Instruction::SRem: return VAL_SREMINST;
  case Instruction::FRem: return VAL_FREMINST;
  case Instruction::Shl: return VAL_SHLINST;
  case Instruction::LShr: return VAL_LSHRINST;
  case Instruction::AShr: return VAL_ASHRINST;
  case Instruction::And: return VAL_ANDINST;
  case Instruction::Or: return VAL_ORINST;
  case Instruction::Xor: return VAL_XORINST;
  case Instruction::Alloca: return VAL_ALLOCAINST;
  case Instruction::Load: return VAL_LOADINST;
  case Instruction::Store: return VAL_STOREINST;
  case Instruction::GetElementPtr: return VAL_GETELEMENTPTRINST;
  case Instruction::Trunc: return VAL_TRUNCINST;
  case Instruction::ZExt: return VAL_ZEXTINST;
  case Instruction::SExt: return VAL_SEXTINST;
  case Instruction::FPToUI: return VAL_FPTOUIINST;
  case Instruction::FPToSI: return VAL_FPTOSIINST;
  case Instruction::UIToFP: return VAL_UITOFPINST;
  case Instruction::SIToFP: return VAL_SITOFPINST;
  case Instruction::FPTrunc: return VAL_FPTRUNCINST;
  case Instruction::FPExt: return VAL_FPEXTINST;
  case Instruction::PtrToInt: return VAL_PTRTOINTINST;
  case Instruction::IntToPtr: return VAL_INTTOPTRINST;
  case Instruction::BitCast: return VAL_BITCASTINST;
  case Instruction::ICmp: return VAL_ICMPINST;
  case Instruction::FCmp: return VAL_FCMPINST;
  case Instruction::PHI: return VAL_PHINODE;
  case Instruction::Call: return VAL_CALLINST;
  case Instruction::Select: return VAL_SELECTINST;
  case Instruction::VAArg: return VAL_VAARGINST;
  case Instruction::ExtractElement: return VAL_EXTRACTELEMENTINST;
  case Instruction::InsertElement: return VAL_INSERTELEMENTINST;
  case Instruction::ShuffleVector: return VAL_SHUFFLEVECTORINST;
  case Instruction::ExtractValue: return VAL_EXTRACTVALUEINST;
  case Instruction::InsertValue: return VAL_INSERTVALUEINST;
  }

  ostringstream os;
  os << "Unhandled instruction type in opcode translator: " << opcode;
  throw os.str();
}

static CmpPredicate decodePredicate(CmpInst::Predicate p) {
  switch(p) {
  case CmpInst::FCMP_FALSE: return F_CMP_FALSE;
  case CmpInst::FCMP_OEQ: return F_CMP_OEQ;
  case CmpInst::FCMP_OGT: return F_CMP_OGT;
  case CmpInst::FCMP_OGE: return F_CMP_OGE;
  case CmpInst::FCMP_OLT: return F_CMP_OLT;
  case CmpInst::FCMP_OLE: return F_CMP_OLE;
  case CmpInst::FCMP_ONE: return F_CMP_ONE;
  case CmpInst::FCMP_ORD: return F_CMP_ORD;
  case CmpInst::FCMP_UNO: return F_CMP_UNO;
  case CmpInst::FCMP_UEQ: return F_CMP_UEQ;
  case CmpInst::FCMP_UGT: return F_CMP_UGT;
  case CmpInst::FCMP_UGE: return F_CMP_UGE;
  case CmpInst::FCMP_ULT: return F_CMP_ULT;
  case CmpInst::FCMP_ULE: return F_CMP_ULE;
  case CmpInst::FCMP_UNE: return F_CMP_UNE;
  case CmpInst::FCMP_TRUE: return F_CMP_TRUE;
  case CmpInst::ICMP_EQ: return I_CMP_EQ;
  case CmpInst::ICMP_NE: return I_CMP_NE;
  case CmpInst::ICMP_UGT: return I_CMP_UGT;
  case CmpInst::ICMP_UGE: return I_CMP_UGE;
  case CmpInst::ICMP_ULT: return I_CMP_ULT;
  case CmpInst::ICMP_ULE: return I_CMP_ULE;
  case CmpInst::ICMP_SGT: return I_CMP_SGT;
  case CmpInst::ICMP_SGE: return I_CMP_SGE;
  case CmpInst::ICMP_SLT: return I_CMP_SLT;
  case CmpInst::ICMP_SLE: return I_CMP_SLE;
  }

  ostringstream os;
  os << "Unhandled comparison predicate: " << p;
  throw os.str();
}

static TypeTag decodeTypeTag(Type::TypeID t) {
  switch(t) {
  case Type::VoidTyID: return TYPE_VOID;
  case Type::FloatTyID: return TYPE_FLOAT;
  case Type::DoubleTyID: return TYPE_DOUBLE;
  case Type::X86_FP80TyID: return TYPE_X86_FP80;
  case Type::FP128TyID: return TYPE_FP128;
  case Type::PPC_FP128TyID: return TYPE_PPC_FP128;
  case Type::LabelTyID: return TYPE_LABEL;
  case Type::MetadataTyID: return TYPE_METADATA;
  case Type::X86_MMXTyID: return TYPE_X86_MMX;
  case Type::IntegerTyID: return TYPE_INTEGER;
  case Type::FunctionTyID: return TYPE_FUNCTION;
  case Type::StructTyID: return TYPE_STRUCT;
  case Type::ArrayTyID: return TYPE_ARRAY;
  case Type::PointerTyID: return TYPE_POINTER;
  case Type::OpaqueTyID: return TYPE_OPAQUE;
  case Type::VectorTyID: return TYPE_VECTOR;
  }

  ostringstream os;
  os << "Unhandled type tag case: " << t;
  throw os.str();
}

static LinkageType decodeLinkage(const GlobalValue *gv) {
  switch(gv->getLinkage()) {
  case GlobalValue::ExternalLinkage: return LTExternal;
  case GlobalValue::AvailableExternallyLinkage: return LTAvailableExternally;
  case GlobalValue::LinkOnceAnyLinkage: return LTLinkOnceAny;
  case GlobalValue::LinkOnceODRLinkage: return LTLinkOnceODR;
  case GlobalValue::WeakAnyLinkage: return LTWeakAny;
  case GlobalValue::WeakODRLinkage: return LTWeakODR;
  case GlobalValue::AppendingLinkage: return LTAppending;
  case GlobalValue::InternalLinkage: return LTInternal;
  case GlobalValue::PrivateLinkage: return LTPrivate;
  case GlobalValue::LinkerPrivateLinkage: return LTLinkerPrivate;
  case GlobalValue::LinkerPrivateWeakLinkage: return LTLinkerPrivateWeak;
  case GlobalValue::LinkerPrivateWeakDefAutoLinkage: return LTLinkerPrivateWeakDefAuto;
  case GlobalValue::DLLImportLinkage: return LTDLLImport;
  case GlobalValue::DLLExportLinkage: return LTDLLExport;
  case GlobalValue::ExternalWeakLinkage: return LTExternalWeak;
  case GlobalValue::CommonLinkage: return LTCommon;
  }

  ostringstream os;
  os << "Unhandled linkage type: " << gv->getLinkage();
  throw os.str();
}

static VisibilityStyle decodeVisibility(const GlobalValue *gv) {
  switch(gv->getVisibility()) {
  case GlobalValue::DefaultVisibility: return VisibilityDefault;
  case GlobalValue::HiddenVisibility: return VisibilityHidden;
  case GlobalValue::ProtectedVisibility: return VisibilityProtected;
  }

  ostringstream os;
  os << "Unhandled visibility style: " << gv->getVisibility();
  throw os.str();
}

static CallingConvention decodeCallingConvention(CallingConv::ID cc) {
  switch(cc) {
  case CallingConv::C: return CC_C;
  case CallingConv::Fast: return CC_FAST;
  case CallingConv::Cold: return CC_COLD;
  case CallingConv::GHC: return CC_GHC;
  case CallingConv::X86_StdCall: return CC_X86_STDCALL;
  case CallingConv::X86_FastCall: return CC_X86_FASTCALL;
  case CallingConv::ARM_APCS: return CC_ARM_APCS;
  case CallingConv::ARM_AAPCS: return CC_ARM_AAPCS;
  case CallingConv::ARM_AAPCS_VFP: return CC_ARM_AAPCS_VFP;
  case CallingConv::MSP430_INTR: return CC_MSP430_INTR;
  case CallingConv::X86_ThisCall: return CC_X86_THISCALL;
  case CallingConv::PTX_Kernel: return CC_PTX_KERNEL;
  case CallingConv::PTX_Device: return CC_PTX_DEVICE;
  case CallingConv::MBLAZE_INTR: return CC_MBLAZE_INTR;
  case CallingConv::MBLAZE_SVOL: return CC_MBLAZE_SVOL;
  }

  ostringstream os;
  os << "Unhandled calling convention: " << cc;
  throw os.str();
}

static void disposeCType(CType *ct) {
  if(ct->typeTag == TYPE_NAMED)
    disposeCType(ct->innerType);

  free(ct->name);
  free(ct->typeList);
  free(ct);
}

static void disposeCMeta(CMeta *meta) {
  switch(meta->metaTag) {
  case META_LOCATION:
    free(meta->u.metaLocationInfo.filename);
    free(meta->u.metaLocationInfo.directory);
    break;

  case META_DERIVEDTYPE:
  case META_COMPOSITETYPE:
  case META_BASICTYPE:
    free(meta->u.metaTypeInfo.name);
    free(meta->u.metaTypeInfo.directory);
    free(meta->u.metaTypeInfo.filename);
    break;

  case META_VARIABLE:
    free(meta->u.metaVariableInfo.name);
    break;

  case META_SUBPROGRAM:
    free(meta->u.metaSubprogramInfo.name);
    free(meta->u.metaSubprogramInfo.displayName);
    free(meta->u.metaSubprogramInfo.linkageName);
    free(meta->u.metaSubprogramInfo.returnTypeName);
    free(meta->u.metaSubprogramInfo.filename);
    free(meta->u.metaSubprogramInfo.directory);
    break;

  case META_GLOBALVARIABLE:
    free(meta->u.metaGlobalInfo.name);
    free(meta->u.metaGlobalInfo.displayName);
    free(meta->u.metaGlobalInfo.linkageName);
    break;

  case META_FILE:
    free(meta->u.metaFileInfo.filename);
    free(meta->u.metaFileInfo.directory);
    break;

  case META_COMPILEUNIT:
    free(meta->u.metaCompileUnitInfo.filename);
    free(meta->u.metaCompileUnitInfo.directory);
    free(meta->u.metaCompileUnitInfo.producer);
    free(meta->u.metaCompileUnitInfo.flags);
    break;

  case META_NAMESPACE:
    free(meta->u.metaNamespaceInfo.name);
    free(meta->u.metaNamespaceInfo.directory);
    free(meta->u.metaNamespaceInfo.filename);
    break;

  case META_LEXICALBLOCK:
    free(meta->u.metaLexicalBlockInfo.directory);
    free(meta->u.metaLexicalBlockInfo.filename);
    break;

  case META_SUBRANGE:
    break;

  case META_ENUMERATOR:
    free(meta->u.metaEnumeratorInfo.enumName);
    break;

  case META_ARRAY:
    free(meta->u.metaArrayInfo.arrayElts);
    break;

  case META_TEMPLATETYPEPARAMETER:
    free(meta->u.metaTemplateTypeInfo.name);
    free(meta->u.metaTemplateTypeInfo.filename);
    free(meta->u.metaTemplateTypeInfo.directory);
    break;

  case META_TEMPLATEVALUEPARAMETER:
    free(meta->u.metaTemplateValueInfo.name);
    free(meta->u.metaTemplateValueInfo.filename);
    free(meta->u.metaTemplateValueInfo.directory);
    break;
  }

  free(meta);
}

// Have to do the delete in this function since the pointer must be
// cast to the correct type.
static void disposeData(ValueTag t, void* data) {
  switch(t) {
  case VAL_ARGUMENT:
  {
    CArgumentInfo *ai = (CArgumentInfo*)data;
    free(ai);
    return;
  }

  case VAL_BASICBLOCK:
  {
    CBasicBlockInfo *bbi = (CBasicBlockInfo*)data;

    // The actual values are deleted from the valueMap
    free(bbi->instructions);

    free(bbi);
    return;
  }

  case VAL_INLINEASM:
  {
    CInlineAsmInfo *ii = (CInlineAsmInfo*)data;
    free(ii->asmString);
    free(ii->constraintString);
    free(ii);
    return;
  }

  case VAL_ALIAS:
  case VAL_GLOBALVARIABLE:
  {
    CGlobalInfo *gi = (CGlobalInfo*)data;

    free(gi->section);

    free(gi);
    return;
  }

  case VAL_FUNCTION:
  {
    CFunctionInfo *fi = (CFunctionInfo*)data;

    free(fi->section);
    free(fi->gcName);

    free(fi->arguments);
    free(fi->body);

    free(fi);
    return;
  }

  case VAL_UNWINDINST:
  case VAL_UNREACHABLEINST:
  {
    // No data
    return;
  }

  case VAL_INVOKEINST:
  case VAL_CALLINST:
  {
    CCallInfo *ci = (CCallInfo*)data;

    free(ci->arguments);
    free(ci);
    return;
  }

  case VAL_PHINODE:
  {
    CPHIInfo *pi = (CPHIInfo*)data;
    free(pi->incomingValues);
    free(pi->valueBlocks);
    free(pi);
    return;
  }

  case VAL_RETINST:
  case VAL_BRANCHINST:
  case VAL_SWITCHINST:
  case VAL_INDIRECTBRINST:
  case VAL_GETELEMENTPTRINST:
  case VAL_STOREINST:
  case VAL_ALLOCAINST:
  case VAL_LOADINST:
  case VAL_ADDINST:
  case VAL_FADDINST:
  case VAL_SUBINST:
  case VAL_FSUBINST:
  case VAL_MULINST:
  case VAL_FMULINST:
  case VAL_UDIVINST:
  case VAL_SDIVINST:
  case VAL_FDIVINST:
  case VAL_UREMINST:
  case VAL_SREMINST:
  case VAL_FREMINST:
  case VAL_SHLINST:
  case VAL_LSHRINST:
  case VAL_ASHRINST:
  case VAL_ANDINST:
  case VAL_ORINST:
  case VAL_XORINST:
  case VAL_TRUNCINST:
  case VAL_ZEXTINST:
  case VAL_SEXTINST:
  case VAL_FPTOUIINST:
  case VAL_FPTOSIINST:
  case VAL_UITOFPINST:
  case VAL_SITOFPINST:
  case VAL_FPTRUNCINST:
  case VAL_FPEXTINST:
  case VAL_PTRTOINTINST:
  case VAL_INTTOPTRINST:
  case VAL_BITCASTINST:
  case VAL_ICMPINST:
  case VAL_FCMPINST:
  case VAL_VAARGINST:
  case VAL_SELECTINST:
  case VAL_EXTRACTELEMENTINST:
  case VAL_INSERTELEMENTINST:
  case VAL_SHUFFLEVECTORINST:
  case VAL_EXTRACTVALUEINST:
  case VAL_INSERTVALUEINST:
  {
    CInstructionInfo *ii = (CInstructionInfo*)data;
    free(ii->operands);
    free(ii->indices);
    free(ii);
    return;
  }

  case VAL_BLOCKADDRESS:
  {
    CBlockAddrInfo *bi = (CBlockAddrInfo*)data;
    free(bi);
    return;
  }

  case VAL_CONSTANTINT:
  {
    CConstInt *d = (CConstInt*)data;
    free(d);
    return;
  }

  case VAL_CONSTANTFP:
  {
    CConstFP *d = (CConstFP*)data;
    free(d);
    return;
  }

  case VAL_CONSTANTPOINTERNULL:
  case VAL_CONSTANTAGGREGATEZERO:
  case VAL_UNDEFVALUE:
  {
    // No data
    return;
  }

  case VAL_CONSTANTSTRUCT:
  case VAL_CONSTANTVECTOR:
  case VAL_CONSTANTARRAY:
  {
    CConstAggregate *c = (CConstAggregate*)data;
    free(c->constants);
    free(c);
    return;
  }

  case VAL_CONSTANTEXPR:
  {
    CConstExprInfo *ce = (CConstExprInfo*)data;
    free(ce->ii->operands);
    free(ce->ii->indices);
    free(ce->ii);
    free(ce);
    return;
  }

  }

  ostringstream os;
  os << "Unhandled cleanup case for value tag: " << t;
  throw os.str();
}

static void disposeCValue(CValue *v) {
  // Do not dispose the type - that is taken care of in bulk in the
  // CModule disposal.  Same for MD.
  free(v->name);
  free(v->md);

  disposeData(v->valueTag, v->data);

  free(v);
}

static MetaTag extractMetaTag(const MDNode *md) {
  DIDescriptor desc(md);
  // Ranked roughly by frequency.
  if(desc.isLexicalBlock()) return META_LEXICALBLOCK;
  if(desc.isVariable()) return META_VARIABLE;
  if(desc.isSubprogram()) return META_SUBPROGRAM;
  if(desc.isGlobalVariable()) return META_GLOBALVARIABLE;
  if(desc.isDerivedType()) return META_DERIVEDTYPE;
  if(desc.isCompositeType()) return META_COMPOSITETYPE;
  if(desc.isBasicType()) return META_BASICTYPE;
  if(desc.isFile()) return META_FILE;
  if(desc.isCompileUnit()) return META_COMPILEUNIT;
  if(desc.isNameSpace()) return META_NAMESPACE;
  if(desc.isSubrange()) return META_SUBRANGE;
  if(desc.isEnumerator()) return META_ENUMERATOR;
  if(desc.isTemplateTypeParameter()) return META_TEMPLATETYPEPARAMETER;
  if(desc.isTemplateValueParameter()) return META_TEMPLATEVALUEPARAMETER;

  string msg;
  raw_string_ostream os(msg);
  os << "Unhandled metadata node type: ";
  md->print(os);
  throw os.str();
}

static CValue* translateConstant(CModule *m, const Constant *c);
static CValue* translateValue(CModule *m, const Value *v);
static CValue* translateBasicBlock(CModule *m, const BasicBlock *bb);
static CMeta* translateMetadata(CModule *m, const MDNode *md);

static void makeMetaLocation(CModule *m, const MDNode *md, CMeta *meta) {
  DILocation loc(md);
  meta->u.metaLocationInfo.lineNumber = loc.getLineNumber();
  meta->u.metaLocationInfo.columnNumber = loc.getColumnNumber();
  meta->u.metaLocationInfo.scope = translateMetadata(m, loc.getScope());
  meta->u.metaLocationInfo.origLocation = translateMetadata(m, loc.getOrigLocation());
  meta->u.metaLocationInfo.filename = strdup(loc.getFilename().str().c_str());
  meta->u.metaLocationInfo.directory = strdup(loc.getDirectory().str().c_str());
}

static void makeMetaDerivedType(CModule *m, const MDNode *md, CMeta *meta) {
  DIDerivedType dt(md);
  meta->u.metaTypeInfo.context = translateMetadata(m, dt.getContext());
  meta->u.metaTypeInfo.name = strdup(dt.getName().str().c_str());
  meta->u.metaTypeInfo.compileUnit = translateMetadata(m, dt.getCompileUnit());
  meta->u.metaTypeInfo.file = translateMetadata(m, dt.getFile());
  meta->u.metaTypeInfo.lineNumber = dt.getLineNumber();
  meta->u.metaTypeInfo.sizeInBits = dt.getSizeInBits();
  meta->u.metaTypeInfo.alignInBits = dt.getAlignInBits();
  meta->u.metaTypeInfo.offsetInBits = dt.getOffsetInBits();
  meta->u.metaTypeInfo.flags = dt.getFlags();
  meta->u.metaTypeInfo.isPrivate = dt.isPrivate();
  meta->u.metaTypeInfo.isProtected = dt.isProtected();
  meta->u.metaTypeInfo.isForward = dt.isForwardDecl();
  meta->u.metaTypeInfo.isByRefStruct = dt.isBlockByrefStruct();
  meta->u.metaTypeInfo.isVirtual = dt.isVirtual();
  meta->u.metaTypeInfo.isArtificial = dt.isArtificial();
  meta->u.metaTypeInfo.directory = strdup(dt.getDirectory().str().c_str());
  meta->u.metaTypeInfo.filename = strdup(dt.getFilename().str().c_str());

  meta->u.metaTypeInfo.typeDerivedFrom = translateMetadata(m, dt.getTypeDerivedFrom());
  meta->u.metaTypeInfo.originalTypeSize = dt.getOriginalTypeSize();
}

static void makeMetaCompositeType(CModule *m, const MDNode *md, CMeta *meta) {
  DICompositeType dt(md);
  meta->u.metaTypeInfo.context = translateMetadata(m, dt.getContext());
  meta->u.metaTypeInfo.name = strdup(dt.getName().str().c_str());
  meta->u.metaTypeInfo.compileUnit = translateMetadata(m, dt.getCompileUnit());
  meta->u.metaTypeInfo.file = translateMetadata(m, dt.getFile());
  meta->u.metaTypeInfo.lineNumber = dt.getLineNumber();
  meta->u.metaTypeInfo.sizeInBits = dt.getSizeInBits();
  meta->u.metaTypeInfo.alignInBits = dt.getAlignInBits();
  meta->u.metaTypeInfo.offsetInBits = dt.getOffsetInBits();
  meta->u.metaTypeInfo.flags = dt.getFlags();
  meta->u.metaTypeInfo.isPrivate = dt.isPrivate();
  meta->u.metaTypeInfo.isProtected = dt.isProtected();
  meta->u.metaTypeInfo.isForward = dt.isForwardDecl();
  meta->u.metaTypeInfo.isByRefStruct = dt.isBlockByrefStruct();
  meta->u.metaTypeInfo.isVirtual = dt.isVirtual();
  meta->u.metaTypeInfo.isArtificial = dt.isArtificial();
  meta->u.metaTypeInfo.directory = strdup(dt.getDirectory().str().c_str());
  meta->u.metaTypeInfo.filename = strdup(dt.getFilename().str().c_str());

  meta->u.metaTypeInfo.typeDerivedFrom = translateMetadata(m, dt.getTypeDerivedFrom());
  meta->u.metaTypeInfo.originalTypeSize = dt.getOriginalTypeSize();

  meta->u.metaTypeInfo.typeArray = translateMetadata(m, dt.getTypeArray());
  meta->u.metaTypeInfo.runTimeLang = dt.getRunTimeLang();
  meta->u.metaTypeInfo.containingType = translateMetadata(m, dt.getContainingType());
  meta->u.metaTypeInfo.templateParams = translateMetadata(m, dt.getTemplateParams());
}

static void makeMetaBasicType(CModule *m, const MDNode *md, CMeta *meta) {
  DIBasicType dt(md);
  meta->u.metaTypeInfo.context = translateMetadata(m, dt.getContext());
  meta->u.metaTypeInfo.name = strdup(dt.getName().str().c_str());
  meta->u.metaTypeInfo.compileUnit = translateMetadata(m, dt.getCompileUnit());
  meta->u.metaTypeInfo.file = translateMetadata(m, dt.getFile());
  meta->u.metaTypeInfo.lineNumber = dt.getLineNumber();
  meta->u.metaTypeInfo.sizeInBits = dt.getSizeInBits();
  meta->u.metaTypeInfo.alignInBits = dt.getAlignInBits();
  meta->u.metaTypeInfo.offsetInBits = dt.getOffsetInBits();
  meta->u.metaTypeInfo.flags = dt.getFlags();
  meta->u.metaTypeInfo.isPrivate = dt.isPrivate();
  meta->u.metaTypeInfo.isProtected = dt.isProtected();
  meta->u.metaTypeInfo.isForward = dt.isForwardDecl();
  meta->u.metaTypeInfo.isByRefStruct = dt.isBlockByrefStruct();
  meta->u.metaTypeInfo.isVirtual = dt.isVirtual();
  meta->u.metaTypeInfo.isArtificial = dt.isArtificial();
  meta->u.metaTypeInfo.directory = strdup(dt.getDirectory().str().c_str());
  meta->u.metaTypeInfo.filename = strdup(dt.getFilename().str().c_str());

  meta->u.metaTypeInfo.encoding = dt.getEncoding();
}

static void makeMetaVariable(CModule *m, const MDNode *md, CMeta *meta) {
  DIVariable dv(md);
  meta->u.metaVariableInfo.context = translateMetadata(m, dv.getContext());
  meta->u.metaVariableInfo.name = strdup(dv.getName().str().c_str());
  meta->u.metaVariableInfo.compileUnit = translateMetadata(m, dv.getCompileUnit());
  meta->u.metaVariableInfo.lineNumber = dv.getLineNumber();
  meta->u.metaVariableInfo.argNumber = dv.getArgNumber();
  meta->u.metaVariableInfo.type = translateMetadata(m, dv.getType());
  meta->u.metaVariableInfo.isArtificial = dv.isArtificial();
  meta->u.metaVariableInfo.hasComplexAddress = dv.hasComplexAddress();
  meta->u.metaVariableInfo.numAddrElements = dv.getNumAddrElements();
  if(meta->u.metaVariableInfo.numAddrElements > 0) {
    meta->u.metaVariableInfo.addrElements =
      (uint64_t*)calloc(meta->u.metaVariableInfo.numAddrElements, sizeof(uint64_t));

    for(unsigned int i = 0; i < meta->u.metaVariableInfo.numAddrElements; ++i) {
      meta->u.metaVariableInfo.addrElements[i] = dv.getAddrElement(i);
    }
  }

  meta->u.metaVariableInfo.isBlockByRefVar = dv.isBlockByrefVariable();
}

static void makeMetaSubprogram(CModule *m, const MDNode *md, CMeta *meta) {
  DISubprogram ds(md);
  meta->u.metaSubprogramInfo.context = translateMetadata(m, ds.getContext());
  meta->u.metaSubprogramInfo.name = strdup(ds.getName().str().c_str());
  meta->u.metaSubprogramInfo.displayName = strdup(ds.getDisplayName().str().c_str());
  meta->u.metaSubprogramInfo.linkageName = strdup(ds.getLinkageName().str().c_str());
  meta->u.metaSubprogramInfo.compileUnit = translateMetadata(m, ds.getCompileUnit());
  meta->u.metaSubprogramInfo.lineNumber = ds.getLineNumber();
  meta->u.metaSubprogramInfo.type = translateMetadata(m, ds.getType());
  meta->u.metaSubprogramInfo.returnTypeName = strdup(ds.getReturnTypeName().str().c_str());
  meta->u.metaSubprogramInfo.isLocalToUnit = ds.isLocalToUnit();
  meta->u.metaSubprogramInfo.isDefinition = ds.isDefinition();
  meta->u.metaSubprogramInfo.virtuality = ds.getVirtuality();
  meta->u.metaSubprogramInfo.virtualIndex = ds.getVirtualIndex();
  meta->u.metaSubprogramInfo.containingType = translateMetadata(m, ds.getContainingType());
  meta->u.metaSubprogramInfo.isArtificial = ds.isArtificial();
  meta->u.metaSubprogramInfo.isPrivate = ds.isPrivate();
  meta->u.metaSubprogramInfo.isProtected = ds.isProtected();
  meta->u.metaSubprogramInfo.isExplicit = ds.isExplicit();
  meta->u.metaSubprogramInfo.isPrototyped = ds.isPrototyped();
  meta->u.metaSubprogramInfo.isOptimized = ds.isOptimized();
  meta->u.metaSubprogramInfo.filename = strdup(ds.getFilename().str().c_str());
  meta->u.metaSubprogramInfo.directory = strdup(ds.getDirectory().str().c_str());
  const Value *func = ds.getFunction();
  if(func)
    meta->u.metaSubprogramInfo.function = translateValue(m, func);
}

static void makeMetaGlobalVariable(CModule *m, const MDNode *md, CMeta *meta) {
  DIGlobalVariable dg(md);
  meta->u.metaGlobalInfo.context = translateMetadata(m, dg.getContext());
  meta->u.metaGlobalInfo.name = strdup(dg.getName().str().c_str());
  meta->u.metaGlobalInfo.displayName = strdup(dg.getDisplayName().str().c_str());
  meta->u.metaGlobalInfo.linkageName = strdup(dg.getLinkageName().str().c_str());
  meta->u.metaGlobalInfo.compileUnit = translateMetadata(m, dg.getCompileUnit());
  meta->u.metaGlobalInfo.lineNumber = dg.getLineNumber();
  meta->u.metaGlobalInfo.globalType = translateMetadata(m, dg.getType());
  meta->u.metaGlobalInfo.isLocalToUnit = dg.isLocalToUnit();
  meta->u.metaGlobalInfo.isDefinition = dg.isDefinition();
  const Value *global = dg.getConstant();
  if(global)
    meta->u.metaGlobalInfo.global = translateValue(m, global);
}

static void makeMetaFile(CModule *m, const MDNode *md, CMeta *meta) {
  DIFile df(md);
  meta->u.metaFileInfo.filename = strdup(df.getFilename().str().c_str());
  meta->u.metaFileInfo.directory = strdup(df.getDirectory().str().c_str());
  meta->u.metaFileInfo.compileUnit = translateMetadata(m, df.getCompileUnit());
}

static void makeMetaCompileUnit(CModule *, const MDNode *md, CMeta *meta) {
  DICompileUnit dc(md);
  meta->u.metaCompileUnitInfo.language = dc.getLanguage();
  meta->u.metaCompileUnitInfo.filename = strdup(dc.getFilename().str().c_str());
  meta->u.metaCompileUnitInfo.directory = strdup(dc.getDirectory().str().c_str());
  meta->u.metaCompileUnitInfo.producer = strdup(dc.getProducer().str().c_str());
  meta->u.metaCompileUnitInfo.isMain = dc.isMain();
  meta->u.metaCompileUnitInfo.isOptimized = dc.isOptimized();
  meta->u.metaCompileUnitInfo.flags = strdup(dc.getFlags().str().c_str());
  meta->u.metaCompileUnitInfo.runtimeVersion = dc.getRunTimeVersion();
}

static void makeMetaNamespace(CModule *m, const MDNode *md, CMeta *meta) {
  DINameSpace dn(md);
  meta->u.metaNamespaceInfo.context = translateMetadata(m, dn.getContext());
  meta->u.metaNamespaceInfo.name = strdup(dn.getName().str().c_str());
  meta->u.metaNamespaceInfo.directory = strdup(dn.getDirectory().str().c_str());
  meta->u.metaNamespaceInfo.filename = strdup(dn.getFilename().str().c_str());
  meta->u.metaNamespaceInfo.compileUnit = translateMetadata(m, dn.getCompileUnit());
  meta->u.metaNamespaceInfo.lineNumber = dn.getLineNumber();
}

static void makeMetaLexicalBlock(CModule *m, const MDNode *md, CMeta *meta) {
  DILexicalBlock dl(md);
  meta->u.metaLexicalBlockInfo.context = translateMetadata(m, dl.getContext());
  meta->u.metaLexicalBlockInfo.lineNumber = dl.getLineNumber();
  meta->u.metaLexicalBlockInfo.columnNumber = dl.getColumnNumber();
  meta->u.metaLexicalBlockInfo.directory = strdup(dl.getDirectory().str().c_str());
  meta->u.metaLexicalBlockInfo.filename = strdup(dl.getFilename().str().c_str());
}

static void makeMetaSubrange(CModule *, const MDNode *md, CMeta *meta) {
  DISubrange ds(md);
  meta->u.metaSubrangeInfo.lo = ds.getLo();
  meta->u.metaSubrangeInfo.hi = ds.getHi();
}

static void makeMetaEnumerator(CModule *, const MDNode *md, CMeta *meta) {
  DIEnumerator de(md);
  meta->u.metaEnumeratorInfo.enumName = strdup(de.getName().str().c_str());
  meta->u.metaEnumeratorInfo.enumValue = de.getEnumValue();
}

static void makeMetaArray(CModule *m, const MDNode *md, CMeta *meta) {
  DIArray da(md);
  meta->u.metaArrayInfo.arrayLen = da.getNumElements();
  if(meta->u.metaArrayInfo.arrayLen == 0) return;

  meta->u.metaArrayInfo.arrayElts =
    (CMeta**)calloc(meta->u.metaArrayInfo.arrayLen, sizeof(CMeta*));
  for(int i = 0; i < meta->u.metaArrayInfo.arrayLen; ++i) {
    meta->u.metaArrayInfo.arrayElts[i] = translateMetadata(m, da.getElement(i));
  }
}

static void makeMetaTemplateTypeParameter(CModule *m, const MDNode *md, CMeta *meta) {
  DITemplateTypeParameter dt(md);
  meta->u.metaTemplateTypeInfo.context = translateMetadata(m, dt.getContext());
  meta->u.metaTemplateTypeInfo.name = strdup(dt.getName().str().c_str());
  meta->u.metaTemplateTypeInfo.type = translateMetadata(m, dt.getType());
  meta->u.metaTemplateTypeInfo.filename = strdup(dt.getFilename().str().c_str());
  meta->u.metaTemplateTypeInfo.directory = strdup(dt.getDirectory().str().c_str());
  meta->u.metaTemplateTypeInfo.lineNumber = dt.getLineNumber();
  meta->u.metaTemplateTypeInfo.columnNumber = dt.getColumnNumber();
}

static void makeMetaTemplateValueParameter(CModule *m, const MDNode *md, CMeta *meta) {
  DITemplateValueParameter dt(md);
  meta->u.metaTemplateValueInfo.context = translateMetadata(m, dt.getContext());
  meta->u.metaTemplateValueInfo.name = strdup(dt.getName().str().c_str());
  meta->u.metaTemplateValueInfo.type = translateMetadata(m, dt.getType());
  meta->u.metaTemplateValueInfo.value = dt.getValue();
  meta->u.metaTemplateValueInfo.filename = strdup(dt.getFilename().str().c_str());
  meta->u.metaTemplateValueInfo.directory = strdup(dt.getDirectory().str().c_str());
  meta->u.metaTemplateValueInfo.lineNumber = dt.getLineNumber();
  meta->u.metaTemplateValueInfo.columnNumber = dt.getColumnNumber();
}

static CMeta* translateMetadata(CModule *m, const MDNode *md) {
  // I expect some metadata fields to be empty (e.g., templateParams).
  // Just propagate nulls.
  if(md == NULL) return NULL;

  PrivateData *pd = (PrivateData*)m->privateData;
  unordered_map<const MDNode*,CMeta*>::const_iterator it = pd->metaMap.find(md);
  if(it != pd->metaMap.end())
    return it->second;

  CMeta *meta = (CMeta*)calloc(1, sizeof(CMeta));
  pd->metaMap[md] = meta;
  meta->metaTag = extractMetaTag(md);
  DIDescriptor desc(md);
  meta->tag = desc.getTag();

  switch(meta->metaTag) {
  case META_LOCATION: makeMetaLocation(m, md, meta); break;
  case META_DERIVEDTYPE: makeMetaDerivedType(m, md, meta); break;
  case META_COMPOSITETYPE: makeMetaCompositeType(m, md, meta); break;
  case META_BASICTYPE: makeMetaBasicType(m, md, meta); break;
  case META_VARIABLE: makeMetaVariable(m, md, meta); break;
  case META_SUBPROGRAM: makeMetaSubprogram(m, md, meta); break;
  case META_GLOBALVARIABLE: makeMetaGlobalVariable(m, md, meta); break;
  case META_FILE: makeMetaFile(m, md, meta); break;
  case META_COMPILEUNIT: makeMetaCompileUnit(m, md, meta); break;
  case META_NAMESPACE: makeMetaNamespace(m, md, meta); break;
  case META_LEXICALBLOCK: makeMetaLexicalBlock(m, md, meta); break;
  case META_SUBRANGE: makeMetaSubrange(m, md, meta); break;
  case META_ENUMERATOR: makeMetaEnumerator(m, md, meta); break;
  case META_ARRAY: makeMetaArray(m, md, meta); break;
  case META_TEMPLATETYPEPARAMETER: makeMetaTemplateTypeParameter(m, md, meta); break;
  case META_TEMPLATEVALUEPARAMETER: makeMetaTemplateValueParameter(m, md, meta); break;
  }

  return meta;
}

static CType* translateType(CModule *m, const Type *t) {
  PrivateData *pd = (PrivateData*)m->privateData;
  unordered_map<const Type*,CType*>::const_iterator it = pd->typeMap.find(t);
  if(it != pd->typeMap.end())
    return it->second;

  CType *nt = (CType*)calloc(1, sizeof(CType));
  CType *ret = nt;
  nt->typeTag = decodeTypeTag(t->getTypeID());


  string typeName = pd->original->getTypeName(t);
  if(typeName != "") {
    CType *namedTypeWrapper = (CType*)calloc(1, sizeof(CType));

    // Normally we would just return the type we are building, but in
    // this case we need to return the wrapper or it won't be
    // exported.
    ret = namedTypeWrapper;

    namedTypeWrapper->innerType = nt;
    namedTypeWrapper->typeTag = TYPE_NAMED;
    namedTypeWrapper->name = strdup(typeName.c_str());
  }

  // Need to put this in the table before making any recursive calls,
  // otherwise it might never terminate.
  pd->typeMap[t] = ret;

  switch(nt->typeTag) {
    // Primitives don't require any work
  case TYPE_VOID:
  case TYPE_FLOAT:
  case TYPE_DOUBLE:
  case TYPE_X86_FP80:
  case TYPE_FP128:
  case TYPE_PPC_FP128:
  case TYPE_LABEL:
  case TYPE_METADATA:
  case TYPE_X86_MMX:
    break;

    // Also nothing to do here
  case TYPE_OPAQUE:
    break;

  case TYPE_INTEGER:
    nt->size = t->getPrimitiveSizeInBits();
    break;

  case TYPE_FUNCTION:
  {
    const FunctionType *ft = dynamic_cast<const FunctionType*>(t);
    nt->isVarArg = ft->isVarArg();
    nt->innerType = translateType(m, ft->getReturnType());
    nt->typeListLen = ft->getNumParams();
    nt->typeList = (CType**)calloc(nt->typeListLen, sizeof(CType*));
    for(int i = 0; i < nt->typeListLen; ++i) {
      nt->typeList[i] = translateType(m, ft->getParamType(i));
    }

    break;
  }

  case TYPE_STRUCT:
  {
    const StructType *st = dynamic_cast<const StructType*>(t);
    nt->isPacked = st->isPacked();
    nt->typeListLen = st->getNumElements();
    nt->typeList = (CType**)calloc(nt->typeListLen, sizeof(CType*));
    for(int i = 0; i < nt->typeListLen; ++i) {
      nt->typeList[i] = translateType(m, st->getElementType(i));
    }
    break;
  }

  case TYPE_ARRAY:
  {
    const ArrayType *at = dynamic_cast<const ArrayType*>(t);
    nt->size = at->getNumElements();
    nt->innerType = translateType(m, at->getElementType());
    break;
  }

  case TYPE_POINTER:
  {
    const PointerType *pt = dynamic_cast<const PointerType*>(t);
    nt->innerType = translateType(m, pt->getElementType());
    nt->addrSpace = pt->getAddressSpace();
    break;
  }

  case TYPE_VECTOR:
  {
    const VectorType *vt = dynamic_cast<const VectorType*>(t);
    nt->size = vt->getNumElements();
    nt->innerType = translateType(m, vt->getElementType());
    break;
  }

  case TYPE_NAMED:
    throw "TYPE_NAMED is never directly generated by LLVM";
  }

  return ret;
}

static CValue* translateGlobalAlias(CModule *m, const GlobalAlias *ga) {
  PrivateData *pd = (PrivateData*)m->privateData;
  unordered_map<const Value*, CValue*>::iterator it = pd->valueMap.find(ga);
  if(it != pd->valueMap.end())
    return it->second;

  CValue *v = (CValue*)calloc(1, sizeof(CValue));
  pd->valueMap[ga] = v;

  v->valueTag = VAL_ALIAS;
  v->valueType = translateType(m, ga->getType());
  v->name = strdup(ga->getNameStr().c_str());

  CGlobalInfo *gi = (CGlobalInfo*)calloc(1, sizeof(CGlobalInfo));
  v->data = (void*)gi;

  gi->isExternal = ga->isDeclaration();
  gi->alignment = ga->getAlignment();
  gi->visibility = decodeVisibility(ga);
  gi->linkage = decodeLinkage(ga);
  if(ga->hasSection())
    gi->section = strdup(ga->getSection().c_str());

  gi->aliasee = translateConstant(m, ga->getAliasee());

  return v;
}

static CValue* translateArgument(CModule *m, const Argument *a) {
  PrivateData *pd = (PrivateData*)m->privateData;
  // Arguments are translated before instructions, so we don't really
  // need to check to see if the argument exists already (it won't).
  CValue *v = (CValue*)calloc(1, sizeof(CValue));
  pd->valueMap[a] = v;

  v->valueTag = VAL_ARGUMENT;
  v->valueType = translateType(m, a->getType());
  v->name = strdup(a->getNameStr().c_str());

  // Metadata will be attached as instructions are processed (calls to
  // the Debug intrinsics)

  CArgumentInfo *ai = (CArgumentInfo*)calloc(1, sizeof(CArgumentInfo));
  v->data = (void*)ai;

  ai->hasSRet = a->hasStructRetAttr();
  ai->hasByVal = a->hasByValAttr();
  ai->hasNest = a->hasNestAttr();
  ai->hasNoAlias = a->hasNoAliasAttr();
  ai->hasNoCapture = a->hasNoCaptureAttr();

  return v;
}

static void buildRetInst(CModule *m, CValue *v, const ReturnInst *ri) {
  v->valueTag = VAL_RETINST;
  v->valueType = translateType(m, ri->getType());
  // Never has a name

  CInstructionInfo *ii = (CInstructionInfo*)calloc(1, sizeof(CInstructionInfo));
  v->data = (void*)ii;

  if(Value* rv = ri->getReturnValue())
  {
    ii->numOperands = 1;
    ii->operands = (CValue**)calloc(1, sizeof(CValue*));
    ii->operands[0] = translateValue(m, rv);
  }

  // Otherwise, the data fields default to 0 as intended
}

static void buildSimpleInst(CModule *m, CValue *v, ValueTag t, const Instruction *inst) {
  v->valueTag = t;
  v->valueType = translateType(m, inst->getType());

  CInstructionInfo *ii = (CInstructionInfo*)calloc(1, sizeof(CInstructionInfo));
  v->data = (void*)ii;

  ii->numOperands = inst->getNumOperands();
  ii->operands = (CValue**)calloc(ii->numOperands, sizeof(CValue*));

  for(size_t i = 0; i < inst->getNumOperands(); ++i) {
    ii->operands[i] = translateValue(m, inst->getOperand(i));
  }
}

static void buildBinaryInst(CModule *m, CValue *v, ValueTag t, const Instruction *inst) {
  const BinaryOperator *bi = dynamic_cast<const BinaryOperator*>(inst);
  assert(bi);

  v->valueTag = t;
  v->valueType = translateType(m, bi->getType());

  CInstructionInfo *ii = (CInstructionInfo*)calloc(1, sizeof(CInstructionInfo));
  v->data = (void*)ii;

  ii->numOperands = inst->getNumOperands();
  ii->operands = (CValue**)calloc(ii->numOperands, sizeof(CValue*));
  for(int i = 0; i < ii->numOperands; ++i) {
    ii->operands[i] = translateValue(m, inst->getOperand(i));
  }

  ii->flags = ArithNone;
  if(bi->hasNoUnsignedWrap() && bi->hasNoSignedWrap())
    ii->flags = ArithBoth;
  else if(bi->hasNoUnsignedWrap())
    ii->flags = ArithNUW;
  else if(bi->hasNoSignedWrap())
    ii->flags = ArithNSW;
}

static void buildInvokeInst(CModule *m, CValue *v, const InvokeInst *ii) {
  v->valueTag = VAL_INVOKEINST;
  v->valueType = translateType(m, ii->getType());
  if(ii->hasName())
    v->name = strdup(ii->getNameStr().c_str());

  CCallInfo *ci = (CCallInfo*)calloc(1, sizeof(CCallInfo));
  v->data = (void*)ci;

  ci->calledValue = translateValue(m, ii->getCalledValue());
  ci->callingConvention = decodeCallingConvention(ii->getCallingConv());
  ci->hasSRet = ii->hasStructRetAttr();
  ci->normalDest = translateBasicBlock(m, ii->getNormalDest());
  ci->unwindDest = translateBasicBlock(m, ii->getUnwindDest());
  ci->argListLen = ii->getNumArgOperands();
  ci->arguments = (CValue**)calloc(ci->argListLen, sizeof(CValue*));

  for(unsigned i = 0; i < ii->getNumArgOperands(); ++i) {
    ci->arguments[i] = translateValue(m, ii->getArgOperand(i));
  }
}

static bool buildCallInst(CModule *m, CValue *v, const CallInst *ii) {

  /*
    I wanted to use the Intrinsics directly here, but it didn't seem
    to work.  I didn't look into the matter too closely, but these
    instructions insisted that they were simply calls and I didn't
    know how to convert them to intrinsics.
   */

//  if(const DbgDeclareInst *di = dynamic_cast<const DbgDeclareInst*>(ii)) {
  if(ii->getCalledValue()->getNameStr() == "llvm.dbg.declare") {
    CValue *addr = NULL;
    if(const MDNode *addrWrapper = dynamic_cast<const MDNode*>(ii->getArgOperand(0))) {
      const Value* realAddr = addrWrapper->getOperand(0);
      // If the alloca was completely eliminated, this debug
      // information can't be attached to anything as far as I can
      // tell.
      if(realAddr == NULL || dynamic_cast<const ConstantPointerNull*>(realAddr))
        return false;

      addr = translateValue(m, realAddr);
    }
    CMeta *md = NULL;
    if(const MDNode *variableVal = dynamic_cast<const MDNode*>(ii->getArgOperand(1))) {
      md = translateMetadata(m, variableVal);
    }

    if(!md) return false;

    // CValue *addr = translateValue(m, di->getAddress());
    // CMeta *md = translateMetadata(m, di->getVariable());

    // It should be the case that there should be exactly one of these
    // per alloca, and allocas should not have location info of their
    // own.
    if(addr->md) {
      return false;
    } // throw "Address of MD already has metadata";

    addr->numMetadata = 1;
    addr->md = (CMeta**)calloc(1, sizeof(CMeta*));
    addr->md[0] = md;

    return false;
  }

  // if(const DbgValueInst *di = dynamic_cast<const DbgValueInst*>(ii)) {
  if(ii->getCalledValue()->getNameStr() == "llvm.dbg.value") {
    // In this case, we could see llvm.dbg.value "calls" for updates
    // to parameters.  We should only attach the *first* one we
    // encounter, which will be the one describing parameters or
    // locals.  This is an important case when the bitcode has been
    // run through the mem2reg optimization pass and most allocas are
    // eliminated.  I don't care about when the value of a variable
    // changes (that is apparent from the instruction stream).  I
    // could be convinced to change the behavior here if there is a
    // good use case.

    CValue *val = NULL;
    if(const MDNode *valWrapper = dynamic_cast<const MDNode*>(ii->getArgOperand(0))) {
      const Value *realAddr = valWrapper->getOperand(0);
      if(realAddr == NULL || dynamic_cast<const ConstantPointerNull*>(realAddr))
        return false;

      val = translateValue(m, realAddr);
    }
    // CValue *val = translateValue(m, di->getValue());

    if(val->numMetadata == 0) {
      CMeta *md = NULL;
      if(const MDNode *variable = dynamic_cast<const MDNode*>(ii->getArgOperand(2))) {
        md = translateMetadata(m, variable);
      }

      if(!md) return false;

      // CMeta *md = translateMetadata(m, di->getVariable());
      val->numMetadata = 1;
      val->md = (CMeta**)calloc(1, sizeof(CMeta*));
      val->md[0] = md;
    }

    return false;
  }

  v->valueTag = VAL_CALLINST;
  v->valueType = translateType(m, ii->getType());
  if(ii->hasName())
    v->name = strdup(ii->getNameStr().c_str());

  CCallInfo *ci = (CCallInfo*)calloc(1, sizeof(CCallInfo));
  v->data = (void*)ci;

  ci->calledValue = translateValue(m, ii->getCalledValue());
  ci->callingConvention = decodeCallingConvention(ii->getCallingConv());
  ci->hasSRet = ii->hasStructRetAttr();
  ci->isTail = ii->isTailCall();
  ci->argListLen = ii->getNumArgOperands();
  ci->arguments = (CValue**)calloc(ci->argListLen, sizeof(CValue*));

  for(unsigned i = 0; i < ii->getNumArgOperands(); ++i) {
    ci->arguments[i] = translateValue(m, ii->getArgOperand(i));
  }

  return true;
}

static void buildAllocaInst(CModule *m, CValue *v, const AllocaInst *ai) {
  v->valueTag = VAL_ALLOCAINST;
  v->valueType = translateType(m, ai->getType());
  if(ai->hasName())
    v->name = strdup(ai->getNameStr().c_str());

  CInstructionInfo *ii = (CInstructionInfo*)calloc(1, sizeof(CInstructionInfo));
  v->data = (void*)ii;

  ii->numOperands = ai->getNumOperands();
  ii->operands = (CValue**)calloc(ii->numOperands, sizeof(CValue*));
  for(int i = 0; i < ii->numOperands; ++i) {
    ii->operands[i] = translateValue(m, ai->getOperand(i));
  }

  ii->align = ai->getAlignment();
}

static void buildLoadInst(CModule *m, CValue *v, const LoadInst *li) {
  v->valueTag = VAL_LOADINST;
  v->valueType = translateType(m, li->getType());
  if(li->hasName())
    v->name = strdup(li->getNameStr().c_str());

  CInstructionInfo *ii = (CInstructionInfo*)calloc(1, sizeof(CInstructionInfo));
  v->data = (void*)ii;

  ii->numOperands = li->getNumOperands();
  ii->operands = (CValue**)calloc(ii->numOperands, sizeof(CValue*));
  for(int i = 0; i < ii->numOperands; ++i) {
    ii->operands[i] = translateValue(m, li->getOperand(i));
  }

  ii->isVolatile = li->isVolatile();
  ii->align = li->getAlignment();
  ii->addrSpace = li->getPointerAddressSpace();
}

static void buildStoreInst(CModule *m, CValue *v, const StoreInst *si) {
  v->valueTag = VAL_STOREINST;
  v->valueType = translateType(m, si->getType());
  if(si->hasName())
    v->name = strdup(si->getNameStr().c_str());

  CInstructionInfo *ii = (CInstructionInfo*)calloc(1, sizeof(CInstructionInfo));
  v->data = (void*)ii;


  ii->numOperands = si->getNumOperands();
  ii->operands = (CValue**)calloc(ii->numOperands, sizeof(CValue*));
  for(int i = 0; i < ii->numOperands; ++i) {
    ii->operands[i] = translateValue(m, si->getOperand(i));
  }

  ii->addrSpace = si->getPointerAddressSpace();
  ii->align = si->getAlignment();
  ii->isVolatile = si->isVolatile();
}

static void buildGEPInst(CModule *m, CValue *v, const GetElementPtrInst *gi) {
  v->valueTag = VAL_GETELEMENTPTRINST;
  v->valueType = translateType(m, gi->getType());
  if(gi->hasName())
    v->name = strdup(gi->getNameStr().c_str());

  CInstructionInfo *ii = (CInstructionInfo*)calloc(1, sizeof(CInstructionInfo));
  v->data = (void*)ii;


  ii->numOperands = gi->getNumOperands();
  ii->operands = (CValue**)calloc(ii->numOperands, sizeof(CValue*));
  for(int i = 0; i < ii->numOperands; ++i) {
    ii->operands[i] = translateValue(m, gi->getOperand(i));
  }

  ii->inBounds = gi->isInBounds();
  ii->addrSpace = gi->getPointerAddressSpace();
}

static void buildCastInst(CModule *m, CValue *v, ValueTag t, const Instruction *inst) {
  const CastInst *ci = dynamic_cast<const CastInst*>(inst);
  v->valueTag = t;
  v->valueType = translateType(m, ci->getType());
  if(ci->hasName())
    v->name = strdup(ci->getNameStr().c_str());

  CInstructionInfo *ii = (CInstructionInfo*)calloc(1, sizeof(CInstructionInfo));
  v->data = (void*)ii;


  ii->numOperands = ci->getNumOperands();
  ii->operands = (CValue**)calloc(ii->numOperands, sizeof(CValue*));
  for(int i = 0; i < ii->numOperands; ++i) {
    ii->operands[i] = translateValue(m, ci->getOperand(i));
  }
}

static void buildCmpInst(CModule *m, CValue *v, ValueTag t, const Instruction *inst) {
  const CmpInst *ci = dynamic_cast<const CmpInst*>(inst);
  v->valueTag = t;
  v->valueType = translateType(m, ci->getType());
  if(ci->hasName())
    v->name = strdup(ci->getNameStr().c_str());

  CInstructionInfo *ii = (CInstructionInfo*)calloc(1, sizeof(CInstructionInfo));
  v->data = (void*)ii;

  ii->numOperands = ci->getNumOperands();
  ii->operands = (CValue**)calloc(ii->numOperands, sizeof(CValue*));
  for(int i = 0; i < ii->numOperands; ++i) {
    ii->operands[i] = translateValue(m, ci->getOperand(i));
  }

  ii->cmpPred = decodePredicate(ci->getPredicate());
}

static void buildPHINode(CModule *m, CValue *v, const PHINode* n) {
  v->valueTag = VAL_PHINODE;
  v->valueType = translateType(m, n->getType());
  if(n->hasName())
    v->name = strdup(n->getNameStr().c_str());

  CPHIInfo *pi = (CPHIInfo*)calloc(1, sizeof(CPHIInfo));
  v->data = (void*)pi;

  pi->numIncomingValues = n->getNumIncomingValues();
  pi->incomingValues = (CValue**)calloc(pi->numIncomingValues, sizeof(CValue*));
  pi->valueBlocks = (CValue**)calloc(pi->numIncomingValues, sizeof(CValue*));

  for(int i = 0; i < pi->numIncomingValues; ++i) {
    pi->incomingValues[i] = translateValue(m, n->getIncomingValue(i));
    pi->valueBlocks[i] = translateValue(m, n->getIncomingBlock(i));
  }
}

static void buildVAArgInst(CModule *m, CValue *v, const VAArgInst *vi) {
  v->valueTag = VAL_VAARGINST;
  v->valueType = translateType(m, vi->getType());
  if(vi->hasName())
    v->name = strdup(vi->getNameStr().c_str());

  CInstructionInfo *ii = (CInstructionInfo*)calloc(1, sizeof(CInstructionInfo));
  v->data = (void*)ii;

  ii->numOperands = vi->getNumOperands();
  ii->operands = (CValue**)calloc(ii->numOperands, sizeof(CValue*));
  for(int i = 0; i < ii->numOperands; ++i) {
    ii->operands[i] = translateValue(m, vi->getOperand(i));
  }
}

static void buildExtractValueInst(CModule *m, CValue *v, const ExtractValueInst *ei) {
  v->valueTag = VAL_EXTRACTVALUEINST;
  v->valueType = translateType(m, ei->getType());
  if(ei->hasName())
    v->name = strdup(ei->getNameStr().c_str());

  CInstructionInfo *ii = (CInstructionInfo*)calloc(1, sizeof(CInstructionInfo));
  v->data = (void*)ii;


  ii->numOperands = ei->getNumOperands();
  ii->operands = (CValue**)calloc(ii->numOperands, sizeof(CValue*));
  for(int i = 0; i < ii->numOperands; ++i) {
    ii->operands[i] = translateValue(m, ei->getOperand(i));
  }

  ii->numIndices = ei->getNumIndices();
  ii->indices = (int*)calloc(ii->numIndices, sizeof(int));

  std::copy(ei->idx_begin(), ei->idx_end(), ii->indices);
}

static void buildInsertValueInst(CModule *m, CValue *v, const InsertValueInst *ei) {
  v->valueTag = VAL_INSERTVALUEINST;
  v->valueType = translateType(m, ei->getType());
  if(ei->hasName())
    v->name = strdup(ei->getNameStr().c_str());

  CInstructionInfo *ii = (CInstructionInfo*)calloc(1, sizeof(CInstructionInfo));
  v->data = (void*)ii;

  ii->numOperands = ei->getNumOperands();
  ii->operands = (CValue**)calloc(ii->numOperands, sizeof(CValue*));
  for(int i = 0; i < ii->numOperands; ++i) {
    ii->operands[i] = translateValue(m, ei->getOperand(i));
  }

  ii->numIndices = ei->getNumIndices();
  ii->indices = (int*)calloc(ii->numIndices, sizeof(int));

  std::copy(ei->idx_begin(), ei->idx_end(), ii->indices);
}

static CValue* translateInstruction(CModule *m, const Instruction *i) {
  PrivateData *pd = (PrivateData*)m->privateData;
  unordered_map<const Value*, CValue*>::iterator it = pd->valueMap.find(i);
  if(it != pd->valueMap.end())
    return it->second;

  CValue *v = (CValue*)calloc(1, sizeof(CValue));
  pd->valueMap[i] = v;

  SmallVectorImpl<std::pair<unsigned, MDNode*> > md(0);
  if(pd->includeLocs) {
    i->getAllMetadata(md);
  }
  else {
    i->getAllMetadataOtherThanDebugLoc(md);
  }

  v->numMetadata = md.size();
  v->md = (CMeta**)calloc(v->numMetadata, sizeof(CMeta*));
  for(int ix = 0; ix < v->numMetadata; ++ix) {
    v->md[ix] = translateMetadata(m, md[ix].second);
  }

  switch(i->getOpcode()) {
    // Terminator instructions
  case Instruction::Ret:
    buildRetInst(m, v, dynamic_cast<const ReturnInst*>(i));
    break;
  case Instruction::Br:
    buildSimpleInst(m, v, VAL_BRANCHINST, i);
    break;
  case Instruction::Switch:
    buildSimpleInst(m, v, VAL_SWITCHINST, i);
    break;
  case Instruction::IndirectBr:
    buildSimpleInst(m, v, VAL_INDIRECTBRINST, i);
    break;
  case Instruction::Invoke:
    buildInvokeInst(m, v, dynamic_cast<const InvokeInst*>(i));
    break;
  case Instruction::Unwind:
    buildSimpleInst(m, v, VAL_UNWINDINST, i);
    break;
  case Instruction::Unreachable:
    buildSimpleInst(m, v, VAL_UNREACHABLEINST, i);
    break;

    // Binary instructions
  case Instruction::Add:
    buildBinaryInst(m, v, VAL_ADDINST, i);
    break;
  case Instruction::FAdd:
    buildBinaryInst(m, v, VAL_FADDINST, i);
    break;
  case Instruction::Sub:
    buildBinaryInst(m, v, VAL_SUBINST, i);
    break;
  case Instruction::FSub:
    buildBinaryInst(m, v, VAL_FSUBINST, i);
    break;
  case Instruction::Mul:
    buildBinaryInst(m, v, VAL_MULINST, i);
    break;
  case Instruction::FMul:
    buildBinaryInst(m, v, VAL_FMULINST, i);
    break;
  case Instruction::UDiv:
    buildBinaryInst(m, v, VAL_UDIVINST, i);
    break;
  case Instruction::SDiv:
    buildBinaryInst(m, v, VAL_SDIVINST, i);
    break;
  case Instruction::FDiv:
    buildBinaryInst(m, v, VAL_FDIVINST, i);
    break;
  case Instruction::URem:
    buildBinaryInst(m, v, VAL_UREMINST, i);
    break;
  case Instruction::SRem:
    buildBinaryInst(m, v, VAL_SREMINST, i);
    break;
  case Instruction::FRem:
    buildBinaryInst(m, v, VAL_FREMINST, i);
    break;
  case Instruction::Shl:
    buildBinaryInst(m, v, VAL_SHLINST, i);
    break;
  case Instruction::LShr:
    buildBinaryInst(m, v, VAL_LSHRINST, i);
    break;
  case Instruction::AShr:
    buildBinaryInst(m, v, VAL_ASHRINST, i);
    break;
  case Instruction::And:
    buildBinaryInst(m, v, VAL_ANDINST, i);
    break;
  case Instruction::Or:
    buildBinaryInst(m, v, VAL_ORINST, i);
    break;
  case Instruction::Xor:
    buildBinaryInst(m, v, VAL_XORINST, i);
    break;

    // Memory operations
  case Instruction::Alloca:
    buildAllocaInst(m, v, dynamic_cast<const AllocaInst*>(i));
    break;

  case Instruction::Load:
    buildLoadInst(m, v, dynamic_cast<const LoadInst*>(i));
    break;

  case Instruction::Store:
    buildStoreInst(m, v, dynamic_cast<const StoreInst*>(i));
    break;

  case Instruction::GetElementPtr:
    buildGEPInst(m, v, dynamic_cast<const GetElementPtrInst*>(i));
    break;

    // Casts
  case Instruction::Trunc:
    buildCastInst(m, v, VAL_TRUNCINST, i);
    break;

  case Instruction::ZExt:
    buildCastInst(m, v, VAL_ZEXTINST, i);
    break;

  case Instruction::SExt:
    buildCastInst(m, v, VAL_SEXTINST, i);
    break;

  case Instruction::FPToUI:
    buildCastInst(m, v, VAL_FPTOUIINST, i);
    break;

  case Instruction::FPToSI:
    buildCastInst(m, v, VAL_FPTOSIINST, i);
    break;

  case Instruction::UIToFP:
    buildCastInst(m, v, VAL_UITOFPINST, i);
    break;

  case Instruction::SIToFP:
    buildCastInst(m, v, VAL_SITOFPINST, i);
    break;

  case Instruction::FPTrunc:
    buildCastInst(m, v, VAL_FPTRUNCINST, i);
    break;

  case Instruction::FPExt:
    buildCastInst(m, v, VAL_FPEXTINST, i);
    break;

  case Instruction::PtrToInt:
    buildCastInst(m, v, VAL_PTRTOINTINST, i);
    break;

  case Instruction::IntToPtr:
    buildCastInst(m, v, VAL_INTTOPTRINST, i);
    break;

  case Instruction::BitCast:
    buildCastInst(m, v, VAL_BITCASTINST, i);
    break;

    // Other instructions
  case Instruction::ICmp:
    buildCmpInst(m, v, VAL_ICMPINST, i);
    break;

  case Instruction::FCmp:
    buildCmpInst(m, v, VAL_FCMPINST, i);
    break;

  case Instruction::PHI:
    buildPHINode(m, v, dynamic_cast<const PHINode*>(i));
    break;

  case Instruction::Call:
    // If this is a call to llvm.dbg.*, return NULL and delete what we
    // made so far.  We don't want these in the instruction stream
    // (the builder will attach the debug information to the relevant
    // entities)
    if(!buildCallInst(m, v, dynamic_cast<const CallInst*>(i))) {
      pd->valueMap.erase(i);
      free(v);
      return NULL;
    }
    break;

  case Instruction::Select:
    buildSimpleInst(m, v, VAL_SELECTINST, i);
    break;

  case Instruction::VAArg:
    buildVAArgInst(m, v, dynamic_cast<const VAArgInst*>(i));
    break;

  case Instruction::ExtractElement:
    buildSimpleInst(m, v, VAL_EXTRACTELEMENTINST, i);
    break;

  case Instruction::InsertElement:
    buildSimpleInst(m, v, VAL_INSERTELEMENTINST, i);
    break;

  case Instruction::ShuffleVector:
    buildSimpleInst(m, v, VAL_SHUFFLEVECTORINST, i);
    break;

  case Instruction::ExtractValue:
    buildExtractValueInst(m, v, dynamic_cast<const ExtractValueInst*>(i));
    break;

  case Instruction::InsertValue:
    buildInsertValueInst(m, v, dynamic_cast<const InsertValueInst*>(i));
    break;

  default:
  {
    ostringstream os;
    os << "Unhandled instruction type: " << i->getOpcode();
    throw os.str();
  }
  }

  return v;
}

static CValue* translateBasicBlock(CModule *m, const BasicBlock *bb) {
  PrivateData *pd = (PrivateData*)m->privateData;
  unordered_map<const Value*, CValue*>::iterator it = pd->valueMap.find(bb);
  if(it != pd->valueMap.end())
    return it->second;

  CValue *v = (CValue*)calloc(1, sizeof(CValue));
  pd->valueMap[bb] = v;

  v->valueTag = VAL_BASICBLOCK;
  v->valueType = translateType(m, bb->getType());
  v->name = strdup(bb->getNameStr().c_str());

  // No metadata for these

  CBasicBlockInfo* bbi = (CBasicBlockInfo*)calloc(1, sizeof(CBasicBlockInfo));
  v->data = (void*)bbi;

  bbi->blockLen = bb->size();
  bbi->instructions = (CValue**)calloc(bbi->blockLen, sizeof(CValue*));

  int idx = 0;
  for(BasicBlock::const_iterator it = bb->begin(),
        ed = bb->end(); it != ed; ++it)
  {
    CValue *tr = translateInstruction(m, &*it);
    if(!tr) {
      // This was a metadata call, so do not insert it into the
      // instruction stream
      --bbi->blockLen;
      continue;
    }

    bbi->instructions[idx++] = tr;
  }

  return v;
}

static CValue* translateFunction(CModule *m, const Function *f) {
  PrivateData *pd = (PrivateData*)m->privateData;
  unordered_map<const Value*, CValue*>::iterator it = pd->valueMap.find(f);
  if(it != pd->valueMap.end())
    return it->second;

  CValue *v = (CValue*)calloc(1, sizeof(CValue));
  pd->valueMap[f] = v;

  v->valueTag = VAL_FUNCTION;
  v->valueType = translateType(m, f->getFunctionType());
  v->name = strdup(f->getNameStr().c_str());

  CFunctionInfo *fi = (CFunctionInfo*)calloc(1, sizeof(CFunctionInfo));
  v->data = (void*)fi;

  if(f->hasSection())
    fi->section = strdup(f->getSection().c_str());

  fi->visibility = decodeVisibility(f);
  fi->linkage = decodeLinkage(f);
  fi->isExternal = f->isDeclaration();
  fi->callingConvention = decodeCallingConvention(f->getCallingConv());
  fi->isVarArg = f->isVarArg();
  if(f->hasGC())
    fi->gcName = strdup(f->getGC());

  fi->argListLen = f->arg_size();
  fi->arguments = (CValue**)calloc(fi->argListLen, sizeof(CValue*));
  int idx = 0;
  for(Function::const_arg_iterator it = f->arg_begin(),
        ed = f->arg_end(); it != ed; ++it)
  {
    fi->arguments[idx++] = translateArgument(m, &*it);
  }

  fi->blockListLen = f->size();
  fi->body = (CValue**)calloc(fi->blockListLen, sizeof(CValue*));
  idx = 0;
  for(Function::const_iterator it = f->begin(),
        ed = f->end(); it != ed; ++it)
  {
    fi->body[idx++] = translateBasicBlock(m, &*it);
  }

  return v;
}

static CValue* translateGlobalVariable(CModule *m, const GlobalVariable *gv) {
  PrivateData *pd = (PrivateData*)m->privateData;
  unordered_map<const Value*, CValue*>::iterator it = pd->valueMap.find(gv);
  if(it != pd->valueMap.end())
    return it->second;

  CValue *v = (CValue*)calloc(1, sizeof(CValue));
  pd->valueMap[gv] = v;

  v->valueTag = VAL_GLOBALVARIABLE;
  v->valueType = translateType(m, gv->getType());
  if(gv->hasName())
    v->name = strdup(gv->getNameStr().c_str());

  CGlobalInfo *gi = (CGlobalInfo*)calloc(1, sizeof(CGlobalInfo));
  v->data = (void*)gi;

  if(gv->hasSection())
    gi->section = strdup(gv->getSection().c_str());

  gi->visibility = decodeVisibility(gv);
  gi->linkage = decodeLinkage(gv);
  gi->isExternal = gv->isDeclaration();
  gi->isThreadLocal = gv->isThreadLocal();
  gi->isConstant = gv->isConstant();

  if(gv->hasInitializer())
    gi->initializer = translateConstant(m, gv->getInitializer());

  return v;
}

static CValue* translateInlineAsm(CModule *m, const InlineAsm* a) {
  PrivateData *pd = (PrivateData*)m->privateData;
  CValue *v = (CValue*)calloc(1, sizeof(CValue));
  pd->valueMap[a] = v;

  v->valueTag = VAL_INLINEASM;
  v->valueType = translateType(m, a->getType());
  if(a->hasName())
    v->name = strdup(a->getNameStr().c_str());

  CInlineAsmInfo *ii = (CInlineAsmInfo*)calloc(1, sizeof(CInlineAsmInfo));
  v->data = (void*)ii;

  ii->asmString = strdup(a->getAsmString().c_str());
  ii->constraintString = strdup(a->getConstraintString().c_str());

  return v;
}

static CValue* translateGlobalValue(CModule *m, const GlobalValue *gv) {
  if(const Function *f = dynamic_cast<const Function*>(gv)) {
    return translateFunction(m, f);
  }

  if(const GlobalVariable *v = dynamic_cast<const GlobalVariable*>(gv)) {
    return translateGlobalVariable(m, v);
  }

  if(const GlobalAlias *a = dynamic_cast<const GlobalAlias*>(gv)) {
    return translateGlobalAlias(m, a);
  }

  string msg;
  raw_string_ostream os(msg);
  os << "Non-global constant: ";
  gv->print(os);
  throw os.str();
}

static CValue* translateEmptyConstant(CModule *m, ValueTag t, const Constant *p) {
  PrivateData *pd = (PrivateData*)m->privateData;
  CValue *v = (CValue*)calloc(1, sizeof(CValue));
  pd->valueMap[p] = v;

  v->valueTag = t;
  v->valueType = translateType(m, p->getType());

  // No data or metadata

  return v;
}

static CValue* translateConstantInt(CModule *m, const ConstantInt* i) {
  PrivateData *pd = (PrivateData*)m->privateData;
  CValue *v = (CValue*)calloc(1, sizeof(CValue));
  pd->valueMap[i] = v;

  v->valueTag = VAL_CONSTANTINT;
  v->valueType = translateType(m, i->getType());

  // No name

  CConstInt *d = (CConstInt*)calloc(1, sizeof(CConstInt));
  v->data = (void*)d;

  d->val = i->getSExtValue();

  return v;
}

static CValue* translateConstantFP(CModule *m, const ConstantFP *fp) {
  PrivateData *pd = (PrivateData*)m->privateData;
  CValue *v = (CValue*)calloc(1, sizeof(CValue));
  pd->valueMap[fp] = v;

  v->valueTag = VAL_CONSTANTFP;
  v->valueType = translateType(m, fp->getType());

  // No name

  CConstFP *d = (CConstFP*)calloc(1, sizeof(CConstFP));
  v->data = (void*)d;

  d->val = fp->getValueAPF().convertToDouble();

  return v;
}

static CValue* translateBlockAddress(CModule *m, const BlockAddress *ba) {
  PrivateData *pd = (PrivateData*)m->privateData;
  CValue *v = (CValue*)calloc(1, sizeof(CValue));
  pd->valueMap[ba] = v;

  v->valueTag = VAL_BLOCKADDRESS;
  v->valueType = translateType(m, ba->getType());
  if(ba->hasName())
    v->name = strdup(ba->getNameStr().c_str());

  CBlockAddrInfo *i = (CBlockAddrInfo*)calloc(1, sizeof(CBlockAddrInfo));
  v->data = (void*)i;

  i->func = translateValue(m, ba->getFunction());
  i->block = translateValue(m, ba->getBasicBlock());

  return v;
}

static CValue* translateConstantAggregate(CModule *m, ValueTag t, const Constant *ca) {
  PrivateData *pd = (PrivateData*)m->privateData;
  CValue *v = (CValue*)calloc(1, sizeof(CValue));
  pd->valueMap[ca] = v;

  v->valueTag = t;
  v->valueType = translateType(m, ca->getType());
  if(ca->hasName())
    v->name = strdup(ca->getNameStr().c_str());

  CConstAggregate *a = (CConstAggregate*)calloc(1, sizeof(CConstAggregate));
  v->data = (void*)a;

  a->numElements = ca->getNumOperands();
  a->constants = (CValue**)calloc(a->numElements, sizeof(CValue*));

  int idx = 0;
  for(User::const_op_iterator it = ca->op_begin(),
        ed = ca->op_end(); it != ed; ++it)
  {
    a->constants[idx++] = translateValue(m, it->get());
  }

  return v;
}

static CValue* translateConstantExpr(CModule *m, const ConstantExpr *ce) {
  PrivateData *pd = (PrivateData*)m->privateData;
  CValue *v = (CValue*)calloc(1, sizeof(CValue));
  pd->valueMap[ce] = v;

  v->valueTag = VAL_CONSTANTEXPR;
  v->valueType = translateType(m, ce->getType());
  if(ce->hasName())
    v->name = strdup(ce->getNameStr().c_str());

  CConstExprInfo *ci = (CConstExprInfo*)calloc(1, sizeof(CConstExprInfo));
  CInstructionInfo *ii = (CInstructionInfo*)calloc(1, sizeof(CInstructionInfo));
  v->data = (void*)ci;
  ci->ii = ii;

  ci->instrType = decodeOpcode(ce->getOpcode());
  ii->numOperands = ce->getNumOperands();
  ii->operands = (CValue**)calloc(ii->numOperands, sizeof(CValue*));

  int idx = 0;
  for(User::const_op_iterator it = ce->op_begin(),
        ed = ce->op_end(); it != ed; ++it)
  {
    ii->operands[idx++] = translateValue(m, it->get());
  }

  if(ce->isCompare()) {
    ii->cmpPred = decodePredicate((CmpInst::Predicate)ce->getPredicate());
  }
  else if(ce->hasIndices()) {
    ii->numIndices = ce->getIndices().size();
    ii->indices = (int*)calloc(ii->numIndices, sizeof(int));
    std::copy(ce->getIndices().begin(), ce->getIndices().end(), ii->indices);
  }

  return v;
}

static CValue* translateConstant(CModule *m, const Constant *c) {
  PrivateData *pd = (PrivateData*)m->privateData;
  unordered_map<const Value*, CValue*>::iterator it = pd->valueMap.find(c);
  if(it != pd->valueMap.end())
    return it->second;

  // Order these in order of frequency
  if(const ConstantInt *ci = dynamic_cast<const ConstantInt*>(c)) {
    return translateConstantInt(m, ci);
  }

  if(const ConstantPointerNull *pn = dynamic_cast<const ConstantPointerNull*>(c)) {
    return translateEmptyConstant(m, VAL_CONSTANTPOINTERNULL, pn);
  }

  if(const ConstantExpr *ce = dynamic_cast<const ConstantExpr*>(c)) {
    return translateConstantExpr(m, ce);
  }

  if(const GlobalValue *gv = dynamic_cast<const GlobalValue*>(c)) {
    return translateGlobalValue(m, gv);
  }

  if(const ConstantArray *ca = dynamic_cast<const ConstantArray*>(c)) {
    return translateConstantAggregate(m, VAL_CONSTANTARRAY, ca);
  }

  if(const ConstantVector *cv = dynamic_cast<const ConstantVector*>(c)) {
    return translateConstantAggregate(m, VAL_CONSTANTVECTOR, cv);
  }

  if(const ConstantStruct *cs = dynamic_cast<const ConstantStruct*>(c)) {
    return translateConstantAggregate(m, VAL_CONSTANTSTRUCT, cs);
  }

  if(const ConstantFP *fp = dynamic_cast<const ConstantFP*>(c)) {
    return translateConstantFP(m, fp);
  }

  if(const ConstantAggregateZero *az = dynamic_cast<const ConstantAggregateZero*>(c)) {
    return translateEmptyConstant(m, VAL_CONSTANTAGGREGATEZERO, az);
  }

  if(const UndefValue *uv = dynamic_cast<const UndefValue*>(c)) {
    return translateEmptyConstant(m, VAL_UNDEFVALUE, uv);
  }

  if(const BlockAddress *ba = dynamic_cast<const BlockAddress*>(c)) {
    return translateBlockAddress(m, ba);
  }


  string msg;
  raw_string_ostream os(msg);
  os << "Unhandled constant type: ";
  c->print(os);
  throw os.str();
}

static CValue* translateValue(CModule *m, const Value *v) {
  PrivateData *pd = (PrivateData*)m->privateData;
  unordered_map<const Value*, CValue*>::iterator it = pd->valueMap.find(v);
  if(it != pd->valueMap.end())
    return it->second;

  // This order is pretty reasonable since constants will be the most
  // frequent un-cached values.
  if(const Constant *c = dynamic_cast<const Constant*>(v)) {
    return translateConstant(m, c);
  }

  if(const Instruction *i = dynamic_cast<const Instruction*>(v)) {
    return translateInstruction(m, i);
  }

  if(const BasicBlock *bb = dynamic_cast<const BasicBlock*>(v)) {
    return translateBasicBlock(m, bb);
  }

  if(const InlineAsm *a = dynamic_cast<const InlineAsm*>(v)) {
    return translateInlineAsm(m, a);
  }

  if(dynamic_cast<const Argument*>(v)) {
    string msg = "Un-cached Argument passed to translateValue";
    throw msg;
  }

  string msg;
  raw_string_ostream os(msg);
  os << "Unhandled value type: ";
  v->print(os);
  throw os.str();
}

static void attachFunctionMetadata(CModule *m, Module *M) {
  NamedMDNode *sp = M->getNamedMetadata("llvm.dbg.sp");
  // No debug information
  if(!sp) return;

  for(unsigned int i = 0; i < sp->getNumOperands(); ++i) {
    CMeta *md = translateMetadata(m, sp->getOperand(i));
    if(md->metaTag != META_SUBPROGRAM) {
      throw "Non-subprogram in llvm.dbg.sp";
    }

    // If there isn't a function listed to attach this to (why?) we
    // can't do much.
    if(md->u.metaSubprogramInfo.function == NULL)
      continue;

    int currentCapacity = md->u.metaSubprogramInfo.function->metaCapacity;
    int metaCount = md->u.metaSubprogramInfo.function->numMetadata;
    if(metaCount == 0) {
      currentCapacity = 10;
      md->u.metaSubprogramInfo.function->metaCapacity = currentCapacity;
      md->u.metaSubprogramInfo.function->md =
        (CMeta**)calloc(currentCapacity, sizeof(CMeta*));
    }
    else if(metaCount == currentCapacity - 1) {
      int newCapacity = 2 * currentCapacity;
      CMeta **newMeta = (CMeta**)calloc(newCapacity, sizeof(CMeta*));
      memcpy(newMeta, md->u.metaSubprogramInfo.function->md, currentCapacity);
      md->u.metaSubprogramInfo.function->metaCapacity = newCapacity;
    }

    md->u.metaSubprogramInfo.function->md[metaCount] = md;
    md->u.metaSubprogramInfo.function->numMetadata++;
  }
}

static void attachGlobalMetadata(CModule *m, Module *M) {
  NamedMDNode *gv = M->getNamedMetadata("llvm.dbg.gv");
  // No debug information
  if(!gv) return;

  for(unsigned int i = 0; i < gv->getNumOperands(); ++i) {
    CMeta *md = translateMetadata(m, gv->getOperand(i));
    if(md->metaTag != META_GLOBALVARIABLE) {
      throw "Non-global in llvm.dbg.gv";
    }

    if(md->u.metaGlobalInfo.global == NULL)
      continue;

    int currentCapacity = md->u.metaGlobalInfo.global->metaCapacity;
    int metaCount = md->u.metaGlobalInfo.global->numMetadata;
    if(metaCount == 0) {
      currentCapacity = 10;
      md->u.metaGlobalInfo.global->metaCapacity = currentCapacity;
      md->u.metaGlobalInfo.global->md = (CMeta**)calloc(currentCapacity, sizeof(CMeta*));
    }
    else if(metaCount == currentCapacity - 1) {
      int newCapacity = 2 * currentCapacity;
      CMeta **newMeta = (CMeta**)calloc(newCapacity, sizeof(CMeta*));
      memcpy(newMeta, md->u.metaGlobalInfo.global->md, currentCapacity);
      md->u.metaGlobalInfo.global->metaCapacity = newCapacity;
    }

    md->u.metaGlobalInfo.global->md[metaCount] = md;
    md->u.metaGlobalInfo.global->numMetadata++;
  }
}

extern "C" {

  /*!
    Free all of the resources allocated by the exposed module,
    including the underlying C++ LLVM Module.
   */
  void disposeCModule(CModule *m) {
    free(m->errMsg);
    free(m->moduleIdentifier);
    free(m->moduleDataLayout);
    free(m->targetTriple);
    free(m->moduleInlineAsm);

    // The actual variables are deleted with disposeCValue from the
    // valueMap.
    free(m->globalVariables);
    free(m->globalAliases);
    free(m->functions);

    PrivateData *pd = (PrivateData*)m->privateData;

    for(unordered_map<const Type*,CType*>::iterator it = pd->typeMap.begin(),
          ed = pd->typeMap.end(); it != ed; ++it)
    {
      disposeCType(it->second);
    }

    for(unordered_map<const Value*,CValue*>::iterator it = pd->valueMap.begin(),
          ed = pd->valueMap.end(); it != ed; ++it)
    {
      disposeCValue(it->second);
    }

    for(unordered_map<const MDNode*,CMeta*>::iterator it = pd->metaMap.begin(),
          ed = pd->metaMap.end(); it != ed; ++it)
    {
      disposeCMeta(it->second);
    }

    // These two are actually allocated with new
    delete pd->original;
    delete pd;

    free(m);
  }

  CModule* marshalLLVM(const char * filename, int includeLocs) {
    CModule *ret = (CModule*)calloc(1, sizeof(CModule));
    PrivateData *pd = new PrivateData;
    ret->privateData = (void*)pd;

    std::string errMsg;
    error_code ec = MemoryBuffer::getFile(filename, pd->buffer);

    if(pd->buffer.get() == NULL){
      ret->hasError = 1;
      ret->errMsg = strdup(ec.message().c_str());
      return ret;
    }

    Module *m = ParseBitcodeFile(pd->buffer.get(), pd->ctxt, &errMsg);

    if(m == NULL) {
      ret->hasError = 1;
      ret->errMsg = strdup(errMsg.c_str());
      return ret;
    }

    pd->original = m;
    pd->includeLocs = includeLocs;

    ret->moduleIdentifier = strdup(m->getModuleIdentifier().c_str());
    ret->moduleDataLayout = strdup(m->getDataLayout().c_str());
    ret->targetTriple = strdup(m->getTargetTriple().c_str());
    ret->moduleInlineAsm = strdup(m->getModuleInlineAsm().c_str());


    try
    {
      std::vector<CValue*> globalVariables;
      for(Module::const_global_iterator it = m->global_begin(),
            ed = m->global_end(); it != ed; ++it)
      {
        const GlobalVariable *globalVar = dynamic_cast<const GlobalVariable*>(&*it);
        if(!globalVar) throw "Not a global";

        CValue *gv = translateGlobalVariable(ret, globalVar);
        globalVariables.push_back(gv);
      }

      ret->numGlobalVariables = globalVariables.size();
      ret->globalVariables = (CValue**)calloc(ret->numGlobalVariables, sizeof(CValue*));
      std::copy(globalVariables.begin(), globalVariables.end(), ret->globalVariables);

      std::vector<CValue*> functions;
      for(Module::const_iterator it = m->begin(),
            ed = m->end(); it != ed; ++it)
      {
        const Function *func = dynamic_cast<const Function*>(&*it);
        if(!func) throw "Not a function";

        CValue *f = translateFunction(ret, func);
        functions.push_back(f);
      }

      ret->numFunctions = functions.size();
      ret->functions = (CValue**)calloc(ret->numFunctions, sizeof(CValue*));
      std::copy(functions.begin(), functions.end(), ret->functions);

      std::vector<CValue*> globalAliases;
      for(Module::const_alias_iterator it = m->alias_begin(),
            ed = m->alias_end(); it != ed; ++it)
      {
        const GlobalAlias *globalAlias = dynamic_cast<const GlobalAlias*>(&*it);
        if(!globalAlias) throw "Not a global alias";

        CValue *ga = translateGlobalAlias(ret, globalAlias);
        globalAliases.push_back(ga);
      }

      ret->numGlobalAliases = globalAliases.size();
      ret->globalAliases = (CValue**)calloc(ret->numGlobalAliases, sizeof(CValue*));
      std::copy(globalAliases.begin(), globalAliases.end(), ret->globalAliases);

      // Now process the global metadata to attach metadata to global
      // variables and functions.
      attachFunctionMetadata(ret, m);
      attachGlobalMetadata(ret, m);
    }
    catch(const string &msg) {
      ret->hasError = 1;
      ret->errMsg = strdup(msg.c_str());
    }
    catch(const char *msg) {
      ret->hasError = 1;
      ret->errMsg = strdup(msg);
    }
    catch(...) {
      ret->hasError = 1;
      ret->errMsg = strdup("Unknown error");
    }

    return ret;
  }
}
