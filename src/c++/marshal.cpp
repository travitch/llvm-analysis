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
#include <llvm/Instructions.h>
#include <llvm/LLVMContext.h>
#include <llvm/Module.h>
#include <llvm/Type.h>
#include <llvm/DerivedTypes.h>
#include <llvm/ADT/OwningPtr.h>
#include <llvm/Bitcode/ReaderWriter.h>
#include <llvm/Support/MemoryBuffer.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/Support/system_error.h>

using namespace llvm;
using std::ostringstream;
using std::string;
using std::tr1::unordered_map;

struct PrivateData {
  // Foreign callers do not need to access below this point.
  Module* original;

  // This map is actually state only for this translation code.  Since
  // types have pointer equality in LLVM, every type will just be
  // translated once to a heap-allocated CType.  On the Haskell side,
  // each CType needs to be translated once (mapping the address of
  // the CType to the translated version).
  unordered_map<const Type*, CType*> typeMap;
  unordered_map<const Value*, CValue*> valueMap;
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
  if(ct->innerType)
    disposeCType(ct->innerType);
  free(ct->name);

  for(int i = 0; i < ct->typeListLen; ++i)
  {
    disposeCType(ct->typeList[i]);
  }
  delete[] ct->typeList;

  delete ct;
}

// Have to do the delete in this function since the pointer must be
// cast to the correct type.
static void disposeData(ValueTag t, void* data) {
  switch(t) {
  case VAL_ARGUMENT:
  {
    CArgumentInfo *ai = (CArgumentInfo*)data;
    delete ai;
    return;
  }

  case VAL_BASICBLOCK:
  {
    CBasicBlockInfo *bbi = (CBasicBlockInfo*)data;

    // The actual values are deleted from the valueMap
    delete[] bbi->instructions;

    delete bbi;
    return;
  }

  case VAL_INLINEASM:
  {
    CInlineAsmInfo *ii = (CInlineAsmInfo*)data;
    free(ii->asmString);
    free(ii->constraintString);
    delete ii;
  }

  case VAL_ALIAS:
  case VAL_GLOBALVARIABLE:
  {
    CGlobalInfo *gi = (CGlobalInfo*)data;

    free(gi->section);

    delete gi;
    return;
  }

  case VAL_FUNCTION:
  {
    CFunctionInfo *fi = (CFunctionInfo*)data;

    free(fi->section);
    free(fi->gcName);

    delete[] fi->arguments;
    delete[] fi->body;

    delete fi;
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

    delete[] ci->arguments;
    delete ci;
    return;
  }

  case VAL_PHINODE:
  {
    CPHIInfo *pi = (CPHIInfo*)data;
    delete[] pi->incomingValues;
    delete[] pi->valueBlocks;
    delete pi;
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
    delete[] ii->operands;
    delete[] ii->indices;
    delete ii;
    return;
  }

  case VAL_BLOCKADDRESS:
  {
    CBlockAddrInfo *bi = (CBlockAddrInfo*)data;
    delete bi;
    return;
  }

  case VAL_CONSTANTINT:
  {
    CConstInt *d = (CConstInt*)data;
    delete d;
    return;
  }

  case VAL_CONSTANTFP:
  {
    CConstFP *d = (CConstFP*)data;
    delete d;
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
    delete[] c->constants;
    delete c;
    return;
  }

  case VAL_CONSTANTEXPR:
  {
    CConstExprInfo *ce = (CConstExprInfo*)data;
    delete[] ce->ii->operands;
    delete[] ce->ii->indices;
    delete ce->ii;
    delete ce;
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
  delete[] v->md;

  disposeData(v->valueTag, v->data);

  delete v;
}

static CType* translateType(CModule *m, const Type *t) {
  PrivateData *pd = (PrivateData*)m->privateData;
  unordered_map<const Type*,CType*>::const_iterator it = pd->typeMap.find(t);
  if(it != pd->typeMap.end())
    return it->second;

  CType *nt = new CType;
  nt->typeTag = decodeTypeTag(t->getTypeID());


  string typeName = pd->original->getTypeName(t);
  if(typeName != "") {
    CType *namedTypeWrapper = new CType;
    namedTypeWrapper->innerType = nt;
    namedTypeWrapper->name = strdup(typeName.c_str());

    pd->typeMap[t] = namedTypeWrapper;
  }
  else {
    // Need to put this in the table before making any recursive calls,
    // otherwise it might never terminate.
    pd->typeMap[t] = nt;
  }

  switch(t->getTypeID()) {
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
    nt->typeList = new CType*[nt->typeListLen];
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
    nt->typeList = new CType*[nt->typeListLen];
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
  }

  return nt;
}

static CValue* translateConstant(CModule *m, const Constant *c);
static CValue* translateValue(CModule *m, const Value *v);
static CValue* translateBasicBlock(CModule *m, const BasicBlock *bb);

static CValue* translateGlobalAlias(CModule *m, const GlobalAlias *ga) {
  PrivateData *pd = (PrivateData*)m->privateData;
  unordered_map<const Value*, CValue*>::iterator it = pd->valueMap.find(ga);
  if(it != pd->valueMap.end())
    return it->second;

  CValue *v = new CValue;
  pd->valueMap[ga] = v;

  v->valueTag = VAL_ALIAS;
  v->valueType = translateType(m, ga->getType());
  v->name = strdup(ga->getNameStr().c_str());

  // FIXME: Get metadata

  CGlobalInfo *gi = new CGlobalInfo;
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
  CValue *v = new CValue;
  pd->valueMap[a] = v;

  v->valueTag = VAL_ARGUMENT;
  v->valueType = translateType(m, a->getType());
  v->name = strdup(a->getNameStr().c_str());

  // Metadata will be attached as instructions are processed.

  CArgumentInfo *ai = new CArgumentInfo;
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

  CInstructionInfo *ii = new CInstructionInfo;
  v->data = (void*)ii;

  if(Value* rv = ri->getReturnValue())
  {
    ii->numOperands = 1;
    ii->operands = new CValue*[1];
    ii->operands[0] = translateValue(m, rv);
  }

  // Otherwise, the data fields default to 0 as intended
}

static void buildSimpleInst(CModule *m, CValue *v, ValueTag t, const Instruction *inst) {
  v->valueTag = t;
  v->valueType = translateType(m, inst->getType());

  CInstructionInfo *ii = new CInstructionInfo;
  v->data = (void*)ii;

  ii->numOperands = inst->getNumOperands();
  ii->operands = new CValue*[ii->numOperands];

  for(size_t i = 0; i < inst->getNumOperands(); ++i) {
    ii->operands[i] = translateValue(m, inst->getOperand(i));
  }
}

static void buildBinaryInst(CModule *m, CValue *v, ValueTag t, const Instruction *inst) {
  const BinaryOperator *bi = dynamic_cast<const BinaryOperator*>(inst);
  assert(bi);

  v->valueTag = t;
  v->valueType = translateType(m, bi->getType());

  CInstructionInfo *ii = new CInstructionInfo;
  v->data = (void*)ii;

  ii->numOperands = inst->getNumOperands();
  ii->operands = new CValue*[ii->numOperands];
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

  CCallInfo *ci = new CCallInfo;
  v->data = (void*)ci;

  ci->calledValue = translateValue(m, ii->getCalledValue());
  ci->callingConvention = decodeCallingConvention(ii->getCallingConv());
  ci->hasSRet = ii->hasStructRetAttr();
  ci->normalDest = translateBasicBlock(m, ii->getNormalDest());
  ci->unwindDest = translateBasicBlock(m, ii->getUnwindDest());
  ci->argListLen = ii->getNumArgOperands();
  ci->arguments = new CValue*[ci->argListLen];

  for(unsigned i = 0; i < ii->getNumArgOperands(); ++i) {
    ci->arguments[i] = translateValue(m, ii->getArgOperand(i));
  }
}

static void buildCallInst(CModule *m, CValue *v, const CallInst *ii) {
  v->valueTag = VAL_CALLINST;
  v->valueType = translateType(m, ii->getType());
  if(ii->hasName())
    v->name = strdup(ii->getNameStr().c_str());

  CCallInfo *ci = new CCallInfo;
  v->data = (void*)ci;

  ci->calledValue = translateValue(m, ii->getCalledValue());
  ci->callingConvention = decodeCallingConvention(ii->getCallingConv());
  ci->hasSRet = ii->hasStructRetAttr();
  ci->isTail = ii->isTailCall();
  ci->argListLen = ii->getNumArgOperands();
  ci->arguments = new CValue*[ci->argListLen];

  for(unsigned i = 0; i < ii->getNumArgOperands(); ++i) {
    ci->arguments[i] = translateValue(m, ii->getArgOperand(i));
  }
}

static void buildAllocaInst(CModule *m, CValue *v, const AllocaInst *ai) {
  v->valueTag = VAL_ALLOCAINST;
  v->valueType = translateType(m, ai->getType());
  if(ai->hasName())
    v->name = strdup(ai->getNameStr().c_str());

  CInstructionInfo *ii = new CInstructionInfo;
  v->data = (void*)ii;

  ii->numOperands = ai->getNumOperands();
  ii->operands = new CValue*[ii->numOperands];
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

  CInstructionInfo *ii = new CInstructionInfo;
  v->data = (void*)ii;

  ii->numOperands = li->getNumOperands();
  ii->operands = new CValue*[ii->numOperands];
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

  CInstructionInfo *ii = new CInstructionInfo;
  v->data = (void*)ii;


  ii->numOperands = si->getNumOperands();
  ii->operands = new CValue*[ii->numOperands];
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

  CInstructionInfo *ii = new CInstructionInfo;
  v->data = (void*)ii;


  ii->numOperands = gi->getNumOperands();
  ii->operands = new CValue*[ii->numOperands];
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

  CInstructionInfo *ii = new CInstructionInfo;
  v->data = (void*)ii;


  ii->numOperands = ci->getNumOperands();
  ii->operands = new CValue*[ii->numOperands];
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

  CInstructionInfo *ii = new CInstructionInfo;
  v->data = (void*)ii;

  ii->numOperands = ci->getNumOperands();
  ii->operands = new CValue*[ii->numOperands];
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

  CPHIInfo *pi = new CPHIInfo;
  v->data = (void*)pi;

  pi->numIncomingValues = n->getNumIncomingValues();
  pi->incomingValues = new CValue*[pi->numIncomingValues];
  pi->valueBlocks = new CValue*[pi->numIncomingValues];

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

  CInstructionInfo *ii = new CInstructionInfo;
  v->data = (void*)ii;

  ii->numOperands = vi->getNumOperands();
  ii->operands = new CValue*[ii->numOperands];
  for(int i = 0; i < ii->numOperands; ++i) {
    ii->operands[i] = translateValue(m, vi->getOperand(i));
  }
}

static void buildExtractValueInst(CModule *m, CValue *v, const ExtractValueInst *ei) {
  v->valueTag = VAL_EXTRACTVALUEINST;
  v->valueType = translateType(m, ei->getType());
  if(ei->hasName())
    v->name = strdup(ei->getNameStr().c_str());

  CInstructionInfo *ii = new CInstructionInfo;
  v->data = (void*)ii;


  ii->numOperands = ei->getNumOperands();
  ii->operands = new CValue*[ii->numOperands];
  for(int i = 0; i < ii->numOperands; ++i) {
    ii->operands[i] = translateValue(m, ei->getOperand(i));
  }

  ii->numIndices = ei->getNumIndices();
  ii->indices = new int[ii->numIndices];

  std::copy(ei->idx_begin(), ei->idx_end(), ii->indices);
}

static void buildInsertValueInst(CModule *m, CValue *v, const InsertValueInst *ei) {
  v->valueTag = VAL_INSERTVALUEINST;
  v->valueType = translateType(m, ei->getType());
  if(ei->hasName())
    v->name = strdup(ei->getNameStr().c_str());

  CInstructionInfo *ii = new CInstructionInfo;
  v->data = (void*)ii;

  ii->numOperands = ei->getNumOperands();
  ii->operands = new CValue*[ii->numOperands];
  for(int i = 0; i < ii->numOperands; ++i) {
    ii->operands[i] = translateValue(m, ei->getOperand(i));
  }

  ii->numIndices = ei->getNumIndices();
  ii->indices = new int[ii->numIndices];

  std::copy(ei->idx_begin(), ei->idx_end(), ii->indices);
}

static CValue* translateInstruction(CModule *m, const Instruction *i) {
  PrivateData *pd = (PrivateData*)m->privateData;
  unordered_map<const Value*, CValue*>::iterator it = pd->valueMap.find(i);
  if(it != pd->valueMap.end())
    return it->second;

  CValue *v = new CValue;
  pd->valueMap[i] = v;

  // FIXME: Handle metadata here

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
    buildCallInst(m, v, dynamic_cast<const CallInst*>(i));
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

  CValue *v = new CValue;
  pd->valueMap[bb] = v;

  v->valueTag = VAL_BASICBLOCK;
  v->valueType = translateType(m, bb->getType());
  v->name = strdup(bb->getNameStr().c_str());

  // No metadata for these

  CBasicBlockInfo* bbi = new CBasicBlockInfo;
  bbi->blockLen = bb->size();
  bbi->instructions = new CValue*[bbi->blockLen];

  int idx = 0;
  for(BasicBlock::const_iterator it = bb->begin(),
        ed = bb->end(); it != ed; ++it)
  {
    bbi->instructions[idx++] = translateInstruction(m, &*it);
  }

  v->data = (void*)bbi;

  return v;
}

static CValue* translateFunction(CModule *m, const Function *f) {
  PrivateData *pd = (PrivateData*)m->privateData;
  unordered_map<const Value*, CValue*>::iterator it = pd->valueMap.find(f);
  if(it != pd->valueMap.end())
    return it->second;

  CValue *v = new CValue;
  pd->valueMap[f] = v;

  v->valueTag = VAL_FUNCTION;
  v->valueType = translateType(m, f->getFunctionType());
  v->name = strdup(f->getNameStr().c_str());

  // FIXME: Get metadata from module

  CFunctionInfo *fi = new CFunctionInfo;
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
  fi->arguments = new CValue*[f->arg_size()];
  int idx = 0;
  for(Function::const_arg_iterator it = f->arg_begin(),
        ed = f->arg_end(); it != ed; ++it)
  {
    fi->arguments[idx++] = translateArgument(m, &*it);
  }

  fi->blockListLen = f->size();
  fi->body = new CValue*[f->size()];
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

  CValue *v = new CValue;
  pd->valueMap[gv] = v;

  v->valueTag = VAL_GLOBALVARIABLE;
  v->valueType = translateType(m, gv->getType());
  if(gv->hasName())
    v->name = strdup(gv->getNameStr().c_str());

  // FIXME: Query the Module for metadata here...


  CGlobalInfo *gi = new CGlobalInfo;
  if(gv->hasSection())
    gi->section = strdup(gv->getSection().c_str());

  gi->visibility = decodeVisibility(gv);
  gi->linkage = decodeLinkage(gv);
  gi->isExternal = gv->isDeclaration();
  gi->isThreadLocal = gv->isThreadLocal();
  gi->isConstant = gv->isConstant();

  if(gv->hasInitializer())
    gi->initializer = translateConstant(m, gv->getInitializer());

  v->data = (void*)gi;

  return v;
}

static CValue* translateInlineAsm(CModule *m, const InlineAsm* a) {
  PrivateData *pd = (PrivateData*)m->privateData;
  CValue *v = new CValue;
  pd->valueMap[a] = v;

  v->valueTag = VAL_INLINEASM;
  v->valueType = translateType(m, a->getType());
  if(a->hasName())
    v->name = strdup(a->getNameStr().c_str());

  CInlineAsmInfo *ii = new CInlineAsmInfo;
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
  CValue *v = new CValue;
  pd->valueMap[p] = v;

  v->valueTag = t;
  v->valueType = translateType(m, p->getType());

  // No data or metadata

  return v;
}

static CValue* translateConstantInt(CModule *m, const ConstantInt* i) {
  PrivateData *pd = (PrivateData*)m->privateData;
  CValue *v = new CValue;
  pd->valueMap[i] = v;

  v->valueTag = VAL_CONSTANTINT;
  v->valueType = translateType(m, i->getType());

  // No name

  CConstInt *d = new CConstInt;
  v->data = (void*)d;

  d->val = i->getSExtValue();

  return v;
}

static CValue* translateConstantFP(CModule *m, const ConstantFP *fp) {
  PrivateData *pd = (PrivateData*)m->privateData;
  CValue *v = new CValue;
  pd->valueMap[fp] = v;

  v->valueTag = VAL_CONSTANTFP;
  v->valueType = translateType(m, fp->getType());

  // No name

  CConstFP *d = new CConstFP;
  v->data = (void*)d;

  d->val = fp->getValueAPF().convertToDouble();

  return v;
}

static CValue* translateBlockAddress(CModule *m, const BlockAddress *ba) {
  PrivateData *pd = (PrivateData*)m->privateData;
  CValue *v = new CValue;
  pd->valueMap[ba] = v;

  v->valueTag = VAL_BLOCKADDRESS;
  v->valueType = translateType(m, ba->getType());
  if(ba->hasName())
    v->name = strdup(ba->getNameStr().c_str());

  CBlockAddrInfo *i = new CBlockAddrInfo;
  v->data = (void*)i;

  i->func = translateValue(m, ba->getFunction());
  i->block = translateValue(m, ba->getBasicBlock());

  return v;
}

static CValue* translateConstantAggregate(CModule *m, ValueTag t, const Constant *ca) {
  PrivateData *pd = (PrivateData*)m->privateData;
  CValue *v = new CValue;
  pd->valueMap[ca] = v;

  v->valueTag = t;
  v->valueType = translateType(m, ca->getType());
  if(ca->hasName())
    v->name = strdup(ca->getNameStr().c_str());

  CConstAggregate *a = new CConstAggregate;
  v->data = (void*)a;

  a->numElements = ca->getNumOperands();
  a->constants = new CValue*[a->numElements];

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
  CValue *v = new CValue;
  pd->valueMap[ce] = v;

  v->valueTag = VAL_CONSTANTEXPR;
  v->valueType = translateType(m, ce->getType());
  if(ce->hasName())
    v->name = strdup(ce->getNameStr().c_str());

  CConstExprInfo *ci = new CConstExprInfo;
  CInstructionInfo *ii = new CInstructionInfo;
  v->data = (void*)ci;
  ci->ii = ii;

  ci->instrType = decodeOpcode(ce->getOpcode());
  ii->numOperands = ce->getNumOperands();
  ii->operands = new CValue*[ii->numOperands];

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
    ii->indices = new int[ii->numIndices];
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
    delete[] m->globalVariables;
    delete[] m->globalAliases;
    delete[] m->functions;

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


    delete pd->original;
    delete pd;
    delete m;
  }

  CModule* marshalLLVM(const char * filename) {
    CModule *ret = new CModule;
    std::string errMsg;
    OwningPtr<MemoryBuffer> buffer;
    error_code ec = MemoryBuffer::getFile(filename, buffer);

    if(buffer.get() == NULL){
      ret->hasError = 1;
      ret->errMsg = strdup(ec.message().c_str());
      return ret;
    }

    LLVMContext ctxt;
    Module *m = ParseBitcodeFile(buffer.get(), ctxt, &errMsg);

    if(m == NULL) {
      ret->hasError = 1;
      ret->errMsg = strdup(errMsg.c_str());
      return ret;
    }

    ret->moduleIdentifier = strdup(m->getModuleIdentifier().c_str());
    ret->moduleDataLayout = strdup(m->getDataLayout().c_str());
    ret->targetTriple = strdup(m->getTargetTriple().c_str());
    ret->moduleInlineAsm = strdup(m->getModuleInlineAsm().c_str());

    PrivateData *pd = new PrivateData;
    pd->original = m;
    ret->privateData = (void*)pd;

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
      ret->globalVariables = new CValue*[ret->numGlobalVariables];
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
      ret->functions = new CValue*[ret->numFunctions];
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
      ret->globalAliases = new CValue*[ret->numGlobalAliases];
      std::copy(globalAliases.begin(), globalAliases.end(), ret->globalAliases);
    }
    catch(const string &msg) {
      ret->hasError = 1;
      ret->errMsg = strdup(msg.c_str());
    }
    catch(...) {
      ret->hasError = 1;
      ret->errMsg = strdup("Unknown error");
    }

    return ret;
  }
}
