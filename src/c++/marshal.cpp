#define __STDC_LIMIT_MACROS
#define __STDC_CONSTANT_MACROS

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

#include "marshal.h"

using namespace llvm;
using std::ostringstream;
using std::string;
using std::tr1::unordered_map;

struct CModule {
  char *moduleIdentifier;
  char *moduleDataLayout;
  char *targetTriple;
  int littleEndian;
  int pointerSize;
  char *moduleInlineAsm;

  CValue **globalVariables;
  CValue **globalAliases;
  CValue **functions;

  int isError;
  char *errMsg;

  // Foreign callers do not need to access below this point.
  Module* original;

  // This map is actually state only for this translation code.  Since
  // types have pointer equality in LLVM, every type will just be
  // translated once to a heap-allocated CType.  On the Haskell side,
  // each CType needs to be translated once (mapping the address of
  // the CType to the translated version).
  unordered_map<const Type*, CType*> *typeMap;
  unordered_map<const Value*, CValue*> *valueMap;
};


CmpPredicate decodePredicate(CmpInst::Predicate p) {
  switch(p) {
  case CmpInst::FCMP_FALSE: return FCMP_FALSE;
  case CmpInst::FCMP_OEQ: return FCMP_OEQ;
  case CmpInst::FCMP_OGT: return FCMP_OGT;
  case CmpInst::FCMP_OGE: return FCMP_OGE;
  case CmpInst::FCMP_OLT: return FCMP_OLT;
  case CmpInst::FCMP_OLE: return FCMP_OLE;
  case CmpInst::FCMP_ONE: return FCMP_ONE;
  case CmpInst::FCMP_ORD: return FCMP_ORD;
  case CmpInst::FCMP_UNO: return FCMP_UNO;
  case CmpInst::FCMP_UEQ: return FCMP_UEQ;
  case CmpInst::FCMP_UGT: return FCMP_UGT;
  case CmpInst::FCMP_UGE: return FCMP_UGE;
  case CmpInst::FCMP_ULT: return FCMP_ULT;
  case CmpInst::FCMP_ULE: return FCMP_ULE;
  case CmpInst::FCMP_UNE: return FCMP_UNE;
  case CmpInst::FCMP_TRUE: return FCMP_TRUE;
  case CmpInst::ICMP_EQ: return ICMP_EQ;
  case CmpInst::ICMP_NE: return ICMP_NE;
  case CmpInst::ICMP_UGT: return ICMP_UGT;
  case CmpInst::ICMP_UGE: return ICMP_UGE;
  case CmpInst::ICMP_ULT: return ICMP_ULT;
  case CmpInst::ICMP_ULE: return ICMP_ULE;
  case CmpInst::ICMP_SGT: return ICMP_SGT;
  case CmpInst::ICMP_SGE: return ICMP_SGE;
  case CmpInst::ICMP_SLT: return ICMP_SLT;
  case CmpInst::ICMP_SLE: return ICMP_SLE;
  }

  ostringstream os;
  os << "Unhandled comparison predicate: " << p;
  throw os.str();
}

TypeTag decodeTypeTag(Type::TypeID t) {
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

LinkageType decodeLinkage(const GlobalValue *gv) {
  switch(gv->getLinkage()) {
  case GlobalValue::ExternalLinkage: return ExternalLinkage;
  case GlobalValue::AvailableExternallyLinkage: return AvailableExternallyLinkage;
  case GlobalValue::LinkOnceAnyLinkage: return LinkOnceAnyLinkage;
  case GlobalValue::LinkOnceODRLinkage: return LinkOnceODRLinkage;
  case GlobalValue::WeakAnyLinkage: return WeakAnyLinkage;
  case GlobalValue::WeakODRLinkage: return WeakODRLinkage;
  case GlobalValue::AppendingLinkage: return AppendingLinkage;
  case GlobalValue::InternalLinkage: return InternalLinkage;
  case GlobalValue::PrivateLinkage: return PrivateLinkage;
  case GlobalValue::LinkerPrivateLinkage: return LinkerPrivateLinkage;
  case GlobalValue::LinkerPrivateWeakLinkage: return LinkerPrivateWeakLinkage;
  case GlobalValue::LinkerPrivateWeakDefAutoLinkage: return LinkerPrivateWeakDefAutoLinkage;
  case GlobalValue::DLLImportLinkage: return DLLImportLinkage;
  case GlobalValue::DLLExportLinkage: return DLLExportLinkage;
  case GlobalValue::ExternalWeakLinkage: return ExternalWeakLinkage;
  case GlobalValue::CommonLinkage: return CommonLinkage;
  }

  ostringstream os;
  os << "Unhandled linkage type: " << gv->getLinkage();
  throw os.str();
}

VisibilityType decodeVisibility(const GlobalValue *gv) {
  switch(gv->getVisibility()) {
  case GlobalValue::DefaultVisibility: return DefaultVisibility;
  case GlobalValue::HiddenVisibility: return HiddenVisibility;
  case GlobalValue::ProtectedVisibility: return ProtectedVisibility;
  }

  ostringstream os;
  os << "Unhandled visibility style: " << gv->getVisibility();
  throw os.str();
}

CallingConvention decodeCallingConvention(CallingConv::ID cc) {
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

void disposeCType(CType *ct) {
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
void disposeData(ValueTag t, void* data) {
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

  case VAL_RETINST:
  case VAL_UBRANCHINST:
  case VAL_CBRANCHINST:
  case VAL_SWITCHINST:
  case VAL_INDIRECTBRINST:
  {
    CInstructionInfo *ii = (CInstructionInfo*)data;
    delete[] ii->operands;
    delete ii;
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
  {
    CBinaryOpInfo *bi = (CBinaryOpInfo*)data;
    delete bi;
    return;
  }

  case VAL_ALLOCAINST:
  case VAL_LOADINST:
  {
    CUnaryOpInfo *ui = (CUnaryOpInfo*)data;
    delete ui;
    return;
  }

  case VAL_STOREINST:
  {
    CStoreInfo *si = (CStoreInfo*)data;
    delete si;
    return;
  }

  case VAL_GETELEMENTPTRINST:
  {
    CGEPInfo *gi = (CGEPInfo*)data;
    delete[] gi->indices;
    delete gi;
    return;
  }


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
  {
    CUnaryOpInfo *ui = (CUnaryOpInfo*)data;
    delete ui;
    return;
  }

  case VAL_ICMPINST:
  case VAL_FCMPINST:
  {
    CCmpInfo *ci = (CCmpInfo*)data;
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

  case VAL_SELECTINST:
  {
    CInstructionInfo *ii = (CInstructionInfo*)data;
    delete[] ii->operands;
    delete ii;
    return;
  }

  case VAL_VAARGINST:
  {
    CUnaryOpInfo *ui = (CUnaryOpInfo*)data;
    delete ui;
    return;
  }

  case VAL_EXTRACTELEMENTINST:
  case VAL_INSERTELEMENTINST:
  case VAL_SHUFFLEVECTORINST:
  {
    CInstructionInfo *ii = (CInstructionInfo*)data;
    delete[] ii->operands;
    delete ii;
    return;
  }

  case VAL_EXTRACTVALUEINST:
  case VAL_INSERTVALUEINST:
  {
    CInsExtValInfo *vi = (CInsExtValInfo*)data;
    delete[] vi->indices;
    delete vi;
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

  }

  ostringstream os;
  os << "Unhandled cleanup case for value tag: " << t;
  throw os.str();
}

void disposeCValue(CValue *v) {
  // Do not dispose the type - that is taken care of in bulk in the
  // CModule disposal.  Same for MD.  Free local constant operands.
  free(v->name);
  // for(int i = 0; i < v->numOperands; ++i) {
  //   if(isLocalConstant(v->operands[i]))
  //     disposeCValue(v->operands[i]);
  // }

  // delete[] v->operands;
  delete[] v->md;

  disposeData(v->valueTag, v->data);

  delete v;
}

// FIXME: Add in named types to help break cycles.  This information
// is kind of available from the module.
CType* translateType(CModule *m, const Type *t) {
  unordered_map<const Type*,CType*>::const_iterator it = m->typeMap->find(t);
  if(it != m->typeMap->end())
    return it->second;

  CType *nt = new CType;
  nt->typeTag = decodeTypeTag(t->getTypeID());

  // Need to put this in the table before making any recursive calls,
  // otherwise it might never terminate.
  (*m->typeMap)[t] = nt;

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

CValue* translateConstant(CModule *m, const Constant *c);
CValue* translateValue(CModule *m, const Value *v);
CValue* translateBasicBlock(CModule *m, const BasicBlock *bb);

CValue* translateGlobalAlias(CModule *m, const GlobalAlias *ga) {
  unordered_map<const Value*, CValue*>::iterator it = m->valueMap->find(ga);
  if(it != m->valueMap->end())
    return it->second;

  CValue *v = new CValue;
  (*m->valueMap)[ga] = v;

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

CValue* translateArgument(CModule *m, const Argument *a) {
  // Arguments are translated before instructions, so we don't really
  // need to check to see if the argument exists already (it won't).
  CValue *v = new CValue;
  (*m->valueMap)[a] = v;

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

void buildRetInst(CModule *m, CValue *v, const ReturnInst *ri) {
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

void buildBranchInst(CModule *m, CValue *v, const BranchInst *bi) {
  v->valueType = translateType(m, bi->getType());

  CInstructionInfo *ii = new CInstructionInfo;
  v->data = (void*)ii;

  if(bi->isConditional())
  {
    v->valueTag = VAL_CBRANCHINST;
    ii->numOperands = 3;
    ii->operands = new CValue*[ii->numOperands];
    ii->operands[0] = translateValue(m, bi->getCondition());
    ii->operands[1] = translateValue(m, bi->getSuccessor(0));
    ii->operands[2] = translateValue(m, bi->getSuccessor(1));
  }
  else
  {
    v->valueTag = VAL_UBRANCHINST;
    ii->numOperands = 1;
    ii->operands = new CValue*[ii->numOperands];
    ii->operands[0] = translateValue(m, bi->getSuccessor(0));
  }
}

void buildSimpleInst(CModule *m, CValue *v, ValueTag t, const Instruction *inst) {
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

void buildBinaryInst(CModule *m, CValue *v, ValueTag t, const Instruction *inst) {
  const BinaryOperator *bi = dynamic_cast<const BinaryOperator*>(inst);
  assert(bi);

  v->valueTag = t;
  v->valueType = translateType(m, bi->getType());

  CBinaryOpInfo *ii = new CBinaryOpInfo;
  v->data = (void*)ii;

  ii->lhs = translateValue(m, bi->getOperand(0));
  ii->rhs = translateValue(m, bi->getOperand(1));
  ii->flags = 0;
  if(bi->hasNoUnsignedWrap())
    ii->flags += 1;
  if(bi->hasNoSignedWrap())
    ii->flags += 2;
}

void buildInvokeInst(CModule *m, CValue *v, const InvokeInst *ii) {
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

void buildCallInst(CModule *m, CValue *v, const CallInst *ii) {
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

void buildAllocaInst(CModule *m, CValue *v, const AllocaInst *ai) {
  v->valueTag = VAL_ALLOCAINST;
  v->valueType = translateType(m, ai->getType());
  if(ai->hasName())
    v->name = strdup(ai->getNameStr().c_str());

  CUnaryOpInfo *io = new CUnaryOpInfo;
  v->data = (void*)io;

  io->val = translateValue(m, ai->getArraySize());
  io->align = ai->getAlignment();
}

void buildLoadInst(CModule *m, CValue *v, const LoadInst *li) {
  v->valueTag = VAL_LOADINST;
  v->valueType = translateType(m, li->getType());
  if(li->hasName())
    v->name = strdup(li->getNameStr().c_str());

  CUnaryOpInfo *io = new CUnaryOpInfo;
  v->data = (void*)io;

  io->val = translateValue(m, li->getPointerOperand());
  io->isVolatile = li->isVolatile();
  io->align = li->getAlignment();
  io->addrSpace = li->getPointerAddressSpace();
}

void buildStoreInst(CModule *m, CValue *v, const StoreInst *si) {
  v->valueTag = VAL_STOREINST;
  v->valueType = translateType(m, si->getType());
  if(si->hasName())
    v->name = strdup(si->getNameStr().c_str());

  CStoreInfo *i = new CStoreInfo;
  v->data = (void*)i;

  i->value = translateValue(m, si->getValueOperand());
  i->pointer = translateValue(m, si->getPointerOperand());
  i->addrSpace = si->getPointerAddressSpace();
  i->align = si->getAlignment();
  i->isVolatile = si->isVolatile();
}

void buildGEPInst(CModule *m, CValue *v, const GetElementPtrInst *i) {
  v->valueTag = VAL_GETELEMENTPTRINST;
  v->valueType = translateType(m, i->getType());
  if(i->hasName())
    v->name = strdup(i->getNameStr().c_str());

  CGEPInfo *gi = new CGEPInfo;
  v->data = (void*)gi;

  gi->operand = translateValue(m, i->getPointerOperand());
  gi->indexListLen = i->getNumIndices();
  gi->indices = new CValue*[gi->indexListLen];
  gi->inBounds = i->isInBounds();
  gi->addrSpace = i->getPointerAddressSpace();

  size_t idx = 0;
  for(GetElementPtrInst::const_op_iterator it = i->idx_begin(),
        ed = i->idx_end(); it != ed; ++it) {
    gi->indices[idx++] = translateValue(m, it->get());
  }
}

void buildCastInst(CModule *m, CValue *v, ValueTag t, const Instruction *i) {
  const CastInst *ci = dynamic_cast<const CastInst*>(i);
  v->valueTag = t;
  v->valueType = translateType(m, ci->getType());
  if(ci->hasName())
    v->name = strdup(ci->getNameStr().c_str());

  CUnaryOpInfo *ui = new CUnaryOpInfo;
  v->data = (void*)ui;

  ui->val = translateValue(m, ci->getOperand(0));
}

void buildCmpInst(CModule *m, CValue *v, ValueTag t, const Instruction *i) {
  const CmpInst *ci = dynamic_cast<const CmpInst*>(i);
  v->valueTag = t;
  v->valueType = translateType(m, ci->getType());
  if(ci->hasName())
    v->name = strdup(ci->getNameStr().c_str());

  CCmpInfo *cmp = new CCmpInfo;
  v->data = (void*)cmp;

  cmp->op1 = translateValue(m, ci->getOperand(0));
  cmp->op2 = translateValue(m, ci->getOperand(1));
  cmp->pred = decodePredicate(ci->getPredicate());
}

void buildPHINode(CModule *m, CValue *v, const PHINode* n) {
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

void buildVAArgInst(CModule *m, CValue *v, const VAArgInst *vi) {
  v->valueTag = VAL_VAARGINST;
  v->valueType = translateType(m, vi->getType());
  if(vi->hasName())
    v->name = strdup(vi->getNameStr().c_str());

  CUnaryOpInfo *ui = new CUnaryOpInfo;
  v->data = (void*)ui;

  ui->val = translateValue(m, vi->getPointerOperand());
}

void buildExtractValueInst(CModule *m, CValue *v, const ExtractValueInst *ei) {
  v->valueTag = VAL_EXTRACTVALUEINST;
  v->valueType = translateType(m, ei->getType());
  if(ei->hasName())
    v->name = strdup(ei->getNameStr().c_str());

  CInsExtValInfo *vi = new CInsExtValInfo;
  v->data = (void*)vi;

  vi->aggregate = translateValue(m, ei->getAggregateOperand());
  vi->numIndices = ei->getNumIndices();
  vi->indices = new int[vi->numIndices];

  std::copy(ei->idx_begin(), ei->idx_end(), vi->indices);
}

void buildInsertValueInst(CModule *m, CValue *v, const InsertValueInst *ii) {
  v->valueTag = VAL_INSERTVALUEINST;
  v->valueType = translateType(m, ii->getType());
  if(ii->hasName())
    v->name = strdup(ii->getNameStr().c_str());

  CInsExtValInfo *vi = new CInsExtValInfo;
  v->data = (void*)vi;

  vi->aggregate = translateValue(m, ii->getAggregateOperand());
  vi->val = translateValue(m, ii->getInsertedValueOperand());
  vi->numIndices = ii->getNumIndices();
  vi->indices = new int[vi->numIndices];

  std::copy(ii->idx_begin(), ii->idx_end(), vi->indices);
}

CValue* translateInstruction(CModule *m, const Instruction *i) {
  unordered_map<const Value*, CValue*>::iterator it = m->valueMap->find(i);
  if(it != m->valueMap->end())
    return it->second;

  CValue *v = new CValue;
  (*m->valueMap)[i] = v;

  // FIXME: Handle metadata here

  switch(i->getOpcode()) {
    // Terminator instructions
  case Instruction::Ret:
    buildRetInst(m, v, dynamic_cast<const ReturnInst*>(i));
    break;
  case Instruction::Br:
    buildBranchInst(m, v, dynamic_cast<const BranchInst*>(i));
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

CValue* translateBasicBlock(CModule *m, const BasicBlock *bb) {
  unordered_map<const Value*, CValue*>::iterator it = m->valueMap->find(bb);
  if(it != m->valueMap->end())
    return it->second;

  CValue *v = new CValue;
  (*m->valueMap)[bb] = v;

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

CValue* translateFunction(CModule *m, const Function *f) {
  unordered_map<const Value*, CValue*>::iterator it = m->valueMap->find(f);
  if(it != m->valueMap->end())
    return it->second;

  CValue *v = new CValue;
  (*m->valueMap)[f] = v;

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

CValue* translateGlobalVariable(CModule *m, const GlobalVariable *gv) {
  unordered_map<const Value*, CValue*>::iterator it = m->valueMap->find(gv);
  if(it != m->valueMap->end())
    return it->second;

  CValue *v = new CValue;
  (*m->valueMap)[gv] = v;

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

  if(gv->hasInitializer())
    gi->initializer = translateConstant(m, gv->getInitializer());

  v->data = (void*)gi;

  return v;
}

CValue* translateInlineAsm(CModule *m, const InlineAsm* a) {
  CValue *v = new CValue;
  (*m->valueMap)[a] = v;

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

CValue* translateGlobalValue(CModule *m, const GlobalValue *gv) {
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

CValue* translateEmptyConstant(CModule *m, ValueTag t, const Constant *p) {
  CValue *v = new CValue;
  (*m->valueMap)[p] = v;

  v->valueTag = t;
  v->valueType = translateType(m, p->getType());

  // No data or metadata

  return v;
}

CValue* translateConstantInt(CModule *m, const ConstantInt* i) {
  CValue *v = new CValue;
  (*m->valueMap)[i] = v;

  v->valueTag = VAL_CONSTANTINT;
  v->valueType = translateType(m, i->getType());

  // No name

  CConstInt *d = new CConstInt;
  v->data = (void*)d;

  d->val = i->getSExtValue();

  return v;
}

CValue* translateConstantFP(CModule *m, const ConstantFP *fp) {
  CValue *v = new CValue;
  (*m->valueMap)[fp] = v;

  v->valueTag = VAL_CONSTANTFP;
  v->valueType = translateType(m, fp->getType());

  // No name

  CConstFP *d = new CConstFP;
  v->data = (void*)d;

  d->val = fp->getValueAPF().convertToDouble();

  return v;
}

CValue* translateBlockAddress(CModule *m, const BlockAddress *ba) {
  CValue *v = new CValue;
  (*m->valueMap)[ba] = v;

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

CValue* translateConstantAggregate(CModule *m, ValueTag t, const Constant *ca) {
  CValue *v = new CValue;
  (*m->valueMap)[ca] = v;

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

CValue* translateConstant(CModule *m, const Constant *c) {
  unordered_map<const Value*, CValue*>::iterator it = m->valueMap->find(c);
  if(it != m->valueMap->end())
    return it->second;

  // Order these in order of frequency
  if(const ConstantInt *ci = dynamic_cast<const ConstantInt*>(c)) {
    return translateConstantInt(m, ci);
  }

  if(const ConstantPointerNull *pn = dynamic_cast<const ConstantPointerNull*>(c)) {
    return translateEmptyConstant(m, VAL_CONSTANTPOINTERNULL, pn);
  }

  if(const GlobalValue *gv = dynamic_cast<const GlobalValue*>(c)) {
    return translateGlobalValue(m, gv);
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

  if(const ConstantArray *ca = dynamic_cast<const ConstantArray*>(c)) {
    return translateConstantAggregate(m, VAL_CONSTANTARRAY, ca);
  }

  if(const ConstantVector *cv = dynamic_cast<const ConstantVector*>(c)) {
    return translateConstantAggregate(m, VAL_CONSTANTVECTOR, cv);
  }

  if(const ConstantStruct *cs = dynamic_cast<const ConstantStruct*>(c)) {
    return translateConstantAggregate(m, VAL_CONSTANTSTRUCT, cs);
  }


  string msg;
  raw_string_ostream os(msg);
  os << "Unhandled constant type: ";
  c->print(os);
  throw os.str();
}

CValue* translateValue(CModule *m, const Value *v) {
  unordered_map<const Value*, CValue*>::iterator it = m->valueMap->find(v);
  if(it != m->valueMap->end())
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

    for(unordered_map<const Type*,CType*>::iterator it = m->typeMap->begin(),
          ed = m->typeMap->end(); it != ed; ++it)
    {
      disposeCType(it->second);
    }

    delete m->typeMap;

    for(unordered_map<const Value*,CValue*>::iterator it = m->valueMap->begin(),
          ed = m->valueMap->end(); it != ed; ++it)
    {
      disposeCValue(it->second);
    }
    delete m->valueMap;

    delete m->original;
    delete m;
  }

  CModule * marshall(const char * filename) {
    CModule *ret = new CModule;
    std::string errMsg;
    OwningPtr<MemoryBuffer> buffer;
    error_code ec = MemoryBuffer::getFile(filename, buffer);

    if(buffer.get() == NULL){
      ret->isError = 1;
      ret->errMsg = strdup(ec.message().c_str());
      return ret;
    }

    LLVMContext ctxt;
    Module *m = ParseBitcodeFile(buffer.get(), ctxt, &errMsg);

    if(m == NULL) {
      ret->isError = 1;
      ret->errMsg = strdup(errMsg.c_str());
      return ret;
    }

    ret->moduleIdentifier = strdup(m->getModuleIdentifier().c_str());
    ret->moduleDataLayout = strdup(m->getDataLayout().c_str());
    ret->targetTriple = strdup(m->getTargetTriple().c_str());
    ret->moduleInlineAsm = strdup(m->getModuleInlineAsm().c_str());
    ret->typeMap = new unordered_map<const Type*, CType*>();
    ret->valueMap = new unordered_map<const Value*, CValue*>();
    ret->original = m;

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

      ret->globalVariables = new CValue*[globalVariables.size()];
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

      ret->functions = new CValue*[functions.size()];
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

      ret->globalAliases = new CValue*[globalAliases.size()];
      std::copy(globalAliases.begin(), globalAliases.end(), ret->globalAliases);
    }
    catch(const string &msg) {
      ret->isError = 1;
      ret->errMsg = strdup(msg.c_str());
    }
    catch(...) {
      ret->isError = 1;
      ret->errMsg = strdup("Unknown error");
    }

    return ret;
  }
}
