#define __STDC_LIMIT_MACROS
#define __STDC_CONSTANT_MACROS

#include <algorithm>
#include <cassert>
#include <cstdlib>
#include <cstring>
#include <string>
#include <vector>
#include <tr1/unordered_map>

#include <llvm/CallingConv.h>
#include <llvm/Instructions.h>
#include <llvm/LLVMContext.h>
#include <llvm/Module.h>
#include <llvm/Type.h>
#include <llvm/DerivedTypes.h>
#include <llvm/ADT/OwningPtr.h>
#include <llvm/Bitcode/ReaderWriter.h>
#include <llvm/Support/MemoryBuffer.h>
#include <llvm/Support/system_error.h>

#include "marshal.h"

using namespace llvm;
using std::string;
using std::tr1::unordered_map;


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

  assert(false && "Unhandled type tag case");
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
}

VisibilityType decodeVisibility(const GlobalValue *gv) {
  switch(gv->getVisibility()) {
  case GlobalValue::DefaultVisibility: return DefaultVisibility;
  case GlobalValue::HiddenVisibility: return HiddenVisibility;
  case GlobalValue::ProtectedVisibility: return ProtectedVisibility;
  }
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

// FIXME:
bool isLocalConstant(CValue *v) {
  return false;
}

// FIXME:
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
  }
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

  // This map is actually state only for this translation code.  Since
  // types have pointer equality in LLVM, every type will just be
  // translated once to a heap-allocated CType.  On the Haskell side,
  // each CType needs to be translated once (mapping the address of
  // the CType to the translated version).
  unordered_map<const Type*, CType*> *typeMap;
  unordered_map<const Value*, CValue*> *valueMap;
  Module* original;
};

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

CValue* translateValue(CModule *m, const Value *v);
CValue* translateBasicBlock(CModule *m, const BasicBlock *bb);

CValue* translateConstant(CModule *m, const Constant *c) {
  unordered_map<const Value*, CValue*>::iterator it = m->valueMap->find(c);
  if(it != m->valueMap->end())
    return it->second;

  return NULL;
}

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
  gi->isExternal = ga->isDeclaration();
  gi->alignment = ga->getAlignment();
  gi->visibility = decodeVisibility(ga);
  gi->linkage = decodeLinkage(ga);
  if(ga->hasSection())
    gi->section = strdup(ga->getSection().c_str());

  gi->aliasee = translateConstant(m, ga->getAliasee());

  v->data = (void*)gi;

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
  ai->hasSRet = a->hasStructRetAttr();
  ai->hasByVal = a->hasByValAttr();
  ai->hasNest = a->hasNestAttr();
  ai->hasNoAlias = a->hasNoAliasAttr();
  ai->hasNoCapture = a->hasNoCaptureAttr();

  v->data = (void*)ai;

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

void buildInvokeInst(CModule *m, CValue *v, const InvokeInst *ii) {
  v->valueTag = VAL_INVOKEINST;
  v->valueType = translateType(m, ii->getType());
  if(ii->hasName())
    v->name = strdup(ii->getNameStr().c_str());

  CCallInfo *ci = new CCallInfo;
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

CValue* translateInstruction(CModule *m, const Instruction *i) {
  unordered_map<const Value*, CValue*>::iterator it = m->valueMap->find(i);
  if(it != m->valueMap->end())
    return it->second;

  CValue *v = new CValue;
  (*m->valueMap)[i] = v;

  switch(i->getOpcode()) {
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
  case Instruction::Call:
    buildCallInst(m, v, dynamic_cast<const CallInst*>(i));
    break;
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

  v->data = (void*)fi;

  return NULL;
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

CValue* translateValue(CModule *m, const Value *v) {
  unordered_map<const Value*, CValue*>::iterator it = m->valueMap->find(v);
  if(it != m->valueMap->end())
    return it->second;

  // FIXME
  return NULL;
}

extern "C" {

  /*!
    Free all of the resources allocated by the exposed module,
    including the underlying C++ LLVM Module.
   */
  void disposeCModule(CModule *m) {
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
    std::string errMsg;
    OwningPtr<MemoryBuffer> buffer;
    // FIXME: Figure out how to handle this if it is an error
    error_code ec = MemoryBuffer::getFile(filename, buffer);
    LLVMContext ctxt;
    Module *m = ParseBitcodeFile(buffer.get(), ctxt, &errMsg);
    CModule *ret = new CModule;

    ret->moduleIdentifier = strdup(m->getModuleIdentifier().c_str());
    ret->moduleDataLayout = strdup(m->getDataLayout().c_str());
    ret->targetTriple = strdup(m->getTargetTriple().c_str());
    ret->moduleInlineAsm = strdup(m->getModuleInlineAsm().c_str());
    ret->typeMap = new unordered_map<const Type*, CType*>();
    ret->valueMap = new unordered_map<const Value*, CValue*>();
    ret->original = m;

    std::vector<CValue*> globalVariables;
    for(Module::const_global_iterator it = m->global_begin(),
          ed = m->global_end(); it != ed; ++it)
    {
      const GlobalVariable *globalVar = dynamic_cast<const GlobalVariable*>(&*it);
      assert(globalVar && "Not a global FIXME: change to exception");
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
      assert(func && "Not a function");
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
      assert(globalAlias && "Not a global alias");
      CValue *ga = translateGlobalAlias(ret, globalAlias);
      globalAliases.push_back(ga);
    }

    ret->globalAliases = new CValue*[globalAliases.size()];
    std::copy(globalAliases.begin(), globalAliases.end(), ret->globalAliases);

    return ret;
  }
}
