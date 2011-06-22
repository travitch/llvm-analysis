#define __STDC_LIMIT_MACROS
#define __STDC_CONSTANT_MACROS

#include <algorithm>
#include <cassert>
#include <cstdlib>
#include <cstring>
#include <string>
#include <vector>
#include <tr1/unordered_map>

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
void disposeData(ValueTag t, void* data) {

}

void disposeCValue(CValue *v) {
  // Do not dispose the type - that is taken care of in bulk in the
  // CModule disposal.  Same for MD.  Free local constant operands.
  free(v->name);
  for(int i = 0; i < v->numOperands; ++i) {
    if(isLocalConstant(v->operands[i]))
      disposeCValue(v->operands[i]);
  }

  delete[] v->operands;
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

CValue* translateFunction(CModule *m, const Function *f) {
unordered_map<const Value*, CValue*>::iterator it = m->valueMap->find(f);
  if(it != m->valueMap->end())
    return it->second;

  CValue *v = new CValue;
  (*m->valueMap)[f] = v;

  v->valueTag = VAL_FUNCTION;


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
