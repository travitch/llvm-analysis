#define __STDC_LIMIT_MACROS
#define __STDC_CONSTANT_MACROS

#include <cassert>
#include <cstdlib>
#include <cstring>
#include <string>
#include <tr1/unordered_map>

#include <llvm/LLVMContext.h>
#include <llvm/Module.h>
#include <llvm/Type.h>
#include <llvm/DerivedTypes.h>
#include <llvm/ADT/OwningPtr.h>
#include <llvm/Bitcode/ReaderWriter.h>
#include <llvm/Support/MemoryBuffer.h>
#include <llvm/Support/system_error.h>


using namespace llvm;
using std::string;
using std::tr1::unordered_map;


struct CType {
  Type::TypeID typeTag;
  // For TypeInt, lengths of TypeArray and TypeVector
  unsigned long long size;

  // For TypeFunction
  int isVarArg;

  // For structs
  int isPacked;

  // For TypeFunction, TypeStruct
  CType** typeList;
  int typeListLen;

  // For FunctionType returnType, TypePointer, TypeNamed, and
  // TypeArray and TypeVector
  CType* innerType;

  // Only for TypeNamed
  char *name;
};

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

enum ValueTag {
  VAL_ARGUMENT,
  VAL_BASICBLOCK,
  VAL_INLINEASM,
  VAL_BLOCKADDRESS,
  VAL_CONSTANTAGGREGATEZERO,
  VAL_CONSTANTARRAY,
  VAL_CONSTANTFP,
  VAL_CONSTANTINT,
  VAL_CONSTANTPOINTERNULL,
  VAL_CONSTANTSTRUCT,
  VAL_CONSTANTVECTOR,
  VAL_UNDEFVALUE,
  VAL_CONSTANTEXPR,
  // Insts
  VAL_BINARYOPERATOR,
  VAL_CALLINST,
  VAL_CMPINST,

};

struct CMetadata {

};

struct CValue {
  ValueTag valueTag;
  CType *valueType;
  char *name;
  CMetadata **md;
  int numOperands;
  CValue **operands;
};

void disposeCValue(CValue *v) {
  // Do not dispose the type - that is taken care of in bulk in the
  // CModule disposal.  Same for MD.  Free local constant operands.
  free(v->name);
  for(int i = 0; i < v->numOperands; ++i) {
    if(isLocalConstant(v->operands[i]))
      disposeCValue(v->operands[i]);
  }

  delete[] operands;
  delete[] md;

  delete v;
}

struct CModule {
  char *moduleIdentifier;
  char *moduleDataLayout;
  char *targetTriple;
  int littleEndian;
  int pointerSize;
  char *moduleInlineAsm;

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
  nt->typeTag = t->getTypeID();

  // Need to put this in the table before making any recursive calls,
  // otherwise it might never terminate.
  (*m->typeMap)[t] = nt;

  switch(t->getTypeID()) {
    // Primitives don't require any work
  case Type::VoidTyID:
  case Type::FloatTyID:
  case Type::DoubleTyID:
  case Type::X86_FP80TyID:
  case Type::FP128TyID:
  case Type::PPC_FP128TyID:
  case Type::LabelTyID:
  case Type::MetadataTyID:
  case Type::X86_MMXTyID:
    break;

    // Also nothing to do here
  case Type::OpaqueTyID:
    break;

  case Type::IntegerTyID:
    nt->size = t->getPrimitiveSizeInBits();
    break;

  case Type::FunctionTyID:
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

  case Type::StructTyID:
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

  case Type::ArrayTyID:
  {
    const ArrayType *at = dynamic_cast<const ArrayType*>(t);
    nt->size = at->getNumElements();
    nt->innerType = translateType(m, at->getElementType());
    break;
  }

  case Type::PointerTyID:
  {
    const PointerType *pt = dynamic_cast<const PointerType*>(t);
    nt->innerType = translateType(m, pt->getElementType());
    break;
  }

  case Type::VectorTyID:
  {
    const VectorType *vt = dynamic_cast<const VectorType*>(t);
    nt->size = vt->getNumElements();
    nt->innerType = translateType(m, vt->getElementType());
    break;
  }
  }

  return nt;
}

CValue* translateGlobalVariable(CModule *m, const GlobalVariable* gv) {
  unordered_map<const Value*, CValue*>::iterator it = m->valueMap->find(gv);
  if(it != m->valueMap->end())
    return it->second;

  CValue *v = new CValue;
  m->valueMap[gv] = v;

  return v;
}

extern "C" {

  void disposeCModule(CModule *m) {
    free(m->moduleIdentifier);
    free(m->moduleDataLayout);
    free(m->targetTriple);
    free(m->moduleInlineAsm);

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

    for(Module::const_global_iterator it = m->global_begin(),
          ed = m->global_end(); it != ed; ++it)
    {
      const GlobalVariable* globalVar = dynamic_cast<const GlobalVariable*>(*it);
      assert(globalVar && "Not a global FIXME: change to exception");
      translateGlobal(ret, *it);
    }

    return ret;
  }

}
