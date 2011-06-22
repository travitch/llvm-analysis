
enum TypeTag {
  TYPE_VOID,
  TYPE_FLOAT,
  TYPE_DOUBLE,
  TYPE_X86_FP80,
  TYPE_FP128,
  TYPE_PPC_FP128,
  TYPE_LABEL,
  TYPE_METADATA,
  TYPE_X86_MMX,
  TYPE_OPAQUE,
  TYPE_INTEGER,
  TYPE_FUNCTION,
  TYPE_STRUCT,
  TYPE_ARRAY,
  TYPE_POINTER,
  TYPE_VECTOR,
  TYPE_NAMED
};

struct CType {
  TypeTag typeTag;
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
  // Globals
  VAL_FUNCTION,
  VAL_GLOBALVARIABLE,
  VAL_ALIAS
};

struct CMetadata {

};

enum LinkageType {
  ExternalLinkage,
  AvailableExternallyLinkage,
  LinkOnceAnyLinkage,
  LinkOnceODRLinkage,
  WeakAnyLinkage,
  WeakODRLinkage,
  AppendingLinkage,
  InternalLinkage,
  PrivateLinkage,
  LinkerPrivateLinkage,
  LinkerPrivateWeakLinkage,
  LinkerPrivateWeakDefAutoLinkage,
  DLLImportLinkage,
  DLLExportLinkage,
  ExternalWeakLinkage,
  CommonLinkage
};

enum VisibilityType {
  DefaultVisibility,
  HiddenVisibility,
  ProtectedVisibility
};

struct CValue;

struct CGlobalInfo {
  int isExternal; // Declaration
  int alignment;
  VisibilityType visibility;
  LinkageType linkage;
  char *section;

  // Only for global vars
  CValue *initializer;
  int isThreadLocal;

  // Only for global aliases
  CValue *aliasee;
};

struct CValue {
  ValueTag valueTag;
  CType *valueType;
  char *name;
  CMetadata **md;
  int numOperands;
  CValue **operands;

  void *data;
};
