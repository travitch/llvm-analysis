
enum CallingConvention {
  CC_C,
  CC_FAST,
  CC_COLD,
  CC_GHC,
  CC_X86_STDCALL,
  CC_X86_FASTCALL,
  CC_ARM_APCS,
  CC_ARM_AAPCS,
  CC_ARM_AAPCS_VFP,
  CC_MSP430_INTR,
  CC_X86_THISCALL,
  CC_PTX_KERNEL,
  CC_PTX_DEVICE,
  CC_MBLAZE_INTR,
  CC_MBLAZE_SVOL
};

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
  // Constants
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
  VAL_RETINST,
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

struct CArgumentInfo {
  int hasSRet;
  int hasByVal;
  int hasNest;
  int hasNoAlias;
  int hasNoCapture;
};

struct CBasicBlockInfo {
  CValue **instructions;
  int blockLen;
};

struct CFunctionInfo {
  int isExternal; // Declaration
  int alignment;
  VisibilityType visibility;
  LinkageType linkage;
  char *section;

  CallingConvention callingConvention;
  char *gcName;
  CValue **arguments;
  int argListLen;
  CValue **body;
  int blockListLen;
  // FIXME: Add attributes
};

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

struct CInstructionInfo {
  CValue **operands;
  int numOperands;
};

struct CValue {
  ValueTag valueTag;
  CType *valueType;
  char *name;
  CMetadata **md;

  void *data;
};
