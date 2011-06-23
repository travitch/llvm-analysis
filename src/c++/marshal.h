enum CmpPredicate {
  FCMP_FALSE,
  FCMP_OEQ,
  FCMP_OGT,
  FCMP_OGE,
  FCMP_OLT,
  FCMP_OLE,
  FCMP_ONE,
  FCMP_ORD,
  FCMP_UNO,
  FCMP_UEQ,
  FCMP_UGT,
  FCMP_UGE,
  FCMP_ULT,
  FCMP_ULE,
  FCMP_UNE,
  FCMP_TRUE,
  ICMP_EQ,
  ICMP_NE,
  ICMP_UGT,
  ICMP_UGE,
  ICMP_ULT,
  ICMP_ULE,
  ICMP_SGT,
  ICMP_SGE,
  ICMP_SLT,
  ICMP_SLE
};

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

typedef struct CType_t CType;

struct CType_t {
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
  VAL_RETINST,     // 0 or 1 operand
  VAL_BRANCHINST, // 1 or 3 operands
  VAL_SWITCHINST,  // op[0] = switchval, op[1] = default dest, op[2n]
                   // = value to match, op[2n+1] = dest for match
  VAL_INDIRECTBRINST, // op[0] = address, rest are possible dests
  VAL_INVOKEINST,
  VAL_UNWINDINST,
  VAL_UNREACHABLEINST,
  VAL_ADDINST,
  VAL_FADDINST,
  VAL_SUBINST,
  VAL_FSUBINST,
  VAL_MULINST,
  VAL_FMULINST,
  VAL_UDIVINST,
  VAL_SDIVINST,
  VAL_FDIVINST,
  VAL_UREMINST,
  VAL_SREMINST,
  VAL_FREMINST,
  VAL_SHLINST,
  VAL_LSHRINST,
  VAL_ASHRINST,
  VAL_ANDINST,
  VAL_ORINST,
  VAL_XORINST,
  VAL_ALLOCAINST,
  VAL_LOADINST,
  VAL_STOREINST,
  VAL_GETELEMENTPTRINST,
  VAL_TRUNCINST,
  VAL_ZEXTINST,
  VAL_SEXTINST,
  VAL_FPTOUIINST,
  VAL_FPTOSIINST,
  VAL_UITOFPINST,
  VAL_SITOFPINST,
  VAL_FPTRUNCINST,
  VAL_FPEXTINST,
  VAL_PTRTOINTINST,
  VAL_INTTOPTRINST,
  VAL_BITCASTINST,
  VAL_ICMPINST,
  VAL_FCMPINST,
  VAL_PHINODE,
  VAL_CALLINST,
  VAL_SELECTINST, // 0 = condition, 1 = trueval, 2 = falseval
  VAL_VAARGINST,
  VAL_EXTRACTELEMENTINST, // 0 = vector, 1 = index
  VAL_INSERTELEMENTINST, // 0 = vector, 1 = value, 2 = index
  VAL_SHUFFLEVECTORINST, // 0 = v1, 1 = v2, v3 = mask
  VAL_EXTRACTVALUEINST,
  VAL_INSERTVALUEINST,
  // Globals
  VAL_FUNCTION,
  VAL_GLOBALVARIABLE,
  VAL_ALIAS
};

typedef struct {

} CMetadata;

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

typedef struct CValue_t CValue;

typedef struct {
  int hasSRet;
  int hasByVal;
  int hasNest;
  int hasNoAlias;
  int hasNoCapture;
} CArgumentInfo;

typedef struct {
  CValue **instructions;
  int blockLen;
} CBasicBlockInfo;

typedef struct {
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
} CFunctionInfo;

typedef struct {
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
} CGlobalInfo;

typedef struct {
  CValue **operands;
  int numOperands;
} CInstructionInfo;

typedef struct {
  CValue *lhs;
  CValue *rhs;
  // 0 == no flags
  // 1 == hasNoUnsignedWrap
  // 2 == hasNoSignedWrap
  // 3 == both
  int flags;
} CBinaryOpInfo;

typedef struct {
  CValue *val;
  int align;

  // Load
  int isVolatile;
  int addrSpace;
} CUnaryOpInfo;

typedef struct {
  CValue *value;
  CValue *pointer;
  int addrSpace;
  int align;
  int isVolatile;
} CStoreInfo;

typedef struct {
  CValue *operand;
  CValue **indices;
  int indexListLen;
  int inBounds;
  int addrSpace;
} CGEPInfo;

typedef struct {
  CValue *op1;
  CValue *op2;
  CmpPredicate pred;
} CCmpInfo;

typedef struct {
  CValue **incomingValues;
  CValue **valueBlocks;
  int numIncomingValues;
} CPHIInfo;

typedef struct {
  CValue *aggregate;
  CValue *val; // only insert
  int *indices;
  int numIndices;
} CInsExtValInfo;

// Also for invoke
typedef struct {
  CValue *calledValue;
  CValue **arguments;
  int argListLen;
  CallingConvention callingConvention;
  int hasSRet;
  int isTail;

  // FIXME: Add attributes

  // Invoke only
  CValue *normalDest;
  CValue *unwindDest;
} CCallInfo;

typedef struct {
  char *asmString;
  char *constraintString;
} CInlineAsmInfo;

typedef struct {
  CValue *func;
  CValue *block;
} CBlockAddrInfo;

// This is lossy but good enough for all practical purposes.
typedef struct {
  long long int val;
} CConstInt;

typedef struct {
  double val;
} CConstFP;

typedef struct {
  CValue **constants;
  int numElements;
} CConstAggregate;

typedef struct {
  CValue **operands;
  int numOperands;
  ValueTag instrType;
} CConstExprInfo;

struct CValue_t {
  ValueTag valueTag;
  CType *valueType;
  char *name;
  CMetadata **md;

  void *data;
};

typedef struct CModule_t CModule;

#if defined(__cplusplus)
extern "C" {
#endif
  void disposeCModule(CModule *m);
  CModule* marshalLLVM(const char *filename);
  int cmoduleIsError(CModule *m);
  const char* cmoduleErrMsg(CModule *m);
  const char* cmoduleDataLayout(CModule *m);
  const char* cmoduleIdentifier(CModule *m);
  const char* cmoduleTargetTriple(CModule *m);
  int cmoduleIsLittleEndian(CModule *m);
  int cmodulePointerSize(CModule *m);
  const char* cmoduleInlineAsm(CModule *m);
  CValue** cmoduleGlobalVariables(CModule *m);
  CValue** cmoduleGlobalAliases(CModule *m);
  CValue** cmoduleFunctions(CModule *m);
#if defined(__cplusplus)
}
#endif
