#include <stdint.h>

/*!
  Arithmetic flags
 */
typedef enum {
  ArithNone,
  ArithNUW,
  ArithNSW,
  ArithBoth
} ArithFlags;

/*!
  Possible predicates for the icmp and fcmp instructions
 */
typedef enum {
  F_CMP_FALSE,
  F_CMP_OEQ,
  F_CMP_OGT,
  F_CMP_OGE,
  F_CMP_OLT,
  F_CMP_OLE,
  F_CMP_ONE,
  F_CMP_ORD,
  F_CMP_UNO,
  F_CMP_UEQ,
  F_CMP_UGT,
  F_CMP_UGE,
  F_CMP_ULT,
  F_CMP_ULE,
  F_CMP_UNE,
  F_CMP_TRUE,
  I_CMP_EQ,
  I_CMP_NE,
  I_CMP_UGT,
  I_CMP_UGE,
  I_CMP_ULT,
  I_CMP_ULE,
  I_CMP_SGT,
  I_CMP_SGE,
  I_CMP_SLT,
  I_CMP_SLE
} CmpPredicate;

/*!
  Function calling conventions
 */
typedef enum {
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
} CallingConvention;

/*!
  Type tags
 */
typedef enum {
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
} TypeTag;

typedef struct CValue_t CValue;
typedef struct CMeta_t CMeta;
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

  // For TypePointer
  int addrSpace;
};

/*!
  Metadata tags
*/
typedef enum {
  META_LOCATION,
  META_DERIVEDTYPE,
  META_COMPOSITETYPE,
  META_BASICTYPE,
  META_VARIABLE,
  META_SUBPROGRAM,
  META_GLOBALVARIABLE,
  META_FILE,
  META_COMPILEUNIT,
  META_NAMESPACE,
  META_LEXICALBLOCK,
  META_SUBRANGE,
  META_ENUMERATOR,
  META_ARRAY,
  META_TEMPLATETYPEPARAMETER,
  META_TEMPLATEVALUEPARAMETER
} MetaTag;

typedef struct {
  int arrayLen;
  CMeta **arrayElts;
} MetaArrayInfo;

typedef struct {
  char *enumName;
  uint64_t enumValue;
} MetaEnumeratorInfo;

typedef struct {
  CMeta *context;
  char *name;
  char *displayName;
  char *linkageName;
  CMeta *compileUnit;
  unsigned lineNumber;
  CMeta *globalType;
  int isLocalToUnit;
  int isDefinition;
  CValue *global;
} MetaGlobalInfo;

typedef struct {
  unsigned lineNumber;
  unsigned columnNumber;
  CMeta *scope;
  CMeta *origLocation;
  char *filename;
  char *directory;
} MetaLocationInfo;

typedef struct {
  int64_t lo;
  int64_t hi;
} MetaSubrangeInfo;

typedef struct {
  CMeta *context;
  char *name;
  CMeta *type;
  char *filename;
  char *directory;
  unsigned lineNumber;
  unsigned columnNumber;
} MetaTemplateTypeInfo;

typedef struct {
  CMeta *context;
  char *name;
  CMeta *type;
  uint64_t value;
  char *filename;
  char *directory;
  unsigned lineNumber;
  unsigned columnNumber;
} MetaTemplateValueInfo;

typedef struct {
  CMeta *context;
  char *name;
  CMeta *compileUnit;
  unsigned lineNumber;
  unsigned argNumber;
  CMeta *type;
  int isArtificial;
  int hasComplexAddress;
  unsigned numAddrElements;
  uint64_t *addrElements;
  int isBlockByRefVar;
} MetaVariableInfo;

typedef struct {
  unsigned language;
  char *filename;
  char *directory;
  char *producer;
  int isMain;
  int isOptimized;
  char *flags;
  unsigned runtimeVersion;
} MetaCompileUnitInfo;

typedef struct {
  char *filename;
  char *directory;
  CMeta *compileUnit;
} MetaFileInfo;

typedef struct {
  CMeta *context;
  unsigned lineNumber;
  unsigned columnNumber;
  char *directory;
  char *filename;
} MetaLexicalBlockInfo;

typedef struct {
  CMeta *context;
  char *name;
  char *directory;
  char *filename;
  CMeta *compileUnit;
  unsigned lineNumber;
} MetaNamespaceInfo;

typedef struct {
  CMeta *context;
  char *name;
  char *displayName;
  char *linkageName;
  CMeta *compileUnit;
  unsigned lineNumber;
  CMeta *type;
  char *returnTypeName;
  int isLocalToUnit;
  int isDefinition;
  unsigned virtuality;
  unsigned virtualIndex;
  CMeta *containingType;
  int isArtificial;
  int isPrivate;
  int isProtected;
  int isExplicit;
  int isPrototyped;
  int isOptimized;
  char *filename;
  char *directory;
  CValue *function;
  // This is only available in LLVM 3.0+
  // CMeta *templateParams;
} MetaSubprogramInfo;

typedef struct {
  CMeta *context;
  char *name;
  CMeta *compileUnit;
  CMeta *file;
  unsigned lineNumber;
  uint64_t sizeInBits;
  uint64_t alignInBits;
  uint64_t offsetInBits;
  unsigned flags;
  int isPrivate;
  int isProtected;
  int isForward;
  int isByRefStruct;
  int isVirtual;
  int isArtificial;
  char *directory;
  char *filename;

  // Basic type
  unsigned encoding;

  // Derived and Composite Types
  CMeta *typeDerivedFrom;
  uint64_t originalTypeSize;

  // Composite Type
  CMeta *typeArray;
  unsigned runTimeLang;
  CMeta *containingType;
  CMeta *templateParams;
} MetaTypeInfo;

struct CMeta_t {
  MetaTag metaTag;
  unsigned int tag;
  union {
    MetaArrayInfo metaArrayInfo;
    MetaEnumeratorInfo metaEnumeratorInfo;
    MetaGlobalInfo metaGlobalInfo;
    MetaLocationInfo metaLocationInfo;
    MetaSubrangeInfo metaSubrangeInfo;
    MetaTemplateTypeInfo metaTemplateTypeInfo;
    MetaTemplateValueInfo metaTemplateValueInfo;
    MetaVariableInfo metaVariableInfo;
    MetaCompileUnitInfo metaCompileUnitInfo;
    MetaFileInfo metaFileInfo;
    MetaLexicalBlockInfo metaLexicalBlockInfo;
    MetaNamespaceInfo metaNamespaceInfo;
    MetaSubprogramInfo metaSubprogramInfo;
    MetaTypeInfo metaTypeInfo;
  } u;
};

/*!
  Value tags
 */
typedef enum {
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
} ValueTag;


typedef enum {
  LTExternal,
  LTAvailableExternally,
  LTLinkOnceAny,
  LTLinkOnceODR,
  LTWeakAny,
  LTWeakODR,
  LTAppending,
  LTInternal,
  LTPrivate,
  LTLinkerPrivate,
  LTLinkerPrivateWeak,
  LTLinkerPrivateWeakDefAuto,
  LTDLLImport,
  LTDLLExport,
  LTExternalWeak,
  LTCommon
} LinkageType;

typedef enum {
  VisibilityDefault,
  VisibilityHidden,
  VisibilityProtected
} VisibilityStyle;

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
  VisibilityStyle visibility;
  LinkageType linkage;
  char *section;

  CallingConvention callingConvention;
  int isVarArg;
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
  VisibilityStyle visibility;
  LinkageType linkage;
  char *section;

  // Only for global vars
  CValue *initializer;
  int isThreadLocal;
  int isConstant;

  // Only for global aliases
  CValue *aliasee;
} CGlobalInfo;

typedef struct {
  CValue **operands;
  int numOperands;

  // Value insts
  int *indices;
  int numIndices;

  // Comparisons
  CmpPredicate cmpPred;

  // Binary operators
  ArithFlags flags;

  // Misc
  int align;
  int addrSpace;

  // Load/Store
  int isVolatile;

  // GEP
  int inBounds;
} CInstructionInfo;


typedef struct {
  CValue **incomingValues;
  CValue **valueBlocks;
  int numIncomingValues;
} CPHIInfo;

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
  CInstructionInfo *ii;
  ValueTag instrType;
} CConstExprInfo;

struct CValue_t {
  ValueTag valueTag;
  CType *valueType;
  char *name;
  CMeta **md;
  int numMetadata;
  int metaCapacity;

  void *data;
};

typedef struct {
  char *moduleIdentifier;
  char *moduleDataLayout;
  char *targetTriple;
  int littleEndian;
  int pointerSize;
  char *moduleInlineAsm;

  CValue **globalVariables;
  int numGlobalVariables;
  CValue **globalAliases;
  int numGlobalAliases;
  CValue **functions;
  int numFunctions;

  int hasError;
  char *errMsg;

  void *privateData;
} CModule;

#if defined(__cplusplus)
extern "C" {
#endif
  void disposeCModule(CModule *m);
  CModule* marshalLLVM(const char *filename, int includeLocs);
#if defined(__cplusplus)
}
#endif
