Preliminary thought dump for grin bytecode interpreter.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Word, and Pointer to Word.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Requirement: sizeof(GB_Ptr) == sizeof(GB_Word)

%%[8
#if USE_64_BITS
typedef uint64_t GB_Word ;
typedef  int64_t GB_SWord ;
#else
typedef uint32_t GB_Word ;
typedef  int32_t GB_SWord ;
#endif

typedef GB_Word* GB_Ptr ;
typedef uint8_t* GB_BytePtr ;
typedef GB_SWord GB_Int ;
typedef uint16_t GB_NodeTag ;
typedef uint16_t GB_NodeSize ;
typedef uint8_t  GB_Byte ;
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Node structure
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
typedef struct GB_Node {
  GB_NodeSize 	size ;
  GB_NodeTag 	tag ;
  GB_Word 		fields[0] ;    
} GB_Node ;

#define	GB_TagTag
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% GB_Ptr and GB_Int are tagged values, stored in a GB_Word, GB_Ints are shifted left
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#define GB_Word_TagSize 	1
#define GB_Word_TagMask 	1
#define GB_Word_TagInt 		1
#define GB_Word_TagPtr 		0

#define GB_Int_ShiftPow2	Bits_Pow2(GB_Int,GB_Word_TagSize)

#define GB_Word_IsInt(x)		((x) & GB_Word_TagMask)
#define GB_Word_IsPtr(x)		(! GB_Word_IsInt(x))
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% GB_Int arithmetic
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#define GB_FromInt(ty,x)		((ty)((x) / GB_Int_ShiftPow2))
#define GB_ToInt(x)				((Cast(GB_Int,x)) * GB_Int_ShiftPow2)

#define GB_Int0						GB_ToInt(0)
#define GB_Int1						GB_ToInt(1)
#define GB_Int2						GB_ToInt(2)

#define GB_Int_Add(x,y)		((x) + (y) - GB_Word_TagInt)
#define GB_Int_Sub(x,y)		((x) - (y) + GB_Word_TagInt)
#define GB_Int_Mul(x,y)		(((x)-GB_Word_TagInt) * ((y)/GB_Int_ShiftPow2) + GB_Word_TagInt)
#define GB_Int_Div(x,y)		(((x)-GB_Word_TagInt) / ((y)/GB_Int_ShiftPow2) + GB_Word_TagInt)
#define GB_Int_Neg(x)		GB_Int_Sub(GB_Int0,x)

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Groups/categories/prefixes of/for instruction opcodes
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#define GB_Ins_Prefix(pre,sh)		(Cast(GB_Byte,pre) << (sh))

#define GB_Ins_PreLd				GB_Ins_Prefix(0x0,7)
#define GB_Ins_PreSt				GB_Ins_Prefix(0x4,5)
#define GB_Ins_PreArith				GB_Ins_Prefix(0x5,5)
#define GB_Ins_PreCall				GB_Ins_Prefix(0x18,3)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Instruction opcode inline operands
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
/* Location variant: load source, store destination, extensive variant */
#define GB_InsOp_LocE_TOS			0x0
#define GB_InsOp_LocE_Reg			0x1
#define GB_InsOp_LocE_Imm			0x2
#define GB_InsOp_LocE_PC			0x3

/* Location variant: load destination, store source, brief variant */
#define GB_InsOp_LocB_TOS			0x0
#define GB_InsOp_LocB_Reg			0x1

/* Location variant: operator destination */
#define GB_InsOp_LocO_TOS			0x0
#define GB_InsOp_LocO_Reg			0x1
#define GB_InsOp_LocO_SP			0x2
#define GB_InsOp_LocO_PC			0x3

/* Operator kind/type */
#define GB_InsOp_TyOp_Add			0x0
#define GB_InsOp_TyOp_Sub			0x1
#define GB_InsOp_TyOp_Mul			0x2
#define GB_InsOp_TyOp_Div			0x3

/* Operator data kind/type */
#define GB_InsOp_DataOp_I1			0x0
#define GB_InsOp_DataOp_I2			0x1
#define GB_InsOp_DataOp_F1			0x2
#define GB_InsOp_DataOp_F2			0x3

/* Immediate constant size */
#define GB_InsOp_ImmSz_08			0x0
#define GB_InsOp_ImmSz_16			0x1
#define GB_InsOp_ImmSz_32			0x2
#define GB_InsOp_ImmSz_64			0x3

/* Indirection level, deref times */
#define GB_InsOp_Deref0				0x0
#define GB_InsOp_Deref1				0x1
#define GB_InsOp_Deref2				0x2

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Instruction opcodes
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#define GB_InsLd(indLev,locB,locE,immSz)		(GB_Ins_PreLd | ((indLev) << 5) | ((locB) << 4) | ((locE) << 2) | ((immSz) << 0))
#define GB_InsCall(locB)						(GB_Ins_PreCall | ((0x0) << 1) | ((locB) << 0))
#define GB_InsTailCall(locB)					(GB_Ins_PreCall | ((0x1) << 1) | ((locB) << 0))
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Groups/categories/prefixes of/for instruction codes
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
extern void interpretLoop() ;
%%]
