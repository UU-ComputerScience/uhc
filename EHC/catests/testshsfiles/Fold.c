#include "rts.h"
#include "bc/interpreter.h"

 
static Word8 Fold_bytePool[] =
{ 0x46,0x6f,0x6c,0x64,0x2e,0x6c,0x67,0x6f,0x5f,0x40,0x55,0x4e,0x51,0x5f,0x40,0x32,0x33,0x00
, 0x46,0x6f,0x6c,0x64,0x2e,0x79,0x73,0x5f,0x40,0x55,0x4e,0x51,0x5f,0x40,0x32,0x35,0x00
, 0x46,0x6f,0x6c,0x64,0x2e,0x78,0x73,0x5f,0x40,0x55,0x4e,0x51,0x5f,0x40,0x32,0x39,0x5f,0x40,0x46,0x4c,0x44,0x5f,0x40,0x31,0x00
, 0x0f,0x04
, 0x46,0x6f,0x6c,0x64,0x2e,0x7a,0x5f,0x40,0x55,0x4e,0x51,0x5f,0x40,0x32,0x34,0x00
, 0x46,0x6f,0x6c,0x64,0x2e,0x66,0x6f,0x00
, 0x46,0x6f,0x6c,0x64,0x2e,0x74,0x36,0x00
, 0x46,0x6f,0x6c,0x64,0x2e,0x75,0x34,0x5f,0x33,0x37,0x5f,0x30,0x00
, 0x09,0x04
, 0x46,0x6f,0x6c,0x64,0x2e,0x75,0x35,0x5f,0x33,0x38,0x00
, 0x46,0x6f,0x6c,0x64,0x2e,0x75,0x34,0x5f,0x33,0x37,0x5f,0x30,0x5f,0x40,0x49,0x4e,0x4c,0x5f,0x40,0x34,0x00
, 0x46,0x6f,0x6c,0x64,0x2e,0x75,0x35,0x5f,0x33,0x36,0x00
, 0x46,0x6f,0x6c,0x64,0x2e,0x75,0x35,0x5f,0x33,0x34,0x00
, 0x46,0x6f,0x6c,0x64,0x2e,0x6d,0x61,0x69,0x6e,0x00
, 0x46,0x6f,0x6c,0x64,0x2e,0x75,0x35,0x5f,0x34,0x32,0x00
, 0x6d,0x61,0x69,0x6e,0x00
};
 
static GB_LinkChainResolvedInfo Fold_linkChainIndirections[] =
{};
 
static GCStackInfo Fold_gcStackInfos[] =
{ { 9
  , 2
  , &(Fold_bytePool[60])
  }
, { 6
  , 2
  , &(Fold_bytePool[107])
  }
};
 
static FunctionInfo Fold_functionInfos[] =
{ {64,FunctionInfoFlag_None,&(Fold_bytePool[0])}
, {16,FunctionInfoFlag_None,&(Fold_bytePool[78])}
, {40,FunctionInfoFlag_None,&(Fold_bytePool[86])}
, {40,FunctionInfoFlag_None,&(Fold_bytePool[109])}
, {8,FunctionInfoFlag_None,&(Fold_bytePool[141])}
, {8,FunctionInfoFlag_None,&(Fold_bytePool[152])}
, {8,FunctionInfoFlag_None,&(Fold_bytePool[163])}
, {8,FunctionInfoFlag_None,&(Fold_bytePool[173])}
, {8,FunctionInfoFlag_None,&(Fold_bytePool[184])}
};
 
static CallInfo Fold_callinfos[] =
{ MkCallInfoWith(2,&(Fold_bytePool[18]),FunctionInfo_Inx_None,FunctionInfo_Inx_None,NULL,NULL) 
, MkCallInfoWith(2,&(Fold_bytePool[35]),FunctionInfo_Inx_None,FunctionInfo_Inx_None,NULL,NULL) 
, MkCallInfoWith(11,&(Fold_bytePool[62]),FunctionInfo_Inx_None,FunctionInfo_Inx_None,NULL,NULL) 
, MkCallInfoWith(0,&(Fold_bytePool[0]),0,25,NULL,NULL) 
, MkCallInfoWith(5,&(Fold_bytePool[94]),FunctionInfo_Inx_None,FunctionInfo_Inx_None,NULL,NULL) 
, MkCallInfoWith(5,&(Fold_bytePool[120]),FunctionInfo_Inx_None,FunctionInfo_Inx_None,NULL,NULL) 
, MkCallInfoWith(0,&(Fold_bytePool[0]),3,164,NULL,NULL) 
, MkCallInfoWith(0,&(Fold_bytePool[0]),1,14,NULL,NULL) 
};
 
static GB_Byte Fold_bytecode[] =
{    0xfe,0xff
,    0xe1,0x02,0x00,0x00,0x00,0x00,0x00,0x00
,    0x20,0x20
,    0xe0,0x19,0x03,0x00,0x00,0x00,0x00,0x00,0x00
,    0x20,0x00
,    0xfc
,    0xf6,0x15,0x07,0x00,0x00,0x02,0x00,0x00,0x00
,    0x08,0x00,0x00,0x00,0x00,0x00,0x00,0x00
,    0x3a,0x00,0x00,0x00,0x00,0x00,0x00,0x00
,    0xee
,    0x20,0x08
,    0xe0,0x99,0x06,0x00,0x00,0x01,0x00,0x00,0x00
,    0x20,0x08
,    0x20,0x40
,    0x20,0x40
,    0x0b,0x01,0x00,0x00,0x40,0x04,0x00,0x00,0x00
,    0x08,0x20
,    0xec,0xc5,0x03,0x00,0x00,0x01,0x00,0x00,0x00
,    0x20,0x18
,    0x20,0x08
,    0x20,0x48
,    0xe6,0xcd,0x04,0x00,0x00,0x00,0x00,0x00,0x00
,    0xfe,0x00,0x00,0x18,0x18
,    0x20,0x28
,    0xfe,0x02,0x00,0x18,0x19,0x02,0x00,0x00,0x02,0x00,0x00,0x00
,    0xe1,0x03,0x00,0x00,0x01,0x00,0x00,0x00
,    0x20,0x20
,    0x20,0x20
,    0x20,0x20
,    0xe6,0x4d,0x03,0x00,0x00,0x00,0x00,0x00,0x00
,    0xfe,0x00,0x00,0x18,0x18
,    0x61,0x02,0x00,0x00,0x02,0x00,0x00,0x00
,    0xe7,0xe5,0x02,0x00,0x00,0x00,0x00,0x00,0x00
,    0x24,0x38
,    0xe7,0x25,0x03,0x00,0x00,0x00,0x00,0x00,0x00
,    0x25,0xf8,0x00
,    0xf0,0x59,0x04,0x00,0x00,0x03,0x00,0x00,0x00
,    0x20,0x18
,    0x68,0x01
,    0x08,0x02
,    0x20,0x18
,    0xe2,0x59,0x02,0x00,0x00,0x04,0x00,0x00,0x00
,    0xe7,0xe5,0x02,0x00,0x00,0x00,0x00,0x00,0x00
,    0x24,0x68
,    0xe7,0xa5,0x05,0x00,0x00,0x00,0x00,0x00,0x00
,    0x24,0x18
,    0x0b,0x01,0x00,0x00,0x00,0x03,0x00,0x00,0x00
,    0x08,0x18
,    0xec,0xc5,0x03,0x00,0x00,0x02,0x00,0x00,0x00
,    0x20,0x08
,    0x68,0x00
,    0x20,0x10
,    0xe6,0x4d,0x03,0x00,0x00,0x00,0x00,0x00,0x00
,    0xfe,0x00,0x00,0x18,0x08
,    0x61,0x02,0x00,0x00,0x03,0x00,0x00,0x00
,    0xe7,0xe5,0x02,0x00,0x00,0x00,0x00,0x00,0x00
,    0x24,0x38
,    0xe7,0x25,0x03,0x00,0x00,0x00,0x00,0x00,0x00
,    0x25,0xf8,0x00
,    0xf0,0x19,0x05,0x00,0x00,0x03,0x00,0x00,0x00
,    0x6a,0x80,0x96,0x98,0x00
,    0x68,0x01
,    0x08,0x02
,    0x20,0x18
,    0xe2,0x59,0x02,0x00,0x00,0x05,0x00,0x00,0x00
,    0xe7,0xe5,0x02,0x00,0x00,0x00,0x00,0x00,0x00
,    0x24,0x68
,    0xe7,0xa5,0x05,0x00,0x00,0x00,0x00,0x00,0x00
,    0x24,0x18
,    0x0b,0x01,0x00,0x00,0x00,0x03,0x00,0x00,0x00
,    0x08,0x18
,    0xec,0xc5,0x03,0x00,0x00,0x02,0x00,0x00,0x00
,    0x20,0x08
,    0x68,0x00
,    0x20,0x10
,    0xe6,0x4d,0x03,0x00,0x00,0x00,0x00,0x00,0x00
,    0xfe,0x00,0x00,0x18,0x00
,    0x61,0x02,0x00,0x00,0x04,0x00,0x00,0x00
,    0xe6,0xcd,0x02,0x00,0x00,0x03,0x00,0x00,0x00
,    0x68,0x64
,    0xe7,0x25,0x04,0x00,0x00,0x03,0x00,0x00,0x00
,    0x25,0x68,0x01
,    0xfe,0x00,0x00,0x10,0x00
,    0x61,0x02,0x00,0x00,0x05,0x00,0x00,0x00
,    0xe7,0xe5,0x02,0x00,0x00,0x00,0x00,0x00,0x00
,    0x24,0x68
,    0xe7,0x25,0x03,0x00,0x00,0x03,0x00,0x00,0x00
,    0x25,0xc8,0x01
,    0xf0,0x59,0x02,0x00,0x00,0x06,0x00,0x00,0x00
,    0xe6,0x4d,0x04,0x00,0x00,0x04,0x00,0x00,0x00
,    0x08,0x01
,    0x20,0x10
,    0xfe,0x04,0x00,0x18,0x00
,    0x61,0x02,0x00,0x00,0x06,0x00,0x00,0x00
,    0xe6,0x4d,0x02,0x00,0x00,0x05,0x00,0x00,0x00
,    0xe7,0x25,0x03,0x00,0x00,0x03,0x00,0x00,0x00
,    0x25,0xc0,0x00
,    0xe7,0xe5,0x03,0x00,0x00,0x04,0x00,0x00,0x00
,    0x24,0x58
,    0xfe,0x00,0x00,0x10,0x00
,    0xa1,0x03,0x00,0x00,0x07,0x00,0x00,0x00
,    0x68,0x00
,    0xf4,0x00,0x08,0x00
,    0x61,0x02,0x00,0x00,0x08,0x00,0x00,0x00
,    0xe6,0x4d,0x02,0x00,0x00,0x06,0x00,0x00,0x00
,    0xe7,0xe5,0x02,0x00,0x00,0x01,0x00,0x00,0x00
,    0x24,0x08
,    0xf0,0x59,0x02,0x00,0x00,0x07,0x00,0x00,0x00
,    0xe6,0x0d,0x00,0x00,0x00,0x07,0x00,0x00,0x00
,    0x08,0x01
,    0x20,0x10
,    0xfe,0x04,0x00,0x18,0x00
};
GB_ByteCodeModule Fold_bytecodeModule =
  { "Fold"
  , NULL
  , 0
  , Fold_bytecode
  , 598
  } ;
 
static GB_Word Fold_constants[] =
{};
 
static HalfWord Fold_cafGlEntryIndices[] =
{ 3
, 4
, 5
, 6
, 7
, 8
};
 
static GB_BytePtr Fold_globalEntries[] =
{ &(Fold_bytecode[10])
, &(Fold_bytecode[129])
, &(Fold_bytecode[157])
, &(Fold_bytecode[276])
, &(Fold_bytecode[398])
, &(Fold_bytecode[434])
, &(Fold_bytecode[492])
, &(Fold_bytecode[537])
, &(Fold_bytecode[551])
};
GB_NodePtr Fold_expNode ;
static int Fold_expNode_offs[] =
  {} ;
int Fold_expNode_size = 0 ;

static GB_ImpModEntry Fold_impMods[] =
         { { "UHC.BaseOne"
           , 0 
           }
         , { "UHC.Run"
           , 0 
           }
         , { "Prelude"
           , 0 
           }
         , { "UHC.BaseTwo"
           , 0 
           }
         , { "System.IO"
           , 0 
           }
         , { "UHC.BaseZero"
           , 0 
           }
         } ;

void Fold_initModule(GB_ModEntry* modTbl, Word modTblInx) {
  gb_InitTables( Fold_bytecode
               , 598
               , Fold_cafGlEntryIndices
               , 6
               , Fold_globalEntries
               , 9
               , Fold_constants
               , Fold_gcStackInfos
               , Fold_linkChainIndirections
               , Fold_callinfos
               , 8
               , Fold_functionInfos
               , 9
               , Fold_bytePool
               , 2
               , Fold_impMods
               , 6
               , &(Fold_expNode)
               , Fold_expNode_size
               , Fold_expNode_offs
               , modTbl
               , modTblInx
               ) ;
}

static GB_BytePtr* Fold_mainEntryPtr = &(Fold_globalEntries[8]) ;

 
extern void UHC_BaseZero_initModule(GB_ModEntry*,Word) ;
extern GB_NodePtr UHC_BaseZero_expNode ;
extern int UHC_BaseZero_expNode_size ;
extern GB_ByteCodeModule UHC_BaseZero_bytecodeModule ;
extern void UHC_BaseOne_initModule(GB_ModEntry*,Word) ;
extern GB_NodePtr UHC_BaseOne_expNode ;
extern int UHC_BaseOne_expNode_size ;
extern GB_ByteCodeModule UHC_BaseOne_bytecodeModule ;
extern void UHC_BaseTwo_initModule(GB_ModEntry*,Word) ;
extern GB_NodePtr UHC_BaseTwo_expNode ;
extern int UHC_BaseTwo_expNode_size ;
extern GB_ByteCodeModule UHC_BaseTwo_bytecodeModule ;
extern void UHC_BaseThree_initModule(GB_ModEntry*,Word) ;
extern GB_NodePtr UHC_BaseThree_expNode ;
extern int UHC_BaseThree_expNode_size ;
extern GB_ByteCodeModule UHC_BaseThree_bytecodeModule ;
extern void UHC_BaseFour_initModule(GB_ModEntry*,Word) ;
extern GB_NodePtr UHC_BaseFour_expNode ;
extern int UHC_BaseFour_expNode_size ;
extern GB_ByteCodeModule UHC_BaseFour_bytecodeModule ;
extern void UHC_BaseFive_initModule(GB_ModEntry*,Word) ;
extern GB_NodePtr UHC_BaseFive_expNode ;
extern int UHC_BaseFive_expNode_size ;
extern GB_ByteCodeModule UHC_BaseFive_bytecodeModule ;
extern void UHC_Base_initModule(GB_ModEntry*,Word) ;
extern GB_NodePtr UHC_Base_expNode ;
extern int UHC_Base_expNode_size ;
extern GB_ByteCodeModule UHC_Base_bytecodeModule ;
extern void UHC_StackTrace_initModule(GB_ModEntry*,Word) ;
extern GB_NodePtr UHC_StackTrace_expNode ;
extern int UHC_StackTrace_expNode_size ;
extern GB_ByteCodeModule UHC_StackTrace_bytecodeModule ;
extern void UHC_Enum_initModule(GB_ModEntry*,Word) ;
extern GB_NodePtr UHC_Enum_expNode ;
extern int UHC_Enum_expNode_size ;
extern GB_ByteCodeModule UHC_Enum_bytecodeModule ;
extern void UHC_MutVar_initModule(GB_ModEntry*,Word) ;
extern GB_NodePtr UHC_MutVar_expNode ;
extern int UHC_MutVar_expNode_size ;
extern GB_ByteCodeModule UHC_MutVar_bytecodeModule ;
extern void UHC_Read_initModule(GB_ModEntry*,Word) ;
extern GB_NodePtr UHC_Read_expNode ;
extern int UHC_Read_expNode_size ;
extern GB_ByteCodeModule UHC_Read_bytecodeModule ;
extern void UHC_Real_initModule(GB_ModEntry*,Word) ;
extern GB_NodePtr UHC_Real_expNode ;
extern int UHC_Real_expNode_size ;
extern GB_ByteCodeModule UHC_Real_bytecodeModule ;
extern void UHC_ST_initModule(GB_ModEntry*,Word) ;
extern GB_NodePtr UHC_ST_expNode ;
extern int UHC_ST_expNode_size ;
extern GB_ByteCodeModule UHC_ST_bytecodeModule ;
extern void UHC_Generics_Tuple_initModule(GB_ModEntry*,Word) ;
extern GB_NodePtr UHC_Generics_Tuple_expNode ;
extern int UHC_Generics_Tuple_expNode_size ;
extern GB_ByteCodeModule UHC_Generics_Tuple_bytecodeModule ;
extern void UHC_Generics_initModule(GB_ModEntry*,Word) ;
extern GB_NodePtr UHC_Generics_expNode ;
extern int UHC_Generics_expNode_size ;
extern GB_ByteCodeModule UHC_Generics_bytecodeModule ;
extern void UHC_Eq_initModule(GB_ModEntry*,Word) ;
extern GB_NodePtr UHC_Eq_expNode ;
extern int UHC_Eq_expNode_size ;
extern GB_ByteCodeModule UHC_Eq_bytecodeModule ;
extern void UHC_Ord_initModule(GB_ModEntry*,Word) ;
extern GB_NodePtr UHC_Ord_expNode ;
extern int UHC_Ord_expNode_size ;
extern GB_ByteCodeModule UHC_Ord_bytecodeModule ;
extern void UHC_Bits_initModule(GB_ModEntry*,Word) ;
extern GB_NodePtr UHC_Bits_expNode ;
extern int UHC_Bits_expNode_size ;
extern GB_ByteCodeModule UHC_Bits_bytecodeModule ;
extern void Data_Maybe_initModule(GB_ModEntry*,Word) ;
extern GB_NodePtr Data_Maybe_expNode ;
extern int Data_Maybe_expNode_size ;
extern GB_ByteCodeModule Data_Maybe_bytecodeModule ;
extern void Unsafe_Coerce_initModule(GB_ModEntry*,Word) ;
extern GB_NodePtr Unsafe_Coerce_expNode ;
extern int Unsafe_Coerce_expNode_size ;
extern GB_ByteCodeModule Unsafe_Coerce_bytecodeModule ;
extern void UHC_Types_initModule(GB_ModEntry*,Word) ;
extern GB_NodePtr UHC_Types_expNode ;
extern int UHC_Types_expNode_size ;
extern GB_ByteCodeModule UHC_Types_bytecodeModule ;
extern void UHC_Prims_initModule(GB_ModEntry*,Word) ;
extern GB_NodePtr UHC_Prims_expNode ;
extern int UHC_Prims_expNode_size ;
extern GB_ByteCodeModule UHC_Prims_bytecodeModule ;
extern void Data_Bits_initModule(GB_ModEntry*,Word) ;
extern GB_NodePtr Data_Bits_expNode ;
extern int Data_Bits_expNode_size ;
extern GB_ByteCodeModule Data_Bits_bytecodeModule ;
extern void UHC_Bounded_initModule(GB_ModEntry*,Word) ;
extern GB_NodePtr UHC_Bounded_expNode ;
extern int UHC_Bounded_expNode_size ;
extern GB_ByteCodeModule UHC_Bounded_bytecodeModule ;
extern void UHC_Char_initModule(GB_ModEntry*,Word) ;
extern GB_NodePtr UHC_Char_expNode ;
extern int UHC_Char_expNode_size ;
extern GB_ByteCodeModule UHC_Char_bytecodeModule ;
extern void UHC_STRef_initModule(GB_ModEntry*,Word) ;
extern GB_NodePtr UHC_STRef_expNode ;
extern int UHC_STRef_expNode_size ;
extern GB_ByteCodeModule UHC_STRef_bytecodeModule ;
extern void UHC_Float_initModule(GB_ModEntry*,Word) ;
extern GB_NodePtr UHC_Float_expNode ;
extern int UHC_Float_expNode_size ;
extern GB_ByteCodeModule UHC_Float_bytecodeModule ;
extern void UHC_Show_initModule(GB_ModEntry*,Word) ;
extern GB_NodePtr UHC_Show_expNode ;
extern int UHC_Show_expNode_size ;
extern GB_ByteCodeModule UHC_Show_bytecodeModule ;
extern void UHC_Int_initModule(GB_ModEntry*,Word) ;
extern GB_NodePtr UHC_Int_expNode ;
extern int UHC_Int_expNode_size ;
extern GB_ByteCodeModule UHC_Int_bytecodeModule ;
extern void Data_Char_initModule(GB_ModEntry*,Word) ;
extern GB_NodePtr Data_Char_expNode ;
extern int Data_Char_expNode_size ;
extern GB_ByteCodeModule Data_Char_bytecodeModule ;
extern void Data_List_initModule(GB_ModEntry*,Word) ;
extern GB_NodePtr Data_List_expNode ;
extern int Data_List_expNode_size ;
extern GB_ByteCodeModule Data_List_bytecodeModule ;
extern void Data_Either_initModule(GB_ModEntry*,Word) ;
extern GB_NodePtr Data_Either_expNode ;
extern int Data_Either_expNode_size ;
extern GB_ByteCodeModule Data_Either_bytecodeModule ;
extern void UHC_Ptr_initModule(GB_ModEntry*,Word) ;
extern GB_NodePtr UHC_Ptr_expNode ;
extern int UHC_Ptr_expNode_size ;
extern GB_ByteCodeModule UHC_Ptr_bytecodeModule ;
extern void UHC_WeakPtr_initModule(GB_ModEntry*,Word) ;
extern GB_NodePtr UHC_WeakPtr_expNode ;
extern int UHC_WeakPtr_expNode_size ;
extern GB_ByteCodeModule UHC_WeakPtr_bytecodeModule ;
extern void UHC_StablePtr_initModule(GB_ModEntry*,Word) ;
extern GB_NodePtr UHC_StablePtr_expNode ;
extern int UHC_StablePtr_expNode_size ;
extern GB_ByteCodeModule UHC_StablePtr_bytecodeModule ;
extern void Foreign_StablePtr_initModule(GB_ModEntry*,Word) ;
extern GB_NodePtr Foreign_StablePtr_expNode ;
extern int Foreign_StablePtr_expNode_size ;
extern GB_ByteCodeModule Foreign_StablePtr_bytecodeModule ;
extern void UHC_Storable_initModule(GB_ModEntry*,Word) ;
extern GB_NodePtr UHC_Storable_expNode ;
extern int UHC_Storable_expNode_size ;
extern GB_ByteCodeModule UHC_Storable_bytecodeModule ;
extern void UHC_ByteArray_initModule(GB_ModEntry*,Word) ;
extern GB_NodePtr UHC_ByteArray_expNode ;
extern int UHC_ByteArray_expNode_size ;
extern GB_ByteCodeModule UHC_ByteArray_bytecodeModule ;
extern void UHC_IOBase_initModule(GB_ModEntry*,Word) ;
extern GB_NodePtr UHC_IOBase_expNode ;
extern int UHC_IOBase_expNode_size ;
extern GB_ByteCodeModule UHC_IOBase_bytecodeModule ;
extern void UHC_OldException_initModule(GB_ModEntry*,Word) ;
extern GB_NodePtr UHC_OldException_expNode ;
extern int UHC_OldException_expNode_size ;
extern GB_ByteCodeModule UHC_OldException_bytecodeModule ;
extern void Data_IORef_initModule(GB_ModEntry*,Word) ;
extern GB_NodePtr Data_IORef_expNode ;
extern int Data_IORef_expNode_size ;
extern GB_ByteCodeModule Data_IORef_bytecodeModule ;
extern void UHC_Word_initModule(GB_ModEntry*,Word) ;
extern GB_NodePtr UHC_Word_expNode ;
extern int UHC_Word_expNode_size ;
extern GB_ByteCodeModule UHC_Word_bytecodeModule ;
extern void Data_Word_initModule(GB_ModEntry*,Word) ;
extern GB_NodePtr Data_Word_expNode ;
extern int Data_Word_expNode_size ;
extern GB_ByteCodeModule Data_Word_bytecodeModule ;
extern void Control_Monad_initModule(GB_ModEntry*,Word) ;
extern GB_NodePtr Control_Monad_expNode ;
extern int Control_Monad_expNode_size ;
extern GB_ByteCodeModule Control_Monad_bytecodeModule ;
extern void System_IO_Unsafe_initModule(GB_ModEntry*,Word) ;
extern GB_NodePtr System_IO_Unsafe_expNode ;
extern int System_IO_Unsafe_expNode_size ;
extern GB_ByteCodeModule System_IO_Unsafe_bytecodeModule ;
extern void Data_Int_initModule(GB_ModEntry*,Word) ;
extern GB_NodePtr Data_Int_expNode ;
extern int Data_Int_expNode_size ;
extern GB_ByteCodeModule Data_Int_bytecodeModule ;
extern void Data_Typeable_initModule(GB_ModEntry*,Word) ;
extern GB_NodePtr Data_Typeable_expNode ;
extern int Data_Typeable_expNode_size ;
extern GB_ByteCodeModule Data_Typeable_bytecodeModule ;
extern void UHC_Weak_initModule(GB_ModEntry*,Word) ;
extern GB_NodePtr UHC_Weak_expNode ;
extern int UHC_Weak_expNode_size ;
extern GB_ByteCodeModule UHC_Weak_bytecodeModule ;
extern void UHC_MVar_initModule(GB_ModEntry*,Word) ;
extern GB_NodePtr UHC_MVar_expNode ;
extern int UHC_MVar_expNode_size ;
extern GB_ByteCodeModule UHC_MVar_bytecodeModule ;
extern void Foreign_Storable_initModule(GB_ModEntry*,Word) ;
extern GB_NodePtr Foreign_Storable_expNode ;
extern int Foreign_Storable_expNode_size ;
extern GB_ByteCodeModule Foreign_Storable_bytecodeModule ;
extern void Foreign_C_TypesZero_initModule(GB_ModEntry*,Word) ;
extern GB_NodePtr Foreign_C_TypesZero_expNode ;
extern int Foreign_C_TypesZero_expNode_size ;
extern GB_ByteCodeModule Foreign_C_TypesZero_bytecodeModule ;
extern void Foreign_C_Types_initModule(GB_ModEntry*,Word) ;
extern GB_NodePtr Foreign_C_Types_expNode ;
extern int Foreign_C_Types_expNode_size ;
extern GB_ByteCodeModule Foreign_C_Types_bytecodeModule ;
extern void Foreign_Ptr_initModule(GB_ModEntry*,Word) ;
extern GB_NodePtr Foreign_Ptr_expNode ;
extern int Foreign_Ptr_expNode_size ;
extern GB_ByteCodeModule Foreign_Ptr_bytecodeModule ;
extern void Foreign_Marshal_Error_initModule(GB_ModEntry*,Word) ;
extern GB_NodePtr Foreign_Marshal_Error_expNode ;
extern int Foreign_Marshal_Error_expNode_size ;
extern GB_ByteCodeModule Foreign_Marshal_Error_bytecodeModule ;
extern void UHC_ForeignPtr_initModule(GB_ModEntry*,Word) ;
extern GB_NodePtr UHC_ForeignPtr_expNode ;
extern int UHC_ForeignPtr_expNode_size ;
extern GB_ByteCodeModule UHC_ForeignPtr_bytecodeModule ;
extern void Foreign_ForeignPtr_initModule(GB_ModEntry*,Word) ;
extern GB_NodePtr Foreign_ForeignPtr_expNode ;
extern int Foreign_ForeignPtr_expNode_size ;
extern GB_ByteCodeModule Foreign_ForeignPtr_bytecodeModule ;
extern void Foreign_Marshal_Alloc_initModule(GB_ModEntry*,Word) ;
extern GB_NodePtr Foreign_Marshal_Alloc_expNode ;
extern int Foreign_Marshal_Alloc_expNode_size ;
extern GB_ByteCodeModule Foreign_Marshal_Alloc_bytecodeModule ;
extern void Foreign_Marshal_Utils_initModule(GB_ModEntry*,Word) ;
extern GB_NodePtr Foreign_Marshal_Utils_expNode ;
extern int Foreign_Marshal_Utils_expNode_size ;
extern GB_ByteCodeModule Foreign_Marshal_Utils_bytecodeModule ;
extern void Foreign_Marshal_Array_initModule(GB_ModEntry*,Word) ;
extern GB_NodePtr Foreign_Marshal_Array_expNode ;
extern int Foreign_Marshal_Array_expNode_size ;
extern GB_ByteCodeModule Foreign_Marshal_Array_bytecodeModule ;
extern void Foreign_C_String_initModule(GB_ModEntry*,Word) ;
extern GB_NodePtr Foreign_C_String_expNode ;
extern int Foreign_C_String_expNode_size ;
extern GB_ByteCodeModule Foreign_C_String_bytecodeModule ;
extern void Foreign_C_Error_initModule(GB_ModEntry*,Word) ;
extern GB_NodePtr Foreign_C_Error_expNode ;
extern int Foreign_C_Error_expNode_size ;
extern GB_ByteCodeModule Foreign_C_Error_bytecodeModule ;
extern void Foreign_C_initModule(GB_ModEntry*,Word) ;
extern GB_NodePtr Foreign_C_expNode ;
extern int Foreign_C_expNode_size ;
extern GB_ByteCodeModule Foreign_C_bytecodeModule ;
extern void Foreign_Marshal_Pool_initModule(GB_ModEntry*,Word) ;
extern GB_NodePtr Foreign_Marshal_Pool_expNode ;
extern int Foreign_Marshal_Pool_expNode_size ;
extern GB_ByteCodeModule Foreign_Marshal_Pool_bytecodeModule ;
extern void Foreign_Marshal_initModule(GB_ModEntry*,Word) ;
extern GB_NodePtr Foreign_Marshal_expNode ;
extern int Foreign_Marshal_expNode_size ;
extern GB_ByteCodeModule Foreign_Marshal_bytecodeModule ;
extern void Foreign_initModule(GB_ModEntry*,Word) ;
extern GB_NodePtr Foreign_expNode ;
extern int Foreign_expNode_size ;
extern GB_ByteCodeModule Foreign_bytecodeModule ;
extern void System_Posix_Types_initModule(GB_ModEntry*,Word) ;
extern GB_NodePtr System_Posix_Types_expNode ;
extern int System_Posix_Types_expNode_size ;
extern GB_ByteCodeModule System_Posix_Types_bytecodeModule ;
extern void UHC_Conc_initModule(GB_ModEntry*,Word) ;
extern GB_NodePtr UHC_Conc_expNode ;
extern int UHC_Conc_expNode_size ;
extern GB_ByteCodeModule UHC_Conc_bytecodeModule ;
extern void System_Posix_Internals_initModule(GB_ModEntry*,Word) ;
extern GB_NodePtr System_Posix_Internals_expNode ;
extern int System_Posix_Internals_expNode_size ;
extern GB_ByteCodeModule System_Posix_Internals_bytecodeModule ;
extern void System_IO_Error_initModule(GB_ModEntry*,Word) ;
extern GB_NodePtr System_IO_Error_expNode ;
extern int System_IO_Error_expNode_size ;
extern GB_ByteCodeModule System_IO_Error_bytecodeModule ;
extern void UHC_Handle_initModule(GB_ModEntry*,Word) ;
extern GB_NodePtr UHC_Handle_expNode ;
extern int UHC_Handle_expNode_size ;
extern GB_ByteCodeModule UHC_Handle_bytecodeModule ;
extern void UHC_IO_initModule(GB_ModEntry*,Word) ;
extern GB_NodePtr UHC_IO_expNode ;
extern int UHC_IO_expNode_size ;
extern GB_ByteCodeModule UHC_IO_bytecodeModule ;
extern void System_IO_Fix_initModule(GB_ModEntry*,Word) ;
extern GB_NodePtr System_IO_Fix_expNode ;
extern int System_IO_Fix_expNode_size ;
extern GB_ByteCodeModule System_IO_Fix_bytecodeModule ;
extern void System_IO_initModule(GB_ModEntry*,Word) ;
extern GB_NodePtr System_IO_expNode ;
extern int System_IO_expNode_size ;
extern GB_ByteCodeModule System_IO_bytecodeModule ;
extern void UHC_Run_initModule(GB_ModEntry*,Word) ;
extern GB_NodePtr UHC_Run_expNode ;
extern int UHC_Run_expNode_size ;
extern GB_ByteCodeModule UHC_Run_bytecodeModule ;
extern void Prelude_initModule(GB_ModEntry*,Word) ;
extern GB_NodePtr Prelude_expNode ;
extern int Prelude_expNode_size ;
extern GB_ByteCodeModule Prelude_bytecodeModule ;
extern void Fold_initModule(GB_ModEntry*,Word) ;
extern GB_NodePtr Fold_expNode ;
extern int Fold_expNode_size ;
extern GB_ByteCodeModule Fold_bytecodeModule ;
static GB_ModEntry Fold_moduleEntries[] =
         { { "UHC.BaseZero"
           , &(UHC_BaseZero_expNode)
           , &(UHC_BaseZero_bytecodeModule)
           , Fold_functionInfos
           }
         , { "UHC.BaseOne"
           , &(UHC_BaseOne_expNode)
           , &(UHC_BaseOne_bytecodeModule)
           , Fold_functionInfos
           }
         , { "UHC.BaseTwo"
           , &(UHC_BaseTwo_expNode)
           , &(UHC_BaseTwo_bytecodeModule)
           , Fold_functionInfos
           }
         , { "UHC.BaseThree"
           , &(UHC_BaseThree_expNode)
           , &(UHC_BaseThree_bytecodeModule)
           , Fold_functionInfos
           }
         , { "UHC.BaseFour"
           , &(UHC_BaseFour_expNode)
           , &(UHC_BaseFour_bytecodeModule)
           , Fold_functionInfos
           }
         , { "UHC.BaseFive"
           , &(UHC_BaseFive_expNode)
           , &(UHC_BaseFive_bytecodeModule)
           , Fold_functionInfos
           }
         , { "UHC.Base"
           , &(UHC_Base_expNode)
           , &(UHC_Base_bytecodeModule)
           , Fold_functionInfos
           }
         , { "UHC.StackTrace"
           , &(UHC_StackTrace_expNode)
           , &(UHC_StackTrace_bytecodeModule)
           , Fold_functionInfos
           }
         , { "UHC.Enum"
           , &(UHC_Enum_expNode)
           , &(UHC_Enum_bytecodeModule)
           , Fold_functionInfos
           }
         , { "UHC.MutVar"
           , &(UHC_MutVar_expNode)
           , &(UHC_MutVar_bytecodeModule)
           , Fold_functionInfos
           }
         , { "UHC.Read"
           , &(UHC_Read_expNode)
           , &(UHC_Read_bytecodeModule)
           , Fold_functionInfos
           }
         , { "UHC.Real"
           , &(UHC_Real_expNode)
           , &(UHC_Real_bytecodeModule)
           , Fold_functionInfos
           }
         , { "UHC.ST"
           , &(UHC_ST_expNode)
           , &(UHC_ST_bytecodeModule)
           , Fold_functionInfos
           }
         , { "UHC.Generics.Tuple"
           , &(UHC_Generics_Tuple_expNode)
           , &(UHC_Generics_Tuple_bytecodeModule)
           , Fold_functionInfos
           }
         , { "UHC.Generics"
           , &(UHC_Generics_expNode)
           , &(UHC_Generics_bytecodeModule)
           , Fold_functionInfos
           }
         , { "UHC.Eq"
           , &(UHC_Eq_expNode)
           , &(UHC_Eq_bytecodeModule)
           , Fold_functionInfos
           }
         , { "UHC.Ord"
           , &(UHC_Ord_expNode)
           , &(UHC_Ord_bytecodeModule)
           , Fold_functionInfos
           }
         , { "UHC.Bits"
           , &(UHC_Bits_expNode)
           , &(UHC_Bits_bytecodeModule)
           , Fold_functionInfos
           }
         , { "Data.Maybe"
           , &(Data_Maybe_expNode)
           , &(Data_Maybe_bytecodeModule)
           , Fold_functionInfos
           }
         , { "Unsafe.Coerce"
           , &(Unsafe_Coerce_expNode)
           , &(Unsafe_Coerce_bytecodeModule)
           , Fold_functionInfos
           }
         , { "UHC.Types"
           , &(UHC_Types_expNode)
           , &(UHC_Types_bytecodeModule)
           , Fold_functionInfos
           }
         , { "UHC.Prims"
           , &(UHC_Prims_expNode)
           , &(UHC_Prims_bytecodeModule)
           , Fold_functionInfos
           }
         , { "Data.Bits"
           , &(Data_Bits_expNode)
           , &(Data_Bits_bytecodeModule)
           , Fold_functionInfos
           }
         , { "UHC.Bounded"
           , &(UHC_Bounded_expNode)
           , &(UHC_Bounded_bytecodeModule)
           , Fold_functionInfos
           }
         , { "UHC.Char"
           , &(UHC_Char_expNode)
           , &(UHC_Char_bytecodeModule)
           , Fold_functionInfos
           }
         , { "UHC.STRef"
           , &(UHC_STRef_expNode)
           , &(UHC_STRef_bytecodeModule)
           , Fold_functionInfos
           }
         , { "UHC.Float"
           , &(UHC_Float_expNode)
           , &(UHC_Float_bytecodeModule)
           , Fold_functionInfos
           }
         , { "UHC.Show"
           , &(UHC_Show_expNode)
           , &(UHC_Show_bytecodeModule)
           , Fold_functionInfos
           }
         , { "UHC.Int"
           , &(UHC_Int_expNode)
           , &(UHC_Int_bytecodeModule)
           , Fold_functionInfos
           }
         , { "Data.Char"
           , &(Data_Char_expNode)
           , &(Data_Char_bytecodeModule)
           , Fold_functionInfos
           }
         , { "Data.List"
           , &(Data_List_expNode)
           , &(Data_List_bytecodeModule)
           , Fold_functionInfos
           }
         , { "Data.Either"
           , &(Data_Either_expNode)
           , &(Data_Either_bytecodeModule)
           , Fold_functionInfos
           }
         , { "UHC.Ptr"
           , &(UHC_Ptr_expNode)
           , &(UHC_Ptr_bytecodeModule)
           , Fold_functionInfos
           }
         , { "UHC.WeakPtr"
           , &(UHC_WeakPtr_expNode)
           , &(UHC_WeakPtr_bytecodeModule)
           , Fold_functionInfos
           }
         , { "UHC.StablePtr"
           , &(UHC_StablePtr_expNode)
           , &(UHC_StablePtr_bytecodeModule)
           , Fold_functionInfos
           }
         , { "Foreign.StablePtr"
           , &(Foreign_StablePtr_expNode)
           , &(Foreign_StablePtr_bytecodeModule)
           , Fold_functionInfos
           }
         , { "UHC.Storable"
           , &(UHC_Storable_expNode)
           , &(UHC_Storable_bytecodeModule)
           , Fold_functionInfos
           }
         , { "UHC.ByteArray"
           , &(UHC_ByteArray_expNode)
           , &(UHC_ByteArray_bytecodeModule)
           , Fold_functionInfos
           }
         , { "UHC.IOBase"
           , &(UHC_IOBase_expNode)
           , &(UHC_IOBase_bytecodeModule)
           , Fold_functionInfos
           }
         , { "UHC.OldException"
           , &(UHC_OldException_expNode)
           , &(UHC_OldException_bytecodeModule)
           , Fold_functionInfos
           }
         , { "Data.IORef"
           , &(Data_IORef_expNode)
           , &(Data_IORef_bytecodeModule)
           , Fold_functionInfos
           }
         , { "UHC.Word"
           , &(UHC_Word_expNode)
           , &(UHC_Word_bytecodeModule)
           , Fold_functionInfos
           }
         , { "Data.Word"
           , &(Data_Word_expNode)
           , &(Data_Word_bytecodeModule)
           , Fold_functionInfos
           }
         , { "Control.Monad"
           , &(Control_Monad_expNode)
           , &(Control_Monad_bytecodeModule)
           , Fold_functionInfos
           }
         , { "System.IO.Unsafe"
           , &(System_IO_Unsafe_expNode)
           , &(System_IO_Unsafe_bytecodeModule)
           , Fold_functionInfos
           }
         , { "Data.Int"
           , &(Data_Int_expNode)
           , &(Data_Int_bytecodeModule)
           , Fold_functionInfos
           }
         , { "Data.Typeable"
           , &(Data_Typeable_expNode)
           , &(Data_Typeable_bytecodeModule)
           , Fold_functionInfos
           }
         , { "UHC.Weak"
           , &(UHC_Weak_expNode)
           , &(UHC_Weak_bytecodeModule)
           , Fold_functionInfos
           }
         , { "UHC.MVar"
           , &(UHC_MVar_expNode)
           , &(UHC_MVar_bytecodeModule)
           , Fold_functionInfos
           }
         , { "Foreign.Storable"
           , &(Foreign_Storable_expNode)
           , &(Foreign_Storable_bytecodeModule)
           , Fold_functionInfos
           }
         , { "Foreign.C.TypesZero"
           , &(Foreign_C_TypesZero_expNode)
           , &(Foreign_C_TypesZero_bytecodeModule)
           , Fold_functionInfos
           }
         , { "Foreign.C.Types"
           , &(Foreign_C_Types_expNode)
           , &(Foreign_C_Types_bytecodeModule)
           , Fold_functionInfos
           }
         , { "Foreign.Ptr"
           , &(Foreign_Ptr_expNode)
           , &(Foreign_Ptr_bytecodeModule)
           , Fold_functionInfos
           }
         , { "Foreign.Marshal.Error"
           , &(Foreign_Marshal_Error_expNode)
           , &(Foreign_Marshal_Error_bytecodeModule)
           , Fold_functionInfos
           }
         , { "UHC.ForeignPtr"
           , &(UHC_ForeignPtr_expNode)
           , &(UHC_ForeignPtr_bytecodeModule)
           , Fold_functionInfos
           }
         , { "Foreign.ForeignPtr"
           , &(Foreign_ForeignPtr_expNode)
           , &(Foreign_ForeignPtr_bytecodeModule)
           , Fold_functionInfos
           }
         , { "Foreign.Marshal.Alloc"
           , &(Foreign_Marshal_Alloc_expNode)
           , &(Foreign_Marshal_Alloc_bytecodeModule)
           , Fold_functionInfos
           }
         , { "Foreign.Marshal.Utils"
           , &(Foreign_Marshal_Utils_expNode)
           , &(Foreign_Marshal_Utils_bytecodeModule)
           , Fold_functionInfos
           }
         , { "Foreign.Marshal.Array"
           , &(Foreign_Marshal_Array_expNode)
           , &(Foreign_Marshal_Array_bytecodeModule)
           , Fold_functionInfos
           }
         , { "Foreign.C.String"
           , &(Foreign_C_String_expNode)
           , &(Foreign_C_String_bytecodeModule)
           , Fold_functionInfos
           }
         , { "Foreign.C.Error"
           , &(Foreign_C_Error_expNode)
           , &(Foreign_C_Error_bytecodeModule)
           , Fold_functionInfos
           }
         , { "Foreign.C"
           , &(Foreign_C_expNode)
           , &(Foreign_C_bytecodeModule)
           , Fold_functionInfos
           }
         , { "Foreign.Marshal.Pool"
           , &(Foreign_Marshal_Pool_expNode)
           , &(Foreign_Marshal_Pool_bytecodeModule)
           , Fold_functionInfos
           }
         , { "Foreign.Marshal"
           , &(Foreign_Marshal_expNode)
           , &(Foreign_Marshal_bytecodeModule)
           , Fold_functionInfos
           }
         , { "Foreign"
           , &(Foreign_expNode)
           , &(Foreign_bytecodeModule)
           , Fold_functionInfos
           }
         , { "System.Posix.Types"
           , &(System_Posix_Types_expNode)
           , &(System_Posix_Types_bytecodeModule)
           , Fold_functionInfos
           }
         , { "UHC.Conc"
           , &(UHC_Conc_expNode)
           , &(UHC_Conc_bytecodeModule)
           , Fold_functionInfos
           }
         , { "System.Posix.Internals"
           , &(System_Posix_Internals_expNode)
           , &(System_Posix_Internals_bytecodeModule)
           , Fold_functionInfos
           }
         , { "System.IO.Error"
           , &(System_IO_Error_expNode)
           , &(System_IO_Error_bytecodeModule)
           , Fold_functionInfos
           }
         , { "UHC.Handle"
           , &(UHC_Handle_expNode)
           , &(UHC_Handle_bytecodeModule)
           , Fold_functionInfos
           }
         , { "UHC.IO"
           , &(UHC_IO_expNode)
           , &(UHC_IO_bytecodeModule)
           , Fold_functionInfos
           }
         , { "System.IO.Fix"
           , &(System_IO_Fix_expNode)
           , &(System_IO_Fix_bytecodeModule)
           , Fold_functionInfos
           }
         , { "System.IO"
           , &(System_IO_expNode)
           , &(System_IO_bytecodeModule)
           , Fold_functionInfos
           }
         , { "UHC.Run"
           , &(UHC_Run_expNode)
           , &(UHC_Run_bytecodeModule)
           , Fold_functionInfos
           }
         , { "Prelude"
           , &(Prelude_expNode)
           , &(Prelude_bytecodeModule)
           , Fold_functionInfos
           }
         , { "Fold"
           , &(Fold_expNode)
           , &(Fold_bytecodeModule)
           , Fold_functionInfos
           }
         , { NULL
           , NULL
           , NULL
           , NULL
           }
         } ;

int main(int argc, char** argv) {
  int nRtsOpts = 0 ;
  main_GB_Init1( argc, argv, &nRtsOpts ) ;
  gb_Opt_TraceSteps =
   False ;
  gb_Opt_Info =
   0 ;
  argc -=
   nRtsOpts ;
  argv +=
   nRtsOpts ;
  GB_MkExpNodeIn( UHC_BaseZero_expNode, UHC_BaseZero_expNode_size ) ;
  GB_MkExpNodeIn( UHC_BaseOne_expNode, UHC_BaseOne_expNode_size ) ;
  GB_MkExpNodeIn( UHC_BaseTwo_expNode, UHC_BaseTwo_expNode_size ) ;
  GB_MkExpNodeIn( UHC_BaseThree_expNode, UHC_BaseThree_expNode_size ) ;
  GB_MkExpNodeIn( UHC_BaseFour_expNode, UHC_BaseFour_expNode_size ) ;
  GB_MkExpNodeIn( UHC_BaseFive_expNode, UHC_BaseFive_expNode_size ) ;
  GB_MkExpNodeIn( UHC_Base_expNode, UHC_Base_expNode_size ) ;
  GB_MkExpNodeIn( UHC_StackTrace_expNode, UHC_StackTrace_expNode_size ) ;
  GB_MkExpNodeIn( UHC_Enum_expNode, UHC_Enum_expNode_size ) ;
  GB_MkExpNodeIn( UHC_MutVar_expNode, UHC_MutVar_expNode_size ) ;
  GB_MkExpNodeIn( UHC_Read_expNode, UHC_Read_expNode_size ) ;
  GB_MkExpNodeIn( UHC_Real_expNode, UHC_Real_expNode_size ) ;
  GB_MkExpNodeIn( UHC_ST_expNode, UHC_ST_expNode_size ) ;
  GB_MkExpNodeIn( UHC_Generics_Tuple_expNode, UHC_Generics_Tuple_expNode_size ) ;
  GB_MkExpNodeIn( UHC_Generics_expNode, UHC_Generics_expNode_size ) ;
  GB_MkExpNodeIn( UHC_Eq_expNode, UHC_Eq_expNode_size ) ;
  GB_MkExpNodeIn( UHC_Ord_expNode, UHC_Ord_expNode_size ) ;
  GB_MkExpNodeIn( UHC_Bits_expNode, UHC_Bits_expNode_size ) ;
  GB_MkExpNodeIn( Data_Maybe_expNode, Data_Maybe_expNode_size ) ;
  GB_MkExpNodeIn( Unsafe_Coerce_expNode, Unsafe_Coerce_expNode_size ) ;
  GB_MkExpNodeIn( UHC_Types_expNode, UHC_Types_expNode_size ) ;
  GB_MkExpNodeIn( UHC_Prims_expNode, UHC_Prims_expNode_size ) ;
  GB_MkExpNodeIn( Data_Bits_expNode, Data_Bits_expNode_size ) ;
  GB_MkExpNodeIn( UHC_Bounded_expNode, UHC_Bounded_expNode_size ) ;
  GB_MkExpNodeIn( UHC_Char_expNode, UHC_Char_expNode_size ) ;
  GB_MkExpNodeIn( UHC_STRef_expNode, UHC_STRef_expNode_size ) ;
  GB_MkExpNodeIn( UHC_Float_expNode, UHC_Float_expNode_size ) ;
  GB_MkExpNodeIn( UHC_Show_expNode, UHC_Show_expNode_size ) ;
  GB_MkExpNodeIn( UHC_Int_expNode, UHC_Int_expNode_size ) ;
  GB_MkExpNodeIn( Data_Char_expNode, Data_Char_expNode_size ) ;
  GB_MkExpNodeIn( Data_List_expNode, Data_List_expNode_size ) ;
  GB_MkExpNodeIn( Data_Either_expNode, Data_Either_expNode_size ) ;
  GB_MkExpNodeIn( UHC_Ptr_expNode, UHC_Ptr_expNode_size ) ;
  GB_MkExpNodeIn( UHC_WeakPtr_expNode, UHC_WeakPtr_expNode_size ) ;
  GB_MkExpNodeIn( UHC_StablePtr_expNode, UHC_StablePtr_expNode_size ) ;
  GB_MkExpNodeIn( Foreign_StablePtr_expNode, Foreign_StablePtr_expNode_size ) ;
  GB_MkExpNodeIn( UHC_Storable_expNode, UHC_Storable_expNode_size ) ;
  GB_MkExpNodeIn( UHC_ByteArray_expNode, UHC_ByteArray_expNode_size ) ;
  GB_MkExpNodeIn( UHC_IOBase_expNode, UHC_IOBase_expNode_size ) ;
  GB_MkExpNodeIn( UHC_OldException_expNode, UHC_OldException_expNode_size ) ;
  GB_MkExpNodeIn( Data_IORef_expNode, Data_IORef_expNode_size ) ;
  GB_MkExpNodeIn( UHC_Word_expNode, UHC_Word_expNode_size ) ;
  GB_MkExpNodeIn( Data_Word_expNode, Data_Word_expNode_size ) ;
  GB_MkExpNodeIn( Control_Monad_expNode, Control_Monad_expNode_size ) ;
  GB_MkExpNodeIn( System_IO_Unsafe_expNode, System_IO_Unsafe_expNode_size ) ;
  GB_MkExpNodeIn( Data_Int_expNode, Data_Int_expNode_size ) ;
  GB_MkExpNodeIn( Data_Typeable_expNode, Data_Typeable_expNode_size ) ;
  GB_MkExpNodeIn( UHC_Weak_expNode, UHC_Weak_expNode_size ) ;
  GB_MkExpNodeIn( UHC_MVar_expNode, UHC_MVar_expNode_size ) ;
  GB_MkExpNodeIn( Foreign_Storable_expNode, Foreign_Storable_expNode_size ) ;
  GB_MkExpNodeIn( Foreign_C_TypesZero_expNode, Foreign_C_TypesZero_expNode_size ) ;
  GB_MkExpNodeIn( Foreign_C_Types_expNode, Foreign_C_Types_expNode_size ) ;
  GB_MkExpNodeIn( Foreign_Ptr_expNode, Foreign_Ptr_expNode_size ) ;
  GB_MkExpNodeIn( Foreign_Marshal_Error_expNode, Foreign_Marshal_Error_expNode_size ) ;
  GB_MkExpNodeIn( UHC_ForeignPtr_expNode, UHC_ForeignPtr_expNode_size ) ;
  GB_MkExpNodeIn( Foreign_ForeignPtr_expNode, Foreign_ForeignPtr_expNode_size ) ;
  GB_MkExpNodeIn( Foreign_Marshal_Alloc_expNode, Foreign_Marshal_Alloc_expNode_size ) ;
  GB_MkExpNodeIn( Foreign_Marshal_Utils_expNode, Foreign_Marshal_Utils_expNode_size ) ;
  GB_MkExpNodeIn( Foreign_Marshal_Array_expNode, Foreign_Marshal_Array_expNode_size ) ;
  GB_MkExpNodeIn( Foreign_C_String_expNode, Foreign_C_String_expNode_size ) ;
  GB_MkExpNodeIn( Foreign_C_Error_expNode, Foreign_C_Error_expNode_size ) ;
  GB_MkExpNodeIn( Foreign_C_expNode, Foreign_C_expNode_size ) ;
  GB_MkExpNodeIn( Foreign_Marshal_Pool_expNode, Foreign_Marshal_Pool_expNode_size ) ;
  GB_MkExpNodeIn( Foreign_Marshal_expNode, Foreign_Marshal_expNode_size ) ;
  GB_MkExpNodeIn( Foreign_expNode, Foreign_expNode_size ) ;
  GB_MkExpNodeIn( System_Posix_Types_expNode, System_Posix_Types_expNode_size ) ;
  GB_MkExpNodeIn( UHC_Conc_expNode, UHC_Conc_expNode_size ) ;
  GB_MkExpNodeIn( System_Posix_Internals_expNode, System_Posix_Internals_expNode_size ) ;
  GB_MkExpNodeIn( System_IO_Error_expNode, System_IO_Error_expNode_size ) ;
  GB_MkExpNodeIn( UHC_Handle_expNode, UHC_Handle_expNode_size ) ;
  GB_MkExpNodeIn( UHC_IO_expNode, UHC_IO_expNode_size ) ;
  GB_MkExpNodeIn( System_IO_Fix_expNode, System_IO_Fix_expNode_size ) ;
  GB_MkExpNodeIn( System_IO_expNode, System_IO_expNode_size ) ;
  GB_MkExpNodeIn( UHC_Run_expNode, UHC_Run_expNode_size ) ;
  GB_MkExpNodeIn( Prelude_expNode, Prelude_expNode_size ) ;
  GB_MkExpNodeIn( Fold_expNode, Fold_expNode_size ) ;
  UHC_BaseZero_initModule(Fold_moduleEntries,0) ;
  UHC_BaseOne_initModule(Fold_moduleEntries,1) ;
  UHC_BaseTwo_initModule(Fold_moduleEntries,2) ;
  UHC_BaseThree_initModule(Fold_moduleEntries,3) ;
  UHC_BaseFour_initModule(Fold_moduleEntries,4) ;
  UHC_BaseFive_initModule(Fold_moduleEntries,5) ;
  UHC_Base_initModule(Fold_moduleEntries,6) ;
  UHC_StackTrace_initModule(Fold_moduleEntries,7) ;
  UHC_Enum_initModule(Fold_moduleEntries,8) ;
  UHC_MutVar_initModule(Fold_moduleEntries,9) ;
  UHC_Read_initModule(Fold_moduleEntries,10) ;
  UHC_Real_initModule(Fold_moduleEntries,11) ;
  UHC_ST_initModule(Fold_moduleEntries,12) ;
  UHC_Generics_Tuple_initModule(Fold_moduleEntries,13) ;
  UHC_Generics_initModule(Fold_moduleEntries,14) ;
  UHC_Eq_initModule(Fold_moduleEntries,15) ;
  UHC_Ord_initModule(Fold_moduleEntries,16) ;
  UHC_Bits_initModule(Fold_moduleEntries,17) ;
  Data_Maybe_initModule(Fold_moduleEntries,18) ;
  Unsafe_Coerce_initModule(Fold_moduleEntries,19) ;
  UHC_Types_initModule(Fold_moduleEntries,20) ;
  UHC_Prims_initModule(Fold_moduleEntries,21) ;
  Data_Bits_initModule(Fold_moduleEntries,22) ;
  UHC_Bounded_initModule(Fold_moduleEntries,23) ;
  UHC_Char_initModule(Fold_moduleEntries,24) ;
  UHC_STRef_initModule(Fold_moduleEntries,25) ;
  UHC_Float_initModule(Fold_moduleEntries,26) ;
  UHC_Show_initModule(Fold_moduleEntries,27) ;
  UHC_Int_initModule(Fold_moduleEntries,28) ;
  Data_Char_initModule(Fold_moduleEntries,29) ;
  Data_List_initModule(Fold_moduleEntries,30) ;
  Data_Either_initModule(Fold_moduleEntries,31) ;
  UHC_Ptr_initModule(Fold_moduleEntries,32) ;
  UHC_WeakPtr_initModule(Fold_moduleEntries,33) ;
  UHC_StablePtr_initModule(Fold_moduleEntries,34) ;
  Foreign_StablePtr_initModule(Fold_moduleEntries,35) ;
  UHC_Storable_initModule(Fold_moduleEntries,36) ;
  UHC_ByteArray_initModule(Fold_moduleEntries,37) ;
  UHC_IOBase_initModule(Fold_moduleEntries,38) ;
  UHC_OldException_initModule(Fold_moduleEntries,39) ;
  Data_IORef_initModule(Fold_moduleEntries,40) ;
  UHC_Word_initModule(Fold_moduleEntries,41) ;
  Data_Word_initModule(Fold_moduleEntries,42) ;
  Control_Monad_initModule(Fold_moduleEntries,43) ;
  System_IO_Unsafe_initModule(Fold_moduleEntries,44) ;
  Data_Int_initModule(Fold_moduleEntries,45) ;
  Data_Typeable_initModule(Fold_moduleEntries,46) ;
  UHC_Weak_initModule(Fold_moduleEntries,47) ;
  UHC_MVar_initModule(Fold_moduleEntries,48) ;
  Foreign_Storable_initModule(Fold_moduleEntries,49) ;
  Foreign_C_TypesZero_initModule(Fold_moduleEntries,50) ;
  Foreign_C_Types_initModule(Fold_moduleEntries,51) ;
  Foreign_Ptr_initModule(Fold_moduleEntries,52) ;
  Foreign_Marshal_Error_initModule(Fold_moduleEntries,53) ;
  UHC_ForeignPtr_initModule(Fold_moduleEntries,54) ;
  Foreign_ForeignPtr_initModule(Fold_moduleEntries,55) ;
  Foreign_Marshal_Alloc_initModule(Fold_moduleEntries,56) ;
  Foreign_Marshal_Utils_initModule(Fold_moduleEntries,57) ;
  Foreign_Marshal_Array_initModule(Fold_moduleEntries,58) ;
  Foreign_C_String_initModule(Fold_moduleEntries,59) ;
  Foreign_C_Error_initModule(Fold_moduleEntries,60) ;
  Foreign_C_initModule(Fold_moduleEntries,61) ;
  Foreign_Marshal_Pool_initModule(Fold_moduleEntries,62) ;
  Foreign_Marshal_initModule(Fold_moduleEntries,63) ;
  Foreign_initModule(Fold_moduleEntries,64) ;
  System_Posix_Types_initModule(Fold_moduleEntries,65) ;
  UHC_Conc_initModule(Fold_moduleEntries,66) ;
  System_Posix_Internals_initModule(Fold_moduleEntries,67) ;
  System_IO_Error_initModule(Fold_moduleEntries,68) ;
  UHC_Handle_initModule(Fold_moduleEntries,69) ;
  UHC_IO_initModule(Fold_moduleEntries,70) ;
  System_IO_Fix_initModule(Fold_moduleEntries,71) ;
  System_IO_initModule(Fold_moduleEntries,72) ;
  UHC_Run_initModule(Fold_moduleEntries,73) ;
  Prelude_initModule(Fold_moduleEntries,74) ;
  Fold_initModule(Fold_moduleEntries,75) ;
  gb_SetModTable(Fold_moduleEntries,76) ;
  main_GB_Run( argc, argv, gb_code_Eval, Cast(GB_Word,*Fold_mainEntryPtr) ) ;
  return main_GB_Exit( argc, argv) ;
}

