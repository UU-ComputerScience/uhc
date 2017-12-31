#include "rts.h"
#include "bc/interpreter.h"

 
static Word8 Main_bytePool[] =
{ 0x4d,0x61,0x69,0x6e,0x2e,0x75,0x35,0x5f,0x32,0x00
, 0x48,0x65,0x6c,0x6c,0x6f,0x20,0x57,0x6f,0x72,0x6c,0x64,0x00
, 0x03,0x04
, 0x4d,0x61,0x69,0x6e,0x2e,0x6d,0x61,0x69,0x6e,0x00
, 0x53,0x79,0x73,0x74,0x65,0x6d,0x2e,0x49,0x4f,0x2e,0x70,0x75,0x74,0x53,0x74,0x72,0x00
, 0x4d,0x61,0x69,0x6e,0x2e,0x75,0x35,0x5f,0x36,0x00
, 0x6d,0x61,0x69,0x6e,0x00
};
 
static GB_LinkChainResolvedInfo Main_linkChainIndirections[] =
{};
 
static GCStackInfo Main_gcStackInfos[] =
{ { 3
  , 2
  , &(Main_bytePool[22])
  }
};
 
static FunctionInfo Main_functionInfos[] =
{ {16,FunctionInfoFlag_None,&(Main_bytePool[0])}
, {8,FunctionInfoFlag_None,&(Main_bytePool[24])}
, {8,FunctionInfoFlag_None,&(Main_bytePool[51])}
, {8,FunctionInfoFlag_None,&(Main_bytePool[61])}
};
 
static CallInfo Main_callinfos[] =
{ MkCallInfoWith(2,&(Main_bytePool[34]),FunctionInfo_Inx_None,FunctionInfo_Inx_None,NULL,NULL) 
, MkCallInfoWith(0,&(Main_bytePool[0]),0,14,NULL,NULL) 
};
 
static GB_Byte Main_bytecode[] =
{    0xfe,0xff
,    0x61,0x02,0x00,0x00,0x00,0x00,0x00,0x00
,    0x0b,0x1d,0x05,0x00,0x00,0x0a,0x00,0x00,0x00
,    0x0b,0x00,0x00,0x00,0x00,0x02,0x00,0x00,0x00
,    0x08,0x10
,    0xec,0xc5,0x02,0x00,0x00,0x01,0x00,0x00,0x00
,    0x20,0x00
,    0xe7,0x25,0x04,0x00,0x00,0x03,0x00,0x00,0x00
,    0x25,0x68,0x0c
,    0xfe,0x00,0x00,0x08,0x00
,    0x61,0x02,0x00,0x00,0x01,0x00,0x00,0x00
,    0xe7,0xe5,0x02,0x00,0x00,0x02,0x00,0x00,0x00
,    0x24,0x68
,    0xe0,0x59,0x02,0x00,0x00,0x00,0x00,0x00,0x00
,    0xe6,0x4d,0x04,0x00,0x00,0x00,0x00,0x00,0x00
,    0x08,0x01
,    0x20,0x10
,    0xfe,0x04,0x00,0x18,0x00
,    0xa1,0x03,0x00,0x00,0x02,0x00,0x00,0x00
,    0x68,0x00
,    0xf4,0x00,0x08,0x00
,    0x61,0x02,0x00,0x00,0x03,0x00,0x00,0x00
,    0xe6,0x4d,0x02,0x00,0x00,0x01,0x00,0x00,0x00
,    0xe7,0xe5,0x02,0x00,0x00,0x00,0x00,0x00,0x00
,    0x24,0x08
,    0xf0,0x59,0x02,0x00,0x00,0x01,0x00,0x00,0x00
,    0xe6,0x0d,0x00,0x00,0x00,0x02,0x00,0x00,0x00
,    0x08,0x01
,    0x20,0x10
,    0xfe,0x04,0x00,0x18,0x00
};
GB_ByteCodeModule Main_bytecodeModule =
  { "Main"
  , NULL
  , 0
  , Main_bytecode
  , 173
  } ;
 
static GB_Word Main_constants[] =
{};
 
static HalfWord Main_cafGlEntryIndices[] =
{ 0
, 1
, 2
, 3
};
 
static GB_BytePtr Main_globalEntries[] =
{ &(Main_bytecode[10])
, &(Main_bytecode[66])
, &(Main_bytecode[112])
, &(Main_bytecode[126])
};
GB_NodePtr Main_expNode ;
static int Main_expNode_offs[] =
  {} ;
int Main_expNode_size = 0 ;

static GB_ImpModEntry Main_impMods[] =
         { { "UHC.Run"
           , 0 
           }
         , { "Prelude"
           , 0 
           }
         , { "System.IO"
           , 0 
           }
         , { "UHC.Base"
           , 0 
           }
         } ;

void Main_initModule(GB_ModEntry* modTbl, Word modTblInx) {
  gb_InitTables( Main_bytecode
               , 173
               , Main_cafGlEntryIndices
               , 4
               , Main_globalEntries
               , 4
               , Main_constants
               , Main_gcStackInfos
               , Main_linkChainIndirections
               , Main_callinfos
               , 2
               , Main_functionInfos
               , 4
               , Main_bytePool
               , 2
               , Main_impMods
               , 4
               , &(Main_expNode)
               , Main_expNode_size
               , Main_expNode_offs
               , modTbl
               , modTblInx
               ) ;
}

static GB_BytePtr* Main_mainEntryPtr = &(Main_globalEntries[3]) ;

 
extern void UHC_Base_initModule(GB_ModEntry*,Word) ;
extern GB_NodePtr UHC_Base_expNode ;
extern int UHC_Base_expNode_size ;
extern GB_ByteCodeModule UHC_Base_bytecodeModule ;
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
extern void UHC_Char_initModule(GB_ModEntry*,Word) ;
extern GB_NodePtr UHC_Char_expNode ;
extern int UHC_Char_expNode_size ;
extern GB_ByteCodeModule UHC_Char_bytecodeModule ;
extern void UHC_Generics_Tuple_initModule(GB_ModEntry*,Word) ;
extern GB_NodePtr UHC_Generics_Tuple_expNode ;
extern int UHC_Generics_Tuple_expNode_size ;
extern GB_ByteCodeModule UHC_Generics_Tuple_bytecodeModule ;
extern void UHC_ST_initModule(GB_ModEntry*,Word) ;
extern GB_NodePtr UHC_ST_expNode ;
extern int UHC_ST_expNode_size ;
extern GB_ByteCodeModule UHC_ST_bytecodeModule ;
extern void UHC_Prims_initModule(GB_ModEntry*,Word) ;
extern GB_NodePtr UHC_Prims_expNode ;
extern int UHC_Prims_expNode_size ;
extern GB_ByteCodeModule UHC_Prims_bytecodeModule ;
extern void UHC_Real_initModule(GB_ModEntry*,Word) ;
extern GB_NodePtr UHC_Real_expNode ;
extern int UHC_Real_expNode_size ;
extern GB_ByteCodeModule UHC_Real_bytecodeModule ;
extern void Data_Bits_initModule(GB_ModEntry*,Word) ;
extern GB_NodePtr Data_Bits_expNode ;
extern int Data_Bits_expNode_size ;
extern GB_ByteCodeModule Data_Bits_bytecodeModule ;
extern void UHC_Read_initModule(GB_ModEntry*,Word) ;
extern GB_NodePtr UHC_Read_expNode ;
extern int UHC_Read_expNode_size ;
extern GB_ByteCodeModule UHC_Read_bytecodeModule ;
extern void UHC_Generics_initModule(GB_ModEntry*,Word) ;
extern GB_NodePtr UHC_Generics_expNode ;
extern int UHC_Generics_expNode_size ;
extern GB_ByteCodeModule UHC_Generics_bytecodeModule ;
extern void UHC_Bounded_initModule(GB_ModEntry*,Word) ;
extern GB_NodePtr UHC_Bounded_expNode ;
extern int UHC_Bounded_expNode_size ;
extern GB_ByteCodeModule UHC_Bounded_bytecodeModule ;
extern void UHC_MutVar_initModule(GB_ModEntry*,Word) ;
extern GB_NodePtr UHC_MutVar_expNode ;
extern int UHC_MutVar_expNode_size ;
extern GB_ByteCodeModule UHC_MutVar_bytecodeModule ;
extern void UHC_STRef_initModule(GB_ModEntry*,Word) ;
extern GB_NodePtr UHC_STRef_expNode ;
extern int UHC_STRef_expNode_size ;
extern GB_ByteCodeModule UHC_STRef_bytecodeModule ;
extern void UHC_Enum_initModule(GB_ModEntry*,Word) ;
extern GB_NodePtr UHC_Enum_expNode ;
extern int UHC_Enum_expNode_size ;
extern GB_ByteCodeModule UHC_Enum_bytecodeModule ;
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
extern void UHC_Word_initModule(GB_ModEntry*,Word) ;
extern GB_NodePtr UHC_Word_expNode ;
extern int UHC_Word_expNode_size ;
extern GB_ByteCodeModule UHC_Word_bytecodeModule ;
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
extern void UHC_StackTrace_initModule(GB_ModEntry*,Word) ;
extern GB_NodePtr UHC_StackTrace_expNode ;
extern int UHC_StackTrace_expNode_size ;
extern GB_ByteCodeModule UHC_StackTrace_bytecodeModule ;
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
extern void UHC_Eq_initModule(GB_ModEntry*,Word) ;
extern GB_NodePtr UHC_Eq_expNode ;
extern int UHC_Eq_expNode_size ;
extern GB_ByteCodeModule UHC_Eq_bytecodeModule ;
extern void UHC_Ord_initModule(GB_ModEntry*,Word) ;
extern GB_NodePtr UHC_Ord_expNode ;
extern int UHC_Ord_expNode_size ;
extern GB_ByteCodeModule UHC_Ord_bytecodeModule ;
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
extern void Main_initModule(GB_ModEntry*,Word) ;
extern GB_NodePtr Main_expNode ;
extern int Main_expNode_size ;
extern GB_ByteCodeModule Main_bytecodeModule ;
static GB_ModEntry Main_moduleEntries[] =
         { { "UHC.Base"
           , &(UHC_Base_expNode)
           , &(UHC_Base_bytecodeModule)
           , Main_functionInfos
           }
         , { "UHC.Bits"
           , &(UHC_Bits_expNode)
           , &(UHC_Bits_bytecodeModule)
           , Main_functionInfos
           }
         , { "Data.Maybe"
           , &(Data_Maybe_expNode)
           , &(Data_Maybe_bytecodeModule)
           , Main_functionInfos
           }
         , { "Unsafe.Coerce"
           , &(Unsafe_Coerce_expNode)
           , &(Unsafe_Coerce_bytecodeModule)
           , Main_functionInfos
           }
         , { "UHC.Types"
           , &(UHC_Types_expNode)
           , &(UHC_Types_bytecodeModule)
           , Main_functionInfos
           }
         , { "UHC.Char"
           , &(UHC_Char_expNode)
           , &(UHC_Char_bytecodeModule)
           , Main_functionInfos
           }
         , { "UHC.Generics.Tuple"
           , &(UHC_Generics_Tuple_expNode)
           , &(UHC_Generics_Tuple_bytecodeModule)
           , Main_functionInfos
           }
         , { "UHC.ST"
           , &(UHC_ST_expNode)
           , &(UHC_ST_bytecodeModule)
           , Main_functionInfos
           }
         , { "UHC.Prims"
           , &(UHC_Prims_expNode)
           , &(UHC_Prims_bytecodeModule)
           , Main_functionInfos
           }
         , { "UHC.Real"
           , &(UHC_Real_expNode)
           , &(UHC_Real_bytecodeModule)
           , Main_functionInfos
           }
         , { "Data.Bits"
           , &(Data_Bits_expNode)
           , &(Data_Bits_bytecodeModule)
           , Main_functionInfos
           }
         , { "UHC.Read"
           , &(UHC_Read_expNode)
           , &(UHC_Read_bytecodeModule)
           , Main_functionInfos
           }
         , { "UHC.Generics"
           , &(UHC_Generics_expNode)
           , &(UHC_Generics_bytecodeModule)
           , Main_functionInfos
           }
         , { "UHC.Bounded"
           , &(UHC_Bounded_expNode)
           , &(UHC_Bounded_bytecodeModule)
           , Main_functionInfos
           }
         , { "UHC.MutVar"
           , &(UHC_MutVar_expNode)
           , &(UHC_MutVar_bytecodeModule)
           , Main_functionInfos
           }
         , { "UHC.STRef"
           , &(UHC_STRef_expNode)
           , &(UHC_STRef_bytecodeModule)
           , Main_functionInfos
           }
         , { "UHC.Enum"
           , &(UHC_Enum_expNode)
           , &(UHC_Enum_bytecodeModule)
           , Main_functionInfos
           }
         , { "UHC.Float"
           , &(UHC_Float_expNode)
           , &(UHC_Float_bytecodeModule)
           , Main_functionInfos
           }
         , { "UHC.Show"
           , &(UHC_Show_expNode)
           , &(UHC_Show_bytecodeModule)
           , Main_functionInfos
           }
         , { "UHC.Int"
           , &(UHC_Int_expNode)
           , &(UHC_Int_bytecodeModule)
           , Main_functionInfos
           }
         , { "UHC.Word"
           , &(UHC_Word_expNode)
           , &(UHC_Word_bytecodeModule)
           , Main_functionInfos
           }
         , { "Data.Char"
           , &(Data_Char_expNode)
           , &(Data_Char_bytecodeModule)
           , Main_functionInfos
           }
         , { "Data.List"
           , &(Data_List_expNode)
           , &(Data_List_bytecodeModule)
           , Main_functionInfos
           }
         , { "Data.Either"
           , &(Data_Either_expNode)
           , &(Data_Either_bytecodeModule)
           , Main_functionInfos
           }
         , { "UHC.Ptr"
           , &(UHC_Ptr_expNode)
           , &(UHC_Ptr_bytecodeModule)
           , Main_functionInfos
           }
         , { "UHC.WeakPtr"
           , &(UHC_WeakPtr_expNode)
           , &(UHC_WeakPtr_bytecodeModule)
           , Main_functionInfos
           }
         , { "UHC.StablePtr"
           , &(UHC_StablePtr_expNode)
           , &(UHC_StablePtr_bytecodeModule)
           , Main_functionInfos
           }
         , { "Foreign.StablePtr"
           , &(Foreign_StablePtr_expNode)
           , &(Foreign_StablePtr_bytecodeModule)
           , Main_functionInfos
           }
         , { "UHC.Storable"
           , &(UHC_Storable_expNode)
           , &(UHC_Storable_bytecodeModule)
           , Main_functionInfos
           }
         , { "UHC.ByteArray"
           , &(UHC_ByteArray_expNode)
           , &(UHC_ByteArray_bytecodeModule)
           , Main_functionInfos
           }
         , { "UHC.StackTrace"
           , &(UHC_StackTrace_expNode)
           , &(UHC_StackTrace_bytecodeModule)
           , Main_functionInfos
           }
         , { "UHC.IOBase"
           , &(UHC_IOBase_expNode)
           , &(UHC_IOBase_bytecodeModule)
           , Main_functionInfos
           }
         , { "UHC.OldException"
           , &(UHC_OldException_expNode)
           , &(UHC_OldException_bytecodeModule)
           , Main_functionInfos
           }
         , { "Data.IORef"
           , &(Data_IORef_expNode)
           , &(Data_IORef_bytecodeModule)
           , Main_functionInfos
           }
         , { "UHC.Eq"
           , &(UHC_Eq_expNode)
           , &(UHC_Eq_bytecodeModule)
           , Main_functionInfos
           }
         , { "UHC.Ord"
           , &(UHC_Ord_expNode)
           , &(UHC_Ord_bytecodeModule)
           , Main_functionInfos
           }
         , { "Data.Word"
           , &(Data_Word_expNode)
           , &(Data_Word_bytecodeModule)
           , Main_functionInfos
           }
         , { "Control.Monad"
           , &(Control_Monad_expNode)
           , &(Control_Monad_bytecodeModule)
           , Main_functionInfos
           }
         , { "System.IO.Unsafe"
           , &(System_IO_Unsafe_expNode)
           , &(System_IO_Unsafe_bytecodeModule)
           , Main_functionInfos
           }
         , { "Data.Int"
           , &(Data_Int_expNode)
           , &(Data_Int_bytecodeModule)
           , Main_functionInfos
           }
         , { "Data.Typeable"
           , &(Data_Typeable_expNode)
           , &(Data_Typeable_bytecodeModule)
           , Main_functionInfos
           }
         , { "UHC.Weak"
           , &(UHC_Weak_expNode)
           , &(UHC_Weak_bytecodeModule)
           , Main_functionInfos
           }
         , { "UHC.MVar"
           , &(UHC_MVar_expNode)
           , &(UHC_MVar_bytecodeModule)
           , Main_functionInfos
           }
         , { "Foreign.Storable"
           , &(Foreign_Storable_expNode)
           , &(Foreign_Storable_bytecodeModule)
           , Main_functionInfos
           }
         , { "Foreign.C.Types"
           , &(Foreign_C_Types_expNode)
           , &(Foreign_C_Types_bytecodeModule)
           , Main_functionInfos
           }
         , { "Foreign.Ptr"
           , &(Foreign_Ptr_expNode)
           , &(Foreign_Ptr_bytecodeModule)
           , Main_functionInfos
           }
         , { "Foreign.Marshal.Error"
           , &(Foreign_Marshal_Error_expNode)
           , &(Foreign_Marshal_Error_bytecodeModule)
           , Main_functionInfos
           }
         , { "UHC.ForeignPtr"
           , &(UHC_ForeignPtr_expNode)
           , &(UHC_ForeignPtr_bytecodeModule)
           , Main_functionInfos
           }
         , { "Foreign.ForeignPtr"
           , &(Foreign_ForeignPtr_expNode)
           , &(Foreign_ForeignPtr_bytecodeModule)
           , Main_functionInfos
           }
         , { "Foreign.Marshal.Alloc"
           , &(Foreign_Marshal_Alloc_expNode)
           , &(Foreign_Marshal_Alloc_bytecodeModule)
           , Main_functionInfos
           }
         , { "Foreign.Marshal.Utils"
           , &(Foreign_Marshal_Utils_expNode)
           , &(Foreign_Marshal_Utils_bytecodeModule)
           , Main_functionInfos
           }
         , { "Foreign.Marshal.Array"
           , &(Foreign_Marshal_Array_expNode)
           , &(Foreign_Marshal_Array_bytecodeModule)
           , Main_functionInfos
           }
         , { "Foreign.C.String"
           , &(Foreign_C_String_expNode)
           , &(Foreign_C_String_bytecodeModule)
           , Main_functionInfos
           }
         , { "Foreign.C.Error"
           , &(Foreign_C_Error_expNode)
           , &(Foreign_C_Error_bytecodeModule)
           , Main_functionInfos
           }
         , { "Foreign.C"
           , &(Foreign_C_expNode)
           , &(Foreign_C_bytecodeModule)
           , Main_functionInfos
           }
         , { "Foreign.Marshal.Pool"
           , &(Foreign_Marshal_Pool_expNode)
           , &(Foreign_Marshal_Pool_bytecodeModule)
           , Main_functionInfos
           }
         , { "Foreign.Marshal"
           , &(Foreign_Marshal_expNode)
           , &(Foreign_Marshal_bytecodeModule)
           , Main_functionInfos
           }
         , { "Foreign"
           , &(Foreign_expNode)
           , &(Foreign_bytecodeModule)
           , Main_functionInfos
           }
         , { "System.Posix.Types"
           , &(System_Posix_Types_expNode)
           , &(System_Posix_Types_bytecodeModule)
           , Main_functionInfos
           }
         , { "UHC.Conc"
           , &(UHC_Conc_expNode)
           , &(UHC_Conc_bytecodeModule)
           , Main_functionInfos
           }
         , { "System.Posix.Internals"
           , &(System_Posix_Internals_expNode)
           , &(System_Posix_Internals_bytecodeModule)
           , Main_functionInfos
           }
         , { "System.IO.Error"
           , &(System_IO_Error_expNode)
           , &(System_IO_Error_bytecodeModule)
           , Main_functionInfos
           }
         , { "UHC.Handle"
           , &(UHC_Handle_expNode)
           , &(UHC_Handle_bytecodeModule)
           , Main_functionInfos
           }
         , { "UHC.IO"
           , &(UHC_IO_expNode)
           , &(UHC_IO_bytecodeModule)
           , Main_functionInfos
           }
         , { "System.IO.Fix"
           , &(System_IO_Fix_expNode)
           , &(System_IO_Fix_bytecodeModule)
           , Main_functionInfos
           }
         , { "System.IO"
           , &(System_IO_expNode)
           , &(System_IO_bytecodeModule)
           , Main_functionInfos
           }
         , { "UHC.Run"
           , &(UHC_Run_expNode)
           , &(UHC_Run_bytecodeModule)
           , Main_functionInfos
           }
         , { "Prelude"
           , &(Prelude_expNode)
           , &(Prelude_bytecodeModule)
           , Main_functionInfos
           }
         , { "Main"
           , &(Main_expNode)
           , &(Main_bytecodeModule)
           , Main_functionInfos
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
  GB_MkExpNodeIn( UHC_Base_expNode, UHC_Base_expNode_size ) ;
  GB_MkExpNodeIn( UHC_Bits_expNode, UHC_Bits_expNode_size ) ;
  GB_MkExpNodeIn( Data_Maybe_expNode, Data_Maybe_expNode_size ) ;
  GB_MkExpNodeIn( Unsafe_Coerce_expNode, Unsafe_Coerce_expNode_size ) ;
  GB_MkExpNodeIn( UHC_Types_expNode, UHC_Types_expNode_size ) ;
  GB_MkExpNodeIn( UHC_Char_expNode, UHC_Char_expNode_size ) ;
  GB_MkExpNodeIn( UHC_Generics_Tuple_expNode, UHC_Generics_Tuple_expNode_size ) ;
  GB_MkExpNodeIn( UHC_ST_expNode, UHC_ST_expNode_size ) ;
  GB_MkExpNodeIn( UHC_Prims_expNode, UHC_Prims_expNode_size ) ;
  GB_MkExpNodeIn( UHC_Real_expNode, UHC_Real_expNode_size ) ;
  GB_MkExpNodeIn( Data_Bits_expNode, Data_Bits_expNode_size ) ;
  GB_MkExpNodeIn( UHC_Read_expNode, UHC_Read_expNode_size ) ;
  GB_MkExpNodeIn( UHC_Generics_expNode, UHC_Generics_expNode_size ) ;
  GB_MkExpNodeIn( UHC_Bounded_expNode, UHC_Bounded_expNode_size ) ;
  GB_MkExpNodeIn( UHC_MutVar_expNode, UHC_MutVar_expNode_size ) ;
  GB_MkExpNodeIn( UHC_STRef_expNode, UHC_STRef_expNode_size ) ;
  GB_MkExpNodeIn( UHC_Enum_expNode, UHC_Enum_expNode_size ) ;
  GB_MkExpNodeIn( UHC_Float_expNode, UHC_Float_expNode_size ) ;
  GB_MkExpNodeIn( UHC_Show_expNode, UHC_Show_expNode_size ) ;
  GB_MkExpNodeIn( UHC_Int_expNode, UHC_Int_expNode_size ) ;
  GB_MkExpNodeIn( UHC_Word_expNode, UHC_Word_expNode_size ) ;
  GB_MkExpNodeIn( Data_Char_expNode, Data_Char_expNode_size ) ;
  GB_MkExpNodeIn( Data_List_expNode, Data_List_expNode_size ) ;
  GB_MkExpNodeIn( Data_Either_expNode, Data_Either_expNode_size ) ;
  GB_MkExpNodeIn( UHC_Ptr_expNode, UHC_Ptr_expNode_size ) ;
  GB_MkExpNodeIn( UHC_WeakPtr_expNode, UHC_WeakPtr_expNode_size ) ;
  GB_MkExpNodeIn( UHC_StablePtr_expNode, UHC_StablePtr_expNode_size ) ;
  GB_MkExpNodeIn( Foreign_StablePtr_expNode, Foreign_StablePtr_expNode_size ) ;
  GB_MkExpNodeIn( UHC_Storable_expNode, UHC_Storable_expNode_size ) ;
  GB_MkExpNodeIn( UHC_ByteArray_expNode, UHC_ByteArray_expNode_size ) ;
  GB_MkExpNodeIn( UHC_StackTrace_expNode, UHC_StackTrace_expNode_size ) ;
  GB_MkExpNodeIn( UHC_IOBase_expNode, UHC_IOBase_expNode_size ) ;
  GB_MkExpNodeIn( UHC_OldException_expNode, UHC_OldException_expNode_size ) ;
  GB_MkExpNodeIn( Data_IORef_expNode, Data_IORef_expNode_size ) ;
  GB_MkExpNodeIn( UHC_Eq_expNode, UHC_Eq_expNode_size ) ;
  GB_MkExpNodeIn( UHC_Ord_expNode, UHC_Ord_expNode_size ) ;
  GB_MkExpNodeIn( Data_Word_expNode, Data_Word_expNode_size ) ;
  GB_MkExpNodeIn( Control_Monad_expNode, Control_Monad_expNode_size ) ;
  GB_MkExpNodeIn( System_IO_Unsafe_expNode, System_IO_Unsafe_expNode_size ) ;
  GB_MkExpNodeIn( Data_Int_expNode, Data_Int_expNode_size ) ;
  GB_MkExpNodeIn( Data_Typeable_expNode, Data_Typeable_expNode_size ) ;
  GB_MkExpNodeIn( UHC_Weak_expNode, UHC_Weak_expNode_size ) ;
  GB_MkExpNodeIn( UHC_MVar_expNode, UHC_MVar_expNode_size ) ;
  GB_MkExpNodeIn( Foreign_Storable_expNode, Foreign_Storable_expNode_size ) ;
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
  GB_MkExpNodeIn( Main_expNode, Main_expNode_size ) ;
  UHC_Base_initModule(Main_moduleEntries,0) ;
  UHC_Bits_initModule(Main_moduleEntries,1) ;
  Data_Maybe_initModule(Main_moduleEntries,2) ;
  Unsafe_Coerce_initModule(Main_moduleEntries,3) ;
  UHC_Types_initModule(Main_moduleEntries,4) ;
  UHC_Char_initModule(Main_moduleEntries,5) ;
  UHC_Generics_Tuple_initModule(Main_moduleEntries,6) ;
  UHC_ST_initModule(Main_moduleEntries,7) ;
  UHC_Prims_initModule(Main_moduleEntries,8) ;
  UHC_Real_initModule(Main_moduleEntries,9) ;
  Data_Bits_initModule(Main_moduleEntries,10) ;
  UHC_Read_initModule(Main_moduleEntries,11) ;
  UHC_Generics_initModule(Main_moduleEntries,12) ;
  UHC_Bounded_initModule(Main_moduleEntries,13) ;
  UHC_MutVar_initModule(Main_moduleEntries,14) ;
  UHC_STRef_initModule(Main_moduleEntries,15) ;
  UHC_Enum_initModule(Main_moduleEntries,16) ;
  UHC_Float_initModule(Main_moduleEntries,17) ;
  UHC_Show_initModule(Main_moduleEntries,18) ;
  UHC_Int_initModule(Main_moduleEntries,19) ;
  UHC_Word_initModule(Main_moduleEntries,20) ;
  Data_Char_initModule(Main_moduleEntries,21) ;
  Data_List_initModule(Main_moduleEntries,22) ;
  Data_Either_initModule(Main_moduleEntries,23) ;
  UHC_Ptr_initModule(Main_moduleEntries,24) ;
  UHC_WeakPtr_initModule(Main_moduleEntries,25) ;
  UHC_StablePtr_initModule(Main_moduleEntries,26) ;
  Foreign_StablePtr_initModule(Main_moduleEntries,27) ;
  UHC_Storable_initModule(Main_moduleEntries,28) ;
  UHC_ByteArray_initModule(Main_moduleEntries,29) ;
  UHC_StackTrace_initModule(Main_moduleEntries,30) ;
  UHC_IOBase_initModule(Main_moduleEntries,31) ;
  UHC_OldException_initModule(Main_moduleEntries,32) ;
  Data_IORef_initModule(Main_moduleEntries,33) ;
  UHC_Eq_initModule(Main_moduleEntries,34) ;
  UHC_Ord_initModule(Main_moduleEntries,35) ;
  Data_Word_initModule(Main_moduleEntries,36) ;
  Control_Monad_initModule(Main_moduleEntries,37) ;
  System_IO_Unsafe_initModule(Main_moduleEntries,38) ;
  Data_Int_initModule(Main_moduleEntries,39) ;
  Data_Typeable_initModule(Main_moduleEntries,40) ;
  UHC_Weak_initModule(Main_moduleEntries,41) ;
  UHC_MVar_initModule(Main_moduleEntries,42) ;
  Foreign_Storable_initModule(Main_moduleEntries,43) ;
  Foreign_C_Types_initModule(Main_moduleEntries,44) ;
  Foreign_Ptr_initModule(Main_moduleEntries,45) ;
  Foreign_Marshal_Error_initModule(Main_moduleEntries,46) ;
  UHC_ForeignPtr_initModule(Main_moduleEntries,47) ;
  Foreign_ForeignPtr_initModule(Main_moduleEntries,48) ;
  Foreign_Marshal_Alloc_initModule(Main_moduleEntries,49) ;
  Foreign_Marshal_Utils_initModule(Main_moduleEntries,50) ;
  Foreign_Marshal_Array_initModule(Main_moduleEntries,51) ;
  Foreign_C_String_initModule(Main_moduleEntries,52) ;
  Foreign_C_Error_initModule(Main_moduleEntries,53) ;
  Foreign_C_initModule(Main_moduleEntries,54) ;
  Foreign_Marshal_Pool_initModule(Main_moduleEntries,55) ;
  Foreign_Marshal_initModule(Main_moduleEntries,56) ;
  Foreign_initModule(Main_moduleEntries,57) ;
  System_Posix_Types_initModule(Main_moduleEntries,58) ;
  UHC_Conc_initModule(Main_moduleEntries,59) ;
  System_Posix_Internals_initModule(Main_moduleEntries,60) ;
  System_IO_Error_initModule(Main_moduleEntries,61) ;
  UHC_Handle_initModule(Main_moduleEntries,62) ;
  UHC_IO_initModule(Main_moduleEntries,63) ;
  System_IO_Fix_initModule(Main_moduleEntries,64) ;
  System_IO_initModule(Main_moduleEntries,65) ;
  UHC_Run_initModule(Main_moduleEntries,66) ;
  Prelude_initModule(Main_moduleEntries,67) ;
  Main_initModule(Main_moduleEntries,68) ;
  gb_SetModTable(Main_moduleEntries,69) ;
  main_GB_Run( argc, argv, gb_code_Eval, Cast(GB_Word,*Main_mainEntryPtr) ) ;
  return main_GB_Exit( argc, argv) ;
}

