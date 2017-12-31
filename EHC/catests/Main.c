#include "rts.h"
#include "bc/interpreter.h"

 
static Word8 Test_bytePool[] =
{ 0x54,0x65,0x73,0x74,0x2e,0x61,0x70,0x70,0x33,0x00
, 0x54,0x65,0x73,0x74,0x2e,0x66,0x5f,0x40,0x55,0x4e,0x51,0x5f,0x40,0x35,0x00
};
 
static GB_LinkChainResolvedInfo Test_linkChainIndirections[] =
{};
 
static GCStackInfo Test_gcStackInfos[] =
{};
 
static FunctionInfo Test_functionInfos[] =
{{24,FunctionInfoFlag_None,&(Test_bytePool[0])}};
 
static CallInfo Test_callinfos[] =
{MkCallInfoWith(2,&(Test_bytePool[10]),FunctionInfo_Inx_None,FunctionInfo_Inx_None,NULL,NULL) };
 
static GB_Byte Test_bytecode[] =
{    0xfe,0xff
,    0xe1,0x02,0x00,0x00,0x00,0x00,0x00,0x00
,    0x20,0x28
,    0xe0,0x19,0x00,0x00,0x00,0x00,0x00,0x00,0x00
,    0x20,0x28
,    0x20,0x28
,    0x20,0x28
,    0x08,0x03
,    0x20,0x20
,    0xfe,0x04,0x00,0x28,0x20
};
GB_ByteCodeModule Test_bytecodeModule =
  { "Test"
  , NULL
  , 0
  , Test_bytecode
  , 36
  } ;
 
static GB_Word Test_constants[] =
{};
 
static HalfWord Test_cafGlEntryIndices[] =
{};
 
static GB_BytePtr Test_globalEntries[] =
{&(Test_bytecode[10])};
GB_NodePtr Test_expNode ;
static int Test_expNode_offs[] =
  {0 } ;
int Test_expNode_size = 1 ;

static GB_ImpModEntry Test_impMods[] =
         { { "Prelude"
           , 0 
           }
         } ;

void Test_initModule(GB_ModEntry* modTbl, Word modTblInx) {
  gb_InitTables( Test_bytecode
               , 36
               , Test_cafGlEntryIndices
               , 0
               , Test_globalEntries
               , 1
               , Test_constants
               , Test_gcStackInfos
               , Test_linkChainIndirections
               , Test_callinfos
               , 1
               , Test_functionInfos
               , 1
               , Test_bytePool
               , 2
               , Test_impMods
               , 1
               , &(Test_expNode)
               , Test_expNode_size
               , Test_expNode_offs
               , modTbl
               , modTblInx
               ) ;
}

