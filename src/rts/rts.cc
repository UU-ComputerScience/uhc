%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Runtime system
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#include "rts.h"
#include <getopt.h>
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Stack, heap
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
Pointer SP, RP, BP ;
Pointer Stack, ReturnArea ;

Pointer StackEnd ;

#if USE_BOEHM_GC
#else
Pointer HP;
Pointer Heap;
Pointer HeapEndCAF, HeapLimit;
#endif
%%]

%%[8
void memorySetup()
{
#if USE_BOEHM_GC
    GC_INIT() ;

    Stack = (Pointer)GC_MALLOC_UNCOLLECTABLE(sizeof(GrWord)*STACKSIZE);
    ReturnArea = (Pointer)GC_MALLOC_UNCOLLECTABLE(sizeof(GrWord)*RETURNSIZE);
#else
    Heap = (Pointer)malloc(sizeof(GrWord)*HEAPSIZE);

    HeapLimit = Heap + HEAPSIZE;

    HP = Heap;

    Stack = (Pointer)malloc(sizeof(GrWord)*STACKSIZE);
    ReturnArea = (Pointer)malloc(sizeof(GrWord)*RETURNSIZE);
#endif
    
    SP = Stack;
    RP = ReturnArea;
    
    StackEnd = Stack + STACKSIZE ;
}
%%]

%%[8
#if USE_BOEHM_GC
#else
GrWord heapalloc(int n)
{
    GrWord res = (GrWord) HP;
    HP += n;
    if (HP>=HeapLimit)
    {
        printf("heap overflow\n");
        exit(1);
    }

    return res;
}
#endif

%%]

%%[8
void memoryDumpResult_Sil()
{
#if USE_BOEHM_GC
     printf("result SP-offset=%d tag=%d value=%d\n", SP-Stack, RP[0], RP[1] );
#else
/*    printf("result SP-offset=%d HP-offset=%d tag=%d arity=%d value=%d\n", SP-Stack, HP-Heap, RP[0], RP[1], RP[2] ); */
     printf("result SP-offset=%d HP-offset=%d tag=%d value=%d\n", SP-Stack, HP-Heap, RP[0], RP[1] );
#endif
}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Main generated entry point for Silly
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
extern int fun_main();
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Main entry points for Silly init,run,exit
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
int main_Sil_Init1(int argc, char** argv)
{
	memorySetup() ;
    initialize();
#if USE_BOEHM_GC
#else
    HeapEndCAF = HP;
#endif

    return 0;
}

int main_Sil_Run(int argc, char** argv)
{
    fun_main();

/*
    int i;
    for (i=0; i<10; i++)
    {
        printf("RP[%d] = %d\n", i, RP[i] );
    }
    for (i=0; i<10; i++)
    {
        printf("%d: St[%d] = %d\n", Stack+i, i, Stack[i] );
    }
    for (i=0; i<HP-Heap; i++)
    {
        printf("%d: Heap[%d] = %d\n", Heap+i, i, Heap[i] );
    }
*/

    return 0;
}

int main_Sil_Exit(int argc, char** argv)
{
	memoryDumpResult_Sil() ;

    return 0;
}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Main entry points for Grin Bytecode (GB) init,run,exit
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
%%]
/* options descriptor */
static struct option gb_longopts[] =
  { { "rts+"	, no_argument	, &gb_opt_rtsOn		, 1 }
  , { "rts-"	, no_argument	, &gb_opt_rtsOn		, 0 }
  , { NULL		, 0				, NULL				, 0 }
  } ;

%%[8
int main_GB_Init1(int argc, char** argv)
{
	memorySetup() ;
	gb_checkInterpreterAssumptions() ;
	gb_Initialize() ;
	
	return 0 ;
}

int main_GB_Run(int argc, char** argv, GB_BytePtr initPC, GB_Word initCAF)
{
	gb_push( initCAF ) ;
    interpretLoopWith( initPC ) ;
#if DUMP_INTERNALS
	gb_prState( "exit state", 1 ) ;
#endif
	return 0 ;
}

int main_GB_Exit(int argc, char** argv)
{	
	return 0 ;
}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Tracing, misc info
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#if TRACE
int traceLevel = TRACE_LEV_DFLT ;
#endif
%%]

