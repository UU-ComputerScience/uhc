%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Runtime system
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%[8
#include "rts.h"
#include <getopt.h>
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Timing
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8.TIMING
#define TIMING		0

#if TIMING
#include <time.h>

static clock_t clockStart, clockStop ;
#endif
%%]

%%[100 -8.TIMING
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Stack, heap
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
Pointer SP, RP ;
Pointer Stack, ReturnArea ;

Pointer StackAreaHigh, StackAreaLow ;

#if ! USE_BOEHM_GC
Pointer HP;
Pointer HeapAreaLow;
Pointer HeapAreaHigh;
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
    HeapAreaLow = (Pointer)malloc(sizeof(GrWord)*HEAPSIZE);
    HeapAreaHigh = HeapAreaLow + HEAPSIZE;
    HP = HeapAreaLow;

    Stack = (Pointer)malloc(sizeof(GrWord)*STACKSIZE);
    ReturnArea = (Pointer)malloc(sizeof(GrWord)*RETURNSIZE);
#endif
    RP = ReturnArea;

	// stack builds bottom-up	    
    // SP = Stack;
    // StackEnd = Stack + STACKSIZE ;
    
    // stack hangs top-down
    SP = Stack + STACKSIZE - 1 - 2;
    StackAreaLow = Stack;
    StackAreaHigh = Stack + STACKSIZE;
    
    
}
%%]

%%[8
#if USE_BOEHM_GC
#else
GrWord heapalloc(int n)
{
    GrWord res = (GrWord) HP;
    HP += n;
    if (HP>=HeapAreaHigh)
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
     //printf("result SP offset=%d tag=%d value=%d\n", Stack+STACKSIZE-1-SP, RP[0], RP[1] );

#if USE_64_BITS
     printf("%lld\n", RP[1] );
#else
     printf("%d\n", RP[1] );
#endif
#else
     //printf("result SP offset=%d HP offset=%d tag=%d value=%d\n", Stack+STACKSIZE-1-SP, HP-HeapAreaLow, RP[0], RP[1] );
#if USE_64_BITS
     printf("%lld\n", RP[1] );
#else
     printf("%d\n", RP[1] );
#endif
#endif
}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Main generated entry point for Silly
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
extern int silly_main();
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Main entry points for Silly init,run,exit
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
int main_Sil_Init1(int argc, char** argv)
{
	memorySetup() ;
    return 0;
}

int main_Sil_Run(int argc, char** argv)
{
#	if TIMING
		clockStart = clock() ;
#	endif
    silly_main();
#	if TIMING
		clockStop = clock() ;
#	endif
    return 0;
}

int main_Sil_Exit(int argc, char** argv)
{
%%[[8
	memoryDumpResult_Sil() ;
%%][99
%%]]
	
#	if TIMING
		double clockDiff = ((double)clockStop - (double)clockStart) / CLOCKS_PER_SEC ;
		printf("Time %.3f secs\n", clockDiff ) ;
#	endif
    return 0;
}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Main entry points for Grin Bytecode (GB) init,run,exit
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8

void gb_prState( char* msg, int maxStkSz );

/* options descriptor */
int gb_opt_rtsOn ;

static struct option gb_longopts1[] =
  { { "rts+"	, no_argument	, &gb_opt_rtsOn		, 1 }
  , { NULL		, 0				, NULL				, 0 }
  } ;

static struct option gb_longopts2[] =
  { { "rts-"	, no_argument	, &gb_opt_rtsOn		, 0 }
  , { NULL		, 0				, NULL				, 0 }
  } ;
%%]

%%[8
int main_GB_Init1(int argc, char** argv, int* nRtsOpt)
{
	memorySetup() ;
%%[[97
#	if USE_GPM
		mp_set_memory_functions( gb_Alloc_GMP, gb_ReAlloc_GMP, gb_Free_GMP ) ;
#	endif
%%]]
%%[[98
	gb_chan_initstd() ;
%%]]
	gb_checkInterpreterAssumptions() ;
	gb_Initialize() ;
	
	
	return 0 ;
	
}
%%]
	// following crashes, dunno why
	gb_opt_rtsOn = False ;
	int ch ;
	char* rtsOpts = "" ;
	int exitLoop = False ;
	struct option* longopts = gb_longopts1 ;
	while ( ! exitLoop && ((ch = getopt_long( argc, argv, rtsOpts, longopts, NULL)) != -1) )
	{
		switch( ch )
		{
			case 't' :
				gb_Opt_TraceSteps = True ;
				break ;
			case 0 :
				if ( gb_opt_rtsOn ) {
					longopts = gb_longopts2 ;
					rtsOpts = "t" ;
				} else {
					exitLoop = True ;
				}
				break ;
			default :
				exitLoop = True ;
				break ;
		}
	}
	*nRtsOpt = optind ;
	optind = 0 ;
	// optreset = True ; // flag unknown at some platforms
	
	return 0 ;

%%[8
int main_GB_Run(int argc, char** argv, GB_BytePtr initPC, GB_Word initCAF)
{
%%[[99
	GB_NodePtr initCAFApp ;
	GB_MkAppNode1In( initCAFApp, initCAF, gb_Unit ) ;
	initCAF = Cast(GB_Word,initCAFApp) ;
%%]]
	gb_push( initCAF ) ;
#	if TIMING
		clockStart = clock() ;
#	endif
#	if GB_COUNT_STEPS
		gb_StepCounter = 0 ;
#	endif
    gb_interpretLoopWith( initPC ) ;
#	if TIMING
		clockStop = clock() ;
#	endif
	if ( IF_INFO_IS(INFO_EXITSTATE) ) {
		gb_prState( "exit state", 1 ) ;
	} else {
#		ifdef DUMP_INTERNALS
			gb_prTOSAsInt() ;
			printf( "\n" ) ;
#		endif
	}
	return 0 ;
}
%%]

%%[8
int main_GB_Exit(int argc, char** argv)
{	
#if TIMING
	double speed = 0 ;
	double clockDiff = ((double)clockStop - (double)clockStart) / CLOCKS_PER_SEC ;
#	if GB_COUNT_STEPS
		speed = gb_StepCounter / clockDiff ;
#	endif
	IF_INFO_EXITSTATE_ON(printf("Time %.3f secs, instr/sec %.0f\n", clockDiff, speed ) ;) ;
#endif
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

