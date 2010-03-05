%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Runtime system
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%[8
#include "rts.h"
#if __UHC_TARGET_BC__
#include "bc/interpreter.h"
#endif
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
%%% Globals: Program arguments
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[99
int rtsArgC ;
char** rtsArgV ;
%%]

%%[99
void globalsSetup(int argc, char** argv)
{
	rtsArgC = argc ;
	rtsArgV = argv ;
}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Stack, heap
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
WPtr SP, RP ;
WPtr Stack, ReturnArea ;

WPtr StackAreaHigh, StackAreaLow ;

#if ! ( USE_BOEHM_GC || USE_EHC_MM )
WPtr HP;
WPtr HeapAreaLow;
WPtr HeapAreaHigh;
#endif
%%]

%%[8
void memorySetup()
{
#if USE_BOEHM_GC
    GC_INIT() ;

    Stack = (WPtr)GC_MALLOC_UNCOLLECTABLE(sizeof(Word)*STACKSIZE);
    ReturnArea = (WPtr)GC_MALLOC_UNCOLLECTABLE(sizeof(Word)*RETURNSIZE);
#elif USE_EHC_MM
    mm_init() ;

    Stack = (WPtr)GC_MALLOC_UNCOLLECTABLE(sizeof(Word)*STACKSIZE);
    ReturnArea = (WPtr)GC_MALLOC_UNCOLLECTABLE(sizeof(Word)*RETURNSIZE);
#else
    HeapAreaLow = (WPtr)malloc(sizeof(Word)*HEAPSIZE);
    HeapAreaHigh = HeapAreaLow + HEAPSIZE;
    HP = HeapAreaLow;

    Stack = (WPtr)malloc(sizeof(Word)*STACKSIZE);
    ReturnArea = (WPtr)malloc(sizeof(Word)*RETURNSIZE);
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
#if USE_BOEHM_GC || USE_EHC_MM
#else
Word heapalloc(int n)
{
    Word res = (Word) HP;
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


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Main entry points for Silly init,run,exit
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8

#ifdef __UHC_TARGET_C__

int main_Sil_Init1(int argc, char** argv)
{
	memorySetup() ;
%%[[99
	globalsSetup( argc, argv ) ;
%%]]
    return 0;
}

int main_Sil_Run(int argc, char** argv, int (*sillymainfunction)() )
{
#	if TIMING
		clockStart = clock() ;
#	endif
        (*sillymainfunction)();
#	if TIMING
		clockStop = clock() ;
#	endif
    return 0;
}

int main_Sil_Exit(int argc, char** argv)
{
%%[[8
     printf("%d\n", (int)RP[1] );
%%][99
%%]]
	
#	if TIMING
		double clockDiff = ((double)clockStop - (double)clockStart) / CLOCKS_PER_SEC ;
		printf("Time %.3f secs\n", clockDiff ) ;
#	endif
    return 0;
}

#endif
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Main entry points for Grin Bytecode (GB) init,run,exit
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8

#ifdef __UHC_TARGET_BC__


// Interface with interpreter
void gb_prState( char* msg, int maxStkSz );
void gb_chan_initstd(void);
void gb_Initialize(void);
void gb_checkInterpreterAssumptions(void);
void gb_interpretLoopWith(GB_BytePtr) ;
void gb_push(GB_Word);
void gb_setTOS(GB_Word);
GB_Word gb_getTOS(void);
void gb_prTOSAsInt(void);
extern unsigned long gb_StepCounter;



int main_GB_Init1(int argc, char** argv, int* nRtsOpt)
{
	memorySetup() ;
%%[[99
	globalsSetup( argc, argv ) ;
%%]]
%%[[98
	gb_chan_initstd() ;
%%]]
	gb_checkInterpreterAssumptions() ;
	gb_Initialize() ;
	return 0 ;
}

/*
int gb_opt_rtsOn ;

static struct option gb_longopts1[] =
  { { "rts+"	, no_argument	, &gb_opt_rtsOn		, 1 }
  , { NULL		, 0				, NULL				, 0 }
  } ;

static struct option gb_longopts2[] =
  { { "rts-"	, no_argument	, &gb_opt_rtsOn		, 0 }
  , { NULL		, 0				, NULL				, 0 }
  } ;



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
*/


int main_GB_Run(int argc, char** argv, GB_BytePtr initPC, GB_Word initCAF)
{
	GB_GCSafe_Enter ;
	gb_push( initCAF ) ;
%%[[99
	GB_NodePtr initCAFApp, gbWorld ;
	GB_GCSafe_2_Zeroed(initCAFApp,gbWorld) ;
	
#	if USE_BOEHM_GC
		GB_MkConNodeN_Fixed(gbWorld,GB_GC_MinAlloc_Field_Words(0),0) ;
#	else
		GB_MkConNodeN(gbWorld,0,0) ;
#	endif
	
	GB_MkAppNode1In( initCAFApp, gb_getTOS(), gbWorld ) ;
	gb_setTOS(Cast(GB_Word,initCAFApp)) ;
%%]]
	// printf( "main_GB_Run\n" ) ;
#	if TIMING
		clockStart = clock() ;
#	endif
#	if GB_COUNT_STEPS
		gb_StepCounter = 0 ;
#	endif

    // here we go...
    gb_interpretLoopWith( initPC ) ;

#	if TIMING
		clockStop = clock() ;
#	endif
	if ( IF_INFO_IS(INFO_EXITSTATE) ) {
		gb_prState( "exit state", 1 ) ;
	} else {
#		ifdef DUMP_INTERNALS
			IF_GB_TR_ON(3,{gb_prTOSAsInt() ;printf( "\n" ) ;})
#		endif
	}
	GB_GCSafe_Leave ;
	return 0 ;
}



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
#ifdef TRACE
	// quick testing of memory manager:
	// mm_pages_Buddy_Test() ;
	// mm_allocator_LOF_Test() ;
	// mm_deque_Test() ;
	// mm_plan_Test() ;
#endif
	return 0 ;
}
#endif

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Tracing, misc info
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#if TRACE
int traceLevel = TRACE_LEV_DFLT ;
#endif

%%]

