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
WPtr Stack, ReturnArea, LocalsArea ;

#ifdef __UHC_TARGET_C__
Word 
  Ret0,  Ret1,  Ret2,  Ret3,  Ret4,  Ret5,  Ret6,  Ret7,  Ret8,  Ret9
, Ret10, Ret11, Ret12, Ret13, Ret14, Ret15, Ret16, Ret17, Ret18, Ret19
, Ret20, Ret21, Ret22, Ret23, Ret24, Ret25, Ret26, Ret27, Ret28, Ret29
, Ret30, Ret31, Ret32, Ret33, Ret34, Ret35, Ret36, Ret37, Ret38, Ret39
, Ret40, Ret41, Ret42, Ret43, Ret44, Ret45, Ret46, Ret47, Ret48, Ret49
, Ret50, Ret51, Ret52, Ret53, Ret54, Ret55, Ret56, Ret57, Ret58, Ret59
, Ret60, Ret61, Ret62, Ret63, Ret64, Ret65, Ret66, Ret67, Ret68, Ret69
, Ret70, Ret71, Ret72, Ret73, Ret74, Ret75, Ret76, Ret77, Ret78, Ret79
, Ret80, Ret81, Ret82, Ret83, Ret84, Ret85, Ret86, Ret87, Ret88, Ret89
, Ret90, Ret91, Ret92, Ret93, Ret94, Ret95, Ret96, Ret97, Ret98, Ret99;

Word 
  Loc0,  Loc1,  Loc2,  Loc3,  Loc4,  Loc5,  Loc6,  Loc7,  Loc8,  Loc9
, Loc10, Loc11, Loc12, Loc13, Loc14, Loc15, Loc16, Loc17, Loc18, Loc19
, Loc20, Loc21, Loc22, Loc23, Loc24, Loc25, Loc26, Loc27, Loc28, Loc29
, Loc30, Loc31, Loc32, Loc33, Loc34, Loc35, Loc36, Loc37, Loc38, Loc39
, Loc40, Loc41, Loc42, Loc43, Loc44, Loc45, Loc46, Loc47, Loc48, Loc49
, Loc50, Loc51, Loc52, Loc53, Loc54, Loc55, Loc56, Loc57, Loc58, Loc59
, Loc60, Loc61, Loc62, Loc63, Loc64, Loc65, Loc66, Loc67, Loc68, Loc69
, Loc70, Loc71, Loc72, Loc73, Loc74, Loc75, Loc76, Loc77, Loc78, Loc79
, Loc80, Loc81, Loc82, Loc83, Loc84, Loc85, Loc86, Loc87, Loc88, Loc89
, Loc90, Loc91, Loc92, Loc93, Loc94, Loc95, Loc96, Loc97, Loc98, Loc99;
#endif

WPtr StackAreaHigh, StackAreaLow ;

#if ! ( USE_BOEHM_GC || USE_EHC_MM )
WPtr HP;
WPtr HeapAreaLow;
WPtr HeapAreaHigh;
#endif
%%]

%%[8
void memory_Initialization()
{
#if USE_BOEHM_GC
    GC_INIT() ;
    Stack = (WPtr)GC_MALLOC_UNCOLLECTABLE(sizeof(Word)*STACKSIZE);    
#elif USE_EHC_MM
    mm_init() ;
    Stack = (WPtr)GC_MALLOC_UNCOLLECTABLE(sizeof(Word)*STACKSIZE);    
#else
    HeapAreaLow = (WPtr)malloc(sizeof(Word)*HEAPSIZE);
    HeapAreaHigh = HeapAreaLow + HEAPSIZE;
    HP = HeapAreaLow;
    Stack = (WPtr)GC_MALLOC_UNCOLLECTABLE(sizeof(Word)*STACKSIZE);    
#endif

    ReturnArea = (WPtr)malloc(sizeof(Word)*RETURNSIZE);
    LocalsArea = (WPtr)malloc(sizeof(Word)*LOCALSSIZE);

    RP = ReturnArea;

	// stack builds bottom-up	    
    // SP = Stack;
    // StackEnd = Stack + STACKSIZE ;
    
    // stack hangs top-down
    SP = Stack + STACKSIZE - 1 - 2;
    StackAreaLow = Stack;
    StackAreaHigh = Stack + STACKSIZE;
    
}

void memory_Finalization()
{
#	if USE_EHC_MM
    	mm_exit() ;
#	endif
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
	memory_Initialization() ;
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
     printf("%d\n", (int)Ret1 );
%%][99
%%]]
	
#	if TIMING
		double clockDiff = ((double)clockStop - (double)clockStart) / CLOCKS_PER_SEC ;
		printf("Time %.3f secs\n", clockDiff ) ;
#	endif
	memory_Finalization() ;
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
	memory_Initialization() ;
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
	memory_Finalization() ;
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

