%%[8
#include "../../rts.h"
%%]


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Internal functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8

static inline WPtr traceAreaUsingDescription( MM_Trace* trace, GCStackInfo* info, WPtr base )
{
    // This auxiliary function calls  mm_Trace_TraceObject  for all live pointers on the stack area upward from "base",
    // as described by the "info" object.
    // It returns the position of the element just above the last one processed.
    
	if ( info != NULL ) 
	{
        // printf("traceAreaWithDescription base=%08x info=%08x size=%d nrdescrS=%d\n", base, info, info->sz, info->nrDescrs);

		// The  info  structure contains an array of bytes. They contain a run-length encoded description of the stack area starting below base.
		Word descrInx ;
		for ( descrInx = 0 ; descrInx < info->nrDescrs ; descrInx++ ) 
		{
            // Fetch one byte from the array
			Word descr = info->descrs[ descrInx ] ;
			
			// The least significant bit describes wheter (1) or not (0) the pointers are live.
			// The other 7 bits of the byte contain the length of a run of adjacent pointers that all have this liveness.
			
			Word sz = descr >> 1 ;
			if ( descr & 1 ) 
			{
			    // process a run of live pointers
				for ( ; sz > 0 ; sz-- ) 
				{
					// printf("    trace pointer at %08x =", base); fflush(stdout); printf("%08x\n", *base );
					*base = mm_Trace_TraceObject( trace, *base ) ;
					base++ ;
				}
			}
			else
			{   // skip a run of non-live pointers
				base += sz ;
				// printf("    skip to %08x\n", base); fflush(stdout);
			}
		}
	}
	return base ;
}
%%]

%%[8
static inline void traceAreaFully( MM_Trace* trace, WPtr from, WPtr to )
{
    // This auxiliary function calls  mm_Trace_TraceObject  for all values between "from" (inclusive) and "to" (exclusive).
    // All these values should be pointers and live.
 
    // printf("traceAreaFully from=%08x to=%08x\n", from, to);
    
	for ( ; from < to ; from++ ) 
	{
        // printf("    trace pointer at %08x = %08x\n", from, *from ); fflush(stdout);
		*from = mm_Trace_TraceObject( trace, *from ) ;
        // printf("    repld pointer at %08x = %08x\n", from, *from ); fflush(stdout);
	}
}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Interface functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
void mm_traceSupplyStack_C_Init( MM_TraceSupply* traceSupply, MM_Malloc* memmgt, MM_Mutator* mutator ) 
{
	MM_TraceSupply_Stack_Data* stackData = memmgt->malloc( sizeof(MM_TraceSupply_Stack_Data) ) ;
	stackData->trace = mutator->trace ;
	traceSupply->data = (MM_TraceSupply_Data_Priv*)stackData ;
}

void mm_traceSupplyStack_C_Reset( MM_TraceSupply* traceSupply, Word gcStackInfo ) 
{
    // printf("{GC}\n"); fflush(stdout);
	MM_TraceSupply_Stack_Data* stackData = (MM_TraceSupply_Stack_Data*)traceSupply->data ;
	stackData->gcStackInfo = (GCStackInfo*)gcStackInfo ;
}

void mm_traceSupplyStack_C_Run( MM_TraceSupply* traceSupply )
{
	MM_TraceSupply_Stack_Data* stackData;
	MM_Trace *trace;
    GCStackInfo *stackInfo;
	

    // Everything we need from the traceSupply argument is actually in its "data" field
	stackData = (MM_TraceSupply_Stack_Data*) traceSupply->data ;
	trace     =                              stackData->trace;
    stackInfo = (GCStackInfo*)               stackData->gcStackInfo;
    
    // int size = stackInfo->sz;

    WPtr here = SP;

    while(1)
    {
	    here = traceAreaUsingDescription( trace, stackInfo, here ) ;
        // printf("here=%08x\n", here); fflush(stdout);
        Word ret = *here;
        // printf("here=%08x retpos=%08x\n", here, ret ); fflush(stdout);
        here++;
        stackInfo = (GCStackInfo*)    *((Word*)(ret - sizeof(Word)));
        int what  = (int)             *((int*) (ret - sizeof(Word) - sizeof(int)));
        // printf("what=%d stackInfo=%08x\n", what, stackInfo );
        
        if (stackInfo==0) break;
	}
    traceAreaFully( trace, here, (WPtr)StackAreaHigh ) ;
}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% MM_TraceSupply interface object
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
MM_TraceSupply mm_traceSupplyStack_C =
	{ NULL
	, &mm_traceSupplyStack_C_Init
	, MM_Undefined
	, &mm_traceSupplyStack_C_Reset
	, &mm_traceSupplyStack_C_Run
	, MM_Undefined
	} ;
%%]
