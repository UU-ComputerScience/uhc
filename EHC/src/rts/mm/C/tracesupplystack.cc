%%[8
#include "../../rts.h"
%%]


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Internal functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
static inline WPtr traceAreaUsingDescription( MM_Trace* trace, GCStackInfo* info, WPtr base )
{
    // This auxiliary function calls  mm_Trace_TraceObject  for all live pointers on the stack area below "base",
    // as described by the "info" object.
    // It returns the position of the last element processed.
    
	if ( info != NULL ) 
	{
        printf("traceAreaWithDescription base=%08x info=%08x size=%d nrdescrS=%d\n", base, info, info->sz, info->nrDescrs);

		// The  info  structure contains an array of bytes. They contain a run-length encoded description of the stack area starting below base.
		Word descrInx ;
		for ( descrInx = 0 ; descrInx < info->nrDescrs ; descrInx++ ) 
		{
            // Fetch one byte from the array
			Word descr = info->descrs[ descrInx ] ;
			
			printf("  descr=%d\n", descr );
			
			// The least significant bit describes wheter (1) or not (0) the pointers are live.
			// The other 7 bits of the byte contain the length of a run of adjacent pointers that all have this liveness.
			
			Word sz = descr >> 1 ;
			if ( descr & 1 ) 
			{
			    // process a run of live pointers
				for ( ; sz > 0 ; sz-- ) 
				{
					base-- ;
					printf("    trace pointer at %08x =", base); fflush(stdout);
					printf("%08x\n", *base );
					// *base = mm_Trace_TraceObject( trace, *base ) ;
				}
			}
			else
			{   // skip a run of non-live pointers
				base -= sz ;
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
 
    printf("traceAreaFully from=%08x to=%08x\n", from, to);
    
	for ( ; from < to ; from++ ) 
	{
        printf("    trace pointer at %08x = %08x\n", from, *from );
		// *from = mm_Trace_TraceObject( trace, *from ) ;
	}
}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Interface functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
void mm_traceSupplyStack_C_Init( MM_TraceSupply* traceSupply, MM_Malloc* memmgt, MM_Mutator* mutator ) 
{
    printf("mm_traceSupplyStack_C_Init\n");

	MM_TraceSupply_Stack_Data* stackData = memmgt->malloc( sizeof(MM_TraceSupply_Stack_Data) ) ;
	stackData->trace = mutator->trace ;
	traceSupply->data = (MM_TraceSupply_Data_Priv*)stackData ;
}

void mm_traceSupplyStack_C_Reset( MM_TraceSupply* traceSupply, Word gcStackInfo ) 
{
    printf("mm_traceSupplyStack_C_Reset\n");
	MM_TraceSupply_Stack_Data* stackData = (MM_TraceSupply_Stack_Data*)traceSupply->data ;
	stackData->gcStackInfo = (GCStackInfo*)gcStackInfo ;
}

void mm_traceSupplyStack_C_Run( MM_TraceSupply* traceSupply )
{

    printf("mm_traceSupplyStack_C_Run\n");

	MM_TraceSupply_Stack_Data* stackData;
	MM_Trace *trace;
    GCStackInfo *stackInfo;
	

    // Everything we need from the traceSupply argument is actually in its "data" field
	stackData = (MM_TraceSupply_Stack_Data*) traceSupply->data ;
	trace     =                              stackData->trace;
    stackInfo = (GCStackInfo*)               stackData->gcStackInfo;
    
    int size = stackInfo->sz;
    printf("data size=%d\n", size );


    // Three local variables are used in the stack walk, containg the high end, low end and mid point of each stack area to be processed
    WPtr areaHigh, areaLow, areaMid;

    // First, the local variables of g are processed.
    // It is the only part of the stack that is not described by the stack itself.
    // Therefore, its descriptor is retrieved from the stackData structure where it luckily resides.
    // This also enables us to deduce the value of the BP (which is not explicitly maintained in this backend).

    WPtr bp = SP+size;
	areaMid = traceAreaUsingDescription( trace, stackInfo, bp ) ;

 
    // Now we iterate through all areas between RET/LINK pairs.
    // In the example outlined above,
    // the first  iteration handles the areas between LINK1 and RET2,
    // the second iteration handles the areas between LINK0 and RET1,
	
	for ( areaLow=bp ; (areaHigh = (WPtr)*areaLow) != NULL ; areaLow=areaHigh ) 
	{
        // Cleverly get the description 

        Word ret = areaLow[1];
        stackInfo = (GCStackInfo*) ((WPtr)ret)[-1];
        Word what = (Word) ((WPtr)ret)[-2];
        
        printf("low=%08x high=%08x *high=%08x ret=%08x si=%08x what=%d\n", areaLow, areaHigh, *areaHigh, ret, stackInfo, what);
        
        // Process the described part of the area,
        // that is, the part from areaHigh, as far down as it gets.
        // (the starting point is not inclusive, the return value is the last one processed)
		areaMid = traceAreaUsingDescription( trace, stackInfo, areaHigh ) ;

        // Process the remaining part of the area, which is assumed to be live.
        // This part starts (inclusive) 2 positions (because the LINK/RET-pair is not data) above the low point;
        // it ends (exclusive) at the mid point where the previous step stopped.
		traceAreaFully( trace, areaLow+2, areaMid ) ;
	}

    // Finally, process the area above the RET0/LINK0 pair
    // It is assumed to be live
    // (in practice, this is a single entry containing the top-level thunk to evaluate).
	traceAreaFully( trace, areaLow+2, (WPtr)StackAreaHigh ) ;

    exit(1);
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
