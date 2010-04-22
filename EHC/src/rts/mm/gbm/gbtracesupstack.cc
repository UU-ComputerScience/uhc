%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Memory management: TraceSupply: GBStack
%%% see associated .ch file for more info.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#include "../../rts.h"
#include "../../bc/interpreter.h"
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% GBStack internal functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
static inline WPtr mm_traceSupply_GBStack_RunWithStackInfo( MM_Trace* trace, GCStackInfo* info, WPtr base )
{
    // This auxiliary function calls  mm_Trace_TraceObject  for all live pointers on the stack area below "base",
    // as described by the "info" object.
    // It returns the position of the last element processed.
    
	IF_GB_TR_ON(3,{printf("mm_traceSupply_GBStack_RunWithStackInfo BEF gcStackInfo=%p b=%p\n",info,base);}) ;
	if ( info != NULL ) 
	{
		IF_GB_TR_ON(3,{printf("mm_traceSupply_GBStack_RunWithStackInfo A gcStackInfo=%p, sz=%x ndesc=%x\n",info,info->sz,info->nrDescrs);}) ;
		
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
				IF_GB_TR_ON(3,{printf("mm_traceSupply_GBStack_RunWithStackInfo B gcStackInfo=%p, descr[%x]=%x, tracesz=%x base=%x(-1=%x)\n",info,descrInx,descr,sz,base,*(base-1));}) ;
				for ( ; sz > 0 ; sz-- ) 
				{
					base-- ;
					*base = mm_Trace_TraceObject( trace, *base ) ;
				}
			}
			else
			{   // skip a run of non-live pointers
				IF_GB_TR_ON(3,{printf("mm_traceSupply_GBStack_RunWithStackInfo C gcStackInfo=%p, descr[%x]=%x, skipsz=%x base=%x(-1=%x)\n",info,descrInx,descr,sz,base,*(base-1));}) ;
				base -= sz ;
			}
		}
	}
	IF_GB_TR_ON(3,{printf("mm_traceSupply_GBStack_RunWithStackInfo AFT gcStackInfo=%p b=%p\n",info,base);}) ;
	return base ;
}
%%]

%%[8
static inline void mm_traceSupply_GBStack_RunStack( MM_Trace* trace, WPtr from, WPtr to )
{
    // This auxiliary function calls  mm_Trace_TraceObject  for all values between "from" (inclusive) and "to" (exclusive).
    // All these values should be pointers and live.
    
	for ( ; from < to ; from++ ) {
		*from = mm_Trace_TraceObject( trace, *from ) ;
	}
}
%%]


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% GBStack interface
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
void mm_traceSupply_GBStack_Init( MM_TraceSupply* traceSupply, MM_Malloc* memmgt, MM_Mutator* mutator ) {
	MM_TraceSupply_GBStack_Data* stackData = memmgt->malloc( sizeof(MM_TraceSupply_GBStack_Data) ) ;
	stackData->trace = mutator->trace ;
	traceSupply->data = (MM_TraceSupply_Data_Priv*)stackData ;
}

void mm_traceSupply_GBStack_Reset( MM_TraceSupply* traceSupply, Word gcStackInfo ) {
	MM_TraceSupply_GBStack_Data* stackData = (MM_TraceSupply_GBStack_Data*)traceSupply->data ;
	stackData->gcStackInfo = (GCStackInfo*)gcStackInfo ;
	// stackData->gcInfo = (GB_GCInfo*)gcStackInfo ;
}
%%]

%%[8
void mm_traceSupply_GBStack_Run4( MM_TraceSupply* traceSupply ) {

    /*  The aim of this function is to redirect all live pointers on the stack, that is, execute
            p = mm_trace_Object(t, p)
        for all live pointers p, using an object t found in the traceSupply structure.

        The actual processing is done by two auxiliary functions:
           res = mm_traceSupply_GBStack_RunWithStackInfo(t,descr,start):  processes pointers starting at position start, described by descriptor descr.
                 mm_traceSupply_GBStack_RunStack(t,end,start):            processes all pointers between start and end
        The result of the first function is the position that was not processed anymore, as became clear from the descriptor. 
        It is typically fed as start position into the second function.
       
        We will make the following assumptions on the stack layout.
        The evaluator is initialized by putting a closure on the stack and calling eval.
        Suppose then, that eval calls function f, and f calls g, and g triggers garbage collection.
        (in practice, f will be the runMain function).
        The stack layout would then be as follows:
       
        StackHigh> parameter of initial closure to be evaluated
            RET0
            LINK0=NULL
            ...locals of eval function...
            ...working area of eval, including params of f...
            RET1 position where eval called f, also describes which locals of main are live
            LINK1 points to link0
            ...locals of f
            ...working area of f, including params of g...
            RET2 position where f called g, also describes which locals of f are live
        bp> LINK2 points to link1
            ...locals of g...
        sp> ...working area of g...
        

        The LINK values form a linked list of stack frames.
        The RET values are code points where to return after the call,
        but they also cleverly encode the stack area between two RET/LINK pairs
        (Clever in the sense that the position just before the return address gives access to a descriptor).

        We have global variables sp and bp
        bp is the basepointer pointing to the last link.
        sp is the stackpointer pointing to the last used value of the stack.

        So, the area between LINK0 and RET1 is described by a descriptor accesible through RET1,
        and the area between LINK1 and RET2 is described by a descriptor accesible through RET2.
        A descriptor is allowed to describe only part of this area (or even nothing); 
        the remaining area is then considered to contain only live pointers.

        The only part that is NOT described is the area between bp and sp, 
        that is the locals and working area of the current function g.
        Its descriptor, if any, is available however in the traceSupply structure.
    */

    // Everything we need from the traceSupply argument is actually in its "data" field
	MM_TraceSupply_GBStack_Data* stackData = (MM_TraceSupply_GBStack_Data*)traceSupply->data ;
	MM_Trace* trace = stackData->trace;

    // Three local variables are used in the stack walk, containg the high end, low end and mid point of each stack area to be processed
    WPtr areaHigh, areaLow, areaMid;

    // Show sp and bp for debuggung
	IF_GB_TR_ON(3,{printf("mm_traceSupply_GBStack_Run3 BEF sp=%p(lo=%p, hi=%p, used=%x, unused=%x) bp=%p\n",sp,(WPtr)StackAreaLow,(WPtr)StackAreaHigh,(BPtr)StackAreaHigh-(BPtr)sp,(BPtr)sp-(BPtr)StackAreaLow,bp);}) ;

    // First, the local variables and working of g are processed.
    // It is the only part of the stack that is not described by the stack itself.
    // Therefore, its descriptor is retrieved from the stackData structure where it luckily resides.
    // Also, we pass the position where this area starts, i.e., bp.
	areaMid = mm_traceSupply_GBStack_RunWithStackInfo( trace, stackData->gcStackInfo, bp ) ;

    // The remaining part (that was not described) is assumed to be live:
	mm_traceSupply_GBStack_RunStack( trace, sp, areaMid ) ;
	
    // Now we iterate through all areas between RET/LINK pairs.
    // In the example outlined above,
    // the first  iteration handles the areas between LINK1 and RET2,
    // the second iteration handles the areas between LINK0 and RET1,
	
	for ( areaLow=bp ; (areaHigh = (WPtr)*areaLow) != NULL ; areaLow=areaHigh ) 
	{
        // Cleverly get the description 
		CallInfo* ci = GB_FromBPToCallInfo(areaLow) ;
		IF_GB_TR_ON(3,{printf("mm_traceSupply_GBStack_Run3 A callinfo=%p(%s) b=%p\n",ci,ci->name,areaLow);}) ;

        // Process the described part of the area,
        // that is, the part from areaHigh, as far down as it gets.
        // (the starting point is not inclusive, the return value is the last one processed)
		areaMid = mm_traceSupply_GBStack_RunWithStackInfo( trace, ci->gcStackInfo, areaHigh ) ;

        // Process the remaining part of the area, which is assumed to be live.
        // This part starts (inclusive) 2 positions (because the LINK/RET-pair is not data) above the low point;
        // it ends (exclusive) at the mid point where the previous step stopped.
		mm_traceSupply_GBStack_RunStack( trace, areaLow+2, areaMid ) ;
	}

    // Finally, process the area above the RET0/LINK0 pair
    // It is assumed to be live
    // (in practice, this is a single entry containing the top-level thunk to evaluate).
	mm_traceSupply_GBStack_RunStack( trace, areaLow+2, (WPtr)StackAreaHigh ) ;

	IF_GB_TR_ON(3,{printf("mm_traceSupply_GBStack_Run3 AFT\n");}) ;
}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% GBStack interface object
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
MM_TraceSupply mm_traceSupply_GBStack =
	{ NULL
	, &mm_traceSupply_GBStack_Init
	, MM_Undefined
	, &mm_traceSupply_GBStack_Reset
	, &mm_traceSupply_GBStack_Run4
	, MM_Undefined
	} ;
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% GBStack dump
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#ifdef TRACE
void mm_traceSupply_GBStack_Dump( MM_TraceSupply* traceSupply ) {
}
#endif
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% GBStack test
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#ifdef TRACE
void mm_traceSupply_GBStack_Test() {
}
#endif
%%]

