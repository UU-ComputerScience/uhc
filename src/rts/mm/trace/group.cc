%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Memory management: Trace: Group
%%% see associated .ch file for more info.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#include "../../rts.h"
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Group internal defs
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Group internal functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Group internal functions, but still externally visible (because of inlining)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Group interface
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
void mm_trace_Group_Init( MM_Trace* trace ) {
	MM_Trace_Group_Data* trgr = mm_malloc_LOF.malloc( sizeof(MM_Trace_Group_Data) ) ;
		
	trace->data = (MM_Trace_Data_Priv*)trgr ;
}

void mm_trace_Group_InitWithSub( MM_Trace* trace, MM_FlexArray* subTraces ) {
	mm_trace_Group_Init( trace ) ;
	MM_Trace_Group_Data* trgr = (MM_Trace_Group_Data*)trace->data ;
	trgr->subTraces = *subTraces ;
	trgr->popTraceInx = 0 ;
}

Word mm_trace_Group_PopWork( MM_Trace* trace, Word* work, Word nrWorkWords ) {
	MM_Trace_Group_Data* trgr = (MM_Trace_Group_Data*)trace->data ;
	Word nrWorkAccum = 0 ;
		
	for ( ; nrWorkAccum < nrWorkWords && trgr->popTraceInx < mm_flexArray_SizeUsed( &trgr->subTraces ) ; ) {
		MM_Trace* subTrace = (MM_Trace*)mm_flexArray_At( &trgr->subTraces, trgr->popTraceInx ) ;
		Word nrTodoWork = nrWorkWords - nrWorkAccum ;
		Word nrDoneWork = subTrace->popWork( subTrace, &work[nrWorkAccum], nrTodoWork ) ;
		if ( nrDoneWork < nrTodoWork ) {
			trgr->popTraceInx++ ;
		}
		nrWorkAccum += nrDoneWork ;
	}

	return nrWorkAccum ;
}

void mm_trace_Group_PushWork( MM_Trace* trace, Word* work, Word nrWorkWords ) {
	MM_Trace_Group_Data* trgr = (MM_Trace_Group_Data*)trace->data ;
		
	MM_Trace* subTrace = (MM_Trace*)mm_flexArray_At( &trgr->subTraces, mm_flexArray_SizeUsed( &trgr->subTraces ) - 1 ) ;
	subTrace->pushWork( subTrace, work, nrWorkWords ) ;
}


%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Group interface object
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
MM_Trace mm_trace_Group =
	{ NULL
	, &mm_trace_Group_Init
	, &mm_trace_Group_InitWithSub
	, &mm_trace_Group_PopWork
	, &mm_trace_Group_PushWork
	} ;
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Group dump
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#ifdef TRACE
mm_trace_Group_Dump( MM_Trace* trace ) {
}
#endif
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Group test
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#ifdef TRACE
void mm_trace_Group_Test() {
}
#endif
%%]

