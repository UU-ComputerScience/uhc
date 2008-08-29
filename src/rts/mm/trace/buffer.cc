%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Memory management: Trace: Buffer
%%% see associated .ch file for more info.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#include "../../rts.h"
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Buffer internal defs
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Buffer internal functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Buffer internal functions, but still externally visible (because of inlining)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Buffer interface
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
void mm_trace_Buffer_Init( MM_Trace* trace ) {
	MM_Trace_Buffer_Data* trgr = mm_malloc_LOF.malloc( sizeof(MM_Trace_Buffer_Data) ) ;
	mm_deque_Init( &trgr->deque, &mm_malloc_LOF ) ;
	trace->data = (MM_Trace_Data_Priv*)trgr ;
}

// unused
void mm_trace_Buffer_InitWithSub( MM_Trace* trace, MM_FlexArray* subTraces ) {
	mm_trace_Buffer_Init( trace ) ;
}

Word mm_trace_Buffer_PopWork( MM_Trace* trace, Word* work, Word nrWorkWords ) {
	MM_Trace_Buffer_Data* trgr = (MM_Trace_Buffer_Data*)trace->data ;
	return mm_deque_HeadPop( &trgr->deque, work, nrWorkWords ) ;
}

void mm_trace_Buffer_PushWork( MM_Trace* trace, Word* work, Word nrWorkWords ) {
	MM_Trace_Buffer_Data* trgr = (MM_Trace_Buffer_Data*)trace->data ;
	mm_deque_TailPush( &trgr->deque, work, nrWorkWords ) ;
}


%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Buffer interface object
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
MM_Trace mm_trace_Buffer =
	{ NULL
	, &mm_trace_Buffer_Init
	, &mm_trace_Buffer_InitWithSub
	, &mm_trace_Buffer_PopWork
	, &mm_trace_Buffer_PushWork
	} ;
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Buffer dump
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#ifdef TRACE
mm_trace_Buffer_Dump( MM_Trace* trace ) {
}
#endif
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Buffer test
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#ifdef TRACE
void mm_trace_Buffer_Test() {
}
#endif
%%]

