%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Memory management: TraceSupply: Buffer
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
void mm_traceSupply_Buffer_Init( MM_TraceSupply* traceSupply, MM_Trace* trace ) {
	MM_TraceSupply_Buffer_Data* trgr = mm_malloc_LOF.malloc( sizeof(MM_TraceSupply_Buffer_Data) ) ;
	trgr->trace = trace ;
	mm_deque_Init( &trgr->deque, &mm_malloc_LOF ) ;
	traceSupply->data = (MM_TraceSupply_Data_Priv*)trgr ;
}

void mm_traceSupply_Buffer_Reset( MM_TraceSupply* traceSupply ) {
	MM_TraceSupply_Buffer_Data* trgr = (MM_TraceSupply_Buffer_Data*)traceSupply->data ;
	mm_deque_Reset( &trgr->deque ) ;
}

void mm_traceSupply_Buffer_Run( MM_TraceSupply* traceSupply ) {
	MM_TraceSupply_Buffer_Data* trgr = (MM_TraceSupply_Buffer_Data*)traceSupply->data ;
	MM_DEQue* q = &trgr->deque ;
	
	while ( ! mm_deque_IsEmpty( q ) ) {
		Word avail ;
		// get availability after each iteration step because new entries may have been added
		for ( avail = mm_deque_HeadAvailRead( q ) ; avail > 0 ; avail = mm_deque_HeadAvailRead( q ) ) {
			Word** objs ;
			Word i ;
			for ( i = 0, objs = (Word**)mm_deque_Head( q ) ; i < avail ; i++, objs++ ) {
				// replace each entry pointed to by *objs
				**objs = mm_Trace_TraceObject( trgr->trace, **objs ) ;
			}
			mm_deque_IncHeadOff( q, avail ) ;
		}
		mm_deque_HeadShrink( q ) ;
	}
}

void mm_traceSupply_Buffer_PushWork( MM_TraceSupply* traceSupply, Word* work, Word nrWorkWords ) {
	MM_TraceSupply_Buffer_Data* trgr = (MM_TraceSupply_Buffer_Data*)traceSupply->data ;
	mm_deque_TailPush( &trgr->deque, work, nrWorkWords ) ;
}

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Buffer interface object
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
MM_TraceSupply mm_traceSupply_Buffer =
	{ NULL
	, &mm_traceSupply_Buffer_Init
	, MM_Undefined
	, &mm_traceSupply_Buffer_Reset
	, &mm_traceSupply_Buffer_Run
	, &mm_traceSupply_Buffer_PushWork
	} ;
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Buffer dump
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#ifdef TRACE
mm_traceSupply_Buffer_Dump( MM_TraceSupply* traceSupply ) {
}
#endif
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Buffer test
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#ifdef TRACE
void mm_traceSupply_Buffer_Test() {
}
#endif
%%]

