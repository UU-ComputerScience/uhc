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
#define MM_TraceSupply_WorkBuffer_NrWords		0x80	/* fairly arbitrary, but large enough to give same performance as BumpSupply */
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
void mm_traceSupply_Buffer_Init( MM_TraceSupply* traceSupply, MM_Malloc* memmgt, MM_Mutator* mutator ) {
	MM_TraceSupply_Buffer_Data* trsup = memmgt->malloc( sizeof(MM_TraceSupply_Buffer_Data) ) ;
	trsup->trace = mutator->trace ;
	mm_deque_InitWithSize( &trsup->deque, &mm_malloc_LOF, 2 ) ;
	traceSupply->data = (MM_TraceSupply_Data_Priv*)trsup ;
}

void mm_traceSupply_Buffer_Reset( MM_TraceSupply* traceSupply, Word gcInfo ) {
	MM_TraceSupply_Buffer_Data* trsup = (MM_TraceSupply_Buffer_Data*)traceSupply->data ;
	mm_deque_Reset( &trsup->deque ) ;
}

void mm_traceSupply_Buffer_Run( MM_TraceSupply* traceSupply ) {
	MM_TraceSupply_Buffer_Data* trsup = (MM_TraceSupply_Buffer_Data*)traceSupply->data ;
	MM_DEQue* q = &trsup->deque ;
	
	IF_GB_TR_ON(3,{printf("mm_traceSupply_Buffer_Run BEF\n");}) ;
	while ( ! mm_deque_IsEmpty( q ) ) {
		Word workBuffer[MM_TraceSupply_WorkBuffer_NrWords] ;
		// assume always the correct nr of words are pushed as work
		Word nrPopped = mm_deque_HeadPop( q, workBuffer, MM_TraceSupply_WorkBuffer_NrWords ) ;
		// IF_GB_TR_ON(3,{mm_deque_Dump(&trsup->deque);}) ;
		Word workPos ;
		for ( workPos = 0 ; workPos < nrPopped ; workPos += 2 ) {
			// IF_GB_TR_ON(3,{printf("mm_traceSupply_Buffer_Run pop work=%x sz=%x\n",workBuffer[workPos+0],workBuffer[workPos+1]);}) ;
			Word hdrSz = trsup->trace->objectHeaderNrWords ;
			// trsup->trace->traceObjects( trsup->trace, (Word*)(workBuffer[workPos+0]) + hdrSz, workBuffer[workPos+1] - hdrSz ) ;
			// mm_trace_TraceObjects( trsup->trace, (Word*)(workBuffer[workPos+0]) + hdrSz, workBuffer[workPos+1] - hdrSz ) ;
			trsup->trace->traceObjectPayload( trsup->trace, workBuffer[workPos] ) ;
		}
	}
	IF_GB_TR_ON(3,{printf("mm_traceSupply_Buffer_Run AFT\n");}) ;
}

// push ptr + size
void mm_traceSupply_Buffer_PushWork( MM_TraceSupply* traceSupply, Word* work, Word nrWorkWords, Word extra ) {
	MM_TraceSupply_Buffer_Data* trsup = (MM_TraceSupply_Buffer_Data*)traceSupply->data ;
	Word workBuffer[2] = {(Word)work, nrWorkWords} ;
	mm_deque_TailPush( &trsup->deque, workBuffer, 2 ) ;
	// IF_GB_TR_ON(3,{printf("mm_traceSupply_Buffer_PushWork work=%x(%p) sz=%x(%x)\n",workBuffer[0],work,workBuffer[1],nrWorkWords);}) ;
	// IF_GB_TR_ON(3,{mm_deque_Dump(&trsup->deque);}) ;
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
void mm_traceSupply_Buffer_Dump( MM_TraceSupply* traceSupply ) {
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

