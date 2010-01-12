%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Memory management: TraceSupply: Bump
%%% see associated .ch file for more info.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#include "../../rts.h"
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Bump internal defs
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
static inline WPtr* mm_traceSupply_Bump_LastPushAddress( MM_TraceSupply_Bump_Data* trsup, MM_Space_FragmentInx inx ) {
	return (WPtr*)mm_flexArray_At( &trsup->lastPushedAddresses, inx ) ;
}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Bump internal functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Bump internal functions, but still externally visible (because of inlining)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Bump interface
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%[8
void mm_traceSupply_Bump_Reset( MM_TraceSupply* traceSupply, Word gcInfo ) {
	MM_TraceSupply_Bump_Data* trsup = (MM_TraceSupply_Bump_Data*)traceSupply->data ;
	MM_Space* spc = trsup->trace->allocator->getSpace( trsup->trace->allocator ) ;

	// mm_flexArray_EnsureSlot( &trsup->lastPushedAddresses, spc->getNrFragments(spc) ) ;
	mm_flexArray_EnsureSlot( &trsup->lastPushedAddresses, trsup->trace->allocator->getTotalSize(trsup->trace->allocator) >> spc->getGrowDefaultLog(spc) ) ;
	
	trsup->lastTracedFragment
		= trsup->lastPushedFragment
		= trsup->trace->allocator->lastAllocFragment( trsup->trace->allocator ) ;
	trsup->lastTracedAddress
		= *mm_traceSupply_Bump_LastPushAddress( trsup, trsup->lastPushedFragment )
		= (Word*)trsup->trace->allocator->lastAllocAddress( trsup->trace->allocator ) ;
	// IF_GB_TR_ON(3,{printf("mm_traceSupply_Bump_Reset last frag=%x flexsz=%x lastPushedFragment=%x lastTracedFragment=%x lastPushedAddress=%p lastTracedAddress=%p\n",trsup->trace->allocator->lastAllocFragment( trsup->trace->allocator ),trsup->trace->allocator->getTotalSize(trsup->trace->allocator) >> spc->getGrowDefaultLog(spc),trsup->lastPushedFragment,trsup->lastTracedFragment,*mm_traceSupply_Bump_LastPushAddress( trsup, trsup->lastPushedFragment ),trsup->lastTracedAddress);}) ;
}

void mm_traceSupply_Bump_Init( MM_TraceSupply* traceSupply, MM_Malloc* memmgt, MM_Mutator* mutator ) {
	MM_TraceSupply_Bump_Data* trsup = memmgt->malloc( sizeof(MM_TraceSupply_Bump_Data) ) ;
	trsup->trace = mutator->trace ;
	mm_flexArray_New( memmgt, &trsup->lastPushedAddresses, sizeof(WPtr), 0, 0 ) ;
	traceSupply->data = (MM_TraceSupply_Data_Priv*)trsup ;
	// mm_traceSupply_Bump_Reset( traceSupply ) ;
}

void mm_traceSupply_Bump_Run( MM_TraceSupply* traceSupply ) {
	MM_TraceSupply_Bump_Data* trsup = (MM_TraceSupply_Bump_Data*)traceSupply->data ;
	MM_Space* spc = trsup->trace->allocator->getSpace( trsup->trace->allocator ) ;

	// mm_traceSupply_Bump_Reset( traceSupply ) ;
	
	// IF_GB_TR_ON(3,{printf("mm_traceSupply_Bump_Run lastPushedFragment=%x lastTracedFragment=%x lPush=%p lTrace=%p\n",trsup->lastPushedFragment,trsup->lastTracedFragment,*mm_traceSupply_Bump_LastPushAddress( trsup, trsup->lastPushedFragment ),trsup->lastTracedAddress);}) ;
	Bool inSameFragment = trsup->lastTracedFragment == trsup->lastPushedFragment ;
	while ( *mm_traceSupply_Bump_LastPushAddress( trsup, trsup->lastPushedFragment ) < trsup->lastTracedAddress || ( ! inSameFragment ) ) {
		// lastPushedAddress + lastTracedAddress may change as side effect of tracing
		Word* lastPushedAddress = *( (WPtr*)mm_flexArray_At( &trsup->lastPushedAddresses, trsup->lastTracedFragment ) ) ;
		Word* lastTracedAddress = trsup->lastTracedAddress ;
		Word* lastTracedAddressNew = lastPushedAddress ;
		for ( ; lastPushedAddress < lastTracedAddress ; ) {
			Word szWords = trsup->trace->objectNrWords( trsup->trace, (Word)lastPushedAddress ) ;
			Word hdrSz = trsup->trace->objectHeaderNrWords ;
			// IF_GB_TR_ON(3,{printf("mm_traceSupply_Bump_Run obj=%p sz=%x tr=%x last=%x lastPushedFragment=%x lastTracedFragment=%x lPush=%p lTrace=%p\n",lastPushedAddress,szWords,lastPushedAddress + hdrSz,lastTracedAddress,trsup->lastPushedFragment,trsup->lastTracedFragment,*mm_traceSupply_Bump_LastPushAddress( trsup, trsup->lastPushedFragment ),trsup->lastTracedAddress);}) ;
			// !!!! Cannot rely on objectHasTraceableWords because traceable fields may be non-contiguous
			if ( trsup->trace->objectHasTraceableWords( trsup->trace, (Word)lastPushedAddress ) ) {
				// trsup->trace->traceObjects( trsup->trace, lastPushedAddress + hdrSz, szWords - hdrSz ) ;
				mm_trace_TraceObjects( trsup->trace, lastPushedAddress + hdrSz, szWords - hdrSz ) ;
			}
			lastPushedAddress += szWords ;
		}
		if ( inSameFragment ) {
			trsup->lastTracedAddress = lastTracedAddressNew ;
		} else {
			trsup->lastTracedFragment++ ;
			trsup->lastTracedAddress = (Word*)( spc->getFragment( spc, trsup->lastTracedFragment )->frag + (1 << spc->getGrowDefaultLog(spc)) ) ;
		}
		inSameFragment = trsup->lastTracedFragment == trsup->lastPushedFragment ;
	}
}

// push ptr + size
// extra is used by this supply as the fragment
void mm_traceSupply_Bump_PushWork( MM_TraceSupply* traceSupply, Word* work, Word nrWorkWords, Word extra ) {
	MM_TraceSupply_Bump_Data* trsup = (MM_TraceSupply_Bump_Data*)traceSupply->data ;
	// trsup->lastPushedAddress  = work  ;
	trsup->lastPushedFragment = extra ;
	*mm_traceSupply_Bump_LastPushAddress( trsup, extra ) = work ;
	// IF_GB_TR_ON(3,{printf("mm_traceSupply_Bump_PushWork lastPushedFragment=%x lastTracedFragment=%x lPush=%p lTrace=%p\n",trsup->lastPushedFragment,trsup->lastTracedFragment,*mm_traceSupply_Bump_LastPushAddress( trsup, trsup->lastPushedFragment ),trsup->lastTracedAddress);}) ;
}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Bump interface object
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
MM_TraceSupply mm_traceSupply_Bump =
	{ NULL
	, &mm_traceSupply_Bump_Init
	, MM_Undefined
	, &mm_traceSupply_Bump_Reset
	, &mm_traceSupply_Bump_Run
	, &mm_traceSupply_Bump_PushWork
	} ;
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Bump dump
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#ifdef TRACE
void mm_traceSupply_Bump_Dump( MM_TraceSupply* traceSupply ) {
}
#endif
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Bump test
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#ifdef TRACE
void mm_traceSupply_Bump_Test() {
}
#endif
%%]

