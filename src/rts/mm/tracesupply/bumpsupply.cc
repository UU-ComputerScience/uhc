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
static inline WPtr* mm_traceSupply_Bump_LastPushAddress( MM_TraceSupply_Bump_Data* trgr, MM_Space_FragmentInx inx ) {
	return (WPtr*)mm_flexArray_At( &trgr->lastPushedAddresses, inx ) ;
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
void mm_traceSupply_Bump_Reset( MM_TraceSupply* traceSupply ) {
	MM_TraceSupply_Bump_Data* trgr = (MM_TraceSupply_Bump_Data*)traceSupply->data ;
	MM_Space* spc = trgr->trace->allocator->getSpace( trgr->trace->allocator ) ;

	// mm_flexArray_EnsureSlot( &trgr->lastPushedAddresses, spc->getNrFragments(spc) ) ;
	mm_flexArray_EnsureSlot( &trgr->lastPushedAddresses, trgr->trace->allocator->getTotalSize(trgr->trace->allocator) >> spc->getGrowDefaultLog(spc) ) ;
	
	trgr->lastTracedFragment
		= trgr->lastPushedFragment
		= trgr->trace->allocator->lastAllocFragment( trgr->trace->allocator ) ;
	trgr->lastTracedAddress
		= *mm_traceSupply_Bump_LastPushAddress( trgr, trgr->lastPushedFragment )
		= (Word*)trgr->trace->allocator->lastAllocAddress( trgr->trace->allocator ) ;
	IF_GB_TR_ON(3,{printf("mm_traceSupply_Bump_Reset last frag=%x flexsz=%x lastPushedFragment=%x lastTracedFragment=%x lastPushedAddress=%x lastTracedAddress=%x\n",trgr->trace->allocator->lastAllocFragment( trgr->trace->allocator ),trgr->trace->allocator->getTotalSize(trgr->trace->allocator) >> spc->getGrowDefaultLog(spc),trgr->lastPushedFragment,trgr->lastTracedFragment,*mm_traceSupply_Bump_LastPushAddress( trgr, trgr->lastPushedFragment ),trgr->lastTracedAddress);}) ;
}

void mm_traceSupply_Bump_Init( MM_TraceSupply* traceSupply, MM_Malloc* memmgt, MM_Trace* trace ) {
	MM_TraceSupply_Bump_Data* trgr = memmgt->malloc( sizeof(MM_TraceSupply_Bump_Data) ) ;
	trgr->trace = trace ;
	mm_flexArray_New( memmgt, &trgr->lastPushedAddresses, sizeof(WPtr), 0, 0 ) ;
	traceSupply->data = (MM_TraceSupply_Data_Priv*)trgr ;
	// mm_traceSupply_Bump_Reset( traceSupply ) ;
}

void mm_traceSupply_Bump_Run( MM_TraceSupply* traceSupply ) {
	MM_TraceSupply_Bump_Data* trgr = (MM_TraceSupply_Bump_Data*)traceSupply->data ;
	MM_Space* spc = trgr->trace->allocator->getSpace( trgr->trace->allocator ) ;

	// mm_traceSupply_Bump_Reset( traceSupply ) ;
	
	IF_GB_TR_ON(3,{printf("mm_traceSupply_Bump_Run lastPushedFragment=%x lastTracedFragment=%x lPush=%x lTrace=%x\n",trgr->lastPushedFragment,trgr->lastTracedFragment,*mm_traceSupply_Bump_LastPushAddress( trgr, trgr->lastPushedFragment ),trgr->lastTracedAddress);}) ;
	Bool inSameFragment = trgr->lastTracedFragment == trgr->lastPushedFragment ;
	while ( *mm_traceSupply_Bump_LastPushAddress( trgr, trgr->lastPushedFragment ) < trgr->lastTracedAddress || ( ! inSameFragment ) ) {
		// lastPushedAddress + lastTracedAddress may change as side effect of tracing
		Word* lastPushedAddress = *( (WPtr*)mm_flexArray_At( &trgr->lastPushedAddresses, trgr->lastTracedFragment ) ) ;
		Word* lastTracedAddress = trgr->lastTracedAddress ;
		Word* lastTracedAddressNew = lastPushedAddress ;
		for ( ; lastPushedAddress < lastTracedAddress ; ) {
			Word szWords = trgr->trace->objectNrWords( trgr->trace, (Word)lastPushedAddress ) ;
			Word hdrSz = trgr->trace->objectHeaderNrWords ;
			IF_GB_TR_ON(3,{printf("mm_traceSupply_Bump_Run obj=%x sz=%x tr=%x last=%x lastPushedFragment=%x lastTracedFragment=%x lPush=%x lTrace=%x\n",lastPushedAddress,szWords,lastPushedAddress + hdrSz,lastTracedAddress,trgr->lastPushedFragment,trgr->lastTracedFragment,*mm_traceSupply_Bump_LastPushAddress( trgr, trgr->lastPushedFragment ),trgr->lastTracedAddress);}) ;
			trgr->trace->traceObjects( trgr->trace, lastPushedAddress + hdrSz, szWords - hdrSz, MM_Trace_Flg_All ) ;
			lastPushedAddress += szWords ;
		}
		if ( inSameFragment ) {
			trgr->lastTracedAddress = lastTracedAddressNew ;
		} else {
			trgr->lastTracedFragment++ ;
			trgr->lastTracedAddress = (Word*)( spc->getFragment( spc, trgr->lastTracedFragment )->frag + (1 << spc->getGrowDefaultLog(spc)) ) ;
		}
		inSameFragment = trgr->lastTracedFragment == trgr->lastPushedFragment ;
	}
}

// push ptr + size
// extra is used by this supply as the fragment
void mm_traceSupply_Bump_PushWork( MM_TraceSupply* traceSupply, Word* work, Word nrWorkWords, Word extra ) {
	MM_TraceSupply_Bump_Data* trgr = (MM_TraceSupply_Bump_Data*)traceSupply->data ;
	// trgr->lastPushedAddress  = work  ;
	trgr->lastPushedFragment = extra ;
	*mm_traceSupply_Bump_LastPushAddress( trgr, extra ) = work ;
	IF_GB_TR_ON(3,{printf("mm_traceSupply_Bump_PushWork lastPushedFragment=%x lastTracedFragment=%x lPush=%x lTrace=%x\n",trgr->lastPushedFragment,trgr->lastTracedFragment,*mm_traceSupply_Bump_LastPushAddress( trgr, trgr->lastPushedFragment ),trgr->lastTracedAddress);}) ;
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

