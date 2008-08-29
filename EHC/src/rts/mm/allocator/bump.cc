%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Memory management: allocator: Bump
%%% see associated .ch file for more info.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#include "../../rts.h"
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Bump internal defs
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Bump internal functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Bump internal functions, but still externally visible (because of inlining)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
Ptr mm_allocator_Bump_Alloc_AndEnsureSpace( MM_Allocator_Bump_Data* alc, Word sz ) {
	if ( alc->space->getNrFragments(alc->space) >= alc->maxFragments ) {
		// trigger GC
	} else {
		Word alcInx = alc->space->growSpaceLog2( alc->space, MM_Allocator_GC_FragmentSize_Log ) ;
		MM_Space_Fragment* frg = alc->space->getFragment( alc->space, alcInx ) ;
		alc->addrFirstFree  = (Word)frg->frag ;
		alc->addrCursorFree = (Word)frg->frag + MM_Allocator_GC_FragmentSize ;
	}
	return mm_allocator_Bump_Alloc_AndCheckCursor( alc, sz ) ;
}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Bump interface
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
void mm_allocator_Bump_Init( MM_Allocator* alcr, MM_Malloc* memmgt, MM_Space* space ) {
	MM_Allocator_Bump_Data* alc = (MM_Allocator_Bump_Data*)memmgt->malloc( sizeof(MM_Allocator_Bump_Data) ) ;
	
	alc->space = space ;
	alc->maxFragments = MM_Allocator_GC_FragmentInitialMax ;
	// first request will trigger allocation
	alc->addrCursorFree = alc->addrFirstFree = -1 ;
	alc->fragmentAfterFree = 0 ;
	
	alcr->data = (MM_Allocator_Data_Priv*)alc ;
}

// this function must be inlined, not yet catered for
Ptr mm_allocator_Bump_Alloc( MM_Allocator* alcr, Word sz ) {
	MM_Allocator_Bump_Data* alc = (MM_Allocator_Bump_Data*)alcr->data ;	
	return mm_allocator_Bump_Alloc_AndCheckCursor( alc, sz ) ;
}

void mm_allocator_Bump_Dealloc( MM_Allocator* alcr, Ptr ptr ) {
	// no effect
}

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Bump interface object
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
MM_Allocator mm_allocator_Bump =
	{ NULL
	, &mm_allocator_Bump_Init
	, &mm_allocator_Bump_Alloc
	, &mm_allocator_Bump_Dealloc
	} ;
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Bump dump
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#ifdef TRACE
mm_allocator_Bump_Dump( MM_Allocator* alcr ) {
}
#endif
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Bump test
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#ifdef TRACE
#define II 1000

void mm_allocator_Bump_Test() {
}
#endif
%%]

