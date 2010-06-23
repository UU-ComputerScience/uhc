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

%%[8
void mm_allocator_Bump_Alloc_InitFromFragment( MM_Allocator_Bump_Data* alc, MM_Space_FragmentInx fragInx ) {
	MM_Space_Fragment* frg = alc->space->getFragment( alc->space, fragInx ) ;
	alc->addrFirstFree  = (Word)frg->frag ;
	alc->addrCursorFree = (Word)frg->frag + MM_GC_CopySpace_FragmentSize ;
}

void mm_allocator_Bump_Alloc_NewFragment( MM_Allocator_Bump_Data* alc ) {
	alc->curFragmentInx = alc->space->growSpaceByDefault( alc->space ) ;
	mm_allocator_Bump_Alloc_InitFromFragment( alc, alc->curFragmentInx ) ;
	// printf( "mm_allocator_Bump_Alloc_NewFragment\n" ) ;
}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Bump internal functions, but still externally visible (because of inlining)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
Ptr mm_allocator_Bump_Alloc_AndEnsureSpace( MM_Allocator_Bump_Data* alc, Word sz, Word gcInfo ) {
	MM_Space_FragmentInx nrFrags = alc->space->getNrFragments( alc->space ) ;
	if ( alc->curFragmentInx + 1 < nrFrags ) {
		// still enough pre-allocated fragments
		IF_GB_TR_ON(3,printf("mm_allocator_Bump_Alloc_AndEnsureSpace use prealloc frag\n");) ;
		alc->curFragmentInx++ ;
		mm_allocator_Bump_Alloc_InitFromFragment( alc, alc->curFragmentInx ) ;
	} else if ( nrFrags >= alc->maxFragments ) {
		// max nr of fragments consumed, trigger GC, this will (amongst other things) reset this allocator
		IF_GB_TR_ON(3,printf("mm_allocator_Bump_Alloc_AndEnsureSpace kick GC\n");) ;
		mm_plan.doGC( &mm_plan, False, gcInfo ) ;
	} else {
		// get a new fragment
		IF_GB_TR_ON(3,printf("mm_allocator_Bump_Alloc_AndEnsureSpace new frag\n");) ;
		mm_allocator_Bump_Alloc_NewFragment( alc ) ;
	}
	// retry allocation again, should (cannot) not fail!!!
	return mm_allocator_Bump_Alloc_AndCheckCursor( alc, sz, gcInfo ) ;
}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Bump interface
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
void mm_allocator_Bump_ResetWithSpace( MM_Allocator* alcr, MM_Space* space ) {
	MM_Allocator_Bump_Data* alc = (MM_Allocator_Bump_Data*)alcr->data ;	

	alc->space = space ;
	MM_Space_FragmentInx nrFrags = space->getNrFragments( space ) ;
	if ( nrFrags > 0 ) {
		// start with yet available fragments
		alc->curFragmentInx = 0 ;
		mm_allocator_Bump_Alloc_InitFromFragment( alc, alc->curFragmentInx ) ;
	} else {
		// first request will trigger allocation
		// alc->addrCursorFree = alc->addrFirstFree = -1 ;
		// alc->curFragmentInx = -1 ;
		mm_allocator_Bump_Alloc_NewFragment( alc ) ;
	}
	// trigger 1st allocation
	// mm_allocator_Bump_Alloc_AndEnsureSpace( alc, 0 ) ;
}

void mm_allocator_Bump_Init( MM_Allocator* alcr, MM_Malloc* memmgt, MM_Space* space ) {
	MM_Allocator_Bump_Data* alc = (MM_Allocator_Bump_Data*)memmgt->malloc( sizeof(MM_Allocator_Bump_Data) ) ;
	
	alc->maxFragments = MM_Allocator_GC_FragmentInitialMax ;
	
	alcr->data = (MM_Allocator_Data_Priv*)alc ;
	
	mm_allocator_Bump_ResetWithSpace( alcr, space ) ;
}

void mm_allocator_Bump_Dealloc( MM_Allocator* alcr, Ptr ptr ) {
	// no effect
}

Ptr mm_allocator_Bump_LastAllocAddress( MM_Allocator* alcr ) {
	MM_Allocator_Bump_Data* alc = (MM_Allocator_Bump_Data*)alcr->data ;	
	return (Ptr)alc->addrCursorFree ;
}

MM_Space_FragmentInx mm_allocator_Bump_LastAllocFragment( MM_Allocator* alcr ) {
	MM_Allocator_Bump_Data* alc = (MM_Allocator_Bump_Data*)alcr->data ;	
	return alc->curFragmentInx ;
}

Word mm_allocator_Bump_GetTotalSize( MM_Allocator* alcr ) {
	MM_Allocator_Bump_Data* alc = (MM_Allocator_Bump_Data*)alcr->data ;	
	return alc->maxFragments << alc->space->getGrowDefaultLog(alc->space) ;
}

void mm_allocator_Bump_SetTotalSize( MM_Allocator* alcr, Word sz ) {
	MM_Allocator_Bump_Data* alc = (MM_Allocator_Bump_Data*)alcr->data ;	
	alc->maxFragments = EntierLogUpShrBy( sz, alc->space->getGrowDefaultLog(alc->space) ) ;
	IF_GB_TR_ON(3,{printf("mm_allocator_Bump_SetTotalSize sz=%x alc->maxFragments=%x\n",sz,alc->maxFragments);}) ;
}

Word mm_allocator_Bump_GetUsedSize( MM_Allocator* alcr ) {
	MM_Allocator_Bump_Data* alc = (MM_Allocator_Bump_Data*)alcr->data ;	
	return
		  ((alc->curFragmentInx + 1) << alc->space->getGrowDefaultLog(alc->space))
		- (alc->addrCursorFree - alc->addrFirstFree)
		;
}

MM_Space* mm_allocator_Bump_GetSpace( MM_Allocator* alcr ) {
	MM_Allocator_Bump_Data* alc = (MM_Allocator_Bump_Data*)alcr->data ;	
	return alc->space ;
}

%%]

// check cursor for sufficient space + bump/alloc
static inline Ptr mm_allocator_Bump_Alloc_AndCheckCursor( MM_Allocator_Bump_Data* alc, Word sz, Word gcInfo ) {
	// printf("mm_allocator_Bump_Alloc_AndCheckCursor 1 sz=%x cursor=%x free=%x space=%x\n", sz, alc->addrCursorFree, alc->addrFirstFree, alc->space);
	alc->addrCursorFree -= sz ;
	if ( alc->addrCursorFree < alc->addrFirstFree ) {
		mm_allocator_Bump_Alloc_AndEnsureSpace( alc, sz, gcInfo ) ;
	}
	// printf("mm_allocator_Bump_Alloc_AndCheckCursor 2 sz=%x cursor=%x free=%x space=%x\n", sz, alc->addrCursorFree, alc->addrFirstFree, alc->space);
	return (Ptr)alc->addrCursorFree ;
}

// this function must be inlined, not yet catered for
// assumptions:
// (1) sz <= MM_Pages_MinSize, i.e. we do not need to cater for large objects
// (2) sz % Word_SizeInBytes == 0, i.e. we do not need to align the size
Ptr mm_allocator_Bump_Alloc( MM_Allocator* alcr, Word sz, Word gcInfo ) {
	MM_Allocator_Bump_Data* alc = (MM_Allocator_Bump_Data*)alcr->data ;	
	return mm_allocator_Bump_Alloc_AndCheckCursor( alc, sz, gcInfo ) ;
}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Bump dump
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#ifdef TRACE
void mm_allocator_Bump_Dump( MM_Allocator* alcr ) {
	MM_Allocator_Bump_Data* alc = (MM_Allocator_Bump_Data*)alcr->data ;

	printf( ">------------------------> MM_Allocator: Bump\n" ) ;
	printf
		( "Bump: addrCursorFree=%x addrFirstFree=%x curFragmentInx=%x maxFragments=%x\n"
		, alc->addrCursorFree, alc->addrFirstFree, alc->curFragmentInx, alc->maxFragments
		) ;
	
	printf( "<------------------------< MM_Allocator: Bump\n" ) ;
}
#endif
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Bump interface object
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
MM_Allocator mm_allocator_Bump =
	{ NULL
	, &mm_allocator_Bump_Init
	, &mm_allocator_Bump_ResetWithSpace
	, &mm_allocator_Bump_Alloc
	, &mm_allocator_Bump_Ensure
	, &mm_allocator_Bump_AllocEnsured
	, &mm_allocator_Bump_Dealloc
	, &mm_allocator_Bump_LastAllocAddress
	, &mm_allocator_Bump_LastAllocFragment
	, &mm_allocator_Bump_GetTotalSize
	, &mm_allocator_Bump_SetTotalSize
	, &mm_allocator_Bump_GetUsedSize
	, &mm_allocator_Bump_GetSpace
#ifdef TRACE
	, &mm_allocator_Bump_Dump
#endif
	, (MM_GC_CopySpace_FragmentSize >> 2)	// 1/4 of a fragment
	} ;
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

