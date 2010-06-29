%%[8
#ifndef __MM_ALLOCATOR_BUMP_H__
#define __MM_ALLOCATOR_BUMP_H__
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Memory management: allocator: bump pointer
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Bump allocation simply increments a cursor, and returns as its allocated
memory a pointer to this increment. Its allocation code is meant to be
inlined, which it currently not yet is.

Bumping goes from high to low addresses, so as to avoid holding a result in some location whilst bumping.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  defs & types
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
// the administration
typedef struct MM_Allocator_Bump_Data {
	MM_Space*				space ;
	Word					addrCursorFree ;	// cursor free location
	Word					addrFirstFree ;		// first free location
	MM_Space_FragmentInx	curFragmentInx ;	// current fragment used for alloc
	MM_Space_FragmentInx	maxFragments ;		// max fragments allocated before GC is triggered
} MM_Allocator_Bump_Data ;
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% interface: (inlineable) ingredients
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
// this may trigger GC
extern Ptr mm_allocator_Bump_Alloc_AndEnsureSpace( MM_Allocator_Bump_Data* alc, Word sz, Word gcInfo ) ;
%%]

%%[8
// (only) check cursor for sufficient space
static inline void mm_allocator_Bump_Alloc_CheckCursor( MM_Allocator_Bump_Data* alc, Word sz, Word gcInfo ) {
	if ( alc->addrCursorFree - sz < alc->addrFirstFree ) {
		mm_allocator_Bump_Alloc_AndEnsureSpace( alc, sz, gcInfo ) ;
	}
}

// (only) bump the cursor
static inline Ptr mm_allocator_Bump_Alloc_FromCursor( MM_Allocator_Bump_Data* alc, Word sz ) {
	return (Ptr)(alc->addrCursorFree -= sz) ;
}

// (both) check cursor for sufficient space + bump/alloc
static inline Ptr mm_allocator_Bump_Alloc_AndCheckCursor( MM_Allocator_Bump_Data* alc, Word sz, Word gcInfo ) {
	// printf("mm_allocator_Bump_Alloc_AndCheckCursor 1 sz=%x cursor=%x free=%x space=%x\n", sz, alc->addrCursorFree, alc->addrFirstFree, alc->space);
	alc->addrCursorFree -= sz ;
	if ( alc->addrCursorFree < alc->addrFirstFree ) {
		mm_allocator_Bump_Alloc_AndEnsureSpace( alc, sz, gcInfo ) ;
	}
	// printf("mm_allocator_Bump_Alloc_AndCheckCursor 2 sz=%x cursor(alloced)=%x free=%x space=%x\n", sz, alc->addrCursorFree, alc->addrFirstFree, alc->space);
	return (Ptr)alc->addrCursorFree ;
}
%%]

%%[8
// the actual alloc function
// assumptions:
// (1) sz <= MM_Pages_MinSize, i.e. we do not need to cater for large objects
// (2) sz % Word_SizeInBytes == 0, i.e. we do not need to align the size
static inline Ptr mm_allocator_Bump_Alloc( MM_Allocator* alcr, Word sz, Word gcInfo ) {
	MM_Allocator_Bump_Data* alc = (MM_Allocator_Bump_Data*)alcr->data ;	
	return mm_allocator_Bump_Alloc_AndCheckCursor( alc, sz, gcInfo ) ;
}

%%]

%%[8
// part of alloc which only checks for sufficient memory
static inline void mm_allocator_Bump_Ensure( MM_Allocator* alcr, Word sz, Word gcInfo ) {
	MM_Allocator_Bump_Data* alc = (MM_Allocator_Bump_Data*)alcr->data ;	
	mm_allocator_Bump_Alloc_CheckCursor( alc, sz, gcInfo ) ;
}

// part of alloc which only allocates, assuming there is enough memory
static inline Ptr mm_allocator_Bump_AllocEnsured( MM_Allocator* alcr, Word sz ) {
	MM_Allocator_Bump_Data* alc = (MM_Allocator_Bump_Data*)alcr->data ;	
	return mm_allocator_Bump_Alloc_FromCursor( alc, sz ) ;
}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  interface
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  interface object
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
extern MM_Allocator mm_allocator_Bump ;
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  test
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#ifdef TRACE
extern void mm_allocator_Bump_Test() ;
#endif
%%]

%%[8
#endif /* __MM_ALLOCATOR_BUMP_H__ */
%%]

