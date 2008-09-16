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
extern Ptr mm_allocator_Bump_Alloc_AndEnsureSpace( MM_Allocator_Bump_Data* alc, Word sz ) ;
%%]

%%[8
// check cursor for sufficient space + bump/alloc
static inline Ptr mm_allocator_Bump_Alloc_AndCheckCursor( MM_Allocator_Bump_Data* alc, Word sz ) {
	alc->addrCursorFree -= sz ;
	if ( alc->addrCursorFree < alc->addrFirstFree ) {
		mm_allocator_Bump_Alloc_AndEnsureSpace( alc, sz ) ;
	}
	printf("mm_allocator_Bump_Alloc_AndCheckCursor sz=%x p=%x space=%x\n", sz, alc->addrCursorFree, alc->space);
	return (Ptr)alc->addrCursorFree ;
}

// check cursor for sufficient space
static inline void mm_allocator_Bump_Alloc_CheckCursor( MM_Allocator_Bump_Data* alc, Word sz ) {
	if ( alc->addrCursorFree - sz < alc->addrFirstFree ) {
		mm_allocator_Bump_Alloc_AndEnsureSpace( alc, sz ) ;
	}
}

// bump the cursor
static inline Ptr mm_allocator_Bump_Alloc_FromCursor( MM_Allocator_Bump_Data* alc, Word sz ) {
	return (Ptr)(alc->addrCursorFree -= sz) ;
}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  interface
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
%%]
extern void mm_allocator_Bump_Init( MM_Allocator*, MM_Malloc* memmgt, MM_Space* space ) ;
extern Ptr mm_allocator_Bump_Alloc( MM_Allocator*, Word sz ) ;
extern void mm_allocator_Bump_Dealloc( MM_Allocator*, Ptr ptr ) ;

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

