%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Memory management: allocator: list of free (LOF)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

List of Free (LOF).
-------------------
Explicit allocation and de-allocation of arbitrary sized chunks of mem.
Each size is allocated on a separate page (or pages), uptil a certain
bound MM_Allocator_LOF_NrExactFit. Beyond that, fragmentation is allowed
and (de)allocation is delegated to the underlying pages mgt.

LOF is intended to be used for:
- internal structures
- large, non GC'ed chunks of memory
- non movable data

No attempt is made to control/limit fragmentation beyond the exact fit
bound MM_Allocator_LOF_NrExactFit. For large blocks of reusable memory a
special purpose allocator avoiding fragmentation should be written
sometime.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% LOF defs & types
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
typedef struct MM_Allocator_LOF_Page {
	struct MM_Allocator_LOF_Page*		next ;		// list of used pages
} __attribute__ ((__packed__)) MM_Allocator_LOF_Page ;

// assumption: size of alloc >= sizeof(MM_Allocator_LOF_ExactFree)
typedef struct MM_Allocator_LOF_ExactFree {
	struct MM_Allocator_LOF_ExactFree*	next ;		// list of free chunks
} __attribute__ ((__packed__)) MM_Allocator_LOF_ExactFree ;

typedef struct MM_Allocator_LOF_OtherFree {
	struct MM_Allocator_LOF_OtherFree*	next ;		// list of free chunks
	Word								size ;		// size of this free chunk
} __attribute__ ((__packed__)) MM_Allocator_LOF_OtherFree ;

// per size admin, the size is implicit by its position in MM_Allocator_LOF_Data.perSizeExact
typedef struct MM_Allocator_LOF_PerSize {
	MM_Allocator_LOF_ExactFree*			free ;		// free chunks
	MM_Allocator_LOF_Page*				pages ;		// used pages
} __attribute__ ((__packed__)) MM_Allocator_LOF_PerSize ;
%%]

%%[8
#define MM_Allocator_LOF_NrExactFit		((MM_Pages_MinSize - sizeof(struct MM_Allocator_LOF_Data_Other)) / sizeof(MM_Allocator_LOF_PerSize))
%%]

%%[8
// the administration
typedef struct MM_Allocator_LOF_Data {
	struct MM_Allocator_LOF_Data_Other {
		MM_Allocator_LOF_OtherFree*		free ;		// free list of other sizes
	} otherSize ;
	MM_Allocator_LOF_PerSize	perSizeExact[MM_Allocator_LOF_NrExactFit] ;			// exact fit
} __attribute__ ((__packed__)) MM_Allocator_LOF_Data ;
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% LOF interface
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
extern void mm_allocator_LOF_Init( MM_Allocator* ) ;
extern Ptr mm_allocator_LOF_Alloc( MM_Allocator*, Word sz ) ;
extern void mm_allocator_LOF_Dealloc( MM_Allocator*, Ptr ptr ) ;
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% LOF interface object
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
extern MM_Allocator mm_allocator_LOF ;
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% LOF test
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#ifdef TRACE
extern void mm_allocator_LOF_Test() ;
#endif
%%]

