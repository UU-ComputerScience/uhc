%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Memory management: allocator: list of free (LOF)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

List of Free (LOF).
-------------------
Explicit allocation and de-allocation of arbitrary sized chunks of mem.
Each size is allocated on a separate page (or pages), uptil a certain
bound MM_Allocator_LOF_NrRoundedFit. Beyond that, fragmentation is allowed
and (de)allocation is delegated to the underlying pages mgt.

LOF is intended to be used for:
- internal structures
- large, non GC'ed chunks of memory
- non movable data

LOF attempts to minimize fragmentation by allocating pages for 1 size,
or range of sizes only. More fragmentation and rounding of sizes is
allowed for larger sizes. No attempt is made to control/limit
fragmentation beyond the exact/rounded fit bound
MM_Allocator_LOF_NrRoundedFit. For large blocks of reusable memory a
special purpose allocator avoiding fragmentation should be written
sometime.


Allocation strategy/policy
--------------------------
A combination of 2 strategies
(1) For smaller sizes, a free list is kept for each size, with increasing sizes however, allocated sizes are rounded up.
    More precisely, with sizes expressed in Words:
    Let p = log2(page size), typically 10
        s = log2(nr slots per group of similarly rounding up), typically 4
        n = nr of such groups
    then the smallest 1..(2^s-1) sizes are allocated exactly without rounding up, 2^s free lists are kept,
    the next smallest 2^s..(2*2^s-1) sizes also are kept in 2^s free lists, but rounded up to 2^1,
    and so on.
    In this way:
    - sizes upto (2^n-1)*2^s can be accomodated
    - the biggest contiguous area allocated is of size 2^(n+p)
    - per contiguous area minimally 2^p / 2^s allocations can be done, external fragmentation is 1 / 2^(p-s),
      less usually because this is pessimistically estimated for the largest size possible.
      Internal fragmentation is 1 / 2^s (because of rounding up).
    - n*2^s free lists are kept
    
    n better be bounded, currently n = 6 so, for the typical values:
    - max contiguous  area = 2^(n+p) = 2^16 = 65K words
    - max allocatable size = (2^n-1)*2^s = (2^6-1)*2^4 = 1008 words
    
    The various constants and their corresponding #define:
      p = MM_Pages_MinSize_Log
      s = MM_Allocator_LOF_RoundGroupSize_Log
      n = MM_Allocator_LOF_NrRoundGroup
    It is yet unclear what the optimal values are.

(2) For the remaining sizes 1 free list for all sizes is maintained, currently with a first-fit searching policy.
    This is rather inefficient, a temporary solution.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% LOF defs & types
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#define MM_Allocator_LOF_RoundGroupSize_Log		4
#define MM_Allocator_LOF_RoundGroupSize			(1 << MM_Allocator_LOF_RoundGroupSize_Log)
#define MM_Allocator_LOF_NrRoundGroup			6

#define MM_Allocator_LOF_NrRoundedFit			(MM_Allocator_LOF_NrRoundGroup * MM_Allocator_LOF_RoundGroupSize)
#define MM_Allocator_LOF_MaxRoundedSize_Words	(((1 << MM_Allocator_LOF_NrRoundGroup) - 1) << MM_Allocator_LOF_RoundGroupSize_Log)
#define MM_Allocator_LOF_MaxRoundedSize			(MM_Allocator_LOF_MaxRoundedSize_Words << Word_SizeInBytes_Log)
%%]

%%[8
#define MM_Allocator_FreeOtherTag_Free		0		// free
#define MM_Allocator_FreeOtherTag_Alloced	1		// allocated
%%]

%%[8
// page admin for pages allocated to rounded size free lists
typedef struct MM_Allocator_LOF_PageRounded {
	struct MM_Allocator_LOF_PageRounded*		next ;		// list of used pages
} __attribute__ ((__packed__)) MM_Allocator_LOF_PageRounded ;

// free list admin for rounded sizes
// assumption: size of alloc >= sizeof(MM_Allocator_LOF_FreeRounded)
typedef struct MM_Allocator_LOF_FreeRounded {
	struct MM_Allocator_LOF_FreeRounded*	next ;		// list of free chunks
} __attribute__ ((__packed__)) MM_Allocator_LOF_FreeRounded ;

// per size admin, the size is implicit by its position in MM_Allocator_LOF_Data.perSizeRounded
typedef struct MM_Allocator_LOF_PerSize {
	MM_Allocator_LOF_FreeRounded*			free ;		// free chunks
	MM_Allocator_LOF_PageRounded*			pages ;		// used pages
	Word									chunkSize ;	// size of chunks as maintained by this PerSize
} __attribute__ ((__packed__)) MM_Allocator_LOF_PerSize ;
%%]

%%[8
// the administration
typedef struct MM_Allocator_LOF_Data {
	MM_Space* 							space ; 												// the space allocator to use
	MM_Allocator_LOF_PerSize			perSizeRounded[MM_Allocator_LOF_NrRoundedFit] ;			// rounded fit
} __attribute__ ((__packed__)) MM_Allocator_LOF_Data ;
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% LOF interface
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
extern void mm_allocator_LOF_Init( MM_Allocator*, MM_Malloc* memmgt, MM_Space* space ) ;
extern Ptr mm_allocator_LOF_Alloc( MM_Allocator*, Word sz, Word gcInfo ) ;
extern void mm_allocator_LOF_Dealloc( MM_Allocator*, Ptr ptr ) ;
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% LOF interface object
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
extern MM_Allocator mm_allocator_LOF ;
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Abstraction interface around allocation: LOF provided malloc
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
extern MM_Malloc mm_malloc_LOF ;
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% LOF test
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#ifdef TRACE
extern void mm_allocator_LOF_Test() ;
#endif
%%]

