%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Memory management: basic/common definitions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Hard constants
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

The size of a page, basic unit of contiguous mem allocation.

%%[8
#define MM_Page_Size_Log						(10 + Word_SizeInBytes_Log)
#define MM_Page_Size							(1 << MM_Page_Size_Log)
%%]

For Fragments as used by GC allocators

%%[8
#define MM_Allocator_GC_FragmentSize_Log		(4 + MM_Pages_MinSize_Log)
#define MM_Allocator_GC_FragmentSize			(1 << MM_Allocator_GC_FragmentSize_Log)

#define MM_Allocator_GC_FragmentInitialMax		4
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Checking wrapper around malloc etc
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
extern Ptr mm_malloc( size_t size ) ;
extern Ptr mm_realloc( Ptr ptr, size_t size ) ;
extern void mm_free( Ptr ptr ) ;
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Abstraction interface around allocation,
%%% to be able to bootstrap with malloc as well as use other malloc alike impls
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
typedef struct MM_Malloc {
	Ptr 	(*malloc)( size_t size ) ;
	Ptr 	(*realloc)( Ptr ptr, size_t size ) ;
	void 	(*free)( Ptr ptr ) ;
} MM_Malloc ;
%%]

%%[8
extern MM_Malloc 	mm_malloc_Sys ;
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Lowest level flexible size ptr array mem managed via malloc/realloc, basically for bootstrapping
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
typedef uint32_t MM_FlexArray_Inx ;

typedef struct MM_FlexArray {
	MM_FlexArray_Inx		sizeElt ;	// size of elements
	BPtr	 				ptr	;
	MM_FlexArray_Inx		size ;		// overall size, expressed in sizeof(Ptr)
	MM_FlexArray_Inx		free ;		// first free slot, free <= size
	MM_Malloc*				memMgt ;	// memory mgt
	Bool					didAlc ;	// did alloc this, used for deallocation. bit of not nice, a hack
} MM_FlexArray ;

%%]


%%[8
// return new array in a, return a
// when a == NULL allocate descriptor in a as well.
// entries uptil free are zeroed
extern MM_FlexArray* mm_flexArray_New( MM_Malloc*, MM_FlexArray* a, MM_FlexArray_Inx szElt, MM_FlexArray_Inx sz ) ;

// deallocate
extern void mm_flexArray_Free( MM_FlexArray* a ) ;

// extend array by sz elements, realloc if necessary, bump free ptr.
// additional entries uptil free are zeroed
extern void mm_flexArray_ExtendBy( MM_FlexArray* a, MM_FlexArray_Inx sz ) ;

// new slot in array, at end
extern MM_FlexArray_Inx mm_flexArray_NewSlot( MM_FlexArray* a ) ;

// ensure availability of slot
extern void mm_flexArray_EnsureSlot( MM_FlexArray* a, MM_FlexArray_Inx i ) ;

// indexing
static inline BPtr mm_flexArray_At( MM_FlexArray* a, MM_FlexArray_Inx i ) {
	return &(a->ptr[ i * a->sizeElt ]) ;
}

// size of used part
static inline MM_FlexArray_Inx mm_flexArray_SizeUsed( MM_FlexArray* a ) {
	return a->free ;
}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Undefined
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
extern void mm_undefined(void) ;
%%]
