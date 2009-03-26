%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Memory management: range map
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

A RangeMap maps a range of consecutive values onto words.
It is to be used to (e.g.) map object frame starts to spaces.
It only grows, at both sides, which thus may involve reallocation.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Range Map
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
typedef uint32_t MM_RangeMap_Inx ;

typedef struct MM_RangeMap {
	WPtr	 				ptr	;		// start address of entries
	MM_RangeMap_Inx			size ;		// overall size, expressed in sizeof(Ptr)
	MM_RangeMap_Inx			firstInx ;	// first index
	MM_Malloc*				memMgt ;	// memory mgt
	Bool					didAlcDescr ;	// did alloc this, used for deallocation. bit of not nice, a hack
	Bool					didAlcPtr ;		// did alloc ptr
} MM_RangeMap ;

%%]


%%[8
// return new array in a, return a
// when a == NULL allocate descriptor in a as well, same for ptr
// entries uptil free are zeroed
extern MM_RangeMap* mm_rangeMap_New( MM_Malloc*, MM_RangeMap* a ) ;

// deallocate
extern void mm_rangeMap_Free( MM_RangeMap* a ) ;

// extend array by giving new first/last indices
extern void mm_rangeMap_Realloc( MM_RangeMap* a, WPtr ptr, MM_RangeMap_Inx firstInx, MM_RangeMap_Inx aftLastInx ) ;
%%]

%%[8
// range
static inline Bool mm_rangeMap_FirstInx( MM_RangeMap* a ) {
	return a->firstInx ;
}

static inline Bool mm_rangeMap_AfterLastInx( MM_RangeMap* a ) {
	return a->firstInx + a->size ;
}

// range check
static inline Bool mm_rangeMap_InRange( MM_RangeMap* a, MM_RangeMap_Inx i ) {
	i -= mm_rangeMap_FirstInx(a) ;
	return i >= 0 && i < a->size ;
}

// indexing
static inline WPtr mm_rangeMap_At( MM_RangeMap* a, MM_RangeMap_Inx i ) {
	return &(a->ptr[ i - mm_rangeMap_FirstInx(a) ]) ;
}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Dump
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#ifdef TRACE
extern void mm_rangeMap_Dump( MM_RangeMap* a ) ;
#endif
%%]
