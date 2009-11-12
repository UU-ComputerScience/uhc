%%[8
#ifndef __MM_FLEXARRAY_H__
#define __MM_FLEXARRAY_H__
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Memory management: flex array
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[doesWhat doclatex
A FlexArray is an array that extends itself when required, that is when
access to a slot is requested. A slot is identified by an index. The
array implementation is independent of actual slot type as it only nows
about the memory size occupied by the slot.
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
extern MM_FlexArray* mm_flexArray_New( MM_Malloc*, MM_FlexArray* a, MM_FlexArray_Inx szElt, MM_FlexArray_Inx sz, MM_FlexArray_Inx free ) ;

// deallocate
extern void mm_flexArray_Free( MM_FlexArray* a ) ;

// extend array by sz elements, realloc if necessary, bump free ptr.
// additional entries uptil free are zeroed
extern void mm_flexArray_ExtendBy( MM_FlexArray* a, MM_FlexArray_Inx sz ) ;

// new slot in array, at end
extern MM_FlexArray_Inx mm_flexArray_AllocSlot( MM_FlexArray* a ) ;

// ensure availability of slot
extern void mm_flexArray_EnsureSlot( MM_FlexArray* a, MM_FlexArray_Inx i ) ;

// indexing
static inline BPtr mm_flexArray_At( MM_FlexArray* a, MM_FlexArray_Inx i ) {
	return &(a->ptr[ i * a->sizeElt ]) ;
}

// inverse indexing, i.e. get index of pointer
static inline MM_FlexArray_Inx mm_flexArray_InxAtPtr( MM_FlexArray* a, BPtr p ) {
	return (p - a->ptr) / a->sizeElt ;
}

// add: allocate + set; return allocated index
static inline MM_FlexArray_Inx mm_flexArray_Add( MM_FlexArray* a, BPtr p ) {
	MM_FlexArray_Inx i = mm_flexArray_AllocSlot( a ) ;
	memcpy( mm_flexArray_At(a,i), p, a->sizeElt ) ;
	return i ;
}

// size of used part
static inline MM_FlexArray_Inx mm_flexArray_SizeUsed( MM_FlexArray* a ) {
	return a->free ;
}

// size of it all
static inline MM_FlexArray_Inx mm_flexArray_Size( MM_FlexArray* a ) {
	return a->size ;
}

// is address in range of flexarray?
static inline Bool mm_flexArray_IsInRange( MM_FlexArray* a, BPtr p ) {
	return p >= mm_flexArray_At(a,0) && p < mm_flexArray_At(a,a->size) ;
}

// still free slots available?
static inline MM_FlexArray_Inx mm_flexArray_NrFreeSlots( MM_FlexArray* a ) {
	return a->size - a->free ;
}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Iteration
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
extern void mm_flexArray_IteratorAt( MM_FlexArray* a, MM_Iterator* i, MM_FlexArray_Inx atInx ) ;

static inline void mm_flexArray_Iterator( MM_FlexArray* a, MM_Iterator* i ) {
	mm_flexArray_IteratorAt( a, i, 0 ) ;
}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% FlexArray dump
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#ifdef TRACE
extern void mm_flexArray_Dump( MM_FlexArray* a ) ;
#endif
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% EOF
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#endif /* __MM_FLEXARRAY_H__ */
%%]
