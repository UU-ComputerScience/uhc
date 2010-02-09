%%[8
#ifndef __MM_FREELISTARRAY_H__
#define __MM_FREELISTARRAY_H__
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Memory management: free list array
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[doesWhat doclatex
A MM_FreeListArray builds on top of a FlexArray by allowing slots to be
reused. The reusable slots are remembered in a linked list embedded in
the unused slots. The reasons for the existence of FreeListArray
(instead of a plain list) are:

\begin{itemize}
\item Slots must be accessible via indices.
\item Space economy; no additional pointer is required for list linking.
\end{itemize}

The 'next entry' in the free list is encoded by the index into the
array, to make it position independent.

Checking whether a slot is free solely based on the index can only be
done when the user of MM_FreeListArray guarantees that the value on the
position of this 'next entry' never falls within the range of indices
used by the MM_FreeListArray. Otherwise the user has to mark & encode a
slot itself as free.
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% A MM_FreeListArray is a MM_FlexArray with additional free list admin, embedded into unused slots
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#define MM_FreeListArray_FreeListNULL 	-1
%%]

%%[8
typedef struct MM_FreeListArray {
	MM_FlexArray		flexarray ;		// embedded flex array
	Word				listPtrOff ;	// offset to free list 'next ptr', relative to start of slot
	MM_FlexArray_Inx	freeList ;		// first entry (index) into free list, with null ==  MM_FreeListArray_FreeListNULL
	Bool				didAlc ;		// did alloc this, used for deallocation. bit of not nice, a hack
} MM_FreeListArray ;

%%]


%%[8
// return new array in a, return a
// when a == NULL allocate descriptor in a as well.
// entries uptil free are zeroed
extern MM_FreeListArray* mm_freeListArray_New( MM_Malloc*, MM_FreeListArray* a, MM_FlexArray_Inx szElt, MM_FlexArray_Inx sz, MM_FlexArray_Inx free, Word listPtrOff ) ;

// deallocate
extern void mm_freeListArray_Free( MM_FreeListArray* a ) ;

// new slot in array, at end
extern MM_FlexArray_Inx mm_freeListArray_AllocSlot( MM_FreeListArray* a ) ;

// new slot in array, at end
void mm_freeListArray_DeallocSlot( MM_FreeListArray* a, MM_FlexArray_Inx i ) ;

// indexing
static inline BPtr mm_freeListArray_At( MM_FreeListArray* a, MM_FlexArray_Inx i ) {
	return mm_flexArray_At( &a->flexarray, i ) ;
}

// add: allocate + set; return allocated index
static inline MM_FlexArray_Inx mm_freeListArray_Add( MM_FreeListArray* a, BPtr p ) {
	MM_FlexArray_Inx i = mm_freeListArray_AllocSlot( a ) ;
	memcpy( mm_freeListArray_At(a,i), p, a->flexarray.sizeElt ) ;
	return i ;
}

// free list 'next entry', meant for internal use only
static inline MM_FlexArray_Inx* mm_freeListArray_FreeListNextAt( MM_FreeListArray* a, MM_FlexArray_Inx i ) {
	return (MM_FlexArray_Inx*)( mm_flexArray_At( &a->flexarray, i ) + a->listPtrOff ) ;
}

// in use at index? See remarks at doc of this file.
static inline Bool mm_freeListArray_IsInUseAt( MM_FreeListArray* a, MM_FlexArray_Inx i ) {
	MM_FlexArray_Inx next = *mm_freeListArray_FreeListNextAt( a, i ) ;
	return next >= mm_flexArray_SizeUsed(&a->flexarray) && next != MM_FreeListArray_FreeListNULL ;
}

// size of used part, not taking into account free list
static inline MM_FlexArray_Inx mm_freeListArray_SizeUsed( MM_FreeListArray* a ) {
	return mm_flexArray_SizeUsed(&a->flexarray) ;
}

// size of it all
static inline MM_FlexArray_Inx mm_freeListArray_Size( MM_FreeListArray* a ) {
	return mm_flexArray_Size(&a->flexarray) ;
}

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Iteration
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
extern void mm_freeListArray_IteratorAt( MM_FreeListArray* a, MM_Iterator* i, MM_FlexArray_Inx atInx ) ;

static inline void mm_freeListArray_Iterator( MM_FreeListArray* a, MM_Iterator* i ) {
	mm_freeListArray_IteratorAt( a, i, 0 ) ;
}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% FlexArray dump
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#ifdef TRACE
extern void mm_freeListArray_Dump( MM_FreeListArray* a ) ;
#endif
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% EOF
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#endif /* __MM_FREELISTARRAY_H__ */
%%]
