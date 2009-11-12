%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Memory management: free list array
%%% see associated .ch file for more info.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#include "../../rts.h"
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% A MM_FreeListArray is a MM_FlexArray with additional free list admin, embedded into unused slots
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
MM_FreeListArray* mm_freeListArray_New( MM_Malloc* memmgt, MM_FreeListArray* a, MM_FlexArray_Inx szElt, MM_FlexArray_Inx sz, MM_FlexArray_Inx free, Word listPtrOff ) {
	if ( a == NULL ) {
		a = (MM_FreeListArray*) memmgt->malloc( sizeof( MM_FreeListArray ) ) ;
		a->didAlc = True ;
	} else {
		a->didAlc = False ;
	}
	mm_flexArray_New( memmgt, &a->flexarray, szElt, sz, free ) ;
	a->listPtrOff = listPtrOff ;
	a->freeList = MM_FreeListArray_FreeListNULL ;
	return a ;
}

void mm_freeListArray_Free( MM_FreeListArray* a ) {
	mm_flexArray_Free( &a->flexarray ) ;
	if ( a->didAlc ) {
		a->flexarray.memMgt->free( a ) ;
	}
}

MM_FlexArray_Inx mm_freeListArray_AllocSlot( MM_FreeListArray* a ) {
	MM_FlexArray_Inx i = a->freeList ;
	if ( i != MM_FreeListArray_FreeListNULL ) {
		a->freeList = *mm_freeListArray_FreeListNextAt( a, i ) ;
	} else {
		i = mm_flexArray_AllocSlot( &a->flexarray ) ;
	}
	return i ;
}

void mm_freeListArray_DeallocSlot( MM_FreeListArray* a, MM_FlexArray_Inx i ) {
	*mm_freeListArray_FreeListNextAt( a, i ) = a->freeList ;
	a->freeList = i ;
}

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Iteration
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
// skip to first in use position, or beyond used area
static inline MM_FlexArray_Inx mm_freeListArray_FirstInUse( MM_FreeListArray* a, MM_FlexArray_Inx inx ) {
	for ( ; (! mm_freeListArray_IsInUseAt(a,inx)) && inx < mm_freeListArray_SizeUsed(a) ; inx++ ) {}
	return inx ;
}

// assume i->hasData
Bool mm_freeListArray_IteratorStep( MM_Iterator* i ) {
	MM_FreeListArray* a = (MM_FreeListArray*)i->iteratee ;
	MM_FlexArray_Inx inx = mm_freeListArray_FirstInUse( a, i->state + 1 ) ;
	if ( inx < mm_freeListArray_SizeUsed(a) ) {
		i->state = inx ;
		i->data = mm_freeListArray_At( a, inx ) ;
	} else {
		i->hasData = False ;
	}
	return i->hasData ;
}

void mm_freeListArray_IteratorAt( MM_FreeListArray* a, MM_Iterator* i, MM_FlexArray_Inx atInx ) {
	MM_FlexArray_Inx inx = mm_freeListArray_FirstInUse( a, atInx ) ;
	if ( i->hasData = (inx < mm_freeListArray_SizeUsed(a)) ) {
		i->state = inx ;
		i->data = mm_freeListArray_At( a, inx ) ;
		i->step = &mm_freeListArray_IteratorStep ;
		i->iteratee = (BPtr)a ;
	}
}

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% FlexArray dump
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#ifdef TRACE
void mm_freeListArray_Dump( MM_FreeListArray* a ) {
	printf( "FreeListArray: a=%p didAlc=%d listPtrOff=%x freeList=%x\n", a, a->didAlc, a->listPtrOff, a->freeList ) ;
	mm_flexArray_Dump( &a->flexarray ) ;

}
#endif
%%]
