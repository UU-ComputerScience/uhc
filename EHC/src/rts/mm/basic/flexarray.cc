%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Memory management: flexarray,
%%% see associated .ch file for more info.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#include "../../rts.h"
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Lowest level flexible size array mem managed via malloc/realloc, basically for bootstrapping
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
MM_FlexArray* mm_flexArray_New( MM_Malloc* memmgt, MM_FlexArray* a, MM_FlexArray_Inx szElt, MM_FlexArray_Inx sz, MM_FlexArray_Inx free ) {
	sz = maxWord( sz, free ) ;
	if ( a == NULL ) {
		a = (MM_FlexArray*) memmgt->malloc( sizeof( MM_FlexArray ) ) ;
		a->didAlc = True ;
	} else {
		a->didAlc = False ;
	}
	MM_FlexArray_Inx szExtra = sz + 1 ;
	MM_FlexArray_Inx asz = szExtra * szElt ;
	a->ptr = memmgt->malloc( asz ) ;
	memset( a->ptr, 0, asz ) ;
	a->free = free ;
	a->size = szExtra ;
	a->sizeElt = szElt ;
	a->memMgt = memmgt ;
	return a ;
}

void mm_flexArray_Free( MM_FlexArray* a ) {
	a->memMgt->free( a->ptr ) ;
	if ( a->didAlc ) {
		a->memMgt->free( a ) ;
	}
}

void mm_flexArray_ExtendBy( MM_FlexArray* a, MM_FlexArray_Inx sz ) {
	// if ( a->free + sz > a->size ) {
	if ( sz > mm_flexArray_NrFreeSlots(a) ) {
		MM_FlexArray_Inx newsz = 3 * ((a->free + sz) / 2 + 1) ;			// reserve space of factor 1.5
		a->ptr = a->memMgt->realloc( a->ptr, newsz * a->sizeElt ) ;
		a->size = newsz ;
	}
	memset( mm_flexArray_At( a, a->free ), 0, sz * a->sizeElt ) ;
	a->free += sz ;
}

MM_FlexArray_Inx mm_flexArray_AllocSlot( MM_FlexArray* a ) {
	MM_FlexArray_Inx i = a->free ;
	mm_flexArray_ExtendBy( a, 1 ) ;
	return i ;
}

void mm_flexArray_EnsureSlot( MM_FlexArray* a, MM_FlexArray_Inx i ) {
	mm_flexArray_ExtendBy( a, i - a->free + 1 ) ;
}

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Iteration
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
// assume i->hasData
Bool mm_flexArray_IteratorStep( MM_Iterator* i ) {
	MM_FlexArray* a = (MM_FlexArray*)i->iteratee ;
	MM_FlexArray_Inx inx = i->state + 1 ;
	if ( inx < mm_flexArray_SizeUsed(a) ) {
		i->state = inx ;
		i->data = mm_flexArray_At( a, inx ) ;
	} else {
		i->hasData = False ;
	}
	return i->hasData ;
}

void mm_flexArray_IteratorAt( MM_FlexArray* a, MM_Iterator* i, MM_FlexArray_Inx atInx ) {
	if ( i->hasData = (mm_flexArray_SizeUsed(a) > atInx) ) {
		i->state = atInx ;
		i->data = mm_flexArray_At( a, atInx ) ;
		i->step = &mm_flexArray_IteratorStep ;
		i->iteratee = (BPtr)a ;
	}
}

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% FlexArray dump
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#ifdef TRACE
void mm_flexArray_Dump( MM_FlexArray* a ) {
	printf( "FlexArray: a=%p didAlc=%d sizeElt=%x ptr=%p size=%x/%x\n", a, a->didAlc, a->sizeElt, a->ptr, a->free, a->size ) ;

}
#endif
%%]
