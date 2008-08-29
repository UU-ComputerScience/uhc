%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Memory management: basic/common definitions,
%%% see associated .ch file for more info.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#include "../rts.h"
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Checking wrapper around malloc etc
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
Ptr mm_malloc( size_t size ) {
	void* p = malloc( size ) ;
	if ( p == NULL ) { rts_panic1_1( "malloc failed", size ) ; }
	return (Ptr)p ;
}

Ptr mm_realloc( Ptr ptr, size_t size ) {
	void* p = realloc( ptr, size ) ;
	if ( p == NULL ) { rts_panic1_1( "realloc failed", size ) ; }
	return (Ptr)p ;
}

void mm_free( Ptr ptr ) {
	free( ptr ) ;
}

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Abstraction interface around allocation: system provided malloc
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
MM_Malloc mm_malloc_Sys =
	{ mm_malloc
	, mm_realloc
	, mm_free
	} ;
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Lowest level flexible size array mem managed via malloc/realloc, basically for bootstrapping
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
MM_FlexArray* mm_flexArray_New( MM_Malloc* memmgt, MM_FlexArray* a, MM_FlexArray_Inx szElt, MM_FlexArray_Inx sz ) {
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
	a->free = sz ;
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
	if ( a->free + sz > a->size ) {
		MM_FlexArray_Inx newsz = 3 * ((a->free + sz) / 2 + 1) ;			// reserve space of factor 1.5
		a->ptr = a->memMgt->realloc( a->ptr, newsz * a->sizeElt ) ;
		a->size = newsz ;
	}
	memset( mm_flexArray_At( a, a->free ), 0, sz * a->sizeElt ) ;
	a->free += sz ;
}

MM_FlexArray_Inx mm_flexArray_NewSlot( MM_FlexArray* a ) {
	MM_FlexArray_Inx i = a->free ;
	mm_flexArray_ExtendBy( a, 1 ) ;
	return i ;
}

void mm_flexArray_EnsureSlot( MM_FlexArray* a, MM_FlexArray_Inx i ) {
	mm_flexArray_ExtendBy( a, i - a->free + 1 ) ;
}

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Undefined
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
void mm_undefined(void) {
	rts_panic1_0( "mm: undefined" ) ;
}
%%]


