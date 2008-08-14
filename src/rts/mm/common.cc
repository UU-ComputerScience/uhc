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
%%% Lowest level flexible size array mem managed via malloc/realloc, basically for bootstrapping
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
MM_FlexArray* mm_flexArray_New( MM_FlexArray* a, MM_FlexArray_Inx szElt, MM_FlexArray_Inx sz ) {
	if ( a == NULL ) {
		a = (MM_FlexArray*) mm_malloc( sizeof( MM_FlexArray ) ) ;
	}
	MM_FlexArray_Inx szExtra = sz + 1 ;
	MM_FlexArray_Inx asz = szExtra * szElt ;
	a->ptr = mm_malloc( asz ) ;
	memset( a->ptr, 0, asz ) ;
	a->free = sz ;
	a->size = szExtra ;
	a->sizeElt = szElt ;
	return a ;
}

void mm_flexArray_ExtendBy( MM_FlexArray* a, MM_FlexArray_Inx sz ) {
	if ( a->free + sz > a->size ) {
		MM_FlexArray_Inx newsz = 3 * ((a->free + sz) / 2 + 1) ;			// reserve space of factor 1.5
		a->ptr = mm_realloc( a->ptr, newsz * a->sizeElt ) ;
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
%%% Doubly linked list
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
Bool mm_dll_IsEmpty( MM_DLL* dll ) {
	return dll->next == dll ;
}

void mm_dll_Init( MM_DLL* dll ) {
	dll->next = dll->prev = dll ;
}

void mm_dll_InsertRight( MM_DLL* dllNew, MM_DLL* dll ) {
	dllNew->prev = dll ;
	dllNew->next = dll->next ;
	dll->next->prev = dllNew ;
	dll->next = dllNew ;
}

void mm_dll_InsertLeft( MM_DLL* dllNew, MM_DLL* dll ) {
	mm_dll_InsertRight( dllNew, dll->prev ) ;
}

void mm_dll_Delete( MM_DLL* dll ) {
	dll->next->prev = dll->prev ;
	dll->prev->next = dll->next ;
}

%%]


