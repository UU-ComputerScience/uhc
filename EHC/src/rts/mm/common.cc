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
%%]
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


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Abstraction interface around allocation: system provided malloc
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
MM_Malloc* mm_malloc_Sys = &sys_malloc_Sys ;
%%]

%%[8
MM_Malloc* mm_malloc_EHC = &sys_malloc_Sys ;
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Undefined
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
void mm_undefined(void) {
	rts_panic1_0( "mm: undefined" ) ;
}
%%]


