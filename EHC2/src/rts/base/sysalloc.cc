%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Low level system mem allocation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#include "../rts.h"
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Checking wrapper around malloc etc
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
Ptr sys_malloc( size_t size ) {
	void* p = malloc( size ) ;
	if ( p == NULL ) { rts_panic1_1( "malloc failed", size ) ; }
	return (Ptr)p ;
}

Ptr sys_realloc( Ptr ptr, size_t size ) {
	void* p = realloc( ptr, size ) ;
	if ( p == NULL ) { rts_panic1_1( "realloc failed", size ) ; }
	return (Ptr)p ;
}

void sys_free( Ptr ptr ) {
	free( ptr ) ;
}

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Abstraction interface around allocation: system provided malloc
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
Sys_Malloc sys_malloc_Sys =
	{ sys_malloc
	, sys_realloc
	, sys_free
	} ;
%%]

%%[8
Sys_Malloc* sys_malloc_EHC = &sys_malloc_Sys ;
%%]

