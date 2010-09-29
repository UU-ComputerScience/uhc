%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Memory management
%%% see associated .ch file for more info.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#include "../rts.h"
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Bypass to internals of MM
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#if MM_BYPASS_PLAN
MM_Allocator* mm_bypass_allocator = NULL ;
#endif
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Initialization
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
void mm_init() {
	// sanity checks for basic assumptions
	char* m = "memory manager sanity check" ;
		
	if ( sizeof(MM_BuddyPage_ExtlData) != 2 * sizeof( Word ) ) {
		rts_panic2_1( m, "size of MM_BuddyPage_ExtlData must be 2 words", sizeof(MM_BuddyPage_ExtlData) ) ;
	}
	
	mm_init_pages() ;
	mm_init_space() ;
	mm_init_allocator() ;
#	if USE_EHC_MM
		mm_malloc_EHC  = &mm_malloc_LOF ;
		sys_malloc_EHC = mm_malloc_EHC ;
#	endif
	mm_init_roots() ;
	mm_init_traceSupply() ;
	mm_init_trace() ;
	mm_init_collector() ;
	mm_init_mutator() ;
#ifdef __UHC_TARGET_BC__ || __UHC_TARGET_C__
%%[[90
	mm_init_weakPtr() ;
%%]]
#endif
	mm_init_plan() ;	
}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Finalization
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
void mm_exit() {
	mm_exit_plan() ;	
}
%%]


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% External interface for linking with non C backends
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8

Ptr mm_itf_alloc_ext( size_t sz, Word gcInfo ) {
    // mm_itf_gc();    
    Ptr p = mm_itf_alloc( sz, gcInfo ); 
    //printf("mm_itf_alloc_ext mem: %i at: %016llx \n", sz, p);
    return p;
}

Ptr mm_itf_allocResident_ext( size_t sz ) {
    return mm_itf_allocResident( sz );
}


void mm_itf_deallocResident_ext( Ptr p ) {
    mm_itf_deallocResident( p );
}


void mm_itf_registerGCRoot_ext( WPtr p ) {
    mm_itf_registerGCRoot( p );
}


void mm_itf_registerGCRoots_ext( WPtr p, Word n ) {
    printf("registerGCRoots: %016llx  nr: %i \n", p, n);
    mm_itf_registerGCRoots( p, n );
}


Bool mm_itf_gc_ext( ) {
    return mm_itf_gc();
}

%%]









