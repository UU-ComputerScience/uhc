%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Memory management
%%% see associated .ch file for more info.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#include "../rts.h"
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
	mm_init_roots() ;
	mm_init_traceSupply() ;
	mm_init_trace() ;
	mm_init_collector() ;
	mm_init_mutator() ;
	mm_init_plan() ;
	
	
}
%%]
