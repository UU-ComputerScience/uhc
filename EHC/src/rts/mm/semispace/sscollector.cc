%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Memory management: Collector: SS
%%% see associated .ch file for more info.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#include "../../rts.h"
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% SS internal defs
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% SS internal functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% SS internal functions, but still externally visible (because of inlining)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% SS interface
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
void mm_collector_SS_Init( MM_Collector* collector, MM_Malloc* memmgt ) {
	// MM_Collector_SS_Data* colss = memmgt->malloc( sizeof(MM_Collector_SS_Data) ) ;
		
	// collector->data = (MM_Collector_Data_Priv*)colss ;
}

void mm_collector_SS_collect( MM_Collector* collector ) {
	// MM_Collector_SS_Data* colss = (MM_Collector_SS_Data*)collector->data ;
	
	IF_GB_TR_ON(3,{printf("mm_collector_SS_collect A\n");}) ;
	// we know we are part of an ss plan
	MM_Plan_SS_Data* plss = (MM_Plan_SS_Data*)(mm_plan.data) ;
	// swap spaces
	SwapPtr( MM_Space*, plss->toSpace, plss->fromSpace ) ;
	// tell this to the allocator
	IF_GB_TR_ON(3,{printf("mm_collector_SS_collect B\n");}) ;
	plss->ssAllocator.resetWithSpace( &plss->ssAllocator, plss->toSpace ) ;
	IF_GB_TR_ON(3,{printf("mm_collector_SS_collect C\n");}) ;
	// the old one is to be collected
	collector->collectedSpace = plss->fromSpace ;
	// run the tracing of objects
	plss->allTraceSupply.reset( &plss->allTraceSupply ) ;
	IF_GB_TR_ON(3,{printf("mm_collector_SS_collect D\n");}) ;
	plss->allTraceSupply.run( &plss->allTraceSupply ) ;
	IF_GB_TR_ON(3,{printf("mm_collector_SS_collect E\n");}) ;
}

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% SS interface object
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
MM_Collector mm_collector_SS =
	{ NULL
	, NULL
	, &mm_collector_SS_Init
	, &mm_collector_SS_collect
	} ;
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% SS dump
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#ifdef TRACE
void mm_collector_SS_Dump( MM_Collector* collector ) {
}
#endif
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% SS test
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#ifdef TRACE
void mm_collector_SS_Test() {
}
#endif
%%]

