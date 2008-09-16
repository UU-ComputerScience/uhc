%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Memory management: plan: semi space (SS)
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
%%% SS interface
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
void mm_plan_SS_Init( MM_Plan* plan ) {
	MM_Plan_SS_Data* plss = mm_malloc_LOF.malloc( sizeof(MM_Plan_SS_Data) ) ;
	// IF_GB_TR_ON(3,{printf("mm_plan_SS_Init plan=%x plss=%x\n",plan,plss);}) ;
	
	plss->memMgt = mm_malloc_LOF ;
	plss->residentAllocator = mm_allocator_LOF ;
	
	plss->fragSpace0 = mm_space_Fragment ;
	plss->fragSpace0.init( &plss->fragSpace0, &plss->memMgt, &mm_pages_Buddy ) ;
	plss->space0 = mm_space_CopySpace ;
	plss->space0.initWithSpace( &plss->space0, &plss->memMgt, &plss->fragSpace0 ) ;
	
	plss->fragSpace1 = mm_space_Fragment ;
	plss->fragSpace1.init( &plss->fragSpace1, &plss->memMgt, &mm_pages_Buddy ) ;
	plss->space1 = mm_space_CopySpace ;
	plss->space1.initWithSpace( &plss->space1, &plss->memMgt, &plss->fragSpace1 ) ;
	
	plss->toSpace = &plss->space0 ;
	plss->fromSpace = &plss->space1 ;

	plss->ssAllocator = mm_allocator_Bump ;
	plss->ssAllocator.init( &plss->ssAllocator, &plss->memMgt, plss->toSpace ) ;
		
	plss->collector = mm_collector_SS ;
	plss->collector.init( &plss->collector, &plss->memMgt ) ;
	// plan->collector = &plss->collector ;
		
	plss->mutator = mm_mutator_GBSS ;
	plss->mutator.init( &plss->mutator, &plss->memMgt, &plss->ssAllocator ) ;
	plan->mutator = &plss->mutator ;
		
	MM_FlexArray* traceSupplies = mm_flexArray_New( &plss->memMgt, NULL, sizeof(MM_TraceSupply), 4, 4 ) ;
	// IF_GB_TR_ON(3,{printf("mm_plan_SS_Init B\n");}) ;
	// the order of these supplies matters, because they are run in this order
	MM_TraceSupply* regsTraceSupply  = (MM_TraceSupply*)mm_flexArray_At( traceSupplies, 0 ) ;
	MM_TraceSupply* rootsTraceSupply = (MM_TraceSupply*)mm_flexArray_At( traceSupplies, 1 ) ;
	MM_TraceSupply* stackTraceSupply = (MM_TraceSupply*)mm_flexArray_At( traceSupplies, 2 ) ;
	MM_TraceSupply* queTraceSupply   = (MM_TraceSupply*)mm_flexArray_At( traceSupplies, 3 ) ;
	
	*regsTraceSupply = mm_traceSupply_GBRegs ;
	regsTraceSupply->init( regsTraceSupply, &plss->gbmTrace ) ;
	
	*rootsTraceSupply = mm_traceSupply_Roots ;
	rootsTraceSupply->init( rootsTraceSupply, &plss->gbmTrace ) ;
	
	*stackTraceSupply = mm_traceSupply_GBStack ;
	stackTraceSupply->init( stackTraceSupply, &plss->gbmTrace ) ;
	
	*queTraceSupply = mm_traceSupply_Bump ; // mm_traceSupply_Buffer ;
	queTraceSupply->init( queTraceSupply, &plss->gbmTrace ) ;
	plss->queTraceSupply = queTraceSupply ;
	
	plss->allTraceSupply = mm_traceSupply_Group ;
	plss->allTraceSupply.initWithSub( &plss->allTraceSupply, &plss->gbmTrace, traceSupplies ) ;
	
	plss->gbmTrace = mm_trace_GBM ;
	plss->gbmTrace.init( &plss->gbmTrace, plss->queTraceSupply, &plss->ssAllocator, &plss->collector ) ;
	
	plan->data = (MM_Plan_Data_Priv*)plss ;
	// IF_GB_TR_ON(3,{printf("mm_plan_SS_Init C plan=%x plss=%x\n",plan,plss);}) ;
}
%%]

%%[8
Bool mm_plan_SS_PollForGC( MM_Plan* plan, Bool isSpaceFull, MM_Space* space ) {
	MM_Plan_SS_Data* plss = (MM_Plan_SS_Data*)plan->data ;

	IF_GB_TR_ON(3,{printf("mm_plan_SS_PollForGC plan=%x plss=%x\n",plan,plss);}) ;
	if ( isSpaceFull ) {
		plss->collector.collect( &plss->collector ) ;
		// rts_panic1_1( "mm_plan_SS_PollForGC", 0 ) ;
		return True ;
	}
	return False ;
}

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% SS dump
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#ifdef TRACE
void mm_plan_SS_Dump( MM_Plan* plan ) {
	MM_Plan_SS_Data* plss = (MM_Plan_SS_Data*)plan->data ;
	
	printf( ">------------------------> MM_Plan: SS: plan=%x plss=%x\n", plan, plss ) ;

	mm_Spaces_Dump() ;
	plss->ssAllocator.dump( &plss->ssAllocator ) ;
	plss->toSpace->dump( plss->toSpace ) ;

	printf( "<------------------------< MM_Plan: SS\n" ) ;
}
#endif
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% SS interface object
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
MM_Plan mm_plan_SS =
	{ NULL
	, NULL
	, &mm_plan_SS_Init
	, &mm_plan_SS_PollForGC
#ifdef TRACE
	, &mm_plan_SS_Dump
#endif
	} ;
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% SS test
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#ifdef TRACE
void mm_plan_SS_Test() {
}
#endif
%%]

