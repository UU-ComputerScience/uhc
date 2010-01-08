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
void mm_collector_SS_Init( MM_Collector* collector, MM_Malloc* memmgt, Ptr /* MM_Plan* */ plan ) {
	collector->plan = plan ;
	// MM_Collector_SS_Data* colss = memmgt->malloc( sizeof(MM_Collector_SS_Data) ) ;
		
	// collector->data = (MM_Collector_Data_Priv*)colss ;
}

void mm_collector_SS_CollectPre( MM_Collector* collector ) {
}

void mm_collector_SS_Collect( MM_Collector* collector, Word gcInfo ) {
	// MM_Collector_SS_Data* colss = (MM_Collector_SS_Data*)collector->data ;
	
	// printf( "Collect!!\n" ) ;
	// we know we are part of an ss plan
	MM_Plan_SS_Data* plss = (MM_Plan_SS_Data*)(((MM_Plan*)collector->plan)->data) ;
	IF_GB_TR_ON(3,{printf("mm_collector_SS_Collect A to=%p, fr=%p\n", plss->toSpace, plss->fromSpace);}) ;
	// swap spaces
	SwapPtr( MM_Space*, plss->toSpace, plss->fromSpace ) ;
	// tell this to the allocator
	IF_GB_TR_ON(3,{printf("mm_collector_SS_Collect B\n");}) ;
	plss->ssAllocator.resetWithSpace( &plss->ssAllocator, plss->toSpace ) ;
	IF_GB_TR_ON(3,{printf("mm_collector_SS_Collect C sp=%p\n",plss->fromSpace);}) ;
	// the old one is to be collected
	collector->collectedSpace = plss->fromSpace ;
#	if 0 && TRACE
		plss->fromSpace->dump( plss->fromSpace ) ;
		plss->fromSpace->dump( plss->toSpace ) ;
		mm_Spaces_Dump() ;
#	endif

	// run the tracing of objects
	plss->allTraceSupply.reset( &plss->allTraceSupply, gcInfo ) ;
	IF_GB_TR_ON(3,{printf("mm_collector_SS_Collect D\n");}) ;
	plss->allTraceSupply.run( &plss->allTraceSupply ) ;
	IF_GB_TR_ON(3,{printf("mm_collector_SS_Collect E\n");}) ;
	
%%[[94
	// find & trace live objects belonging to weakptrs
	MM_WeakPtr* wp = plss->weakPtr ;
	MM_WeakPtr_NewAlive newAlive ;
	MM_Iterator iter ;
	// printf("mm_collector_SS_Collect\n");fflush(stdout);
	wp->startFindLiveObjects( wp ) ;
	do {
		wp->findLiveObjects( wp, &newAlive ) ;
		// run tracing for these objects: queue them as tracesupply and run that one again
		for ( mm_freeListArray_IteratorAt( newAlive.alive, &iter, newAlive.firstAliveInx ) ; iter.hasData ; iter.step( &iter ) ) {
			MM_WeakPtr_ObjectAdmin* admin = (MM_WeakPtr_ObjectAdmin*)iter.data ;
			IF_GB_TR_ON(3,{printf("mm_collector_SS_Collect F1 alive %x\n",admin->obj);}) ;
			admin->obj = wp->traceWeakPtr( wp, &plss->gbmTrace, admin->obj ) ;
			IF_GB_TR_ON(3,{printf("mm_collector_SS_Collect F2 alive %x\n",admin->obj);}) ;
		}
	} while ( newAlive.aftLastAliveInx > newAlive.firstAliveInx ) ;
	MM_FreeListArray* toFinalize = wp->endFindLiveObjects( wp ) ;
	
	// finalize dead objects, reviving & queuing the ones which must be done when collecting is done
	for ( mm_freeListArray_Iterator( toFinalize, &iter ) ; iter.hasData ; iter.step( &iter ) ) {
		Word wpObj = ((MM_WeakPtr_ObjectAdmin*)iter.data)->obj ;
		Word finalizer = wp->finalizeWeakPtr( wp, wpObj ) ;
%%[[99
		if ( finalizer != 0 ) {
			MM_WeakPtrFinalizeQue_Data buf ;
			buf.wpObj = wp->traceWeakPtr( wp, &plss->gbmTrace, wpObj ) ;
			buf.finalizer = mm_Trace_TraceObject( &plss->gbmTrace, finalizer ) ;
			mm_deque_TailPush( &plss->weakPtrFinalizeQue, (WPtr)&buf, MM_WeakPtrFinalizeQue_Data_SizeInWords ) ;
		}
%%]]
	}
%%[[99
	plss->queTraceSupply->run( plss->queTraceSupply ) ;
%%]] 
%%]] 
	
	// done
#	if TRACE
		// mark as fresh for illegal access detection (dangling refs)
		plss->fromSpace->markAsFresh( plss->fromSpace ) ;
#	endif
#	if 0 && TRACE
		plss->fromSpace->dump( plss->fromSpace ) ;
		plss->fromSpace->dump( plss->toSpace ) ;
#	endif
}

void mm_collector_SS_CollectPost( MM_Collector* collector ) {
	MM_Plan* plan = (MM_Plan*)collector->plan ;
	MM_Plan_SS_Data* plss = (MM_Plan_SS_Data*)(plan->data) ;

%%[[99
	MM_WeakPtrFinalizeQue_Data buf /* ( WeakPtr obj, finalizer ) */ ;
	for ( ; mm_deque_HeadPop( &plss->weakPtrFinalizeQue, (WPtr)&buf, MM_WeakPtrFinalizeQue_Data_SizeInWords ) > 0 ; ) {
		plan->mutator->runFinalizer( plan->mutator, buf.finalizer ) ;
	}
%%]]
}

Bool mm_collector_SS_IsInCollectedSpace( MM_Collector* collector, Word obj ) {
	return mm_Spaces_AddressIsGCManagedBySpace( obj, collector->collectedSpace ) ;
}

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% SS interface object
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
MM_Collector mm_collector_SS =
	{ NULL
	, NULL
	, NULL
	, &mm_collector_SS_Init
	, &mm_collector_SS_CollectPre
	, &mm_collector_SS_Collect
	, &mm_collector_SS_CollectPost
	, &mm_collector_SS_IsInCollectedSpace
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

