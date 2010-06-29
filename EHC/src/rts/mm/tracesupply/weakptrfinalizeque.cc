%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Memory management: TraceSupply: WeakPtrFinalizeQue
%%% see associated .ch file for more info.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[99
#include "../../rts.h"
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Dummy values to fool linker into thinking file contains something
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8.dummyForLinker
int dummy_tracesupply_WeakPtrFinalizeQue ;
%%]

%%[99 -8.dummyForLinker
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% WeakPtrFinalizeQue internal defs
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[99
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% WeakPtrFinalizeQue internal functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% WeakPtrFinalizeQue internal functions, but still externally visible (because of inlining)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% WeakPtrFinalizeQue interface
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[99
void mm_traceSupply_WeakPtrFinalizeQue_Init( MM_TraceSupply* traceSupply, MM_Malloc* memmgt, MM_Mutator* mutator ) {
	MM_TraceSupply_WeakPtrFinalizeQue_Data* trsup = memmgt->malloc( sizeof(MM_TraceSupply_WeakPtrFinalizeQue_Data) ) ;
	trsup->mutator = mutator ;
	traceSupply->data = (MM_TraceSupply_Data_Priv*)trsup ;
}

void mm_traceSupply_WeakPtrFinalizeQue_Reset( MM_TraceSupply* traceSupply, Word gcInfo ) {
	MM_TraceSupply_WeakPtrFinalizeQue_Data* trsup = (MM_TraceSupply_WeakPtrFinalizeQue_Data*)traceSupply->data ;
}

void mm_traceSupply_WeakPtrFinalizeQue_Run( MM_TraceSupply* traceSupply ) {
	MM_TraceSupply_WeakPtrFinalizeQue_Data* trsup = (MM_TraceSupply_WeakPtrFinalizeQue_Data*)traceSupply->data ;
	
	MM_Iterator iter ;
	for ( mm_deque_Iterator( trsup->mutator->weakPtrFinalizeQue, &iter ) ; iter.hasData ; iter.step( &iter ) ) {
		MM_WeakPtrFinalizeQue_Data* tup = (MM_WeakPtrFinalizeQue_Data*)iter.data ;
		tup->wpObj = trsup->mutator->weakPtrAdm->traceWeakPtrWhenFinalizing( trsup->mutator->weakPtrAdm, trsup->mutator->trace, tup->wpObj ) ;
		tup->finalizer = mm_Trace_TraceObject( trsup->mutator->trace, tup->finalizer ) ;
	}
}

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% WeakPtrFinalizeQue interface object
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[99
MM_TraceSupply mm_traceSupply_WeakPtrFinalizeQue =
	{ NULL
	, &mm_traceSupply_WeakPtrFinalizeQue_Init
	, MM_Undefined
	, &mm_traceSupply_WeakPtrFinalizeQue_Reset
	, &mm_traceSupply_WeakPtrFinalizeQue_Run
	, MM_Undefined
	} ;
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% WeakPtrFinalizeQue dump
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[99
#ifdef TRACE
void mm_traceSupply_WeakPtrFinalizeQue_Dump( MM_TraceSupply* traceSupply ) {
}
#endif
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% WeakPtrFinalizeQue test
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[99
#ifdef TRACE
void mm_traceSupply_WeakPtrFinalizeQue_Test() {
}
#endif
%%]

