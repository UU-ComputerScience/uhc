%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Memory management: Trace: GBM
%%% see associated .ch file for more info.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Trace a GBM object, knowing its structure etc.

%%[8
#include "../../rts.h"
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% GBM internal defs
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
// is managed by GC
static inline Bool mm_trace_GBM_CanTrace( Word obj, MM_Collector* collector ) {
	return GB_Word_IsPtr( obj ) && mm_Spaces_AddressIsGCManagedBySpace( obj, collector->collectedSpace ) ;
}

// is traceable, but later
static inline Bool mm_trace_GBM_CanScheduleForTrace( Word obj ) {
	return GB_NH_Fld_NdEv( ((GB_NodePtr)obj)->header ) != GB_NodeNdEv_Fwd ;
}

// is a location which is being collected
static inline Bool mm_trace_GBM_InCollectableRange( Word obj ) {
	return GB_NH_Fld_NdEv( ((GB_NodePtr)obj)->header ) != GB_NodeNdEv_Fwd ;
}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% GBM internal functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% GBM internal functions, but still externally visible (because of inlining)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% GBM interface
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
void mm_trace_GBM_Init( MM_Trace* trace, void* traceSupply, MM_Allocator* allocator, MM_Collector* collector ) {
	MM_Trace_GBM_Data* tr = mm_malloc_LOF.malloc( sizeof(MM_Trace_GBM_Data) ) ;
	
	tr->traceSupply = (MM_TraceSupply*)traceSupply ;
	tr->copyAllocator = allocator ;
	trace->collector = collector ;
		
	trace->data = (MM_Trace_Data_Priv*)tr ;
}

Bool mm_trace_GBM_CanTraceObject( MM_Trace* trace, Word obj ) {
	return mm_trace_GBM_CanTrace( obj, trace->collector ) ;
}

Word mm_trace_GBM_TraceKnownToBeObject( MM_Trace* trace, Word obj ) {
	GB_NodePtr objRepl = (GB_NodePtr)obj ;
	
	GB_NodePtr n = (GB_NodePtr)obj ;
	Word h = n->header ;
	switch ( GB_NH_Fld_NdEv(h) ) {
		// is already forwarded, so return the forwarding
		case GB_NodeNdEv_Fwd :
			objRepl = (GB_NodePtr)( n->content.fields[0] ) ;
			break ;

		// otherwise copy and schedule new objects for tracing
		default :
			{
				Word szWords = GB_NH_Fld_Size(h) ;
				MM_Trace_GBM_Data* tr = (MM_Trace_GBM_Data*)trace->data ;
				
				// new obj, copy old into new
				objRepl = (GB_NodePtr)( tr->copyAllocator->alloc( tr->copyAllocator, szWords << Word_SizeInBytes_Log ) ) ;
				objRepl->header = h ;
				
				// each field must be traced as well, combine with copy
				Word* fieldTo = objRepl->content.fields ;
				Word* fieldFr = n->content.fields ;
				for ( szWords-- ; szWords > 0 ; szWords--, fieldTo++, fieldFr++ ) {
					if ( mm_trace_GBM_CanTrace( *fieldTo = *fieldFr, trace->collector ) /* && mm_trace_GBM_CanScheduleForTrace( *fieldTo ) */ ) {
						tr->traceSupply->pushWork( tr->traceSupply, fieldTo, 1 ) ;
					}
				}

				// forward old obj to new
				n->header = GB_NH_SetFld_NdEv(h,GB_NodeNdEv_Fwd) ;
				n->content.fields[0] = (Word)objRepl ;
			}
			break ;
	}
		
	return (Word)objRepl ;
}

Word mm_trace_GBM_TraceObject( MM_Trace* trace, Word obj ) {
	if ( mm_trace_GBM_CanTrace( obj, trace->collector ) ) {
		return mm_trace_GBM_TraceKnownToBeObject( trace, obj ) ;
	} else {
		return obj ;
	}
		
}

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% GBM interface object
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
MM_Trace mm_trace_GBM =
	{ NULL
	, NULL
	, &mm_trace_GBM_Init
	, &mm_trace_GBM_CanTraceObject
	, &mm_trace_GBM_TraceKnownToBeObject
	} ;
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% GBM dump
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#ifdef TRACE
mm_trace_GBM_Dump( MM_Trace* trace ) {
}
#endif
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% GBM test
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#ifdef TRACE
void mm_trace_GBM_Test() {
}
#endif
%%]

