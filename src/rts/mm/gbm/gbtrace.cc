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
	IF_GB_TR_ON(3,{printf("mm_trace_GBM_CanTrace obj=%x\n",obj);}) ;
	Bool res = GB_Word_IsPtr( obj ) && mm_Spaces_AddressIsGCManagedBySpace( obj, collector->collectedSpace ) ;
	IF_GB_TR_ON(3,{printf("mm_trace_GBM_CanTrace B\n");}) ;
	return res ;
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
	trace->collector = collector ;
	trace->allocator = allocator ;
		
	trace->data = (MM_Trace_Data_Priv*)tr ;
}

Bool mm_trace_GBM_CanTraceObject( MM_Trace* trace, Word obj ) {
	return mm_trace_GBM_CanTrace( obj, trace->collector ) ;
}

Word mm_trace_GBM_TraceKnownToBeObject( MM_Trace* trace, Word obj ) {
	GB_NodePtr objRepl = (GB_NodePtr)obj ;
	
	IF_GB_TR_ON(3,{printf("mm_trace_GBM_TraceKnownToBeObject obj=%x\n",obj);}) ;
	GB_NodePtr n = (GB_NodePtr)obj ;
	Word h = n->header ;
	switch ( GB_NH_Fld_NdEv(h) ) {
		// is already forwarded, so return the forwarding
		case GB_NodeNdEv_Fwd :
			objRepl = (GB_NodePtr)( n->content.fields[0] ) ;
			IF_GB_TR_ON(3,{printf("mm_trace_GBM_TraceKnownToBeObject fwd objRepl=%x\n",objRepl);}) ;
			break ;

		// otherwise copy and schedule new objects for tracing
		default :
			{
				Word szWords = GB_NH_Fld_Size(h) ;
				MM_Trace_GBM_Data* tr = (MM_Trace_GBM_Data*)trace->data ;
				
				// new obj, copy old into new, allocate
				// IF_GB_TR_ON(3,{printf("mm_trace_GBM_TraceKnownToBeObject copy\n");}) ;
				objRepl = (GB_NodePtr)( trace->allocator->alloc( trace->allocator, szWords << Word_SizeInBytes_Log ) ) ;
				// copy header
				objRepl->header = h ;
				
				// schedule for tracing, depending on type of node
				if ( GB_NH_Fld_NdEv(h) == GB_NodeNdEv_No && GB_NH_Fld_TagCat(h) == GB_NodeTagCat_Intl ) {
					switch( GB_NH_Fld_Tag(h) ) {
%%[[95
						case GB_NodeTag_Intl_Malloc :
						case GB_NodeTag_Intl_Malloc2 :
%%]]
%%[[97
#						if USE_GMP
							case GB_NodeTag_Intl_GMP_intl :
							case GB_NodeTag_Intl_GMP_mpz :
#						endif
%%]]
%%[[98
						case GB_NodeTag_Intl_Chan :
%%]]
							// no tracing
							break ;
						default :
							tr->traceSupply->pushWork( tr->traceSupply, (Word*)n, szWords ) ;
							break ;
					}
				} else {
					tr->traceSupply->pushWork( tr->traceSupply, (Word*)n, szWords ) ;
				}

				// copy fields
				Word* fieldTo = objRepl->content.fields ;
				Word* fieldFr = n->content.fields ;
				for ( szWords-- ; szWords > 0 ; szWords-- ) {
					*(fieldTo++) = *(fieldFr++) ;
				}

				// forward old obj to new
				n->header = GB_NH_SetFld_NdEv(h,GB_NodeNdEv_Fwd) ;
				n->content.fields[0] = (Word)objRepl ;
			}
			break ;
	}
	IF_GB_TR_ON(3,{printf("mm_trace_GBM_TraceKnownToBeObject B\n");}) ;
		
	return (Word)objRepl ;
}

%%]

%%[8
void mm_trace_GBM_TraceObjects( MM_Trace* trace, Word* objs, Word nrObjs ) {
	for ( ; nrObjs > 0 ; nrObjs--, objs++ ) {
		if ( mm_trace_GBM_CanTrace( *objs, trace->collector ) ) {
			*objs = mm_trace_GBM_TraceKnownToBeObject( trace, *objs ) ;
		}
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
	, NULL
	, sizeof(GB_NodeHeader) >> Word_SizeInBytes_Log
	, &mm_trace_GBM_Init
	, &mm_trace_GBM_CanTraceObject
	, &mm_trace_GBM_TraceKnownToBeObject
	, &mm_trace_GBM_TraceObjects
	, &mm_trace_GBM_ObjectSize
	} ;
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% GBM dump
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#ifdef TRACE
void mm_trace_GBM_Dump( MM_Trace* trace ) {
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

