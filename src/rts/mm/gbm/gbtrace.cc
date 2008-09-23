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
	// IF_GB_TR_ON(3,{printf("mm_trace_GBM_CanTrace obj=%x\n",obj);}) ;
	Bool res = GB_Word_IsPtr( obj ) && mm_Spaces_AddressIsGCManagedBySpace( obj, collector->collectedSpace ) ;
	// IF_GB_TR_ON(3,{printf("mm_trace_GBM_CanTrace B\n");}) ;
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

Word mm_trace_GBM_TraceKnownToBeObject( MM_Trace* trace, Word obj, MM_Trace_Flg flg ) {
	IF_GB_TR_ON(3,{printf("mm_trace_GBM_TraceKnownToBeObject obj=%x\n",obj);}) ;
	
	// skip indirection nodes until we are at a real node; do not bother repeating this for other indirections as there will in general not be many
	GB_NodeHeader h = ((GB_NodePtr)obj)->header ;
	for ( ; GB_NH_Fld_NdEv(h) == GB_NodeNdEv_Yes && GB_NH_Fld_TagCat(h) == GB_NodeTagCat_Ind ; ) {
		obj = ((GB_NodePtr)obj)->content.fields[0] ;
		if ( mm_trace_GBM_CanTraceObject( trace, obj ) ) {
			h = ((GB_NodePtr)obj)->header ;
		} else {
			return obj ;
		}
	}

	IF_GB_TR_ON(3,{printf("mm_trace_GBM_TraceKnownToBeObject ind obj=%x\n",obj);}) ;
	GB_NodePtr objRepl = (GB_NodePtr)obj ;
	
	switch ( GB_NH_Fld_NdEv(h) ) {
		// is already forwarded, so return the forwarding
		case GB_NodeNdEv_Fwd :
			objRepl = (GB_NodePtr)( ((GB_NodePtr)obj)->content.fields[0] ) ;
			// objRepl = (GB_NodePtr)( GB_NH_SetFld_NdEv( h, 0 ) ) ;
			IF_GB_TR_ON(3,{printf("mm_trace_GBM_TraceKnownToBeObject fwd header=%x objRepl=%x\n",h,objRepl);}) ;
			break ;

		// otherwise copy and schedule new object for tracing
		default :
			{
				Word szWords = GB_NH_Fld_Size(h) ;
				MM_Trace_GBM_Data* tr = (MM_Trace_GBM_Data*)trace->data ;
				MM_Allocator* alc = trace->allocator ;
				Bool doTrace = flg & MM_Trace_Flg_Trace ;
				Bool doCopy  = flg & MM_Trace_Flg_Copy ;
				
				// new obj, copy old into new, allocate
				if ( doCopy ) {
					objRepl = (GB_NodePtr)( alc->alloc( alc, szWords << Word_SizeInBytes_Log ) ) ;
					IF_GB_TR_ON(3,{printf("mm_trace_GBM_TraceKnownToBeObject COPY obj=%x, objRepl=%x\n", obj, objRepl);}) ;
					// copy header
					objRepl->header = h ;

					// copy fields
					Word* fieldTo = objRepl->content.fields ;
					Word* fieldFr = ((GB_NodePtr)obj)->content.fields ;
					for ( szWords-- ; szWords > 0 ; szWords-- ) {
						*(fieldTo++) = *(fieldFr++) ;
					}

					// forward old obj to new
					((GB_NodePtr)obj)->header = GB_NH_SetFld_NdEv(h,GB_NodeNdEv_Fwd) ;
					((GB_NodePtr)obj)->content.fields[0] = (Word)objRepl ;
					// ((GB_NodePtr)obj)->header = GB_NH_SetFld_NdEv((Word)objRepl,GB_NodeNdEv_Fwd) ;
				}
				
				if ( doTrace ) {
%%[[95
					// schedule for tracing, depending on type of node
					if ( GB_NH_Fld_NdEv(h) == GB_NodeNdEv_No && GB_NH_Fld_TagCat(h) == GB_NodeTagCat_Intl ) {
						switch( GB_NH_Fld_Tag(h) ) {
							case GB_NodeTag_Intl_Malloc :
							case GB_NodeTag_Intl_Malloc2 :
%%[[97
#							if USE_GMP
								case GB_NodeTag_Intl_GMP_intl :
								case GB_NodeTag_Intl_GMP_mpz :
#							endif
%%]]
%%[[98
							case GB_NodeTag_Intl_Chan :
%%]]
								doTrace = False ;
								break ;
							default :
								break ;
						}
					} 
%%]]
					if ( doTrace ) {
						if ( doCopy ) {
							tr->traceSupply->pushWork( tr->traceSupply, (Word*)objRepl, szWords, alc->lastAllocFragment(alc) ) ;
						} else {
							mm_trace_GBM_TraceObjects( trace, objRepl->content.fields, szWords, ( flg & MM_Trace_Flg_Trace2 ? MM_Trace_Flg_Trace : MM_Trace_Flg_All ) ) ;
						}
					}
				}

			}
			break ;
	}
	IF_GB_TR_ON(3,{printf("mm_trace_GBM_TraceKnownToBeObject B\n");}) ;
		
	return (Word)objRepl ;
}

%%]

%%[8
void mm_trace_GBM_TraceObjects( MM_Trace* trace, Word* objs, Word nrObjs, MM_Trace_Flg flg ) {
	for ( ; nrObjs > 0 ; nrObjs--, objs++ ) {
		if ( mm_trace_GBM_CanTrace( *objs, trace->collector ) ) {
			*objs = mm_trace_GBM_TraceKnownToBeObject( trace, *objs, flg ) ;
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

