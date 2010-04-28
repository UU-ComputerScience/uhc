%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Memory management: Trace: GBM
%%% see associated .ch file for more info.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Trace a GBM object, knowing its structure etc.

%%[8
#include "../../rts.h"
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Auxiliary function
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
static inline Bool mm_trace_GBM_CanTrace( Word obj, MM_Collector* collector ) {

    // This function decides whether obj is a traceable object,
    // that is: whether it is indeed a pointer (and not an unboxed integer),
    // and if so, whether it is in the GC-administered address space.

	// TODO: querying via collector->isInCollectedSpace

	Bool res = GB_Word_IsPtr( obj ) && mm_Spaces_AddressIsGCManagedBySpace( obj, collector->collectedSpace ) ;

#if TRACE
	// IF_GB_TR_ON(3,{printf("mm_trace_GBM_CanTrace obj=%x\n",obj);}) ;
	if (! res ) {
		// IF_GB_TR_ON(3,{printf("mm_trace_GBM_CanTrace NOT: x=%x, space(x)=%p, collspace=%p\n", obj, mm_Spaces_GetSpaceForAddress( obj ), collector->collectedSpace );}) ;
	}
#endif

	return res ;
}

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% MM_Trace implementation for GBM backend
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
void mm_trace_GBM_Init( MM_Trace* trace, void* traceSupply, MM_Allocator* allocator, MM_Collector* collector ) {

    // allocate a small object...
	MM_Trace_GBM_Data* tr = mm_malloc_LOF.malloc( sizeof(MM_Trace_GBM_Data) ) ;
    // ...to keep the traceSupply	
	tr->traceSupply = (MM_TraceSupply*)traceSupply ;
	
	// Store it, and two more fields, in the trace object.
	trace->data      = (MM_Trace_Data_Priv*)tr ;
	trace->collector = collector ;
	trace->allocator = allocator ;
}

Bool mm_trace_GBM_CanTraceObject( MM_Trace* trace, Word obj ) {
    
    // This function decides whether obj is a traceable object,
    // that is: whether it is indeed a pointer (and not an unboxed integer),
    // and if so, whether it is in the GC-administered address space.
    // The actual work is done by an (inlined) auxiliary function,
    // because that function is needed anyway because it is sometimes called directly.
    
	return mm_trace_GBM_CanTrace( obj, trace->collector ) ;
}

Word mm_trace_GBM_TraceKnownToBeObject( MM_Trace* trace, Word obj ) {
    
    // This function is responisble for copying an object "from-space" to "to-space".
    // If the object was already copied before, the copy is returnd.
    // If the object was not yet copied, it is done now:
    // - a new object is allocated
    // - its payload is copied
    // - the pointers in the payload a queued for copying as well
    // The original object is overwritten with a forwarding node.
    // The new object is returned.
    
	// IF_GB_TR_ON(3,{printf("mm_trace_GBM_TraceKnownToBeObject obj=%x obj->header=%x\n",obj,((GB_NodePtr)obj)->header);}) ;
	
	GB_NodeHeader h;

    // If the object is an indirection node, follow the chain of indirections

	for (h=((GB_NodePtr)obj)->header ; gb_NodeHeader_IsIndirection(h) ; h=((GB_NodePtr)obj)->header) {
		IF_GB_TR_ON(3,{printf("mm_trace_GBM_TraceKnownToBeObject ind1 obj=%x(space=%p) -> %x(space=%p)\n",obj, mm_Spaces_GetSpaceForAddress( obj ),((GB_NodePtr)obj)->content.fields[0], mm_Spaces_GetSpaceForAddress( ((GB_NodePtr)obj)->content.fields[0] ));}) ;
		
		// Follow the indirection pointer, effectively removing the indirection node
		obj = ((GB_NodePtr)obj)->content.fields[0] ;
		
		if ( ! mm_trace_GBM_CanTraceObject( trace, obj ) ) 
		{   // The node was appearantly a forwarding node, indicating that the node was copied before, 
		    // so we can just return that and we're done
			return obj ;
		}
	}


    // Now we are sure that node "obj", with header "h", must be copied.
	// IF_GB_TR_ON(3,{printf("mm_trace_GBM_TraceKnownToBeObject BEF COPY obj=%x, h=%x, sz(h)=%x, space=%p\n", obj, h, GB_NH_Fld_Size(h),mm_Spaces_GetSpaceForAddress( obj ));}) ;

    // Find out the size of the new object to be allocated

	Word szWords ;
	szWords = GB_NH_Fld_Size(h) ;

%%[[97
#	if 0 && USE_LTM
		if ( GB_NH_Fld_Tag(h) == GB_NodeTag_Intl_LTM_mpz ) {
			// LTM nodes are overallocated, too large, so we only copy what is necessary.
			szWords = GB_NodeLTMMpzSize(GB_LTM_Int_Used((GB_NodePtr)obj)) ;
			GB_NH_SetFld_Size(h,szWords) ;
		}
#	endif
%%]]


    // Allocate the new object
	MM_Allocator *allocator  =  trace->allocator ;
    GB_NodePtr objRepl;
	objRepl = (GB_NodePtr)( allocator->alloc( allocator, szWords << Word_SizeInBytes_Log, 0 ) ) ;    // alloc wants the size in bytes, so multiply the szWords by the Word_Size


    // Initialize the new object
	objRepl->header = h ;
	Word* fieldTo   =          objRepl ->content.fields ;
	Word* fieldFrom = ((GB_NodePtr)obj)->content.fields ;
	Word sz = szWords ;
	for ( sz-- ; sz > 0 ; sz-- ) {
		*(fieldTo++) = *(fieldFrom++) ;
	}

	IF_GB_TR_ON(3,{printf("mm_trace_GBM_TraceKnownToBeObject AFT COPY obj=%x, h=%x, sz(h)=%x, objRepl=%p, space=%p\n", obj, h, GB_NH_Fld_Size(h),objRepl, mm_Spaces_GetSpaceForAddress( (Word)objRepl ));}) ;


    // Overwrite the original object with a forwarding node, which points to the new object
    // The forwarding node has a special tag
	// ((GB_NodePtr)obj)->header = GB_NH_SetFld_NdEv(h,GB_NodeNdEv_Fwd) ;
	// ((GB_NodePtr)obj)->header = GB_NH_SetFld_NdEv((Word)objRepl,GB_NodeNdEv_Fwd) ;
	((GB_NodePtr)obj)->header= GB_MkHeader(GB_NH_Fld_Size(h),GB_NodeNdEv_Yes,GB_NodeTagCat_Ind,0) ;

	// The forwarding node has a single-word payload, which is the pointer to the new object
	((GB_NodePtr)obj)->content.fields[0] = (Word)objRepl ;
	

    // Queue the payload of the freshly copied object to be processed as well

%%[[95
	if (gb_NH_HasTraceableFields( h ))
%%]]
    {
        // IF_GB_TR_ON(3,{printf("mm_trace_GBM_TraceKnownToBeObject TRACE obj=%x, objRepl=%x", obj, objRepl); gb_prWord( (Word)objRepl ) ; printf("\n") ;}) ;
		gb_assert_IsNotDangling_Node( objRepl, "mm_trace_GBM_TraceKnownToBeObject" ) ;

        // New strategy: queue the payload to be processed
        MM_TraceSupply* traceSupply = ((MM_Trace_GBM_Data*)trace->data)->traceSupply;
		traceSupply->pushWork( traceSupply, (Word*)objRepl, szWords, allocator->lastAllocFragment(allocator) ) ;

        // Obsolete strategy: recurse processing on the payload
		// mm_trace_GBM_TraceObjectPayload( trace, (Word)objRepl ) ;
	}

	// IF_GB_TR_ON(3,{printf("mm_trace_GBM_TraceKnownToBeObject B\n");}) ;
		
	return (Word)objRepl ;
}

%%]

%%[8
void mm_trace_GBM_TraceObjectPayload( MM_Trace* trace, Word obj ) {
    
    // Obsolete(?) function that recursively processes the payload of an object
    
	WPtr payload    = ((GB_NodePtr)obj)->content.fields ;
	WPtr payloadEnd = payload + GB_Node_NrFlds( (GB_NodePtr)obj ) ;
	for ( ; payload < payloadEnd ; payload++ ) {
		if ( mm_trace_GBM_CanTrace( *payload, trace->collector ) ) {
			*payload = mm_trace_GBM_TraceKnownToBeObject( trace, *payload ) ;
		}
	}
}
%%]


%%[8
Word mm_trace_GBM_EnsureNoIndirections( MM_Trace* trace, Word obj ) {
	return gb_Indirection_FollowObject( obj ) ;
}
%%]

%%[8
#ifdef TRACE
void mm_trace_GBM_Dump( MM_Trace* trace ) {
}
#endif
%%]

%%[8
#ifdef TRACE
void mm_trace_GBM_Test() {
}
#endif
%%]


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% MM_Trace interface object
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
MM_Trace mm_trace_GBM =
	{ NULL
	, NULL
	, NULL
	, sizeof(GB_NodeHeader) >> Word_SizeInBytes_Log
	, sizeof(GB_NodeHeader)
	, &mm_trace_GBM_Init
	, &mm_trace_GBM_CanTraceObject
	, &mm_trace_GBM_TraceKnownToBeObject
	, &mm_trace_GBM_TraceObjectPayload
	, &mm_trace_GBM_ObjectSize               // defined in h-file to make it inlineable
	, &mm_trace_GBM_HasTraceableWords        // defined in h-file to make it inlineable
	, &mm_trace_GBM_EnsureNoIndirections
	} ;
%%]
