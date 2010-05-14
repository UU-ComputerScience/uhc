
%%[8
#include "../../rts.h"


//    ,  constantInfoNodeSize    :: Int
//    ,  constantInfoPayloadSize :: Int
//    ,  constantInfoHasPointers :: Bool

struct FDescr 
{
   Word max_fields;
   Word num_fields;
   unsigned char has_pointers;
};


extern struct FDescr _llvm_node_descriptor[];

#define FORWARDING_TAG 0xFFFFFFFF

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% MM_Trace implementation for llvm backend
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
void mm_trace_llvm_Init( MM_Trace* trace, void* traceSupply, MM_Allocator* allocator, MM_Collector* collector ) 
{
    printf("mm_trace_llvm_Init\n");

	// Store the given parameters in the trace object.
	trace->data      = (MM_Trace_Data_Priv*)traceSupply ;
	trace->collector = collector ;
	trace->allocator = allocator ;
}

Bool mm_trace_llvm_CanTraceObject( MM_Trace* trace, Word obj ) 
{
    printf("mm_trace_llvm_CanTraceObject should not be needed!\n");
    return 1;
}

Word mm_trace_llvm_TraceKnownToBeObject( MM_Trace* trace, Word obj ) 
{
    printf("mm_trace_llvm_TraceKnownToBeObject\n");

    // This function is responsible for copying an object "from-space" to "to-space".
    // If the object was already copied before, the copy is returnd.
    // If the object was not yet copied, it is done now:
    // - a new object is allocated
    // - its payload is copied
    // - the pointers in the payload a queued for copying as well
    // The original object is overwritten with a forwarding node.
    // The new object is returned.


    
	Word tag;
	tag=((WPtr)obj)[0];

    printf("TAG: %lld \n", tag );

	if (tag==FORWARDING_TAG)
    {
        // The node is a forwarding node, indicating that the node was copied before,
        // so we can just return that and we're done
		obj = ((WPtr)obj)[1];
		return obj ;
	}

    printf("CHECK1 \n" );

    // Now we are sure that node "obj", with tag "tag", must be copied.

    // Find out the size of the new object to be allocated

	Word szWords ;
	szWords = 1 + _llvm_node_descriptor[tag].max_fields;

    printf("max_fields: %lld \n", szWords );

    printf("CHECK2 \n" );

    // Allocate the new object
	MM_Allocator *allocator  =  trace->allocator ;
    WPtr objRepl;
	objRepl = (WPtr)( allocator->alloc( allocator, szWords << Word_SizeInBytes_Log, 0 ) ) ;    // alloc wants the size in bytes, so multiply the szWords by the Word_Size

    printf("CHECK3 \n" );

    // Initialize the new object
	Word* fieldTo   =       objRepl;
	Word* fieldFrom = (WPtr)obj;
	Word counter;
	for ( counter = 0 ; counter < szWords; counter++ ) {
		*(fieldTo++) = *(fieldFrom++) ;
	}

    printf("CHECK4 \n" );


    // Overwrite the original object with a forwarding node, which points to the new object
    // The forwarding node has a special tag

	((WPtr)obj)[0] = FORWARDING_TAG ;

	// The forwarding node has a single-word payload, which is the pointer to the new object
	((WPtr)obj)[1] = (Word)objRepl ;

    printf("CHECK5 \n" );
	

    // Queue the payload of the freshly copied object to be processed as well
    MM_TraceSupply* traceSupply = (MM_TraceSupply*)trace->data;
	traceSupply->pushWork( traceSupply, (Word*)objRepl, szWords, allocator->lastAllocFragment(allocator) ) ;

    printf("CHECK6 \n" );

	return (Word)objRepl ;

}

void mm_trace_llvm_TraceObjectPayload( MM_Trace* trace, Word obj ) 
{
    printf("mm_trace_llvm_TraceObjectPayload should not be needed!\n");
}

Word mm_trace_llvm_ObjectSize( MM_Trace* trace, Word obj ) 
{
	return 0;
}

Bool mm_trace_llvm_HasTraceableWords( MM_Trace* trace, Word obj ) 
{
	return 0;
}

Word mm_trace_llvm_EnsureNoIndirections( MM_Trace* trace, Word obj ) 
{
    return 0;
}

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% MM_Trace interface object
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
MM_Trace mm_trace_llvm =
	{ NULL
	, NULL
	, NULL
	, 1              // size of node header in Words
	, sizeof(Word)   // size of node header in bytes
	, &mm_trace_llvm_Init
	, &mm_trace_llvm_CanTraceObject
	, &mm_trace_llvm_TraceKnownToBeObject
	, &mm_trace_llvm_TraceObjectPayload
	, &mm_trace_llvm_ObjectSize
	, &mm_trace_llvm_HasTraceableWords
	, &mm_trace_llvm_EnsureNoIndirections
	} ;
%%]
