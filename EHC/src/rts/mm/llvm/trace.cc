
%%[8
#include "../../rts.h"

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

	return obj ;
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
