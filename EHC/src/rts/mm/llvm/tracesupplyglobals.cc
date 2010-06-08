
%%[8
#include "../../rts.h"
%%]



%%[8

extern Word *** _llvm_globals_descriptor;
extern Word     _llvm_globals_descriptor_count;

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Internal functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Interface functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
void mm_traceSupplyGlobals_llvm_Init( MM_TraceSupply* traceSupply, MM_Malloc* memmgt, MM_Mutator* mutator ) 
{

    printf("mm_traceSupplyGlobals_llvm_Init\n");

	MM_TraceSupply_Stack_Data* stackData = memmgt->malloc( sizeof(MM_TraceSupply_Stack_Data) ) ;
	stackData->trace = mutator->trace ;
	traceSupply->data = (MM_TraceSupply_Data_Priv*)stackData ;

    
}

void mm_traceSupplyGlobals_llvm_Reset( MM_TraceSupply* traceSupply, Word gcStackInfo ) 
{

    printf("mm_traceSupplyGlobals_llvm_Reset\n");

	MM_TraceSupply_Stack_Data* stackData = (MM_TraceSupply_Stack_Data*)traceSupply->data ;
	stackData->gcStackInfo = (GCStackInfo*)gcStackInfo ;

}

void mm_traceSupplyGlobals_llvm_Run( MM_TraceSupply* traceSupply )
{

    printf("mm_traceSupplyGlobals_llvm_Run\n");

	MM_TraceSupply_Stack_Data* stackData;
	MM_Trace *trace;

	stackData = (MM_TraceSupply_Stack_Data*) traceSupply->data ;
	trace     =                              stackData->trace;

    Word nrObjs2 = _llvm_globals_descriptor_count;
    printf("nr objs: %i \n", nrObjs2);

    Word * objs = (Word *) &_llvm_globals_descriptor;

    printf("check");

    for ( ; nrObjs2 > 0 ; nrObjs2--, objs++ ) {
        Word * objs2 = (Word*) *objs;
        Word * objs3 = (Word*) *objs2;
        printf("nr: %i p1: %016llx p2: %016llx p3: %016llx v: %i \n", nrObjs2, objs, objs2, objs3, *objs3);
        Word * objst = mm_Trace_TraceObject( trace, objs3 ) ;
        printf("updating global root: %016llx to: %016llx \n", objs3, objst);
        (*objs2) = objst;
        printf("nr: %i p1: %016llx p2: %016llx p3: %016llx v: %i \n", nrObjs2, objs, objs2, objs3, *objs3);

	}
    

    return;

}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% MM_TraceSupply interface object
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
MM_TraceSupply mm_traceSupplyGlobals_llvm =
	{ NULL
	, &mm_traceSupplyGlobals_llvm_Init
	, MM_Undefined
	, &mm_traceSupplyGlobals_llvm_Reset
	, &mm_traceSupplyGlobals_llvm_Run
	, MM_Undefined
	} ;
%%]


