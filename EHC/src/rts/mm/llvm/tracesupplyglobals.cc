
%%[8
#include "../../rts.h"
%%]



%%[8

extern Word ** _llvm_globals_descriptor[];
extern Word _llvm_globals_descriptor_count;

MM_Trace * mmtrace;

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
    //printf("mm_traceSupplyGlobals_llvm_Init\n");
    mmtrace = mutator->trace ;
}

void mm_traceSupplyGlobals_llvm_Reset( MM_TraceSupply* traceSupply, Word gcStackInfo ) 
{
    //printf("mm_traceSupplyGlobals_llvm_Reset\n");
}

void mm_traceSupplyGlobals_llvm_Run( MM_TraceSupply* traceSupply )
{

    //printf("mm_traceSupplyGlobals_llvm_Run\n");

    Word nrObjs = _llvm_globals_descriptor_count;
    //printf("nr objs: %i \n", nrObjs);

    Word ** objs = &_llvm_globals_descriptor;

    for ( ; nrObjs > 0 ; nrObjs--, objs++ ) {
        //printf("global %i \n", nrObjs);
        (**objs) = mm_Trace_TraceObject( mmtrace, (**objs) );
        //printf("updating global root: %016llx to: %016llx \n");
	}

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


/*
    printf("mm_traceSupplyGlobals_llvm_Run\n");

    Word nrObjs = _llvm_globals_descriptor_count;
    printf("nr objs: %i \n", nrObjs);

    Word * objs = (Word *) &_llvm_globals_descriptor;

    for ( ; nrObjs > 0 ; nrObjs--, objs++ ) {
        Word * objs2 = (Word*) *objs;
        Word * objs3 = (Word*) *objs2;

                    printf("nr: %i p1: %016llx p2: %016llx p3: %016llx v: %i \n", nrObjs, objs, objs2, objs3, *objs3);

        Word * objst = mm_Trace_TraceObject( mmtrace, objs3 ) ;

                    printf("updating global root: %016llx to: %016llx \n", objs3, objst);

        (*objs2) = objst;

                    printf("nr: %i p1: %016llx p2: %016llx p3: %016llx v: %i \n", nrObjs, objs, objs2, objs3, *objs3);

	}
    
*/

