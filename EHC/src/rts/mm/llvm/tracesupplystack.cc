%%[8
#include "../../rts.h"
%%]


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Internal functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Interface functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
void mm_traceSupplyStack_llvm_Init( MM_TraceSupply* traceSupply, MM_Malloc* memmgt, MM_Mutator* mutator ) 
{
    printf("mm_traceSupplyStack_llvm_Init\n");

	MM_TraceSupply_Stack_Data* stackData = memmgt->malloc( sizeof(MM_TraceSupply_Stack_Data) ) ;
	stackData->trace = mutator->trace ;
	traceSupply->data = (MM_TraceSupply_Data_Priv*)stackData ;
}

void mm_traceSupplyStack_llvm_Reset( MM_TraceSupply* traceSupply, Word gcStackInfo ) 
{
    printf("mm_traceSupplyStack_llvm_Reset\n");
	MM_TraceSupply_Stack_Data* stackData = (MM_TraceSupply_Stack_Data*)traceSupply->data ;
	stackData->gcStackInfo = (GCStackInfo*)gcStackInfo ;
}

void mm_traceSupplyStack_llvm_Run( MM_TraceSupply* traceSupply )
{

    printf("mm_traceSupplyStack_llvm_Run\n");

    return;
    //exit(1);
}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% MM_TraceSupply interface object
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
MM_TraceSupply mm_traceSupplyStack_llvm =
	{ NULL
	, &mm_traceSupplyStack_llvm_Init
	, MM_Undefined
	, &mm_traceSupplyStack_llvm_Reset
	, &mm_traceSupplyStack_llvm_Run
	, MM_Undefined
	} ;
%%]
