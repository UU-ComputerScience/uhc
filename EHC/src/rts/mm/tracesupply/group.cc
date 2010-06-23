%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Memory management: TraceSupply: Group
%%% see associated .ch file for more info.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#include "../../rts.h"
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Group internal defs
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Group internal functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Group internal functions, but still externally visible (because of inlining)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Group interface
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
void mm_traceSupply_Group_Init( MM_TraceSupply* traceSupply, MM_Malloc* memmgt, MM_Mutator* mutator ) {
	MM_TraceSupply_Group_Data* trsup = memmgt->malloc( sizeof(MM_TraceSupply_Group_Data) ) ;
		
	traceSupply->data = (MM_TraceSupply_Data_Priv*)trsup ;
}

void mm_traceSupply_Group_InitWithSub( MM_TraceSupply* traceSupply, MM_Malloc* memmgt, MM_Mutator* mutator, MM_FlexArray* subTraceSupplies ) {
	mm_traceSupply_Group_Init( traceSupply, memmgt, mutator ) ;
	MM_TraceSupply_Group_Data* trsup = (MM_TraceSupply_Group_Data*)traceSupply->data ;
	trsup->subTraceSupplies = *subTraceSupplies ;
}

void mm_traceSupply_Group_Reset( MM_TraceSupply* traceSupply, Word gcInfo ) {
	MM_TraceSupply_Group_Data* trsup = (MM_TraceSupply_Group_Data*)traceSupply->data ;
	
	MM_FlexArray_Inx i ;
	for ( i = 0 ; i < mm_flexArray_SizeUsed( &trsup->subTraceSupplies ) ; i++ ) {
		MM_TraceSupply* subTraceSupply = (MM_TraceSupply*)mm_flexArray_At( &trsup->subTraceSupplies, i ) ;
		subTraceSupply->reset( subTraceSupply, gcInfo ) ;
	}
}

void mm_traceSupply_Group_Run( MM_TraceSupply* traceSupply ) {
	MM_TraceSupply_Group_Data* trsup = (MM_TraceSupply_Group_Data*)traceSupply->data ;

	IF_GB_TR_ON(3,{printf("mm_traceSupply_Group_Run\n");}) ;
	MM_FlexArray_Inx i ;
	for ( i = 0 ; i < mm_flexArray_SizeUsed( &trsup->subTraceSupplies ) ; i++ ) {
		IF_GB_TR_ON(3,{printf("mm_traceSupply_Group_Run i=%x\n",i);}) ;
		MM_TraceSupply* subTraceSupply = (MM_TraceSupply*)mm_flexArray_At( &trsup->subTraceSupplies, i ) ;
		subTraceSupply->run( subTraceSupply ) ;
	}
	IF_GB_TR_ON(3,{printf("mm_traceSupply_Group_Run B\n");}) ;
}

void mm_traceSupply_Group_PushWork( MM_TraceSupply* traceSupply, Word* work, Word nrWorkWords, Word extra ) {
	MM_TraceSupply_Group_Data* trsup = (MM_TraceSupply_Group_Data*)traceSupply->data ;
		
	// IF_GB_TR_ON(3,{printf("mm_traceSupply_Group_PushWork\n");}) ;
	MM_TraceSupply* subTraceSupply = (MM_TraceSupply*)mm_flexArray_At( &trsup->subTraceSupplies, mm_flexArray_SizeUsed( &trsup->subTraceSupplies ) - 1 ) ;
	subTraceSupply->pushWork( subTraceSupply, work, nrWorkWords, extra ) ;
	// IF_GB_TR_ON(3,{printf("mm_traceSupply_Group_PushWork B\n");}) ;
}

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Group interface object
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
MM_TraceSupply mm_traceSupply_Group =
	{ NULL
	, &mm_traceSupply_Group_Init
	, &mm_traceSupply_Group_InitWithSub
	, &mm_traceSupply_Group_Reset
	, &mm_traceSupply_Group_Run
	, &mm_traceSupply_Group_PushWork
	} ;
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Group dump
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#ifdef TRACE
void mm_traceSupply_Group_Dump( MM_TraceSupply* traceSupply ) {
}
#endif
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Group test
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#ifdef TRACE
void mm_traceSupply_Group_Test() {
}
#endif
%%]

