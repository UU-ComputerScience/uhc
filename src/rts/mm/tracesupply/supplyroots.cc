%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Memory management: TraceSupply: Roots
%%% see associated .ch file for more info.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#include "../../rts.h"
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Roots internal defs
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Roots internal functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Roots internal functions, but still externally visible (because of inlining)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Roots interface
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
void mm_traceSupply_Roots_Init( MM_TraceSupply* traceSupply, MM_Malloc* memmgt, MM_Trace* trace ) {
	MM_TraceSupply_Roots_Data* trgr = memmgt->malloc( sizeof(MM_TraceSupply_Roots_Data) ) ;
	trgr->trace = trace ;
	traceSupply->data = (MM_TraceSupply_Data_Priv*)trgr ;
}

void mm_traceSupply_Roots_Reset( MM_TraceSupply* traceSupply ) {
	// nothing to be done
}

void mm_traceSupply_Roots_Run( MM_TraceSupply* traceSupply ) {
	MM_TraceSupply_Roots_Data* trgr = (MM_TraceSupply_Roots_Data*)traceSupply->data ;
	
	IF_GB_TR_ON(3,{printf("mm_traceSupply_Roots_Run A\n");}) ;
	MM_LclRoot_Grp* grp ;
	for ( grp = mm_LclRoots ; grp != NULL ; grp = grp->next ) {
		MM_LclRoot_One* one ;
		for ( one = grp->ones ; one != NULL ; one = one->next ) {
			*(one->ptrToObj) = mm_Trace_TraceObject( trgr->trace, *(one->ptrToObj), MM_Trace_Flg_All ) ;
		}
	}
	
	IF_GB_TR_ON(3,{printf("mm_traceSupply_Roots_Run B\n");}) ;
	MM_FlexArray_Inx i, j ;
	for ( i = 0 ; i < mm_flexArray_SizeUsed( &mm_Roots ) ; i++ ) {
		MM_Roots_Entry* r = (MM_Roots_Entry*)mm_flexArray_At( &mm_Roots, i ) ;
		// IF_GB_TR_ON(3,{printf("mm_traceSupply_Roots_Run i=%x obj=%x nr=%x flg=%x\n",i,*(r->ptrToObj), r->nrObjs, r->flags);}) ;
		trgr->trace->traceObjects( trgr->trace, r->ptrToObj, r->nrObjs, r->flags ) ;
	}

	IF_GB_TR_ON(3,{printf("mm_traceSupply_Roots_Run C\n");}) ;
}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Roots interface object
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
MM_TraceSupply mm_traceSupply_Roots =
	{ NULL
	, &mm_traceSupply_Roots_Init
	, MM_Undefined
	, &mm_traceSupply_Roots_Reset
	, &mm_traceSupply_Roots_Run
	, MM_Undefined
	} ;
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Roots dump
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#ifdef TRACE
void mm_traceSupply_Roots_Dump( MM_TraceSupply* traceSupply ) {
}
#endif
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Roots test
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#ifdef TRACE
void mm_traceSupply_Roots_Test() {
}
#endif
%%]

