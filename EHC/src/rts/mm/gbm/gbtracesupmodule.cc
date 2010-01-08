%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Memory management: TraceSupply: GBModule
%%% see associated .ch file for more info.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#include "../../rts.h"
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% GBModule internal defs
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% GBModule internal functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% GBModule internal functions, but still externally visible (because of inlining)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% GBModule interface
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
void mm_traceSupply_GBModule_Init( MM_TraceSupply* traceSupply, MM_Malloc* memmgt, MM_Mutator* mutator ) {
	MM_TraceSupply_GBModule_Data* trsup = memmgt->malloc( sizeof(MM_TraceSupply_GBModule_Data) ) ;
	trsup->trace = mutator->trace ;
	traceSupply->data = (MM_TraceSupply_Data_Priv*)trsup ;
}

void mm_traceSupply_GBModule_Reset( MM_TraceSupply* traceSupply, Word gcInfo ) {
	// nothing to be done
}

void mm_traceSupply_GBModule_Run( MM_TraceSupply* traceSupply ) {
	MM_TraceSupply_GBModule_Data* trsup = (MM_TraceSupply_GBModule_Data*)traceSupply->data ;
	
	MM_FlexArray* gbMods = mm_mutator.module->getRegisteredModules( mm_mutator.module ) ;
	MM_FlexArray_Inx i ;
	IF_GB_TR_ON(3,{printf("mm_traceSupply_GBModule_Run A nrmods=%x\n", mm_flexArray_SizeUsed(gbMods));}) ;
	for ( i = 0 ; i < mm_flexArray_SizeUsed(gbMods) ; i++ ) {
		IF_GB_TR_ON(3,{printf("mm_traceSupply_GBModule_Run B i=%x\n",i);}) ;
		MM_Module_GBSS_Module* gbMod = (MM_Module_GBSS_Module*)mm_flexArray_At( gbMods, i ) ;
		int j ;
		for ( j = 0 ; j < gbMod->nrCafGlobalEntryIndices ; j++ ) {
			WPtr e = (WPtr)( &gbMod->globalEntries[ gbMod->cafGlobalEntryIndices[j] ] ) ;
			IF_GB_TR_ON(3,{printf("mm_traceSupply_GBModule_Run C1 i=%x j=%x e=%p *e=%x \n",i,j,e,*e);}) ;
			*e = mm_Trace_TraceObject( trsup->trace, *e ) ;
			IF_GB_TR_ON(3,{printf("mm_traceSupply_GBModule_Run C2 i=%x j=%x e=%p *e=%x \n",i,j,e,*e);}) ;
		}
%%[[20
		WPtr e = (WPtr)(gbMod->expNode) ;
		*e = mm_Trace_TraceObject( trsup->trace, *e ) ;
%%]]
	}
	IF_GB_TR_ON(3,{printf("mm_traceSupply_GBModule_Run D\n");}) ;
}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% GBModule interface object
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
MM_TraceSupply mm_traceSupply_GBModule =
	{ NULL
	, &mm_traceSupply_GBModule_Init
	, MM_Undefined
	, &mm_traceSupply_GBModule_Reset
	, &mm_traceSupply_GBModule_Run
	, MM_Undefined
	} ;
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% GBModule dump
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#ifdef TRACE
void mm_traceSupply_GBModule_Dump( MM_TraceSupply* traceSupply ) {
}
#endif
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% GBModule test
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#ifdef TRACE
void mm_traceSupply_GBModule_Test() {
}
#endif
%%]

