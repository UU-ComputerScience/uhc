%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Memory management: Module: GBSS
%%% see associated .ch file for more info.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#include "../../rts.h"
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% GBSS internal defs
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% GBSS internal functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% GBSS internal functions, but still externally visible (because of inlining)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% GBSS interface
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
void mm_module_GBSS_Init( MM_Module* module, MM_Malloc* memmgt ) {
	MM_Module_GBSS_Data* mod = memmgt->malloc( sizeof(MM_Module_GBSS_Data) ) ;
	
	mm_flexArray_New( memmgt, &mod->gbModules, sizeof(MM_Module_GBSS_Module), 5, 0 ) ;
		
	module->data = (MM_Module_Data_Priv*)mod ;
}

int mm_module_GBSS_RegisterModule( MM_Module* module, Ptr perMod ) {
	MM_Module_GBSS_Data* mod = (MM_Module_GBSS_Data*)module->data ;
	
	MM_FlexArray_Inx inx = mm_flexArray_AllocSlot( &mod->gbModules ) ;
	*( (MM_Module_GBSS_Module*)( mm_flexArray_At( &mod->gbModules, inx ) ) ) = *((MM_Module_GBSS_Module*)perMod) ;
	
	return inx ;
}

MM_FlexArray* mm_module_GBSS_GetRegisteredModules( MM_Module* module ) {
	MM_Module_GBSS_Data* mod = (MM_Module_GBSS_Data*)module->data ;
	return &mod->gbModules ;
}

%%]


void mm_module_GBSS_zzz( MM_Module* module, ... ) {
	MM_Module_GBSS_Data* mod = (MM_Module_GBSS_Data*)module->data ;
		
}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% GBSS interface object
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
MM_Module mm_module_GBSS =
	{ NULL
	, &mm_module_GBSS_Init
	, &mm_module_GBSS_RegisterModule
	, &mm_module_GBSS_GetRegisteredModules
	} ;
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% GBSS dump
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#ifdef TRACE
void mm_module_GBSS_Dump( MM_Module* module ) {
	MM_Module_GBSS_Data* moduleyyy = (MM_Module_GBSS_Data*)module->data ;

	printf( ">------------------------> MM_Module: GBSS: module=%p moduleyyy=%p\n", module, moduleyyy ) ;

	printf( "<------------------------< MM_Module: GBSS\n" ) ;

}
#endif
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% GBSS test
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#ifdef TRACE
void mm_module_GBSS_Test() {
}
#endif
%%]

