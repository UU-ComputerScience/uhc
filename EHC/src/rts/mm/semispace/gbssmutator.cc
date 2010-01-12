%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Memory management: Mutator: GBSS
%%% see associated .ch file for more info.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#include "../../rts.h"
#if __UHC_TARGET_BC__
#include "../../bc/interpreter.h"
#endif
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
void mm_mutator_GBSS_Init
		( MM_Mutator* mutator
		, MM_Malloc* memmgt
		, MM_Allocator* allocator
		, MM_Allocator* resAllocator
		, MM_Trace* trace
		, MM_Module* module
%%[[94
		, MM_WeakPtr* weakPtrAdm
		, MM_DEQue* weakPtrFinalizeQue
%%]]
		) {
	// MM_Mutator_GBSS_Data* mutss = memmgt.malloc( sizeof(MM_Mutator_GBSS_Data) ) ;
	
	mutator->allocator = allocator ;
	mutator->residentAllocator = resAllocator ;
	mutator->trace = trace ;
	mutator->module = module ;
	mutator->malloc = memmgt ;
%%[[94
	mutator->weakPtrAdm = weakPtrAdm ;
	mutator->weakPtrFinalizeQue = weakPtrFinalizeQue ;
%%]]
	
	// mutator->data = (MM_Mutator_Data_Priv*)mutatoryyy ;
}

Bool mm_mutator_GBSS_IsMaintainedByGC( MM_Mutator* mutator, Word obj ) {
	// MM_Mutator_GBSS_Data* mutss = (MM_Mutator_GBSS_Data*)mutator->data ;
	return GB_Word_IsPtr( obj ) && mm_Spaces_GetSpaceForAddress( obj ) != NULL ;
}

%%]
void mm_mutator_GBSS_zzz( MM_Mutator* mutator, ... ) {
	// MM_Mutator_GBSS_Data* mutatoryyy = (MM_Mutator_GBSS_Data*)mutator->data ;
		
}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% GBSS special purpose allocation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[94
Ptr mm_mutator_GBSS_Alloc_WeakPtr( MM_Mutator* mutator ) {
	GB_NodePtr n = (GB_NodePtr)mutator->allocator->alloc( mm_mutator.allocator, GB_NodeWeakPtrSize << Word_SizeInBytes_Log, 0 ) ;
	n->header = GB_MkWeakPtrHeader ;
	return (Ptr)n ;
}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% GBSS running a finalizer, an IO ()
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[99
void mm_mutator_GBSS_RunFinalizer( MM_Mutator* mutator, Word finalizer ) {
#	if __UHC_TARGET_BC__
		gb_runIO( finalizer ) ;
#	endif
}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% GBSS interface object
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
MM_Mutator mm_mutator_GBSS =
	{ NULL
	, NULL
	, NULL
	, NULL
	, NULL
	, NULL
%%[[94
	, NULL
	, NULL
%%]]
	, &mm_mutator_GBSS_Init
	, &mm_mutator_GBSS_IsMaintainedByGC
	// , &
%%[[94
	, &mm_mutator_GBSS_Alloc_WeakPtr
%%]]
%%[[99
	, &mm_mutator_GBSS_RunFinalizer
%%]]
	} ;
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% GBSS dump
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#ifdef TRACE
void mm_mutator_GBSS_Dump( MM_Mutator* mutator ) {
}
#endif
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% GBSS test
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#ifdef TRACE
void mm_mutator_GBSS_Test() {
}
#endif
%%]

