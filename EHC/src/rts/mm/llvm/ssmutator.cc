%%[8
#include "../../rts.h"
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% interface
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
void mm_ssmutator_llvm_Init
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
		) 
{
	mutator->allocator = allocator ;
	mutator->residentAllocator = resAllocator ;
	mutator->trace = trace ;
	mutator->module = module ;
	mutator->malloc = memmgt ;
%%[[94
	mutator->weakPtrAdm = weakPtrAdm ;
	mutator->weakPtrFinalizeQue = weakPtrFinalizeQue ;
%%]]
    
}

Bool mm_ssmutator_llvm_IsMaintainedByGC( MM_Mutator* mutator, Word obj ) 
{
    return 1;
}

%%]


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% special purpose allocation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[94
Ptr mm_ssmutator_llvm_Alloc_WeakPtr( MM_Mutator* mutator ) {
	return NULL;
}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% running a finalizer, an IO ()
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[99
void mm_ssmutator_llvm_RunFinalizer( MM_Mutator* mutator, Word finalizer ) {
}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% MM_Mutator interface object
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
MM_Mutator mm_ssmutator_llvm =
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
	, &mm_ssmutator_llvm_Init
	, &mm_ssmutator_llvm_IsMaintainedByGC
	// , &
%%[[94
	, &mm_ssmutator_llvm_Alloc_WeakPtr
%%]]
%%[[99
	, &mm_ssmutator_llvm_RunFinalizer
%%]]
	} ;
%%]

