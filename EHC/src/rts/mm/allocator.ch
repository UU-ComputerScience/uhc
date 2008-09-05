%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Memory management: allocator
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Responsibilities of an allocator:
- allocation
- de-allocation
- tracing of live pointers
- collection

Allocation is mandatory, the rest is optional, depending on policy.

For more (global) info see mm.h

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Allocator interface
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
typedef Ptr  MM_Allocator_Data_Priv ;

typedef struct MM_Allocator {
	// private data of Allocator
  	MM_Allocator_Data_Priv 		data ;
  	
  	// setup with a particular MM_Space
  	void			 			(*init)( struct MM_Allocator*, MM_Malloc* memmgt, MM_Space* space ) ;
  	void			 			(*resetWithSpace)( struct MM_Allocator*, MM_Space* space ) ;
  	
  	// allocation
  	Ptr 						(*alloc)( struct MM_Allocator*, Word sz ) ;
  	void 						(*dealloc)( struct MM_Allocator*, Ptr ptr ) ;
  	
  	// tracing live pointers
  	
  	// collection
} MM_Allocator ;
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Default allocators
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
// allocator for non-gc managed mem
extern MM_Allocator mm_allocator_Fixed ;

// allocator for first phase gc managed mem, used by the mutator directly
extern MM_Allocator mm_allocator_GC_1 ;

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Initialization
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
extern void mm_init_allocator() ;
%%]
