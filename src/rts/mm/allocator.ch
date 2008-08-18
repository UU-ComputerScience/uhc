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
typedef Ptr  MM_Allocator_Data ;

typedef struct MM_Allocator {
	// private data of Allocator
  	MM_Allocator_Data 	data ;
  	
  	// setup
  	void			 	(*init)( struct MM_Allocator* ) ;
  	
  	// allocation
  	Ptr 				(*alloc)( struct MM_Allocator*, Word sz ) ;
  	void 				(*dealloc)( struct MM_Allocator*, Ptr ptr ) ;
  	
  	// tracing live pointers
  	
  	// collection
} MM_Allocator ;
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Initialization
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
extern void mm_init_allocator() ;
%%]
