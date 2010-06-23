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
  	
  	// allocation, with additional GC info, gcInfo==0 means no info
  	Ptr 						(*alloc)( struct MM_Allocator*, Word sz, Word gcInfo ) ;
  	// only ensure enough mem
  	void 						(*ensure)( struct MM_Allocator*, Word sz, Word gcInfo ) ;
  	// alloc after ensuring enough mem
  	Ptr 						(*allocEnsured)( struct MM_Allocator*, Word sz ) ;

  	// deallocation
  	void 						(*dealloc)( struct MM_Allocator*, Ptr ptr ) ;
  	
  	// last allocated location, or NULL if cannot determine
  	Ptr 						(*lastAllocAddress)( struct MM_Allocator* ) ;
  	// last in use fragment, or -1 if cannot determine
  	MM_Space_FragmentInx		(*lastAllocFragment)( struct MM_Allocator* ) ;

  	// size of total area until runs full, or maxint when runs until system limit
  	Word 						(*getTotalSize)( struct MM_Allocator* ) ;
  	void 						(*setTotalSize)( struct MM_Allocator*, Word sz ) ;
  	// size of used area
  	Word 						(*getUsedSize)( struct MM_Allocator* ) ;

  	// the space used
  	MM_Space* 					(*getSpace)( struct MM_Allocator* ) ;

  	// tracing live pointers
  	
  	// collection
  	
#ifdef TRACE
  	// dumping info
  	void 						(*dump)( struct MM_Allocator* ) ;
#endif

	// constants: max alloc size, (default) 0 if no max
	Word						maxAllocSize ;
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
