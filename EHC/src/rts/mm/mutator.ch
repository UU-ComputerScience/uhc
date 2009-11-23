%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Memory management: Mutator
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[doesWhat doclatex
A mutator represents the variation in the use of memory for objects by
different runtimes. It also acts as the interface between the a virtual
machine and memory management, to be used by both VM and MM to get
something done from the other. In due time this should be split up.
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Mutator interface
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
typedef Ptr  MM_Mutator_Data_Priv ;

typedef struct MM_Mutator {
	// private data of Mutator
  	MM_Mutator_Data_Priv 		data ;
  	
	// private, but inlined for efficiency
  	MM_Allocator* 				allocator ;
  	MM_Allocator* 				residentAllocator ;
  	MM_Trace* 					trace ;
  	MM_Module* 					module ;
  	MM_Malloc*					malloc ;
  	
  	// setup with a particular MM_Pages
  	void			 			(*init)( struct MM_Mutator*, MM_Malloc* memmgt, MM_Allocator* allocator, MM_Allocator* resAllocator, MM_Trace* trace, MM_Module* module ) ;
  	
  	// is maintained by GC?
  	Bool						(*isMaintainedByGC)( struct MM_Mutator*, Word obj ) ;

  	// scan runtime object, add new objects to be traced to trace
  	//void						(*scanObject)( struct MM_Mutator*, MM_TraceSupply* trace ) ;

%%[[94
  	// allocation variants for specific objects required by MM
  	Ptr							(*allocWeakPtr)( struct MM_Mutator* ) ;
%%]]
%%[[99
  	// run a (user specified) finalization object
  	void						(*runFinalizer)( struct MM_Mutator*, Word finalizer ) ;
%%]]
  	
} MM_Mutator ;
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Default Mutator
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
extern MM_Mutator mm_mutator ;
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Initialization
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
extern void mm_init_mutator() ;
%%]
