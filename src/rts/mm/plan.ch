%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Memory management: plan
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[doesWhat doclatex
A plan is the place where all mm ingredients are combined.
A plan makes/decides on policies.
Only one plan can exist for a running program.
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  interface
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
typedef Ptr  MM_Plan_Data_Priv ;

typedef struct MM_Plan {
	// private data of Plan
  	MM_Plan_Data_Priv 			data ;
  	
	// private data of Plan, but included here for fast access
  	// MM_Collector*	 			collector ;
  	MM_Mutator*	 				mutator ;	// must == &mm_mutator
  	
  	// setup with a particular MM_Pages
  	void			 			(*init)( struct MM_Plan* ) ;
  	
  	// stop
  	void			 			(*exit)( struct MM_Plan* ) ;
  	
#if MM_BYPASS_PLAN
  	// setup bypass for efficiency
  	void			 			(*initBypass)( struct MM_Plan* ) ;
#endif
  	
  	// the point where GC can take place.
  	// isPreemptiveGC indicates that space was not yet full, asking for a preemptive GC
  	// return True if collection is/was required/done
  	Bool 						(*doGC)( struct MM_Plan*, Bool isPreemptiveGC, Word gcInfo ) ;

%%[[94
  	// get the queue of to be finalized weak ptrs
  	MM_DEQue* 					(*getWeakPtrFinalizeQue)( struct MM_Plan* ) ;
%%]]

  	// dumping info
  	void 						(*dump)( struct MM_Plan* ) ;
} MM_Plan ;
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  interface object, of the global one and only plan
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
extern MM_Plan mm_plan ;
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Initialization, Finalization
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
extern void mm_init_plan() ;
extern void mm_exit_plan() ;
%%]

