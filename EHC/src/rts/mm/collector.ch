%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Memory management: Collector
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

A Collector is a 'friend' of a Plan, implementing a collection
algorithm. It knows which plan is used, it is only used within the
context of that plan, of which it will use internally available info.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Collector interface
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
typedef Ptr  MM_Collector_Data_Priv ;

typedef struct MM_Collector {
	// private data of Collector
  	MM_Collector_Data_Priv 		data ;
  	
  	// collector is always part of a plan
  	Ptr /* MM_Plan* */ 			plan ;

  	// private data still, but available for fast access ;
  	// the current collected space
  	MM_Space*					collectedSpace ;
  	
  	// setup with a particular MM_Pages
  	void			 			(*init)( struct MM_Collector*, MM_Malloc* memmgt, Ptr /* MM_Plan* */ plan ) ;
  	
  	// called before breaking allocation invariants as required to do collection
  	void						(*collectPre)( struct MM_Collector* ) ;
  	// collect
  	void						(*collect)( struct MM_Collector*, Word gcInfo ) ;
  	// called after allocation invariants are reinstalled
  	void						(*collectPost)( struct MM_Collector* ) ;
  	
  	// tracing live pointers
  	
  	// queries
  	Bool						(*isInCollectedSpace)( struct MM_Collector*, Word obj ) ;
} MM_Collector ;
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Default Collector
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
extern MM_Collector mm_collector ;
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Initialization
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
extern void mm_init_collector() ;
%%]
