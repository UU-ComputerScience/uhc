%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Memory management: Mutual recursive structs with a Mutator somewhere in
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[doesWhat doclatex
This includefile only contains the mutual recursive part of some data structures.
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Mutator interface
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
typedef Ptr  MM_Mutator_Data_Priv ;

typedef struct MM_Mutator {
	// private data of Mutator
  	MM_Mutator_Data_Priv 		data ;
  	
	// private, but inlined for efficiency + always required
  	MM_Allocator* 				allocator ;
  	MM_Allocator* 				residentAllocator ;
  	MM_Trace* 					trace ;
  	MM_Module* 					module ;
  	MM_Malloc*					malloc ;
%%[[94
	struct MM_WeakPtr*			weakPtrAdm ;
  	MM_DEQue* 					weakPtrFinalizeQue ;	// queue of weakptrs to be finalized
%%]]
  	
  	// setup with a particular MM_Pages
  	void			 			(*init)	( struct MM_Mutator*
  										, MM_Malloc* memmgt
  										, MM_Allocator* allocator
  										, MM_Allocator* resAllocator
  										, MM_Trace* trace
  										, MM_Module* module
%%[[94
										, struct MM_WeakPtr* weakPtrAdm
										, MM_DEQue* weakPtrFinalizeQue
%%]]
  										) ;
  	
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
%%% WeakPtr internal for interface
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[94
// the alive result of finding new alive weakptrs
typedef struct MM_WeakPtr_NewAlive {
	MM_FreeListArray*		alive ;				// the list of still alive objects
	MM_FlexArray_Inx		firstAliveInx ;		// the range of new alive objects
	MM_FlexArray_Inx		aftLastAliveInx ;
} MM_WeakPtr_NewAlive ;
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% WeakPtr interface
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[94
typedef Ptr  MM_WeakPtr_Data_Priv ;

typedef struct MM_WeakPtr {
	// private data of WeakPtr
  	MM_WeakPtr_Data_Priv 		data ;
  	
  	// setup with a particular MM_Mutator
  	void			 			(*init)( struct MM_WeakPtr*, struct MM_Mutator* mutator, MM_Collector* collector ) ;
  	
  	// add
  	Word			 			(*newWeakPtr)( struct MM_WeakPtr*, Word key, Word val, Word finalizer ) ;
  	// deref, == 0 means it is already finalized or otherwise considered dead
  	Word			 			(*derefWeakPtr)( struct MM_WeakPtr*, Word wp ) ;
  	// finalize, if return /= 0 it is a finalizer (usually a IO ()): finalization should then be done by the mutator
  	Word			 			(*finalizeWeakPtr)( struct MM_WeakPtr*, Word wp ) ;
  	
  	// finding live pointers, return in newAlive, move those to a new list (internally)
  	void			 			(*findLiveObjects)( struct MM_WeakPtr*, MM_WeakPtr_NewAlive* newAlive ) ;
  	// begin & end finding, as findLiveObjects is called iteratively
  	void			 			(*startFindLiveObjects)( struct MM_WeakPtr* ) ;
  	// return set of objects to be finalized
  	MM_FreeListArray*			(*endFindLiveObjects)( struct MM_WeakPtr* ) ;
  	
  	// trace weakptr, dealing with it as part of weakptr admin
  	Word						(*traceWeakPtr)( struct MM_WeakPtr*, MM_Trace*, Word obj ) ;
  	// trace weakptr, when part of finalizer queue
  	Word						(*traceWeakPtrWhenFinalizing)( struct MM_WeakPtr*, MM_Trace*, Word obj ) ;
  	
  	// collection
} MM_WeakPtr ;
%%]

