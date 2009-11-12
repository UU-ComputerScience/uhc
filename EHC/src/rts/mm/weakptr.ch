%%[8
#ifndef __MM_WEAKPTR_H__
#define __MM_WEAKPTR_H__
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Memory management: WeakPtr
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Doc

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% WeakPtr object itself
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[94
typedef void (*MM_WeakPtr_Finalizer)( Word, Word ) ;

// the alive result of finding new alive weakptrs
typedef struct MM_WeakPtr_NewAlive {
	MM_FreeListArray*		alive ;				// the list of still alive objects
	MM_FlexArray_Inx		firstAliveInx ;		// the range of new alive objects
	MM_FlexArray_Inx		aftLastAliveInx ;
} MM_WeakPtr_NewAlive ;

typedef struct MM_WeakPtr_Object {
	Word					key ;
	Word					val ;
	MM_WeakPtr_Finalizer	finalizer ;	// per object finalization which gets passed the key & value
} __attribute__ ((__packed__)) MM_WeakPtr_Object ;
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
  	void			 			(*init)( struct MM_WeakPtr*, MM_Mutator* mutator, MM_Collector* collector ) ;
  	
  	// add, finalize(&remove)
  	Word			 			(*newWeakPtr)( struct MM_WeakPtr*, Word key, Word val, MM_WeakPtr_Finalizer finalizer ) ;
  	void			 			(*finalizeWeakPtr)( struct MM_WeakPtr*, Word ptr ) ;
  	
  	// finding live pointers, return in newAlive, move those to a new list (internally)
  	void			 			(*findLiveObjects)( struct MM_WeakPtr*, MM_WeakPtr_NewAlive* newAlive ) ;
  	// begin & end finding, as findLiveObjects is called iteratively
  	void			 			(*startFindLiveObjects)( struct MM_WeakPtr* ) ;
  	// return set of objects to be finalized
  	MM_FreeListArray*			(*endFindLiveObjects)( struct MM_WeakPtr* ) ;
  	
  	// trace weakptr
  	Word						(*traceWeakPtr)( struct MM_WeakPtr*, MM_Trace*, Word obj ) ;
  	
  	// collection
} MM_WeakPtr ;
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Default WeakPtr
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[94
extern MM_WeakPtr mm_weakPtr ;
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Initialization
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[94
extern void mm_init_weakPtr() ;
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% EOF
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#endif /* __MM_WEAKPTR_H__ */
%%]
