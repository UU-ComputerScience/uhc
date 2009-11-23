%%[8
#ifndef __MM_WEAKPTR_H__
#define __MM_WEAKPTR_H__
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Memory management: WeakPtr
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[doesWhat doclatex
A weak pointer encapsulates and combines two features:

\begin{itemize}
\item Weak reference, a value is alive (for GC) only if its key is alive
\item Finalization
\end{itemize}

Internally, a MM_WeakPtr_Object embedded in a mutator node stores the
key & value, together with a finalizer. A key is always present, /= 0, a
value may be == 0 or not. If the value == 0, it is assumed that the
corresponding finalizer is a C function of type MM_WeakPtr_Finalizer, if
the value /= 0, it is assumed that the finalizer is a mutator object.
When finalized, it will be copied to alive GC space before a mutator
provided hook will run all finalizers of which the value /= 0.
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% WeakPtr object itself
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[94
typedef void (*MM_WeakPtr_Finalizer)( Word ) ;

// the alive result of finding new alive weakptrs
typedef struct MM_WeakPtr_NewAlive {
	MM_FreeListArray*		alive ;				// the list of still alive objects
	MM_FlexArray_Inx		firstAliveInx ;		// the range of new alive objects
	MM_FlexArray_Inx		aftLastAliveInx ;
} MM_WeakPtr_NewAlive ;

typedef struct MM_WeakPtr_Object {
	Word					key ;
	Word					val ;
	Word					finalizer ;	// either a MM_WeakPtr_Finalizer: per object finalization which gets passed the key & value
	    								// or a Word					: given to mutator for further use
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
