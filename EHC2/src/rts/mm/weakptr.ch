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

typedef struct MM_WeakPtr_Object {
	Word					key ;
	Word					val ;
	Word					finalizer ;	// either a MM_WeakPtr_Finalizer: per object finalization which gets passed the key & value
	    								// or a Word					: given to mutator for further use
} __attribute__ ((__packed__)) MM_WeakPtr_Object ;
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% WeakPtr object + finalizer as put in the que for finalizers yet to be run
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

The elements of the que of to be finalizer wptrs separately keeps the
finalizer, because it has been set a tombstone value when the wptr was
tombstoned. However, it must be remembered still so it can be run and
GC'ed.

%%[94
typedef struct MM_WeakPtrFinalizeQue_Data {
	Word					wpObj ;
	Word					finalizer ;	
} __attribute__ ((__packed__)) MM_WeakPtrFinalizeQue_Data ;

#define MM_WeakPtrFinalizeQue_Data_SizeInWords	EntierLogUpShrBy(sizeof(MM_WeakPtrFinalizeQue_Data), Word_SizeInBytes_Log)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% WeakPtr interface
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[94
// see mutatormutrec
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
