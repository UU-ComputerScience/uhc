%%[8
#ifndef __MM_ITERATOR_H__
#define __MM_ITERATOR_H__
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Memory management: iterator abstraction
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[doesWhat doclatex
An Iterator abstracts over iteration by providing a structure describing the iteration.
Stepping may only be done when the iterator has data (hasData).
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Iterator
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
typedef struct MM_Iterator {
	// visible
	BPtr	 	data ; 		// the data associated with this iteration step
	Bool		hasData ;	// specific state: not yet done
	Word		state ;		// iterator interpretable state, if the structure is index based it is guaranteed to hold the current index
	Word		state2 ;	// extra/2nd state, optionally used
	Word		state3 ;	// extra/3rd state, optionally used
	Word		state4 ;	// extra/4th state, optionally used
	Word		state5 ;	// extra/5th state, optionally used

	// do one step, as specified by the structure the iterator iterates over, return hasData
	Bool		(*step)( struct MM_Iterator* ) ;
	
	// invisible
	BPtr		iteratee ;	// that over which is iterated
} MM_Iterator ;

%%]


%%[8
%%]
static inline Bool mm_Iterator_Step() {
}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Iterator dump
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#ifdef TRACE
// extern void mm_flexArray_Dump( MM_Iterator* a ) ;
#endif
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% EOF
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#endif /* __MM_ITERATOR_H__ */
%%]
