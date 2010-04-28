%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Memory management: Double Ended Queue
%%% see associated .ch file for more info.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#include "../../rts.h"
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% DEQue
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
Bool mm_deque_IsEmpty( MM_DEQue* deque ) {
	return
		(	mm_deque_HeadTailShareBuffer( deque )	// not shared => not empty
		&&	deque->headOff > deque->tailOff
		) ;
}

void mm_deque_InitWithSize( MM_DEQue* deque, MM_Malloc* memmgt, Word nrWordsElt ) {
	MM_DEQue_PageHeader* buf = memmgt->malloc( MM_DEQue_BufSize ) ;
	mm_dll_Init( &deque->dll ) ;
	mm_dll_InsertPrev( &buf->dll, &deque->dll ) ;
	mm_deque_SetHeadOffInit( deque ) ;
	mm_deque_SetTailOff( deque, deque->headOff - Word_SizeInBytes ) ;
	deque->memMgt = memmgt ;
	deque->nrWordsElt = nrWordsElt ;
}

void mm_deque_Reset( MM_DEQue* deque ) {
	MM_DLL* dll = deque->dll.next ;
	for ( ; dll != &deque->dll ; ) {
		MM_DLL* next = dll->next ;
		deque->memMgt->free( dll ) ;
		dll = next ;
	}
	mm_deque_Init( deque, deque->memMgt ) ;
}

void mm_deque_HeadExtend( MM_DEQue* deque ) {
	MM_DEQue_PageHeader* buf = deque->memMgt->malloc( MM_DEQue_BufSize ) ;
	mm_dll_InsertNext( &buf->dll, &deque->dll ) ;
	mm_deque_SetHeadOff( deque, MM_DEQue_BufSize ) ;
}

void mm_deque_TailExtend( MM_DEQue* deque ) {
	MM_DEQue_PageHeader* buf = deque->memMgt->malloc( MM_DEQue_BufSize ) ;
	mm_dll_InsertPrev( &buf->dll, &deque->dll ) ;
	mm_deque_SetTailOff( deque, sizeof(MM_DEQue_PageHeader) - Word_SizeInBytes ) ;
}

void mm_deque_HeadShrink( MM_DEQue* deque ) {
	if ( ! mm_deque_HeadTailShareBuffer( deque ) ) {
		MM_DLL* dll = deque->dll.next ;
		mm_dll_Delete( deque->dll.next ) ;
		deque->memMgt->free( dll ) ;
		mm_deque_SetHeadOffInit( deque ) ;
	}
}

void mm_deque_TailShrink( MM_DEQue* deque ) {
	if ( ! mm_deque_HeadTailShareBuffer( deque ) ) {
		MM_DLL* dll = deque->dll.prev ;
		mm_dll_Delete( deque->dll.prev ) ;
		deque->memMgt->free( dll ) ;
		mm_deque_SetTailOff( deque, sizeof(MM_DEQue_PageHeader) - Word_SizeInBytes ) ;
	}
}

%%]

%%[8
void mm_deque_TailEnsure( MM_DEQue* deque, Word nrWords ) {
	if ( nrWords > mm_deque_TailAvailWrite( deque ) ) {
		mm_deque_TailExtend( deque ) ;
	}
}
%%]

%%[8
void mm_deque_TailPush( MM_DEQue* deque, Word* words, Word nrWords ) {
	Word i ;
	for ( ; nrWords > 0 ; ) {
		Word avail = mm_deque_TailAvailWrite( deque ) ;
		// IF_GB_TR_ON(3,{printf("mm_deque_TailPush nrWords=%x avail=%x\n", nrWords, avail);}) ;
		Word* tail = mm_deque_Tail( deque ) ;
		// guarantee writing multiples of deque->nrWordsElt only
		Word nrWrite = EntierDownBy( minWord( nrWords, avail ), deque->nrWordsElt ) ;
		for ( i = 0 ; i < nrWrite ; i++ ) {
			*(++tail) = *(words++) ;
		}
		nrWords -= nrWrite ;
		mm_deque_IncTailOff( deque, nrWrite ) ;
		if ( nrWords > 0 ) {
			mm_deque_TailExtend( deque ) ;
		}
	}
}

Word mm_deque_HeadPop( MM_DEQue* deque, Word* words, Word nrWords ) {
	Word i ;
	Word nrReadAccum = 0 ;
	Word avail = mm_deque_HeadAvailRead( deque ) ;
	for ( ; nrWords > 0 ; ) {
		Word* head = mm_deque_Head( deque ) ;
		Word nrRead = minWord( nrWords, avail ) ;
		for ( i = 0 ; i < nrRead ; i++ ) {
			*(words++) = *(head++) ;
		}
		nrWords -= nrRead ;
		nrReadAccum += nrRead ;
		mm_deque_IncHeadOff( deque, nrRead ) ;
		if ( avail - nrRead == 0 ) {
			mm_deque_HeadShrink( deque ) ;
			avail = mm_deque_HeadAvailRead( deque ) ;
			if ( avail == 0 )
				break ;
		}
	}
	return nrReadAccum ;
}

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Iteration
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Iteration starts at the head and stops at the tail valid when the Iterator is created.
This allows for writing at the tail without disturbing the Iterator.

In the Iterator the state fields are used as follows:
state : inx into a buffer
state2: current buffer
state3: tail inx of tail buffer
state4: tail buffer
state5: sz of elt in bytes

%%[8
// assume i->hasData
Bool mm_deque_IteratorStep( MM_Iterator* i ) {
	Word off = i->state + i->state5 ;
	Bool isAfterEnd ;
	do {
		if (i->state2 == i->state4) {
			// in the same & last buffer
			isAfterEnd = off > i->state3 ; // beyond tail?
			break ;
		} else {
			MM_DLL* dll = (MM_DLL*)i->state2 ;
			isAfterEnd = off > ((MM_DEQue_PageHeader*)dll)->tailOff ; // beyond tail?
			if ( isAfterEnd ) {
				i->state2 = (Word)dll->next ;
				off = MM_DEQue_HeadOffInit ;
			} else {
				break ;
			}
		}
	} while ( True ) ;
	
	if ( isAfterEnd ) {
		i->hasData = False ;
	} else {
		i->data = &((BPtr)(i->state2))[ off ] ;
		i->state = off ;
	}
	return i->hasData ;
}

void mm_deque_Iterator( MM_DEQue* deque, MM_Iterator* i ) {
	if ( i->hasData = (mm_deque_HeadAvailRead(deque) > 0) ) {
		Word atOff = deque->headOff ;
		i->state  = atOff ;
		i->state2 = (Word)deque->dll.next ;
		i->state3 = deque->tailOff ;
		i->state4 = (Word)deque->dll.prev ;
		i->state5 = deque->nrWordsElt << Word_SizeInBytes_Log ; // cached size of increment (could be removed)
		i->data = &((BPtr)(i->state2))[ atOff ] ;
		i->step = &mm_deque_IteratorStep ;
		i->iteratee = (BPtr)deque ;
	}
}

%%]
// assume i->hasData
static void inline mm_deque_IteratorNextOff( MM_DLL** dll, Word* off, Word nrWords ) {
	MM_DEQue_PageHeader* pg = (MM_DEQue_PageHeader*)*dll ;
	for ( ; *off > pg->tailOff ; ) {
	}
}


Example for construction of the above:

// assume i->hasData
Bool mm_flexArray_IteratorStep( MM_Iterator* i ) {
	MM_FlexArray* a = (MM_FlexArray*)i->iteratee ;
	MM_FlexArray_Inx inx = i->state + 1 ;
	if ( inx < mm_flexArray_SizeUsed(a) ) {
		i->state = inx ;
		i->data = mm_flexArray_At( a, inx ) ;
	} else {
		i->hasData = False ;
	}
	return i->hasData ;
}

void mm_flexArray_IteratorAt( MM_FlexArray* a, MM_Iterator* i, MM_FlexArray_Inx atInx ) {
	if ( i->hasData = (mm_flexArray_SizeUsed(a) > atInx) ) {
		i->state = atInx ;
		i->data = mm_flexArray_At( a, atInx ) ;
		i->step = &mm_flexArray_IteratorStep ;
		i->iteratee = (BPtr)a ;
	}
}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% DEQue dump
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#ifdef TRACE
void mm_deque_Dump( MM_DEQue* deque ) {
	Word i ;
	
	printf( ">------------------------>\n" ) ;
	printf
		( "DEQue: headOff=%x tailOff=%x dll.next=%p dll.prev=%p\n"
		, deque->headOff, deque->tailOff, deque->dll.next, deque->dll.prev
		) ;
	MM_DLL* dll = deque->dll.next ;
	for ( ; dll != &deque->dll ; dll = dll->next ) {
		MM_DEQue_PageHeader* pg = (MM_DEQue_PageHeader*)dll ;
		printf
			( "  Pg: pg=%p tailOff=%x\n"
			, pg, pg->tailOff
			) ;
	}
	
	printf( "<------------------------<\n" ) ;
}
#endif
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% DEQue test
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#ifdef TRACE
void mm_deque_Test() {
	MM_DEQue q ;
	mm_deque_Init( &q, &mm_malloc_LOF ) ;
	mm_deque_Dump( &q ) ;
	Word i = 3 ;
	Word j ;
	mm_deque_TailPush( &q, &i, 1 ) ;
	mm_deque_Dump( &q ) ;
	mm_deque_HeadPop( &q, &j, 1 ) ;
	IF_GB_TR_ON(3,{printf("mm_deque_Test j=%x\n", j);}) ;
	mm_deque_Dump( &q ) ;
	for ( i = 0 ; i < 2000 ; i++ ) {
		mm_deque_TailEnsure( &q, i % 800 ) ;
		mm_deque_TailPush( &q, &i, 1 ) ;
	}
	mm_deque_Dump( &q ) ;
	for ( i = 0 ; i < 2000 ; i++ ) {
		mm_deque_HeadPop( &q, &j, 1 ) ;
	}
	mm_deque_Dump( &q ) ;
}
#endif
%%]

