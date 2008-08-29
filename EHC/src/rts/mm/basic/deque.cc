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

void mm_deque_Init( MM_DEQue* deque, MM_Malloc* memmgt ) {
	MM_DEQue_PageHeader* buf = memmgt->malloc( MM_DEQue_BufSize ) ;
	mm_dll_Init( &deque->dll ) ;
	mm_dll_InsertPrev( &buf->dll, &deque->dll ) ;
	mm_deque_SetHeadOff( deque, sizeof(MM_DEQue_PageHeader) ) ;
	mm_deque_SetTailOff( deque, sizeof(MM_DEQue_PageHeader) - Word_SizeInBytes ) ;
	deque->memMgt = memmgt ;
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
		mm_deque_SetHeadOff( deque, sizeof(MM_DEQue_PageHeader) ) ;
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
		Word nrWrite = minWord( nrWords, avail ) ;
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
	for ( ; nrWords > 0 ; ) {
		Word avail = mm_deque_HeadAvailRead( deque ) ;
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
		}
	}
	return nrReadAccum ;
}

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% DEQue dump
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#ifdef TRACE
mm_deque_Dump( MM_DEQue* deque ) {
	Word i ;
	
	printf( "--------------------------\n" ) ;
	printf
		( "DEQue: headOff=%x tailOff=%x dll.next=%x dll.prev=%x\n"
		, deque->headOff, deque->tailOff, deque->dll.next, deque->dll.prev
		) ;
	MM_DLL* dll = deque->dll.next ;
	for ( ; dll != &deque->dll ; dll = dll->next ) {
		MM_DEQue_PageHeader* pg = (MM_DEQue_PageHeader*)dll ;
		printf
			( "  Pg: pg=%x tailOff=%x\n"
			, pg, pg->tailOff
			) ;
	}
	
	printf( "--------------------------\n" ) ;
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

