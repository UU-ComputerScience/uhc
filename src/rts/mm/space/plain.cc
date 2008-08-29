%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Memory management: Space: Plain
%%% see associated .ch file for more info.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#include "../../rts.h"
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Plain internal functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Plain page management interface
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
void mm_space_Plain_Init( MM_Space* plainSpace, MM_Malloc* memmgt, MM_Pages* pages ) {
	MM_Space_Plain_Data* spc = (MM_Space_Plain_Data*)memmgt->malloc( sizeof( MM_Space_Plain_Data ) ) ;
	
	mm_flexArray_New( memmgt, &spc->fragments, sizeof(MM_Space_Fragment), 5 ) ;
	spc->pages = pages ;
	spc->memMgt = memmgt ;
	
	plainSpace->data = (MM_Space_Data_Priv*)spc ;
}

MM_Space_FragmentInx mm_space_Plain_GrowSpaceLog2( MM_Space* plainSpace, MM_Pages_LogSize szFragLog ) {
	MM_Space_Plain_Data* spc = (MM_Space_Plain_Data*)plainSpace->data ;

	MM_Space_FragmentInx frgInx = mm_flexArray_NewSlot( &spc->fragments ) ;
	MM_Space_Fragment* frg = (MM_Space_Fragment*)mm_flexArray_At( &spc->fragments, frgInx ) ;
	frg->frag = spc->pages->allocPagesLog2( spc->pages, szFragLog ) ;
	frg->sizeLog = szFragLog ;
	frg->size = 1 << szFragLog ;

	return frgInx ;
}

MM_Space_FragmentInx mm_space_Plain_GrowSpace( MM_Space* plainSpace, Word szFrag ) {
	MM_Space_Plain_Data* spc = (MM_Space_Plain_Data*)plainSpace->data ;

	MM_Space_FragmentInx frgInx = mm_flexArray_NewSlot( &spc->fragments ) ;
	MM_Space_Fragment* frg = (MM_Space_Fragment*)mm_flexArray_At( &spc->fragments, frgInx ) ;
	frg->frag = spc->pages->allocPages( spc->pages, szFrag ) ;
	frg->sizeLog = 0 ;
	frg->size = szFrag ;

	return frgInx ;
}

void mm_space_Plain_DeallocFragment( MM_Space* plainSpace, MM_Space_FragmentInx fragmentInx ) {
	MM_Space_Plain_Data* spc = (MM_Space_Plain_Data*)plainSpace->data ;
	MM_Space_Fragment* frg = (MM_Space_Fragment*)mm_flexArray_At( &spc->fragments, fragmentInx ) ;
	
	if ( frg->size > 0 ) {
		spc->pages->deallocPages( spc->pages, frg->frag ) ;
		frg->frag = NULL ;
		frg->size = frg->sizeLog = 0 ;
	}
}

void mm_space_Plain_DeallocSpace( MM_Space* plainSpace ) {
	MM_Space_Plain_Data* spc = (MM_Space_Plain_Data*)plainSpace->data ;
	int i ;
	
	for ( i = 0 ; i < mm_flexArray_SizeUsed(&spc->fragments) ; i++ ) {
		mm_space_Plain_DeallocFragment( plainSpace, i ) ;
	}
	mm_flexArray_Free( &spc->fragments ) ;
	spc->memMgt->free( spc ) ;
}

MM_Space_FragmentInx mm_space_Plain_GetNrFragments( MM_Space* plainSpace ) {
	MM_Space_Plain_Data* spc = (MM_Space_Plain_Data*)plainSpace->data ;
	return mm_flexArray_SizeUsed(&spc->fragments) ;
}

MM_Space_Fragment* mm_space_Plain_GetFragment( MM_Space* plainSpace, MM_Space_FragmentInx fragmentInx ) {
	MM_Space_Plain_Data* spc = (MM_Space_Plain_Data*)plainSpace->data ;
	MM_Space_Fragment* frg = (MM_Space_Fragment*)mm_flexArray_At( &spc->fragments, fragmentInx ) ;
	return frg ;
}

MM_Pages* mm_space_Plain_GetPages( MM_Space* plainSpace ) {
	MM_Space_Plain_Data* spc = (MM_Space_Plain_Data*)plainSpace->data ;
	return spc->pages ;
}

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Space Plain interface object
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
MM_Space mm_space_Plain =
	{ NULL
	, &mm_space_Plain_Init
	, &mm_space_Plain_GrowSpaceLog2
	, &mm_space_Plain_GrowSpace
	, &mm_space_Plain_DeallocFragment
	, &mm_space_Plain_DeallocSpace
	, &mm_space_Plain_GetNrFragments
	, &mm_space_Plain_GetFragment
	, &mm_space_Plain_GetPages
	} ;
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Plain Space dump
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#ifdef TRACE
mm_space_Plain_Dump( MM_Space* plainSpace ) {
}
#endif
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Plain Space test
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#ifdef TRACE
#define II 1000

void mm_space_Plain_Test() {
}
#endif
%%]

