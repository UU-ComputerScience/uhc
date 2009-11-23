%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Memory management: Space: Fragment
%%% see associated .ch file for more info.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#include "../../rts.h"
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Fragment internal functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Fragment page management interface
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
void mm_space_Fragment_Init( MM_Space* fragmentSpace, MM_Malloc* memmgt, MM_Pages* pages ) {
	MM_Space_Fragment_Data* spc = (MM_Space_Fragment_Data*)memmgt->malloc( sizeof( MM_Space_Fragment_Data ) ) ;
	
	mm_flexArray_New( memmgt, &spc->fragments, sizeof(MM_Space_Fragment), 5, 0 ) ;
	spc->pages = pages ;
	spc->memMgt = memmgt ;
	
	fragmentSpace->data = (MM_Space_Data_Priv*)spc ;
	// mm_Spaces_RegisterSpace( fragmentSpace ) ;
}

MM_Space_FragmentInx mm_space_Fragment_GrowSpaceLog2( MM_Space* fragmentSpace, MM_Pages_LogSize szFragLog ) {
	MM_Space_Fragment_Data* spc = (MM_Space_Fragment_Data*)fragmentSpace->data ;

	// IF_GB_TR_ON(3,{printf("mm_space_Fragment_GrowSpaceLog2 szFragLog=%x\n", szFragLog);}) ;
	MM_Space_FragmentInx frgInx = mm_flexArray_AllocSlot( &spc->fragments ) ;
	MM_Space_Fragment* frg = (MM_Space_Fragment*)mm_flexArray_At( &spc->fragments, frgInx ) ;
	frg->frag = spc->pages->allocPagesLog2( spc->pages, szFragLog ) ;
	frg->sizeLog = szFragLog ;
	frg->size = 1 << szFragLog ;

	// mm_Spaces_RegisterSpaceFrame( fragmentSpace, frg ) ;
	return frgInx ;
}

MM_Space_FragmentInx mm_space_Fragment_GrowSpace( MM_Space* fragmentSpace, Word szFrag ) {
	MM_Space_Fragment_Data* spc = (MM_Space_Fragment_Data*)fragmentSpace->data ;

	MM_Space_FragmentInx frgInx = mm_flexArray_AllocSlot( &spc->fragments ) ;
	MM_Space_Fragment* frg = (MM_Space_Fragment*)mm_flexArray_At( &spc->fragments, frgInx ) ;
	frg->frag = spc->pages->allocPages( spc->pages, szFrag ) ;
	frg->sizeLog = 0 ;
	frg->size = szFrag ;

	return frgInx ;
}

void mm_space_Fragment_DeallocFragment( MM_Space* fragmentSpace, MM_Space_FragmentInx fragmentInx ) {
	MM_Space_Fragment_Data* spc = (MM_Space_Fragment_Data*)fragmentSpace->data ;
	MM_Space_Fragment* frg = (MM_Space_Fragment*)mm_flexArray_At( &spc->fragments, fragmentInx ) ;
	
	if ( frg->size > 0 ) {
		spc->pages->deallocPages( spc->pages, frg->frag ) ;
		frg->frag = NULL ;
		frg->size = frg->sizeLog = 0 ;
	}
}

void mm_space_Fragment_DeallocSpace( MM_Space* fragmentSpace ) {
	MM_Space_Fragment_Data* spc = (MM_Space_Fragment_Data*)fragmentSpace->data ;
	int i ;
	
	for ( i = 0 ; i < mm_flexArray_SizeUsed(&spc->fragments) ; i++ ) {
		mm_space_Fragment_DeallocFragment( fragmentSpace, i ) ;
	}
	mm_flexArray_Free( &spc->fragments ) ;
	spc->memMgt->free( spc ) ;
}

MM_Space_FragmentInx mm_space_Fragment_GetNrFragments( MM_Space* fragmentSpace ) {
	MM_Space_Fragment_Data* spc = (MM_Space_Fragment_Data*)fragmentSpace->data ;
	return mm_flexArray_SizeUsed(&spc->fragments) ;
}

MM_Space_Fragment* mm_space_Fragment_GetFragment( MM_Space* fragmentSpace, MM_Space_FragmentInx fragmentInx ) {
	MM_Space_Fragment_Data* spc = (MM_Space_Fragment_Data*)fragmentSpace->data ;
	MM_Space_Fragment* frg = (MM_Space_Fragment*)mm_flexArray_At( &spc->fragments, fragmentInx ) ;
	return frg ;
}

Word mm_space_Fragment_GetFragmentSize( MM_Space* fragmentSpace, MM_Space_FragmentInx fragmentInx ) {
	MM_Space_Fragment_Data* spc = (MM_Space_Fragment_Data*)fragmentSpace->data ;
	MM_Space_Fragment* frg = (MM_Space_Fragment*)mm_flexArray_At( &spc->fragments, fragmentInx ) ;
	return frg->size ;
}

MM_Pages* mm_space_Fragment_GetPages( MM_Space* fragmentSpace ) {
	MM_Space_Fragment_Data* spc = (MM_Space_Fragment_Data*)fragmentSpace->data ;
	return spc->pages ;
}

Word mm_space_Fragment_GetGrowDefaultLog( MM_Space* fragmentSpace ) {
	return MM_GC_CopySpace_FragmentSize_Log ;
}

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Tracing: Fragment Space dump
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#ifdef TRACE
void mm_space_Fragment_Dump( MM_Space* fragmentSpace ) {
	MM_Space_Fragment_Data* spc = (MM_Space_Fragment_Data*)fragmentSpace->data ;

	printf( ">------------------------> MM_Space: Fragment: space=%p spc=%p\n", fragmentSpace, spc ) ;

	int i ;
	for ( i = 0 ; i < mm_flexArray_SizeUsed(&spc->fragments) ; i++ ) {
		MM_Space_Fragment* frg = (MM_Space_Fragment*)mm_flexArray_At( &spc->fragments, i ) ;
		printf( "  Frg %x: frag=%p size=%x\n", i, frg->frag, frg->size ) ;
	}

	printf( "<------------------------< MM_Space: Fragment\n" ) ;

}
#endif
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Tracing: marking fresh
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#ifdef TRACE
void mm_space_Fragment_MarkAsFresh( MM_Space* fragmentSpace ) {
	MM_Space_Fragment_Data* spc = (MM_Space_Fragment_Data*)fragmentSpace->data ;
	int i ;
	
	for ( i = 0 ; i < mm_flexArray_SizeUsed(&spc->fragments) ; i++ ) {
		MM_Space_Fragment* frg = (MM_Space_Fragment*)mm_flexArray_At( &spc->fragments, i ) ;
		// IF_GB_TR_ON(3,{printf("mm_space_Fragment_MarkAsFresh frag=%x, sz(frag)=%x\n", frg->frag, frg->size);}) ;
		memset( frg->frag, MM_GC_FreshMem_Pattern_Byte, frg->size ) ;
	}
}
#endif
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Space Fragment interface object
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
MM_Space mm_space_Fragment =
	{ NULL
	, &mm_space_Fragment_Init
	, MM_Undefined
	, &mm_space_Fragment_GrowSpaceLog2
	, &mm_space_Fragment_GrowSpace
	, MM_Undefined
	, &mm_space_Fragment_DeallocFragment
	, &mm_space_Fragment_DeallocSpace
	, &mm_space_Fragment_GetNrFragments
	, &mm_space_Fragment_GetFragment
	, &mm_space_Fragment_GetFragmentSize
	, &mm_space_Fragment_GetPages
	, &mm_space_Fragment_GetGrowDefaultLog
#ifdef TRACE
	, &mm_space_Fragment_Dump
	, &mm_space_Fragment_MarkAsFresh
#endif
	} ;
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Fragment Space test
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#ifdef TRACE
#define II 1000

void mm_space_Fragment_Test() {
}
#endif
%%]

