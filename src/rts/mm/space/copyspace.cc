%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Memory management: Space: CopySpace
%%% see associated .ch file for more info.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#include "../../rts.h"
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% CopySpace internal functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% CopySpace page management interface
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
void mm_space_CopySpace_InitWithSpace( MM_Space* copySpace, MM_Malloc* memmgt, MM_Space* onTopOfSpace ) {
	MM_Space_CopySpace_Data* spc = (MM_Space_CopySpace_Data*)memmgt->malloc( sizeof( MM_Space_CopySpace_Data ) ) ;
	
	spc->onTopOfSpace = onTopOfSpace ;
	spc->memMgt = memmgt ;
	
	copySpace->data = (MM_Space_Data_Priv*)spc ;
	mm_Spaces_RegisterSpace( copySpace ) ;
}

MM_Space_Fragment* mm_space_CopySpace_GetFragment( MM_Space* copySpace, MM_Space_FragmentInx fragmentInx ) {
	MM_Space_CopySpace_Data* spc = (MM_Space_CopySpace_Data*)copySpace->data ;
	// delegate
	return spc->onTopOfSpace->getFragment( spc->onTopOfSpace, fragmentInx ) ;
}

Word mm_space_CopySpace_GetFragmentSize( MM_Space* copySpace, MM_Space_FragmentInx fragmentInx ) {
	MM_Space_CopySpace_Data* spc = (MM_Space_CopySpace_Data*)copySpace->data ;
	// delegate
	return spc->onTopOfSpace->getFragmentSize( spc->onTopOfSpace, fragmentInx ) ;
}

MM_Space_FragmentInx mm_space_CopySpace_GrowSpaceByDefault( MM_Space* copySpace ) {
	MM_Space_CopySpace_Data* spc = (MM_Space_CopySpace_Data*)copySpace->data ;

	MM_Space_FragmentInx frgInx = spc->onTopOfSpace->growSpaceLog2( spc->onTopOfSpace, MM_GC_CopySpace_FragmentSize_Log ) ;
	MM_Space_Fragment* frg = spc->onTopOfSpace->getFragment( spc->onTopOfSpace, frgInx ) ;
	mm_Spaces_RegisterSpaceFrame( copySpace, frg ) ;

	return frgInx ;
}

void mm_space_CopySpace_DeallocFragment( MM_Space* copySpace, MM_Space_FragmentInx fragmentInx ) {
	MM_Space_CopySpace_Data* spc = (MM_Space_CopySpace_Data*)copySpace->data ;
	MM_Space_Fragment* frg = mm_space_CopySpace_GetFragment( copySpace, fragmentInx ) ;
	mm_Spaces_UnregisterSpaceFrame( copySpace, frg ) ;
	spc->onTopOfSpace->deallocFragment( spc->onTopOfSpace, fragmentInx ) ;
}

void mm_space_CopySpace_DeallocSpace( MM_Space* copySpace ) {
	MM_Space_CopySpace_Data* spc = (MM_Space_CopySpace_Data*)copySpace->data ;
	mm_Spaces_UnregisterSpace( copySpace ) ;
	spc->onTopOfSpace->deallocSpace( spc->onTopOfSpace ) ;
	spc->memMgt->free( spc ) ;
}

MM_Space_FragmentInx mm_space_CopySpace_GetNrFragments( MM_Space* copySpace ) {
	MM_Space_CopySpace_Data* spc = (MM_Space_CopySpace_Data*)copySpace->data ;
	// delegate
	return spc->onTopOfSpace->getNrFragments( spc->onTopOfSpace ) ;
}

MM_Pages* mm_space_CopySpace_GetPages( MM_Space* copySpace ) {
	MM_Space_CopySpace_Data* spc = (MM_Space_CopySpace_Data*)copySpace->data ;
	// delegate
	return spc->onTopOfSpace->getPages( spc->onTopOfSpace ) ;
}

Word mm_space_CopySpace_GetGrowDefaultLog( MM_Space* copySpace ) {
	return MM_GC_CopySpace_FragmentSize_Log ;
}

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Tracing: CopySpace Space dump
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#ifdef TRACE
void mm_space_CopySpace_Dump( MM_Space* copySpace ) {
	MM_Space_CopySpace_Data* spc = (MM_Space_CopySpace_Data*)copySpace->data ;

	printf( ">------------------------> MM_Space: CopySpace: space=%p spc=%p\n", copySpace, spc ) ;

	spc->onTopOfSpace->dump( spc->onTopOfSpace ) ;
	printf( "  Copy nrfrag=%x\n", copySpace->getNrFragments(copySpace) ) ;

	printf( "<------------------------< MM_Space: CopySpace\n" ) ;
}
#endif
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Tracing: marking fresh
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#ifdef TRACE
void mm_space_CopySpace_MarkAsFresh( MM_Space* copySpace ) {
	MM_Space_CopySpace_Data* spc = (MM_Space_CopySpace_Data*)copySpace->data ;
	
	spc->onTopOfSpace->markAsFresh( spc->onTopOfSpace ) ;
}
#endif
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Space CopySpace interface object
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
MM_Space mm_space_CopySpace =
	{ NULL
	, MM_Undefined
	, &mm_space_CopySpace_InitWithSpace
	, MM_Undefined
	, MM_Undefined
	, &mm_space_CopySpace_GrowSpaceByDefault
	, &mm_space_CopySpace_DeallocFragment
	, &mm_space_CopySpace_DeallocSpace
	, &mm_space_CopySpace_GetNrFragments
	, &mm_space_CopySpace_GetFragment
	, &mm_space_CopySpace_GetFragmentSize
	, &mm_space_CopySpace_GetPages
	, &mm_space_CopySpace_GetGrowDefaultLog
#ifdef TRACE
	, &mm_space_CopySpace_Dump
	, &mm_space_CopySpace_MarkAsFresh
#endif
	} ;
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% CopySpace Space test
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#ifdef TRACE
#define II 1000

void mm_space_CopySpace_Test() {
}
#endif
%%]

