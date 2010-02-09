%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Memory management: Space
%%% see associated .ch file for more info.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#include "../rts.h"
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Spaces interface: mapping from obj/frame to space
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
MM_RangeMap mm_Spaces_FrameToSpace ; 
%%]

%%[8
void mm_Spaces_RegisterSpace( MM_Space* space ) {
}

void mm_Spaces_UnregisterSpace( MM_Space* space ) {
}

void mm_Spaces_RegisterSpaceFrame( MM_Space* space, MM_Space_Fragment* frag ) {
	MM_RangeMap_Inx fragInx = (Word)(frag->frag) >> MM_GC_CopySpace_FragmentSize_Log ;
	// mm_Spaces_Dump() ;
	mm_rangeMap_Realloc( &mm_Spaces_FrameToSpace, NULL, fragInx, fragInx+1 ) ;
	// mm_Spaces_Dump() ;
	*(mm_rangeMap_At( &mm_Spaces_FrameToSpace, fragInx )) = (Word)space ;
	// IF_GB_TR_ON(3,{printf("mm_Spaces_RegisterSpaceFrame frag=%p, space=%p\n",frag->frag,space);}) ;
	// mm_Spaces_Dump() ;
}

void mm_Spaces_UnregisterSpaceFrame( MM_Space* space, MM_Space_Fragment* frag ) {
	MM_RangeMap_Inx fragInx = (Word)(frag->frag) >> MM_GC_CopySpace_FragmentSize_Log ;
	*(mm_rangeMap_At( &mm_Spaces_FrameToSpace, fragInx )) = 0 ;
}


%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Default Spaces
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Dump
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#ifdef TRACE
void mm_Spaces_Dump( ) {
	printf( ">------------------------> Spaces\n" ) ;

	mm_rangeMap_Dump( &mm_Spaces_FrameToSpace ) ;
	
	printf( "<------------------------< Spaces\n" ) ;
}
#endif
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Initialization
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
void mm_init_space() {
	mm_rangeMap_New( &mm_malloc_LOF, &mm_Spaces_FrameToSpace ) ;
	mm_space_Fragment.init( &mm_space_Fragment, mm_malloc_Sys, &mm_pages ) ;
}
%%]
