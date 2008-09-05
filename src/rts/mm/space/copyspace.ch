%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Memory management: Space: CopySpace
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

A Space manager for CopySpace used by semispace and generational GC.
It is build on top of another Space. The difference lies in:
- fixed fragment size MM_GC_CopySpace_FragmentSize
- registers itself and its Frames

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Space CopySpace interface object
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
extern MM_Space mm_space_CopySpace ;
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% CopySpace page management defs & types
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
// the administration
typedef struct MM_Space_CopySpace_Data {
	MM_Space*				onTopOfSpace ;
	MM_Malloc*				memMgt ;
} MM_Space_CopySpace_Data ;
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% CopySpace page management interface
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
%%]
extern void	mm_space_CopySpace_Init( MM_Space*, MM_Malloc* memmgt, MM_Pages* pages ) ;
extern MM_Space_FragmentInx	mm_space_CopySpace_GrowSpaceLog2( MM_Space*, MM_Pages_LogSize szFragLog ) ;
extern MM_Space_FragmentInx	mm_space_CopySpace_GrowSpace( MM_Space*, Word szFrag ) ;
extern MM_Space_FragmentInx	mm_space_CopySpace_GrowSpaceByDefault( MM_Space* ) ;
extern void mm_space_CopySpace_DeallocFragment( MM_Space*, MM_Space_FragmentInx fragmentInx ) ;
extern void mm_space_CopySpace_DeallocSpace( MM_Space* ) ;
extern MM_Space_FragmentInx mm_space_CopySpace_GetNrFragments( MM_Space* ) ;
extern MM_Space_Fragment* mm_space_CopySpace_GetFragment( MM_Space*, MM_Space_FragmentInx fragmentInx ) ;
extern MM_Pages* mm_space_CopySpace_GetPages( MM_Space* ) ;


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% CopySpace Space test
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#ifdef TRACE
extern void mm_space_CopySpace_Test() ;
#endif
%%]

