%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Memory management: Space: Plain
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

A plain Space manager, just sitting on top of Pages.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Space Plain interface object
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
extern MM_Space mm_space_Plain ;
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Plain page management defs & types
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
// the administration
typedef struct MM_Space_Plain_Data {
	MM_Pages*				pages ;
	MM_Malloc*				memMgt ;
	MM_FlexArray			fragments ;			// array of MM_Space_Fragment, indexed by a MM_Space_FragmentInx
} MM_Space_Plain_Data ;
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Plain page management interface
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
extern void	mm_space_Plain_Init( MM_Space*, MM_Malloc* memmgt, MM_Pages* pages ) ;
extern MM_Space_FragmentInx	mm_space_Plain_GrowSpaceLog2( MM_Space*, MM_Pages_LogSize szFragLog ) ;
extern MM_Space_FragmentInx	mm_space_Plain_GrowSpace( MM_Space*, Word szFrag ) ;
extern void mm_space_Plain_DeallocFragment( MM_Space*, MM_Space_FragmentInx fragmentInx ) ;
extern void mm_space_Plain_DeallocSpace( MM_Space* ) ;
extern MM_Space_FragmentInx mm_space_Plain_GetNrFragments( MM_Space* ) ;
extern MM_Space_Fragment* mm_space_Plain_GetFragment( MM_Space*, MM_Space_FragmentInx fragmentInx ) ;
extern MM_Pages* mm_space_Plain_GetPages( MM_Space* ) ;

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Plain Space test
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#ifdef TRACE
extern void mm_space_Plain_Test() ;
#endif
%%]

