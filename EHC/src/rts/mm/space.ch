%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Memory management: Space
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

A Space maintains discontiguous groups of contiguous pages (a fragment).
It can grow and shrink by adding new fragments, and later remove them. All
fragments are used by some memory mgt discipline, a Space provides the
abstraction of grouping fragments.

For each Frame used by a Space a mapping from Frame to Space is
provided, so we can have fast access to a Space given any address in a
Frame of a Space

Obligations of Space implementations which are participating in GC:
- (un)register itself and its Frames by mm_Spaces_RegisterSpace and mm_Spaces_RegisterSpaceFrame resp.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Space defs & types
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Space interface
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
typedef Ptr  		MM_Space_Data_Priv ;
typedef int32_t		MM_Space_FragmentInx ;
typedef MM_Page		MM_Fragment ;

// exposed admin for a fragment
typedef struct MM_Space_Fragment {
	MM_Fragment				frag ;
  	Word 					size ;
  	MM_Pages_LogSize		sizeLog ;			// == 0 means not necessarily a power of 2 size
} MM_Space_Fragment ;

// typedef struct MM_Space_Data_Shared {
// } MM_Space_Data_Shared ;

typedef struct MM_Space {
	// shared data (between implementations of Space)
  	// MM_Space_Data_Shared 	shared ;
  	
	// private data
  	MM_Space_Data_Priv 		data ;
  	
  	// setup
  	void		 			(*init)( struct MM_Space*, MM_Malloc* memmgt, MM_Pages* pages ) ;
  	void		 			(*initWithSpace)( struct MM_Space*, MM_Malloc* memmgt, struct MM_Space* onTopOfSpace ) ;
  	
  	// (de)allocation
  	MM_Space_FragmentInx	(*growSpaceLog2)( struct MM_Space*, MM_Pages_LogSize szSpaceLog ) ;		// size in log(nr of pages)
  	MM_Space_FragmentInx	(*growSpace)( struct MM_Space*, Word sz ) ;
  	MM_Space_FragmentInx	(*growSpaceByDefault)( struct MM_Space* ) ;
  	void 					(*deallocFragment)( struct MM_Space*, MM_Space_FragmentInx fragmentInx ) ;
  	void 					(*deallocSpace)( struct MM_Space* ) ;
  	
  	// access
  	MM_Space_FragmentInx	(*getNrFragments)( struct MM_Space* ) ;
  	MM_Space_Fragment*		(*getFragment)( struct MM_Space*, MM_Space_FragmentInx fragmentInx ) ;	// fragment for inx
  	Word					(*getFragmentSize)( struct MM_Space*, MM_Space_FragmentInx fragmentInx ) ;	// fragment size for inx
  	MM_Pages*				(*getPages)( struct MM_Space* ) ;	// underlying Pages implementation

	// info: log of default growth size, or 0 if no such default
  	Word					(*getGrowDefaultLog)( struct MM_Space* ) ;

#ifdef TRACE
  	// dumping info
  	void 					(*dump)( struct MM_Space* ) ;
  	// mark memory as fresh, i.e. unused so we can detect whether we illegally point to it
  	void 					(*markAsFresh)( struct MM_Space* ) ;
#endif
} MM_Space ;
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Space interface object
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Spaces interface: mapping from obj/frame to space
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

This is only to be used for spaces using Frames of size MM_GC_CopySpace_FragmentSize

%%[8
extern MM_RangeMap mm_Spaces_FrameToSpace ; 
%%]

%%[8
extern void mm_Spaces_RegisterSpace( MM_Space* space ) ;
extern void mm_Spaces_UnregisterSpace( MM_Space* space ) ;
extern void mm_Spaces_RegisterSpaceFrame( MM_Space* space, MM_Space_Fragment* frag ) ;
extern void mm_Spaces_UnregisterSpaceFrame( MM_Space* space, MM_Space_Fragment* frag ) ;
%%]

%%[8
// get the GC managing space for an address, NULL if not managed by a space
static inline MM_Space* mm_Spaces_GetSpaceForAddress( Word a ) {
	MM_RangeMap_Inx fragInx = a >> MM_GC_CopySpace_FragmentSize_Log ;
	// printf( "mm_Spaces_GetSpaceForAddress a=%x frag=%x space=%x\n", a, fragInx, *(mm_rangeMap_At( &mm_Spaces_FrameToSpace, fragInx )) ) ;
	if ( mm_rangeMap_InRange( &mm_Spaces_FrameToSpace, fragInx ) )
		return (MM_Space*)( *(mm_rangeMap_At( &mm_Spaces_FrameToSpace, fragInx )) ) ;
	else
		return NULL ;
}

// is address GC managed by space ?
static inline Bool mm_Spaces_AddressIsGCManagedBySpace( Word a, MM_Space* space ) {
	return mm_Spaces_GetSpaceForAddress( a ) == space ;
}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Dump
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#ifdef TRACE
extern void mm_Spaces_Dump( ) ;
#endif
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Initialization
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
extern void mm_init_space() ;
%%]


