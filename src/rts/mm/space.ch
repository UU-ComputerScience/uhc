%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Memory management: Space
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

A Space maintains discontiguous groups of contiguous pages (a fragment).
It can grow and shrink by adding new fragments, and later remove them. All
fragments are used by some memory mgt discipline, a Space provides the
abstraction of grouping fragments.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Space defs & types
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Space interface
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
typedef Ptr  	MM_Space_Data_Priv ;
typedef Word  	MM_Space_FragmentInx ;
typedef MM_Page	MM_Fragment ;

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
  	
  	// (de)allocation
  	MM_Space_FragmentInx	(*growSpaceLog2)( struct MM_Space*, MM_Pages_LogSize szSpaceLog ) ;		// size in log(nr of pages)
  	MM_Space_FragmentInx	(*growSpace)( struct MM_Space*, Word sz ) ;
  	void 					(*deallocFragment)( struct MM_Space*, MM_Space_FragmentInx fragmentInx ) ;
  	void 					(*deallocSpace)( struct MM_Space* ) ;
  	
  	// access
  	MM_Space_FragmentInx	(*getNrFragments)( struct MM_Space* ) ;
  	MM_Space_Fragment*		(*getFragment)( struct MM_Space*, MM_Space_FragmentInx fragmentInx ) ;	// fragment for inx
  	MM_Pages*				(*getPages)( struct MM_Space* ) ;	// underlying Pages implementation
} MM_Space ;
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Space interface object
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Space test
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


