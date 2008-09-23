%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Memory management: page management
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Page management provides the 1st abstraction on top of malloc.

A page
- has size
  - 2^n * 2^k, n >= 0, k determining the smallest size, allocated with allocLog2
  - multiple of 2^k, k determining the smallest size, allocated with alloc.
    its size lies in between 2^(n-1) * 2^k .. 2^n * 2^k, with start aligned on 2^n * 2^k
- has its start address aligned on a 2^k boundary

%%[8
#define MM_Pages_MinSize_Log	MM_Page_Size_Log				// k, from above, depends on word size (?good thing or not?)
#define MM_Pages_MinSize		MM_Page_Size

// maximum allocatable size
#if USE_64_BITS
#define MM_Pages_MaxSize_Log	32								// 4GB
#else
#define MM_Pages_MaxSize_Log	28								// 128MB
#endif
#define MM_Pages_MaxSize		(1 << MM_Pages_MaxSize_Log)
%%]

Currently there is 1 implementation of page management, using the buddy system.
It is incorporated here.
Other implementations are allowed because of the provided pages abstraction (MM_Pages).

For more (global) info see mm.h

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Pages interface
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
typedef Ptr  MM_Pages_Data_Priv ;
typedef BPtr MM_Page ;

typedef HalfWord MM_Pages_LogSize ;

typedef struct MM_Pages {
	// private data
  	MM_Pages_Data_Priv 		data ;
  	
  	// setup
  	void		 			(*init)( struct MM_Pages*, MM_Malloc* memmgt ) ;
  	
  	// (de)allocation
  	MM_Page 				(*allocPagesLog2)( struct MM_Pages*, MM_Pages_LogSize szPagesLog ) ;		// size in log(nr of pages)
  	MM_Page 				(*allocPages)( struct MM_Pages*, Word sz ) ;
  	void 					(*deallocPages)( struct MM_Pages*, MM_Page pg ) ;
  	
  	// user data
  	Word*					(*getUserData)( struct MM_Pages*, MM_Page pg ) ;
  	void 					(*setUserData)( struct MM_Pages*, MM_Page pg, Word sz, Word info ) ;
} MM_Pages ;
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Pages default
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
extern MM_Pages mm_pages ;
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Initialization
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
extern void mm_init_pages() ;
%%]
