%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Memory management: allocator: list of free (LOF)
%%% see associated .ch file for more info.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#include "../../rts.h"
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% LOF internal defs
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#define MM_Allocator_LOF_DirectAlloc_Mask		(1 << (sizeof(int) << Byte_SizeInBits_Log) - 1)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% LOF internal functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% LOF interface
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
void mm_allocator_LOF_Init( MM_Allocator* alcr, MM_Malloc* memmgt, MM_Space* space ) {
	Word alcSizeLog = maxWord( MM_Pages_MinSize_Log, firstHigherPower2( sizeof(MM_Allocator_LOF_Data) ) ) ;
	Word alcInx = space->growSpaceLog2( space, alcSizeLog ) ;
	MM_Allocator_LOF_Data* alc = (MM_Allocator_LOF_Data*)(space->getFragment( space, alcInx )->frag) ;
	
	alc->space = space ;
	
	Word i ;
	for ( i = 0 ; i < MM_Allocator_LOF_NrRoundedFit ; i++ ) {
		MM_Allocator_LOF_PerSize* perSize = &alc->perSizeRounded[ i ] ;
		perSize->free  = NULL ;
		perSize->pages = NULL ;
	}
	
	// further setup is done at the first allocation request
	alcr->data = (MM_Allocator_Data_Priv*)alc ;
}

Ptr mm_allocator_LOF_Alloc( MM_Allocator* alcr, Word sz ) {
	MM_Allocator_LOF_Data* alc = (MM_Allocator_LOF_Data*)alcr->data ;
	MM_Pages* pages = alc->space->getPages(alc->space) ;
	Ptr res = NULL ;
	Word i ;
	
	Word szWord = EntierLogUpShrBy( sz, Word_SizeInBytes_Log ) ;
	
	if ( szWord <= MM_Allocator_LOF_MaxRoundedSize_Words ) {
		// exact fit
		// determine index into perSize, allowing more unused space for greater size, using more pages for greater size.
		Word szBase = szWord + MM_Allocator_LOF_RoundGroupSize - 1 ;
		Word szLog  = firstNonZeroMsBit( szBase >> MM_Allocator_LOF_RoundGroupSize_Log, MM_Allocator_LOF_NrRoundGroup-1 ) ;
		Word szInx  = szLog * MM_Allocator_LOF_RoundGroupSize + Bits_ExtrTo(Word,szBase >> szLog,MM_Allocator_LOF_RoundGroupSize_Log-1) ;
		MM_Allocator_LOF_PerSize* perSize = &alc->perSizeRounded[ szInx ] ;
		// IF_GB_TR_ON(3,{printf("mm_allocator_LOF_Alloc sz=%x szWord=%x szBase=%x szLog=%x szInx=%x perSize=%x perSize->free=%x\n", sz, szWord, szBase, szLog, szInx, perSize, perSize->free );}) ;
		
		// ensure elements on free list
		if ( perSize->free == NULL ) {
			// nr of new free elements - 1, allow for some wasted space at the end + admin space
			Word szRounded = EntierLogUpBy( sz, szLog + Word_SizeInBytes_Log ) ;
			MM_Pages_LogSize szPagesLog = MM_Pages_MinSize_Log + szLog ;
			Word max = ((1 << szPagesLog) - sizeof(MM_Allocator_LOF_PageRounded)) / szRounded - 1 ;
			
			// get 1<<szLog pages
			MM_Space_FragmentInx frgInx = alc->space->growSpaceLog2( alc->space, szPagesLog ) ;
			MM_Space_Fragment* frg = alc->space->getFragment( alc->space, frgInx ) ;
			MM_Allocator_LOF_PageRounded* page = (MM_Allocator_LOF_PageRounded*)frg->frag ;
			
			// link into perSize table
			page->next = perSize->pages ;
			perSize->pages = page ;
			// IF_GB_TR_ON(3,{printf("mm_allocator_LOF_Alloc szRounded=%x szPagesLog=%x max=%x page=%x\n", szRounded, szPagesLog, max, page );}) ;
			
			// make user page info point to the perSize info, so we can later deallocate by inserting in the correct list
			pages->setUserData( pages, (MM_Page)page, 1<<szPagesLog, szInx ) ;
			
			// free space starts after initial admin
			MM_Allocator_LOF_FreeRounded* free = (MM_Allocator_LOF_FreeRounded*)(&page[1]) ;
			perSize->free  = free ;
			
			// split up page into free list
			for ( i = 0 ; i < max ; i++ ) {
				free->next = (MM_Allocator_LOF_FreeRounded*)( (Word)free + szRounded ) ;
				free = free->next ;
			}
			free->next = NULL ;
		}
		
		// take one from free list
		res = perSize->free ;
		perSize->free = perSize->free->next ;
	} else {
		MM_Space_FragmentInx frgInx = alc->space->growSpace( alc->space, sz ) ;
		MM_Space_Fragment* frg = alc->space->getFragment( alc->space, frgInx ) ;
		MM_Page page = frg->frag ;
		pages->setUserData( pages, (MM_Page)page, sz, frgInx | MM_Allocator_LOF_DirectAlloc_Mask ) ;
		res = page ;
	}
	
	// IF_GB_TR_ON(3,{printf("mm_allocator_LOF_Alloc sz=%x res=%x\n", sz, res );}) ;
	return res ;
}

void mm_allocator_LOF_Dealloc( MM_Allocator* alcr, Ptr ptr ) {
	MM_Allocator_LOF_Data* alc = (MM_Allocator_LOF_Data*)alcr->data ;
	MM_Pages* pages = alc->space->getPages(alc->space) ;
	
	// get user info, this points to the proper list to insert into
	Word* userInfo = mm_pages_Buddy_GetUserData( pages, (MM_Page)ptr ) ;
	int inx = *userInfo ;
	// IF_GB_TR_ON(3,{printf("mm_allocator_LOF_Dealloc ptr=%x userInfo=%x perSize=%x inx=%x\n", ptr, userInfo, perSize, inx);}) ;
	
	if ( inx & MM_Allocator_LOF_DirectAlloc_Mask ) {
		// is directly allocated, so deallocate directly via space as well
		alc->space->deallocFragment( alc->space, inx & (~MM_Allocator_LOF_DirectAlloc_Mask) ) ;
	} else {
		// exact fit
		MM_Allocator_LOF_PerSize* perSize = &(alc->perSizeRounded[ inx ]) ;
		MM_Allocator_LOF_FreeRounded* free = (MM_Allocator_LOF_FreeRounded*)ptr ;
		// put back into free list
		free->next = perSize->free ;
		perSize->free = free ;
	}
}

Ptr mm_allocator_LOF_LastAllocAddress( MM_Allocator* alcr ) {
	// not supported (could be done, but until now unnecessary)
	return NULL ;
}

MM_Space_FragmentInx mm_allocator_LOF_LastAllocFragment( MM_Allocator* alcr ) {
	// not supported (could be done, but until now unnecessary)
	return -1 ;
}

MM_Space* mm_allocator_LOF_GetSpace( MM_Allocator* alcr ) {
	MM_Allocator_LOF_Data* alc = (MM_Allocator_LOF_Data*)alcr->data ;
	return alc->space ;
}

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% LOF dump
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#ifdef TRACE
void mm_allocator_LOF_Dump( MM_Allocator* alcr ) {
	MM_Allocator_LOF_Data* alc = (MM_Allocator_LOF_Data*)alcr->data ;
	int i ;
	
	printf( ">------------------------> MM_Allocator: LOF\n" ) ;
	printf
		( "LOF: MM_Allocator_LOF_NrRoundedFit=%x perSizeRounded=%x\n"
		, MM_Allocator_LOF_NrRoundedFit, alc->perSizeRounded
		) ;

	for ( i = 0 ; i < MM_Allocator_LOF_NrRoundedFit ; i++ ) {
		MM_Allocator_LOF_PerSize* perSize = &alc->perSizeRounded[i] ;
		if ( perSize->free != NULL || perSize->pages != NULL ) {
			printf
				( "  Sz: %d: perSize=%x\n"
				, i, perSize
				) ;
			
			MM_Allocator_LOF_PageRounded* pages = perSize->pages ;
			printf( "    Pgs :" ) ;
			for ( ; pages != NULL ; pages = pages->next ) {
				printf( " %x(%x)", pages, mm_pages_Buddy_GetSizeLog(&mm_pages,(MM_Page)pages) ) ;
			}
			printf( "\n" ) ;
			
			MM_Allocator_LOF_FreeRounded* free = perSize->free ;
			printf( "    Free:" ) ;
			for ( ; free != NULL ; free = free->next ) {
				printf( " %x", free ) ;
			}
			printf( "\n" ) ;
		}
	}
	
	printf( "<------------------------< MM_Allocator: LOF\n" ) ;
}
#endif
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% LOF interface object
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
MM_Allocator mm_allocator_LOF =
	{ NULL
	, &mm_allocator_LOF_Init
	, MM_Undefined
	, &mm_allocator_LOF_Alloc
	, &mm_allocator_LOF_Dealloc
	, &mm_allocator_LOF_LastAllocAddress
	, &mm_allocator_LOF_LastAllocFragment
	, MM_Undefined
	, MM_Undefined
	, MM_Undefined
	, &mm_allocator_LOF_GetSpace
#ifdef TRACE
	, &mm_allocator_LOF_Dump
#endif
	} ;
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Malloc alike interface
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
Ptr mm_allocator_LOF_Malloc( size_t size ) {
	void* p = mm_allocator_LOF_Alloc( &mm_allocator_LOF, size ) ;
	return (Ptr)p ;
}

void mm_allocator_LOF_Free( Ptr ptr ) {
	mm_allocator_LOF_Dealloc( &mm_allocator_LOF, ptr ) ;
}

Ptr mm_allocator_LOF_Realloc( Ptr ptr, size_t size ) {
	void* p = mm_allocator_LOF_Malloc( size ) ;
	// warning: a copy from non allocated mem is done when enlarging!! Should be put into LOF impl so size can be taken into account
	memcpy( p, ptr, size ) ;
	mm_allocator_LOF_Free( ptr ) ;
	return (Ptr)p ;
}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Abstraction interface around allocation: LOF provided malloc
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
MM_Malloc mm_malloc_LOF =
	{ mm_allocator_LOF_Malloc
	, mm_allocator_LOF_Realloc
	, mm_allocator_LOF_Free
	} ;
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% LOF test
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#ifdef TRACE
#define II 1000

void mm_allocator_LOF_Test() {
	int i ;
	mm_allocator_LOF_Dump( &mm_allocator_LOF ) ;
	Ptr pg1 = mm_allocator_LOF.alloc( &mm_allocator_LOF, 1 ) ;
	mm_allocator_LOF_Dump( &mm_allocator_LOF ) ;
	Ptr pg2 = mm_allocator_LOF.alloc( &mm_allocator_LOF, 100000 ) ;
	Ptr pg3 = mm_allocator_LOF.alloc( &mm_allocator_LOF, 1 ) ;
	mm_allocator_LOF_Dump( &mm_allocator_LOF ) ;
	// return ;
	mm_allocator_LOF.dealloc( &mm_allocator_LOF, pg1 ) ;
	mm_allocator_LOF_Dump( &mm_allocator_LOF ) ;

	srandom(1) ;
	Ptr ptrs[II] ;
	for ( i = 0 ; i < II ; i++ ) { ptrs[i] = NULL ; }
	for ( i = 0 ; i < 10000000/*0*/ ; i++ ) {
		int ii = i % II ;
		Word sz = random() % 18000 + 1 ;
		if ( ptrs[ii] ) { mm_allocator_LOF.dealloc( &mm_allocator_LOF, ptrs[ii] ) ; }
		ptrs[ii] = mm_allocator_LOF.alloc( &mm_allocator_LOF, sz ) ;
		// memset( ptrs[ii], 0, sz ) ;
	}
	mm_allocator_LOF_Dump( &mm_allocator_LOF ) ;
}
#endif
%%]

