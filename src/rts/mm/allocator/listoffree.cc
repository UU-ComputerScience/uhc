%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Memory management: allocator: list of free (LOF)
%%% see associated .ch file for more info.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#include "../../rts.h"
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% LOF internal functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% LOF interface
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
void mm_allocator_LOF_Init( MM_Allocator* alcr ) {
	Word alcSizeLog = maxWord( MM_Pages_MinSize_Log, firstHigherPower2( sizeof(MM_Allocator_LOF_Data) ) ) ;
	MM_Allocator_LOF_Data* alc = (MM_Allocator_LOF_Data*)mm_pages.allocLog2( &mm_pages, alcSizeLog ) ;
	
	Word i ;
	for ( i = 0 ; i < MM_Allocator_LOF_NrRoundedFit ; i++ ) {
		MM_Allocator_LOF_PerSize* perSize = &alc->perSizeRounded[ i ] ;
		perSize->free  = NULL ;
		perSize->pages = NULL ;
	}
	
	// further setup is done at the first allocation request
	alcr->data = (MM_Allocator_Data*)alc ;
}

Ptr mm_allocator_LOF_Alloc( MM_Allocator* alcr, Word sz ) {
	MM_Allocator_LOF_Data* alc = (MM_Allocator_LOF_Data*)alcr->data ;
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
			MM_Pages_Buddy_FreePages_Inx szPagesLog = MM_Pages_MinSize_Log + szLog ;
			Word max = ((1 << szPagesLog) - sizeof(MM_Allocator_LOF_PageRounded)) / szRounded - 1 ;
			
			// get 1<<szLog pages
			MM_Allocator_LOF_PageRounded* page = (MM_Allocator_LOF_PageRounded*)mm_pages.allocLog2( &mm_pages, szPagesLog ) ;
			page->next = perSize->pages ;
			perSize->pages = page ;
			// IF_GB_TR_ON(3,{printf("mm_allocator_LOF_Alloc szRounded=%x szPagesLog=%x max=%x page=%x\n", szRounded, szPagesLog, max, page );}) ;
			
			// make user page info point to the perSize info, so we can later deallocate by inserting in the correct list
			mm_pages.setUserData( &mm_pages, (MM_Page)page, 1<<szPagesLog, (Word)perSize ) ;
			
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
		MM_Page page = mm_pages.alloc( &mm_pages, sz ) ;
		mm_pages.setUserData( &mm_pages, (MM_Page)page, sz, (Word)page ) ;
		res = page ;
	}
	
	// IF_GB_TR_ON(3,{printf("mm_allocator_LOF_Alloc sz=%x res=%x\n", sz, res );}) ;
	return res ;
}

void mm_allocator_LOF_Dealloc( MM_Allocator* alcr, Ptr ptr ) {
	MM_Allocator_LOF_Data* alc = (MM_Allocator_LOF_Data*)alcr->data ;
	
	// get user info, this points to the proper list to insert into
	Word* userInfo = mm_pages_Buddy_GetUserData( &mm_pages, (MM_Page)ptr ) ;
	MM_Allocator_LOF_PerSize* perSize = (MM_Allocator_LOF_PerSize*)(*userInfo) ;
	Word perSizeInx = (Word)( perSize - &(alc->perSizeRounded[0]) ) ;
	// IF_GB_TR_ON(3,{printf("mm_allocator_LOF_Dealloc ptr=%x userInfo=%x perSize=%x perSizeInx=%x\n", ptr, userInfo, perSize, perSizeInx);}) ;
	
	if ( perSizeInx < MM_Allocator_LOF_NrRoundedFit ) {
		// exact fit
		MM_Allocator_LOF_FreeRounded* free = (MM_Allocator_LOF_FreeRounded*)ptr ;
		free->next = perSize->free ;
		perSize->free = free ;
	} else {
		mm_pages.dealloc( &mm_pages, ptr ) ;
	}
}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% LOF interface object
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
MM_Allocator mm_allocator_LOF =
	{ NULL
	, &mm_allocator_LOF_Init
	, &mm_allocator_LOF_Alloc
	, &mm_allocator_LOF_Dealloc
	} ;

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% LOF dump
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#ifdef TRACE
mm_allocator_LOF_Dump( MM_Allocator* alcr ) {
	MM_Allocator_LOF_Data* alc = (MM_Allocator_LOF_Data*)alcr->data ;
	int i ;
	
	printf( "--------------------------\n" ) ;
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
	
	printf( "--------------------------\n" ) ;
}
#endif
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

