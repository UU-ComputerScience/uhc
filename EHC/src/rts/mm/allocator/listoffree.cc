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
	MM_Allocator_LOF_Data* alc = (MM_Allocator_LOF_Data*)mm_pages.allocPage( &mm_pages, MM_Pages_MinSize_Log ) ;
	
	Word i ;
	for ( i = 0 ; i < MM_Allocator_LOF_NrExactFit ; i++ ) {
		MM_Allocator_LOF_PerSize* perSize = &alc->perSizeExact[ i ] ;
		perSize->free  = NULL ;
		perSize->pages = NULL ;
	}
	
	alc->otherSize.free = NULL ;

	// further setup is done at the first allocation request
	alcr->data = (MM_Allocator_Data*)alc ;
}

Ptr mm_allocator_LOF_Alloc( MM_Allocator* alcr, Word sz ) {
	MM_Allocator_LOF_Data* alc = (MM_Allocator_LOF_Data*)alcr->data ;
	Ptr res = NULL ;
	Word i, szOrig ;
	szOrig = sz ;
	
	// determine index into perSize, allowing more unused space for greater size, using more pages for greater size.
	Word szInx = EntierLogUpShrBy( sz, Word_SizeInBytes_Log ) ;
	Word szRounded ; // = szInx << Word_SizeInBytes_Log ;
	MM_Pages_Buddy_FreePages_Inx szLog = 0 ;
	for ( szRounded = szInx, sz = szRounded ; sz >= 0x80 && szInx < (MM_Allocator_LOF_NrExactFit - 0x80) ; ) {
		// IF_GB_TR_ON(3,{printf("mm_allocator_LOF_Alloc szs1 szOrig=%x sz=%x szInx=%x szRounded=%x szLog=%x\n", szOrig, sz, szInx, szRounded, szLog);}) ;
		szLog++ ;
		szRounded = EntierLogUpBy( szRounded, szLog ) ;
		sz = sz - 0x80 ;
		szInx -= sz ;
		sz = (sz == 0 ? sz : EntierLogUpShrBy( sz, 1 ) ) ;
		szInx += sz ;
		// IF_GB_TR_ON(3,{printf("mm_allocator_LOF_Alloc szs2 szOrig=%x sz=%x szInx=%x szRounded=%x szLog=%x\n", szOrig, sz, szInx, szRounded, szLog);}) ;
	}
	szRounded <<= Word_SizeInBytes_Log ;
	
	// IF_GB_TR_ON(3,{printf("mm_allocator_LOF_Alloc szOrig=%x sz=%x szInx=%x szRounded=%x szLog=%x\n", szOrig, sz, szInx, szRounded, szLog);}) ;
	if ( szInx < MM_Allocator_LOF_NrExactFit ) {
		// exact fit
		MM_Allocator_LOF_PerSize* perSize = &alc->perSizeExact[ szInx ] ;
		
		// ensure elements on free list
		if ( perSize->free == NULL ) {
			// nr of new free elements - 1, allow for some wasted space at the end + admin space
			Word max = (MM_Pages_MinSize - sizeof(MM_Allocator_LOF_Page) ) / szRounded - 1 ;
			
			// get 1<<szLog pages
			MM_Allocator_LOF_Page* page = (MM_Allocator_LOF_Page*)mm_pages.allocPage( &mm_pages, MM_Pages_MinSize_Log + szLog ) ;
			page->next = perSize->pages ;
			perSize->pages = page ;
			
			// make user page info point to the perSize info, so we can later deallocate
			for ( i = 0 ; i < 1<<szLog ; i++ ) {
				Word* userInfo = mm_pages_Buddy_GetUserData( &mm_pages, (MM_Page)((Word)page + i * MM_Pages_MinSize) ) ;
				*userInfo = (Word)perSize ;
			}
			// IF_GB_TR_ON(3,{printf("mm_allocator_LOF_Alloc pg alloc max=%x page=%x userInfo=%x *userInfo=%x\n", max, page, userInfo, *userInfo);}) ;
			
			// free space starts after initial admin
			MM_Allocator_LOF_ExactFree* free = (MM_Allocator_LOF_ExactFree*)(&page[1]) ;
			perSize->free  = free ;
			
			// split up page into free list
			for ( i = 0 ; i < max ; i++ ) {
				free->next = (MM_Allocator_LOF_ExactFree*)( (Word)free + szRounded ) ;
				free = free->next ;
			}
			free->next = NULL ;
		}
		
		// take one from free list
		res = perSize->free ;
		perSize->free = perSize->free->next ;
		// IF_GB_TR_ON(3,{Word* userInfo =mm_pages_Buddy_GetUserData( &mm_pages, (MM_Page)res ) ; printf("mm_allocator_LOF_Alloc res=%x userInfo=%x\n", res, userInfo);}) ;
	} else {
		// no exact fit
	}
	
	return res ;
}

void mm_allocator_LOF_Dealloc( MM_Allocator* alcr, Ptr ptr ) {
	MM_Allocator_LOF_Data* alc = (MM_Allocator_LOF_Data*)alcr->data ;
	
	// get user info, this points to the proper list to insert into
	Word* userInfo = mm_pages_Buddy_GetUserData( &mm_pages, (MM_Page)ptr ) ;
	MM_Allocator_LOF_PerSize* perSize = (MM_Allocator_LOF_PerSize*)(*userInfo) ;
	// IF_GB_TR_ON(3,{printf("mm_allocator_LOF_Dealloc ptr=%x userInfo=%x perSize=%x\n", ptr, userInfo, perSize);}) ;
	
	if ( perSize - &(alc->perSizeExact[0]) < MM_Allocator_LOF_NrExactFit ) {
		// exact fit
		MM_Allocator_LOF_ExactFree* free = (MM_Allocator_LOF_ExactFree*)ptr ;
		free->next = perSize->free ;
		perSize->free = free ;
	} else {
		// no exact fit
		// fprintf( stderr, "mm_allocator_LOF_Dealloc error: ptr=%x perSize=%x\n", ptr, perSize ) ;
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
		( "LOF: MM_Allocator_LOF_NrExactFit=%x perSizeExact=%x otherSize.free=%x\n"
		, MM_Allocator_LOF_NrExactFit, alc->perSizeExact, alc->otherSize.free
		) ;

	for ( i = 0 ; i < MM_Allocator_LOF_NrExactFit ; i++ ) {
		MM_Allocator_LOF_PerSize* perSize = &alc->perSizeExact[i] ;
		if ( perSize->free != NULL || perSize->pages != NULL ) {
			printf
				( "  Sz: %d: perSize=%x\n"
				, i, perSize
				) ;
			
			MM_Allocator_LOF_Page* pages = perSize->pages ;
			printf( "    Pgs :" ) ;
			for ( ; pages != NULL ; pages = pages->next ) {
				printf( " %x", pages ) ;
			}
			printf( "\n" ) ;
			
			MM_Allocator_LOF_ExactFree* free = perSize->free ;
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
	Ptr pg2 = mm_allocator_LOF.alloc( &mm_allocator_LOF, 1 ) ;
	Ptr pg3 = mm_allocator_LOF.alloc( &mm_allocator_LOF, 1 ) ;
	mm_allocator_LOF_Dump( &mm_allocator_LOF ) ;
	mm_allocator_LOF.dealloc( &mm_allocator_LOF, pg1 ) ;
	mm_allocator_LOF_Dump( &mm_allocator_LOF ) ;

	srandom(1) ;
	Ptr ptrs[II] ;
	for ( i = 0 ; i < II ; i++ ) { ptrs[i] = NULL ; }
	for ( i = 0 ; i < 100000000 ; i++ ) {
		int ii = i % II ;
		Word sz = random() % 800 + 1 ;
		if ( ptrs[ii] ) { mm_allocator_LOF.dealloc( &mm_allocator_LOF, ptrs[ii] ) ; }
		ptrs[ii] = mm_allocator_LOF.alloc( &mm_allocator_LOF, sz ) ;
	}
	mm_allocator_LOF_Dump( &mm_allocator_LOF ) ;
}
#endif
%%]

