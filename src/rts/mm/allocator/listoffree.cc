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
	Word alcSizeLog = maxWord( MM_Pages_MinSize_Log, firstHigherPower2( sizeof(MM_Allocator_LOF_Data) + sizeof(MM_Allocator_LOF_AllocOther) ) ) ;
	MM_Allocator_LOF_Data* alc = (MM_Allocator_LOF_Data*)mm_pages.allocPage( &mm_pages, alcSizeLog ) ;
	
	Word i ;
	for ( i = 0 ; i < MM_Allocator_LOF_NrRoundedFit ; i++ ) {
		MM_Allocator_LOF_PerSize* perSize = &alc->perSizeRounded[ i ] ;
		perSize->free  = NULL ;
		perSize->pages = NULL ;
	}
	
	// the other free list, of first fit chunks, dummy entry after the persize admin
	alc->freeOther = (MM_Allocator_LOF_AllocOther*)(&alc[1]) ;
	mm_dll_Init( &(alc->freeOther->dll) ) ;
	alc->freeOther->size = 0 ;
	alc->freeOther->tag = MM_Allocator_FreeOtherTag_Alloced ;
	// IF_GB_TR_ON(3,{printf("mm_allocator_LOF_Init alc=%x alc->freeOther=%x\n", alc, alc->freeOther);}) ;

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
			MM_Allocator_LOF_PageRounded* page = (MM_Allocator_LOF_PageRounded*)mm_pages.allocPage( &mm_pages, szPagesLog ) ;
			page->next = perSize->pages ;
			perSize->pages = page ;
			// IF_GB_TR_ON(3,{printf("mm_allocator_LOF_Alloc szRounded=%x szPagesLog=%x max=%x page=%x\n", szRounded, szPagesLog, max, page );}) ;
			
			// make user page info point to the perSize info, so we can later deallocate by inserting in the correct list
			mm_pages.setUserData( &mm_pages, (MM_Page)page, szPagesLog, (Word)perSize ) ;
			
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
		Bool hasFound = False ;
		sz = szWord << Word_SizeInBytes_Log ;
		// IF_GB_TR_ON(3,{printf("mm_allocator_LOF_Alloc sz=%x\n", sz );}) ;
		// no exact fit
		MM_Allocator_LOF_AllocOther* freeOther ;
		// search, but header of dll (with size==0) also participates
		while ( ! hasFound ) {
			freeOther = alc->freeOther ;
			do {
				// IF_GB_TR_ON(3,{printf("mm_allocator_LOF_Alloc sz=%x alc->freeOther=%x freeOther=%x freeOther sz=%x\n", sz, alc->freeOther, freeOther, freeOther->size );}) ;
				if ( sz <= freeOther->size ) {
					// found a free slot
					MM_Allocator_LOF_AllocOther* allocated ;
					if ( freeOther->size - sz >= MM_Allocator_LOF_MaxRoundedSize + sizeof(MM_Allocator_LOF_AllocOther) ) {
						// room enough to make it worthwhile to put the remaining part back on the free list
						// return location at high end of free block
						allocated = (MM_Allocator_LOF_AllocOther*)( (Word)(&freeOther[1]) + freeOther->size - sz - sizeof(MM_Allocator_LOF_AllocOther) ) ;
						allocated->size = sz ;
						freeOther->size = (Word)allocated - (Word)(&freeOther[1]) ;
					} else {
						// return location after admin
						allocated = freeOther ;
						mm_dll_Delete( &freeOther->dll ) ;
						freeOther = (MM_Allocator_LOF_AllocOther*)(freeOther->dll.next) ; 
					}
					res = &allocated[1] ;
					allocated->tag = MM_Allocator_FreeOtherTag_Alloced ;
					hasFound = True ;
					break ;
				}
				freeOther = (MM_Allocator_LOF_AllocOther*)(freeOther->dll.next) ;
			}
			while ( ! hasFound && freeOther != alc->freeOther ) ;
			
			// when not found insert large enough block and retry
			if ( ! hasFound ) {
				Word szPageLog = firstHigherPower2( 3 * (sz + sizeof(MM_Allocator_LOF_AllocOther)) ) ;
				MM_Allocator_LOF_AllocOther* page = (MM_Allocator_LOF_AllocOther*)mm_pages.allocPage( &mm_pages, szPageLog ) ;
				mm_pages.setUserData( &mm_pages, (MM_Page)page, szPageLog, (Word)page ) ;
				page->tag = MM_Allocator_FreeOtherTag_Free ;
				page->size = (1 << szPageLog) - sizeof(MM_Allocator_LOF_AllocOther) ;
				mm_dll_InsertRight( &(page->dll), &(alc->freeOther->dll) ) ;
			}
		}
		
		// next time we continue where we left of searching, as to spread fragmentation and preserve locality a bit
		alc->freeOther = freeOther ;
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
		// no exact fit
		// header always in front of allocated mem
		MM_Allocator_LOF_AllocOther* allocated = &( ((MM_Allocator_LOF_AllocOther*)ptr)[-1] ) ;
		// put back in free list, currently without trying to merge blocks
		allocated->tag = MM_Allocator_FreeOtherTag_Free ;
		mm_dll_InsertRight( &(allocated->dll), &(alc->freeOther->dll) ) ;
		// IF_GB_TR_ON(3,{printf("mm_allocator_LOF_Dealloc allocated=%x\n", allocated);}) ;
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
		( "LOF: MM_Allocator_LOF_NrRoundedFit=%x perSizeRounded=%x freeOther=%x\n"
		, MM_Allocator_LOF_NrRoundedFit, alc->perSizeRounded, alc->freeOther
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
	
	MM_Allocator_LOF_AllocOther* freeOther = alc->freeOther ;
	printf( "  Other free:" ) ;
	do {
		printf( " %x(%x)", freeOther, freeOther->size ) ;
		freeOther = (MM_Allocator_LOF_AllocOther*)(freeOther->dll.next) ;
	}
	while ( freeOther != alc->freeOther ) ;
	printf( "\n" ) ;

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
	Ptr pg2 = mm_allocator_LOF.alloc( &mm_allocator_LOF, 1000000 ) ;
	Ptr pg3 = mm_allocator_LOF.alloc( &mm_allocator_LOF, 1 ) ;
	mm_allocator_LOF_Dump( &mm_allocator_LOF ) ;
	// return ;
	mm_allocator_LOF.dealloc( &mm_allocator_LOF, pg1 ) ;
	mm_allocator_LOF_Dump( &mm_allocator_LOF ) ;

	srandom(1) ;
	Ptr ptrs[II] ;
	for ( i = 0 ; i < II ; i++ ) { ptrs[i] = NULL ; }
	for ( i = 0 ; i < 100000/*000*/ ; i++ ) {
		int ii = i % II ;
		Word sz = random() % 18000 + 10000 ;
		if ( ptrs[ii] ) { mm_allocator_LOF.dealloc( &mm_allocator_LOF, ptrs[ii] ) ; }
		ptrs[ii] = mm_allocator_LOF.alloc( &mm_allocator_LOF, sz ) ;
		// memset( ptrs[ii], 0, sz ) ;
	}
	mm_allocator_LOF_Dump( &mm_allocator_LOF ) ;
}
#endif
%%]

