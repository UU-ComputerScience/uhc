%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Memory management: page management
%%% see associated .ch file for more info.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#include "../rts.h"
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Buddy internal functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
// find first non empty free pages dll, or end with pgInx referring to the last (empty) entry.
// indices are zero based.
MM_Pages_Buddy_FreePages_Inx mm_pages_Buddy_FindNonEmptyFreePages( MM_Pages_Buddy_Data* pgs, MM_Pages_Buddy_FreePages_Inx szPagesLog0 ) {
	MM_Pages_Buddy_FreePages_Inx pgInx0 ;
	
	for	( pgInx0 = szPagesLog0
		; pgInx0 < MM_Pages_Buddy_FreePages_Size - 1
		  && mm_pages_Buddy_FreePage_IsEmpty( mm_pages_Buddy_FreePage_At( &pgs->freePages, pgInx0 ) )
		; pgInx0++
		) ;
	
	return pgInx0 ;
}
%%]

%%[8
// recombine pages, starting with the smallest, then upwards.
// return True if something could be recombined
void mm_pages_Buddy_Recombine( MM_Pages_Buddy_Data* pgs ) {
	MM_Pages_Buddy_FreePages_Inx pgInx0 ;
	
	for ( pgInx0 = 0 ; pgInx0 < MM_Pages_Buddy_FreePages_Size ; pgInx0++ ) {
		MM_Pages_Buddy_FreePage* fpg = mm_pages_Buddy_FreePage_At( &pgs->freePages, pgInx0 ) ;
		MM_Pages_Buddy_FreePage* pg ;
		MM_Pages_Buddy_FreePage* pgNext ;
		for	( pg = mm_buddyPage_FreePage_FirstFree(fpg)
			; &pg->dllPages != &fpg->dllPages
			; pg = pgNext
			)
		{
			// first remember next page, as this will be overwritten
			pgNext = (MM_Pages_Buddy_FreePage*)(pg->dllPages.next) ;
			// get the buddy page
			MM_Pages_Buddy_FreePage* pgOtherHalf ;
			pgOtherHalf = (MM_Pages_Buddy_FreePage*)MM_Pages_Buddy_OtherHalfOfPage( pg, pgInx0 ) ;
			// IF_GB_TR_ON(3,{printf("mm_pages_Buddy_Recombine pgInx0=%x pg=%x pg'=%x\n", pgInx0, pg, pgOtherHalf);}) ;
			// check whether other half is indeed managed, i.e. in range
			if ( (Word)pgOtherHalf >= pgs->firstPage && (Word)pgOtherHalf < pgs->afterLastPage ) {
				MM_BuddyPage_ExtlData* pgd = MM_Pages_Buddy_ExtlDataOfPage( pgs, pg ) ;
				MM_BuddyPage_ExtlData* pgdOtherHalf = MM_Pages_Buddy_ExtlDataOfPage( pgs, pgOtherHalf ) ;
				// check whether size match && of same malloc'ed group && other half indeed free (this half by def must be free)
				if 	( 	pgd->system.data.sizeLog == pgdOtherHalf->system.data.sizeLog
					&& 	pgd->system.data.groupId == pgdOtherHalf->system.data.groupId
					&& 	pgdOtherHalf->system.data.tag == MM_BuddyPage_ExtlDataTag_Free
					)
				{
					// ensure to skip pOtherHalf (a bit hacky solution....)
					if (pgNext == pgOtherHalf) pgNext = (MM_Pages_Buddy_FreePage*)(pgNext->dllPages.next) ;
					// now we can recombine
					mm_dll_Delete( &pg->dllPages ) ;
					mm_dll_Delete( &pgOtherHalf->dllPages ) ;
					// make sure pg is the lower one
					if ( pg > pgOtherHalf ) {
						SwapPtr( MM_Pages_Buddy_FreePage*, pg , pgOtherHalf  ) ;
						SwapPtr( MM_BuddyPage_ExtlData* , pgd, pgdOtherHalf ) ;
					}
					// update external info
					pgd->system.data.sizeLog++ ;
					pgdOtherHalf->system.data.tag = MM_BuddyPage_ExtlDataTag_PartOf ;
					// insert in free list, one size higher
					MM_Pages_Buddy_FreePage* fpgDouble = mm_pages_Buddy_FreePage_At( &pgs->freePages, pgd->system.data.sizeLog ) ;
					mm_dll_InsertRight( &pg->dllPages, &fpgDouble->dllPages ) ;
				}
			}
		}
	}
}
%%]

%%[8
// fill a MM_BuddyGroup with using allocated mem
void mm_pages_Buddy_FillGroupWithMem( MM_BuddyGroup* buddyGrp, Ptr mem, Word memSz ) {
	// take the part aligned on 2^MM_Pages_MinSize_Log
	Word firstPage     		= EntierLogUpBy( (Word)mem, MM_Pages_MinSize_Log ) ;
	Word afterLastPage  	= EntierLogDownBy( (Word)mem + memSz, MM_Pages_MinSize_Log ) ;
	buddyGrp->malloced 		= mem ;
	buddyGrp->mallocedSize	= memSz ;
	buddyGrp->firstPage     = firstPage ;
	buddyGrp->afterLastPage = afterLastPage ;
	buddyGrp->nrPages		= (afterLastPage - firstPage) >> MM_Pages_MinSize_Log ;
	// IF_GB_TR_ON(3,{printf("mm_pages_Buddy_FillGroupWithMem mem=%x memSz=%x firstPage=%x afterLastPage=%x nrPages=%x\n", mem, memSz, firstPage, afterLastPage, buddyGrp->nrPages);}) ;
}

// init a MM_BuddyGroup so all pages and extl data are set up to their initial value
void mm_pages_Buddy_InitGroup( MM_Pages_Buddy_Data* pgs, MM_BuddyPage_GroupId grpId, MM_BuddyGroup* buddyGrp ) {
	Word pg ;
	for ( pg = buddyGrp->firstPage ; pg < buddyGrp->afterLastPage ; ) {
		// find the largest size possible at this address
		MM_Pages_Buddy_FreePages_Inx pgInx0 ;
		Word pgLast ;
		for ( pgInx0 = firstNonZeroBit( pg >> MM_Pages_MinSize_Log ), pgLast = MM_Pages_Buddy_LastPageOfPage( pg, pgInx0 )
			; pgLast >= buddyGrp->afterLastPage
			; pgInx0--, pgLast = MM_Pages_Buddy_LastPageOfPage( pg, pgInx0 )
			)
		{
		}
		
		// init the extlData
		MM_BuddyPage_ExtlData* pgd = MM_Pages_Buddy_ExtlDataOfPage( pgs, pg ) ;
		pgd->system.data.tag 		= MM_BuddyPage_ExtlDataTag_Free ;
		pgd->system.data.sizeLog 	= pgInx0 ;
		pgd->system.data.groupId 	= grpId ;
		pgd->user				 	= (Word)pg ;
		
		// insert in the free list
		MM_Pages_Buddy_FreePage* fpg = mm_pages_Buddy_FreePage_At( &pgs->freePages, pgInx0 ) ;
		mm_dll_InsertRight( &((MM_Pages_Buddy_FreePage*)pg)->dllPages, &fpg->dllPages ) ;
		
		// init the other extlData entries of this page
		// IF_GB_TR_ON(3,{printf("mm_pages_Buddy_InitGroup pg=%x pgLast=%x pgd=%x pgInx0=%x\n", pg, pgLast, pgd, pgInx0);}) ;
		for ( pg += MM_Pages_MinSize ; pg <= pgLast ; pg += MM_Pages_MinSize ) {
			// IF_GB_TR_ON(3,{printf("mm_pages_Buddy_InitGroup loop pg=%x pgLast=%x pgd=%x pgInx0=%x\n", pg, pgLast, pgd, pgInx0);}) ;
			pgd = MM_Pages_Buddy_ExtlDataOfPage( pgs, pg ) ;
			pgd->system.data.tag 		= MM_BuddyPage_ExtlDataTag_PartOf ;
			pgd->system.data.groupId 	= grpId ;
			pgd->user				 	= (Word)pg ;
		}
	}
}
%%]

%%[8
// allocate another chunk of memory, using malloc, and adapt admin accordingly.
// Minimally of size szPagesLog.
// return True when allocation succeeded
void mm_pages_Buddy_NewBuddyGroup( MM_Pages_Buddy_Data* pgs, MM_Pages_Buddy_FreePages_Inx szPagesLog ) {
	MM_FlexArray_Inx grp ;
	Word i ;
	Bool isFirstAlloc = pgs->buddyGroups.free == 0 ;
	Bool doReuseExtl = pgs->extlDataSize > (2 * MM_Pages_MinSize) ; // will not work for smaller
	
	// new pages
	Word nrNewPages
		= maxWord
			( (5 * (1 << (szPagesLog - MM_Pages_MinSize_Log))) / 2		// 2.5 as many to make sure szPagesLog will fit
			, pgs->nextGroupNrPages										// or the next increment
			) ;
	// IF_GB_TR_ON(3,{printf("mm_pages_Buddy_NewBuddyGroup isFirstAlloc=%x doReuseExtl=%x nrNewPages=%x\n", isFirstAlloc, doReuseExtl, nrNewPages);}) ;

	// determine already existing range of pages by scanning all groups.
	// combined with new pages we then know the full range necessary to (re)allocate extlData
	Word existingFirstPage = -1 ;
	Word existingAfterLastPage = 0 ;
	for ( grp = 0 ; grp < pgs->buddyGroups.free ; grp++ ) {
		MM_BuddyGroup* buddyGrp = (MM_BuddyGroup*)mm_flexArray_At( &pgs->buddyGroups, grp ) ;
		existingFirstPage = minWord( existingFirstPage, buddyGrp->firstPage ) ;
		existingAfterLastPage = maxWord( existingAfterLastPage, buddyGrp->afterLastPage ) ;
	}
	
	// allocate new group
	MM_FlexArray_Inx grpNew = mm_flexArray_NewSlot( &pgs->buddyGroups ) ;
	Word grpNewMallocSz = (nrNewPages + 2) << MM_Pages_MinSize_Log ;
	Ptr grpNewMalloced = mm_malloc( grpNewMallocSz ) ;
	MM_BuddyGroup* buddyGrpNew = (MM_BuddyGroup*)mm_flexArray_At( &pgs->buddyGroups, grpNew ) ;
	mm_pages_Buddy_FillGroupWithMem( buddyGrpNew, grpNewMalloced, grpNewMallocSz ) ;
	
	// the new memory range, yet without reuse of extlData
	Word newFirstPage = minWord( existingFirstPage, buddyGrpNew->firstPage ) ;
	Word newAfterLastPage = maxWord( existingAfterLastPage, buddyGrpNew->afterLastPage ) ;
	
	// if we have extlData we reuse it as an additional buddy group
	Word extlFirstPage = newFirstPage ;
	Word extlAfterLastPage = newAfterLastPage ;
	MM_BuddyGroup* buddyGrpExtl ;
	MM_FlexArray_Inx grpExtl ;
	if ( ! isFirstAlloc && doReuseExtl ) {
		grpExtl = mm_flexArray_NewSlot( &pgs->buddyGroups ) ;
		buddyGrpExtl = (MM_BuddyGroup*)mm_flexArray_At( &pgs->buddyGroups, grpExtl ) ;
		mm_pages_Buddy_FillGroupWithMem( buddyGrpExtl, pgs->extlData, pgs->extlDataSize ) ;
		extlFirstPage = minWord( extlFirstPage, buddyGrpExtl->firstPage ) ;
		extlAfterLastPage = maxWord( extlAfterLastPage, buddyGrpExtl->afterLastPage ) ;
	}
	
	// alloc new extlData
	Word newExtlDataSize = (extlAfterLastPage - extlFirstPage) >> MM_Pages_Buddy_PagesExtlLogDiff ;
	MM_BuddyPage_ExtlData* newExtlData = (MM_BuddyPage_ExtlData*)mm_malloc( newExtlDataSize ) ;
	// IF_GB_TR_ON(3,{printf("mm_pages_Buddy_NewBuddyGroup newExtlData=%x newExtlDataSize=%x\n", newExtlData, newExtlDataSize);}) ;
	
	// init new part as unused, copy old part into new
	if ( ! isFirstAlloc ) {
		Word oldFirstPageInx     	= (pgs->firstPage     - extlFirstPage) >> MM_Pages_MinSize_Log ;
		Word oldAfterLastPageInx 	= (pgs->afterLastPage - extlFirstPage) >> MM_Pages_MinSize_Log ;
		Word newAfterLastPageInx 	= (extlAfterLastPage  - extlFirstPage) >> MM_Pages_MinSize_Log ;
		for ( i = 0 ; i < oldFirstPageInx ; i++ ) {
			// IF_GB_TR_ON(3,{printf("mm_pages_Buddy_NewBuddyGroup extl1 i=%x &newExtlData[i]=%x\n", i, &newExtlData[i]);}) ;
			newExtlData[i].system.data.tag = MM_BuddyPage_ExtlDataTag_Unusable ;
		}
		for ( i = oldFirstPageInx ; i < oldAfterLastPageInx ; i++ ) {
			// IF_GB_TR_ON(3,{printf("mm_pages_Buddy_NewBuddyGroup extl2 i=%x &newExtlData[i]=%x\n", i, &newExtlData[i]);}) ;
			newExtlData[i] = pgs->extlData[i - oldFirstPageInx] ;
		}
		for ( i = oldAfterLastPageInx ; i < newAfterLastPageInx ; i++ ) {
			// IF_GB_TR_ON(3,{printf("mm_pages_Buddy_NewBuddyGroup extl3 i=%x &newExtlData[i]=%x\n", i, &newExtlData[i]);}) ;
			newExtlData[i].system.data.tag = MM_BuddyPage_ExtlDataTag_Unusable ;
		}
	}
	
	// if old extl not reused, free it
	if ( ! isFirstAlloc && ! doReuseExtl ) {
		// IF_GB_TR_ON(3,{printf("mm_pages_Buddy_NewBuddyGroup free extlData=%x extlDataSize=%x\n", pgs->extlData, pgs->extlDataSize);}) ;
		mm_free( pgs->extlData ) ;
	}
	
	// switch to the new data
	pgs->firstPage 			= extlFirstPage ;
	pgs->afterLastPage 		= extlAfterLastPage ;
	pgs->extlData 			= newExtlData ;
	pgs->extlDataSize		= newExtlDataSize ;
	// pgs->extlAndPagesDiff	= newExtlData - extlFirstPage ;
	pgs->nrPages			= (extlAfterLastPage - extlFirstPage) >> MM_Pages_MinSize_Log ;
	
	// init the groups
	mm_pages_Buddy_InitGroup( pgs, grpNew, buddyGrpNew ) ;
	if ( ! isFirstAlloc && doReuseExtl ) {
		mm_pages_Buddy_InitGroup( pgs, grpExtl, buddyGrpExtl ) ;
	}
		
	// next time, grow a bit more
	pgs->nextGroupNrPages = (3 * nrNewPages) / 2 ;
	// IF_GB_TR_ON(3,{printf("mm_pages_Buddy_NewBuddyGroup nextGroupNrPages=%x\n", pgs->nextGroupNrPages);}) ;
}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Buddy page management interface
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
void mm_pages_Buddy_Init( MM_Pages* buddyPages ) {
	MM_Pages_Buddy_Data* pgs = (MM_Pages_Buddy_Data*)mm_malloc( sizeof(MM_Pages_Buddy_Data) ) ;

	// setup the free page lists to be empty
	mm_flexArray_New( &pgs->freePages, sizeof(MM_Pages_Buddy_FreePage), MM_Pages_Buddy_FreePages_Size ) ;
	int i ;
	for ( i = 0 ; i < pgs->freePages.free ; i++ ) {
		MM_Pages_Buddy_FreePage* fpg = mm_pages_Buddy_FreePage_At( &pgs->freePages, i ) ;
		mm_dll_Init( &fpg->dllPages ) ;
	}

	// setup 0 buddy groups
	mm_flexArray_New( &pgs->buddyGroups, sizeof(MM_BuddyGroup), 0 ) ;
	
	// set the initial group size
	pgs->nextGroupNrPages = MM_Pages_Buddy_InitialGroupSize >> MM_Pages_MinSize_Log ;
	
	// must be 0, to make extlData reuse check in mm_pages_Buddy_NewBuddyGroup correct
	pgs->extlDataSize = 0 ;
	
	// further setup is done at the first allocation request
	buddyPages->data = (MM_Pages_Data*)pgs ;
}

MM_Page mm_pages_Buddy_AllocPage( MM_Pages* buddyPages, MM_Pages_Buddy_FreePages_Inx szPagesLog ) {
	MM_Pages_Buddy_Data* pgs = (MM_Pages_Buddy_Data*)buddyPages->data ;
	MM_Pages_Buddy_FreePages_Inx szPagesLog0 = szPagesLog - MM_Pages_MinSize_Log ;
	
	MM_Pages_Buddy_FreePages_Inx pgInx0 = mm_pages_Buddy_FindNonEmptyFreePages( pgs, szPagesLog0 ) ;
	MM_Pages_Buddy_FreePage* fpg = mm_pages_Buddy_FreePage_At( &pgs->freePages, pgInx0 ) ;
	
	if ( mm_pages_Buddy_FreePage_IsEmpty( fpg ) ) {
		// recombine, then re-attempt allocation,
		mm_pages_Buddy_Recombine( pgs ) ;
		pgInx0 = mm_pages_Buddy_FindNonEmptyFreePages( pgs, szPagesLog0 ) ;
		fpg = mm_pages_Buddy_FreePage_At( &pgs->freePages, pgInx0 ) ;
		if ( mm_pages_Buddy_FreePage_IsEmpty( fpg ) ) {
			// ask for memory, then re-re-attempt,
			mm_pages_Buddy_NewBuddyGroup( pgs, szPagesLog ) ;
			// IF_GB_TR_ON(3,{mm_pages_Buddy_Dump( buddyPages ) ;}) ;
			pgInx0 = mm_pages_Buddy_FindNonEmptyFreePages( pgs, szPagesLog0 ) ;
			fpg = mm_pages_Buddy_FreePage_At( &pgs->freePages, pgInx0 ) ;
			// otherwise fail
			if ( mm_pages_Buddy_FreePage_IsEmpty( fpg ) ) {
				rts_panic1_1( "buddy page alloc failed", szPagesLog ) ;
			}
		}
	}

	MM_Pages_Buddy_FreePage* pg = mm_buddyPage_FreePage_FirstFree(fpg) ;
	MM_BuddyPage_ExtlData* pgd = MM_Pages_Buddy_ExtlDataOfPage( pgs, pg ) ;
	mm_dll_Delete( &pg->dllPages ) ;
	
	for ( ; pgInx0 > szPagesLog0 ; pgInx0-- ) {
		// split into halves, put other half in appropriate free dll, continue with half sized pg
		MM_Pages_Buddy_FreePage* pgOtherHalf ;
		pgOtherHalf = (MM_Pages_Buddy_FreePage*)MM_Pages_Buddy_OtherHalfOfPage( pg, pgInx0 - 1 ) ;
		MM_Pages_Buddy_FreePage* fpgHalf = mm_pages_Buddy_FreePage_At( &pgs->freePages, pgInx0 - 1 ) ;
		mm_dll_InsertRight( &pgOtherHalf->dllPages, &fpgHalf->dllPages ) ;
		// init extl data
		MM_BuddyPage_ExtlData* pgdOtherHalf = MM_Pages_Buddy_ExtlDataOfPage( pgs, pgOtherHalf ) ;
		pgdOtherHalf->system.data.tag = MM_BuddyPage_ExtlDataTag_Free ;
		pgdOtherHalf->system.data.sizeLog = pgInx0 - 1 ;
		pgdOtherHalf->system.data.groupId = pgd->system.data.groupId ;
	}

	pgd->system.data.tag = MM_BuddyPage_ExtlDataTag_Alloced ;
	pgd->system.data.sizeLog = szPagesLog0 ;
	
	// IF_GB_TR_ON(3,{printf("mm_pages_Buddy_AllocPage pg=%x\n", pg);}) ;
	return (MM_Page)pg ;
}

void mm_pages_Buddy_FreePage( MM_Pages* buddyPages, MM_Page pg ) {
	MM_Pages_Buddy_Data* pgs = (MM_Pages_Buddy_Data*)buddyPages->data ;
	MM_BuddyPage_ExtlData* pgd = MM_Pages_Buddy_ExtlDataOfPage( pgs, pg ) ;
	MM_Pages_Buddy_FreePage* fpg = mm_pages_Buddy_FreePage_At( &pgs->freePages, pgd->system.data.sizeLog ) ;
	pgd->system.data.tag = MM_BuddyPage_ExtlDataTag_Free ;
	mm_dll_InsertRight( &(((MM_Pages_Buddy_FreePage*)pg)->dllPages), &fpg->dllPages ) ;
}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Pages default interface object
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
MM_Pages mm_pages =
#if ( MM_Cfg_Pages == MM_Cfg_Pages_Buddy )
	{ NULL
	, &mm_pages_Buddy_Init
	, &mm_pages_Buddy_AllocPage
	, &mm_pages_Buddy_FreePage
	, &mm_pages_Buddy_GetUserData
	} ;
#endif

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Buddy page dump
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#ifdef TRACE
mm_pages_Buddy_Dump( MM_Pages* buddyPages ) {
	MM_Pages_Buddy_Data* pgs = (MM_Pages_Buddy_Data*)buddyPages->data ;
	int i ;
	
	printf( "--------------------------\n" ) ;
	printf
		( "Pgs: nrPages=%x firstPage=%x aftPage=%x extl=%x extlSz=%x\n"
		, pgs->nrPages, pgs->firstPage, pgs->afterLastPage, pgs->extlData, pgs->extlDataSize
		) ;

	for ( i = 0 ; i < pgs->buddyGroups.free ; i++ ) {
		MM_BuddyGroup* grp = (MM_BuddyGroup*)mm_flexArray_At( &pgs->buddyGroups, i ) ;
		printf
			( "  Grp: %d: nrPages=%x firstPage=%x aftPage=%x alloc=%x allocSz=%x\n"
			, i, grp->nrPages, grp->firstPage, grp->afterLastPage, grp->malloced, grp->mallocedSize
			) ;
	}

	for ( i = 0 ; i < pgs->freePages.free ; i++ ) {
		MM_Pages_Buddy_FreePage* fpg = mm_pages_Buddy_FreePage_At( &pgs->freePages, i ) ;
		if ( ! mm_pages_Buddy_FreePage_IsEmpty( fpg ) ) {
			MM_DLL* dll = fpg->dllPages.next ;
			printf
				( "  Free: %d: dll=%x\n"
				, i, dll
				) ;
			for ( ; dll != &fpg->dllPages ; dll = dll->next ) {
				MM_Pages_Buddy_FreePage* pg = (MM_Pages_Buddy_FreePage*)dll ;
				MM_BuddyPage_ExtlData* pgd = MM_Pages_Buddy_ExtlDataOfPage( pgs, pg ) ;
				printf
					( "    Pg: pg=%x pgd=%x tag=%x sizeLog=%d grp=%x user=%x\n"
					, pg, pgd, pgd->system.data.tag, pgd->system.data.sizeLog, pgd->system.data.groupId, pgd->user
					) ;
			}
		}
	}
	printf( "--------------------------\n" ) ;
}
#endif
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Buddy pages test
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

- Tested with 10 (see MM_Pages_Buddy_InitialGroupSize in mm/pages) pages
  for various internal state changes (recombine, extend, etc)
- Tested, as below in normal initial config, it should run for some 10 sec and not crash :-)

%%[8
#ifdef TRACE
#define II 1000

void mm_pages_Buddy_Test() {
	int i ;
	mm_pages_Buddy_Dump( &mm_pages ) ;
	MM_Page pg1 = mm_pages_Buddy_AllocPage( &mm_pages, MM_Pages_MinSize_Log ) ;
	MM_Page pg2 = mm_pages_Buddy_AllocPage( &mm_pages, MM_Pages_MinSize_Log ) ;
	MM_Page pg3 = mm_pages_Buddy_AllocPage( &mm_pages, MM_Pages_MinSize_Log ) ;
	mm_pages_Buddy_Dump( &mm_pages ) ;
	mm_pages_Buddy_FreePage( &mm_pages, pg3 ) ;
	mm_pages_Buddy_FreePage( &mm_pages, pg1 ) ;
	mm_pages_Buddy_FreePage( &mm_pages, pg2 ) ;
	mm_pages_Buddy_Dump( &mm_pages ) ;
	MM_Page pg4 = mm_pages_Buddy_AllocPage( &mm_pages, MM_Pages_MinSize_Log + 2 ) ;
	mm_pages_Buddy_Dump( &mm_pages ) ;
	MM_Page pg5 = mm_pages_Buddy_AllocPage( &mm_pages, MM_Pages_MinSize_Log + 2 ) ;
	mm_pages_Buddy_Dump( &mm_pages ) ;
	MM_Page pg6 = mm_pages_Buddy_AllocPage( &mm_pages, MM_Pages_MinSize_Log + 1 ) ;
	mm_pages_Buddy_Dump( &mm_pages ) ;
	MM_Page pg7 = mm_pages_Buddy_AllocPage( &mm_pages, MM_Pages_MinSize_Log + 2 ) ;
	mm_pages_Buddy_Dump( &mm_pages ) ;
	MM_Page pg8 = mm_pages_Buddy_AllocPage( &mm_pages, MM_Pages_MinSize_Log + 5 ) ;
	mm_pages_Buddy_Dump( &mm_pages ) ;
	MM_Page pg9 = mm_pages_Buddy_AllocPage( &mm_pages, MM_Pages_MinSize_Log + 2 ) ;
	mm_pages_Buddy_Dump( &mm_pages ) ;
	MM_Page pg10 = mm_pages_Buddy_AllocPage( &mm_pages, MM_Pages_MinSize_Log + 12 ) ;
	mm_pages_Buddy_Dump( &mm_pages ) ;
	MM_Page pg11 = mm_pages_Buddy_AllocPage( &mm_pages, MM_Pages_MinSize_Log + 13 ) ;
	mm_pages_Buddy_Dump( &mm_pages ) ;
	mm_pages_Buddy_FreePage( &mm_pages, pg4 ) ;
	mm_pages_Buddy_FreePage( &mm_pages, pg5 ) ;
	mm_pages_Buddy_FreePage( &mm_pages, pg6 ) ;
	mm_pages_Buddy_FreePage( &mm_pages, pg7 ) ;
	mm_pages_Buddy_FreePage( &mm_pages, pg8 ) ;
	mm_pages_Buddy_FreePage( &mm_pages, pg9 ) ;
	mm_pages_Buddy_FreePage( &mm_pages, pg10 ) ;
	mm_pages_Buddy_FreePage( &mm_pages, pg11) ;
	MM_Page pg12 = mm_pages_Buddy_AllocPage( &mm_pages, MM_Pages_MinSize_Log + 15 ) ;
	mm_pages_Buddy_Dump( &mm_pages ) ;

	srandom(1) ;
	MM_Page pgs[II] ;
	for ( i = 0 ; i < II ; i++ ) { pgs[i] = NULL ; }
	for ( i = 0 ; i < 100000000 ; i++ ) {
		int ii = i % II ;
		Word sz = random() % 11 ;
		if ( pgs[ii] ) { mm_pages_Buddy_FreePage( &mm_pages, pgs[ii] ) ; }
		pgs[ii] = mm_pages_Buddy_AllocPage( &mm_pages, MM_Pages_MinSize_Log + sz ) ;
	}
	mm_pages_Buddy_Dump( &mm_pages ) ;
}
#endif
%%]

