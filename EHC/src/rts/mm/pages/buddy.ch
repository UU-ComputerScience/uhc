%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Memory management: page management: buddy system
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Pages Buddy interface object
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
extern MM_Pages mm_pages_Buddy ;
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Buddy page management defs & types
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Each page has external data, maintained in a separate array (hence external),
to identify (amongst others) its size.
The data fits in a Word, 2^Word_SizeInBits_Log, to allow the use of shifting for indexing.

Pages are grouped into Buddy groups. Each group corresponds to a single malloc.
This design allows incremental growth of the use of mallocable memory, and therefore only depends on malloc.

%%[8
#define MM_Pages_Buddy_OtherHalfOfPage(this,thisSzLog) \
			( (Word)(this) ^ Bits_Pow2( Word, (thisSzLog) + MM_Pages_MinSize_Log ) )
#define MM_Pages_Buddy_LastPageOfPage(this,thisSzLog) \
			( (Word)(this) + (((1 << thisSzLog) - 1) << MM_Pages_MinSize_Log) )

%%]

%%[8
#define MM_BuddyPage_ExtlDataTag_Free				0		// free
#define MM_BuddyPage_ExtlDataTag_Alloced			1		// allocated
#define MM_BuddyPage_ExtlDataTag_Unusable			2		// cannot be used, because in between groups
#define MM_BuddyPage_ExtlDataTag_PartOf				3		// part of larger buddy, free or alloced
#define MM_BuddyPage_ExtlDataTag_PartOfAlloced		4		// part of larger allocated buddy

typedef HalfWord MM_BuddyPage_GroupId ;

// additional metadata for each page, to be kept when allocated.
// It consists of system + user data, in total 2 words.
typedef struct MM_BuddyPage_ExtlData {
	union {
		struct {
			// sizeLog: relative to MM_Pages_MinSize_Log
			// groupId: id of group of buddies
			// sizePages: size in pages <= 1<<sizeLog, equal for power of 2 pages
#			if USE_64_BITS
				uint8_t						tag 		: 8	;
				uint8_t						sizeLog 	: 8 ;	
				MM_BuddyPage_GroupId		groupId 	: 16 ;	
				HalfWord					sizePages 	: 32 ;	
#			else
				uint8_t						tag 		: 3	;
				uint8_t						sizeLog 	: 5 ;	
				MM_BuddyPage_GroupId		groupId 	: 8 ;	
				HalfWord					sizePages 	: 16 ;	
#			endif
		} 		data ;
		Word	word ;
	} 		system ;
	Word	user ;
} __attribute__ ((__packed__)) MM_BuddyPage_ExtlData ;

#define MM_BuddyPage_ExtlDataSize_Log	(Word_SizeInBytes_Log + 1)
#define MM_BuddyPage_ExtlDataSize		(1 << MM_BuddyPage_ExtlDataSize_Log)
%%]

%%[8
// admin for 1 group of buddies, all from 1 malloc
typedef struct MM_BuddyGroup {
	Ptr							malloced ; 			// ptr to malloc'ed mem
	Word						mallocedSize ; 		// and its size
	Word						firstPage ; 		// ptr to first page
	Word						afterLastPage ; 	// ptr to after last page, which is the beginning of the global extlData
	Word						nrPages ;			// nr of pages
} MM_BuddyGroup ;
%%]

%%[8
#define MM_Pages_Buddy_FreePages_Size		(MM_Pages_MaxSize_Log - MM_Pages_MinSize_Log + 1)

typedef struct MM_Pages_Buddy_FreePage {
	MM_DLL		dllPages ;				// this must be the first because of casting between ML_DLL and MM_Pages_Buddy_FreePage
} MM_Pages_Buddy_FreePage ;

// is empty free pages entry
static inline Bool mm_pages_Buddy_Dealloc_IsEmpty( MM_Pages_Buddy_FreePage* fpg ) {
	return mm_dll_IsEmpty( &(fpg->dllPages) ) ;
}
%%]

%%[8
#if GB_DEBUG
#	define MM_Pages_Buddy_InitialGroupSize__	10
#else
#	define MM_Pages_Buddy_InitialGroupSize__	1024
#endif
#define MM_Pages_Buddy_InitialGroupSize			(MM_Pages_Buddy_InitialGroupSize__ * 1024 * 4)	// (10 * 1024 * 4) // 

// the administration
typedef struct MM_Pages_Buddy_Data {
	// Word						extlAndPagesDiff ;	// (cached) difference between extlData - firstPage
	MM_FlexArray				freePages ;			// array of ptr's to free pages, indexed by log(pagesize) - MM_Pages_MinSize_Log
	MM_FlexArray				buddyGroups ;		// array of ptr's to buddy groups
	MM_BuddyPage_ExtlData*		extlData ;			// meta data of pages
	Word						extlDataSize ;		// and its size
	Word						firstPage ; 		// ptr to first page
	Word						afterLastPage ; 	// ptr to after last page
	Word						nextGroupNrPages ;	// minimal size of next buddy group
	Word						nrPages ;			// (cached) nr of pages
} MM_Pages_Buddy_Data ;

// index, with cast
static inline MM_Pages_Buddy_FreePage* mm_pages_Buddy_FreePage_At( MM_FlexArray* a, MM_Pages_LogSize i ) {
	return (MM_Pages_Buddy_FreePage*)mm_flexArray_At( a, i ) ;
}

// first free page
// pre: dll not empty
static inline MM_Pages_Buddy_FreePage* mm_buddyPage_FreePage_FirstFree( MM_Pages_Buddy_FreePage* fpg ) {
	return (MM_Pages_Buddy_FreePage*)(fpg->dllPages.next) ;
}

// additional external data for page
#define MM_Pages_Buddy_PagesExtlLogDiff  	(MM_Pages_MinSize_Log - MM_BuddyPage_ExtlDataSize_Log)
#define MM_Pages_Buddy_ExtlDataOfPage(pgs,pg) \
			((MM_BuddyPage_ExtlData*) \
				( ((((Word)(pg) - (pgs)->firstPage) >> MM_Pages_Buddy_PagesExtlLogDiff) & Bits_Size2HiMask(Word,MM_BuddyPage_ExtlDataSize_Log)) \
				+ (Word)((pgs)->extlData) \
				))
%%]
#define MM_Pages_Buddy_ExtlDataOfPage(pgs,pg) \
			(MM_BuddyPage_ExtlData*) \
				( ((Word)(pg) >> MM_Pages_Buddy_PagesExtlLogDiff) \
				+ (pgs)->extlAndPagesDiff \
				)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Buddy page management interface
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
extern void mm_pages_Buddy_Init( MM_Pages* buddyPages, MM_Malloc* memmgt ) ;
extern MM_Page mm_pages_Buddy_AllocPagesLog2( MM_Pages* buddyPages, MM_Pages_LogSize szPagesLog ) ;
extern MM_Page mm_pages_Buddy_AllocPages( MM_Pages* buddyPages, Word sz ) ;
extern void mm_pages_Buddy_DeallocPages( MM_Pages* buddyPages, MM_Page pg ) ;
extern void mm_pages_Buddy_SetUserData( MM_Pages* buddyPages, MM_Page pg, Word sz, Word info ) ;
%%]

%%[8
static inline Bool mm_pages_Buddy_IsInRange( MM_Pages* buddyPages, MM_Page pg ) {
	MM_Pages_Buddy_Data* pgs = (MM_Pages_Buddy_Data*)buddyPages->data ;
	return (Word)pg >= pgs->firstPage && (Word)pg < pgs->afterLastPage ;
}

static inline Word* mm_pages_Buddy_GetUserData( MM_Pages* buddyPages, MM_Page pg ) {
	MM_Pages_Buddy_Data* pgs = (MM_Pages_Buddy_Data*)buddyPages->data ;
	return &(MM_Pages_Buddy_ExtlDataOfPage( pgs, pg )->user) ;
}

static inline Word mm_pages_Buddy_GetSizeLog( MM_Pages* buddyPages, MM_Page pg ) {
	MM_Pages_Buddy_Data* pgs = (MM_Pages_Buddy_Data*)buddyPages->data ;
	return MM_Pages_Buddy_ExtlDataOfPage( pgs, pg )->system.data.sizeLog + MM_Pages_MinSize_Log ;
}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Pages test
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#ifdef TRACE
extern void mm_pages_Buddy_Test() ;
#endif
%%]

