%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Memory management: page management
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Page management provides the 1st abstraction on top of malloc.

A page
- has size == 2^n * 2^k, n >= 0, k determining the smallest size
- has its start address aligned on a 2^k boundary

%%[8
#define MM_Pages_MinSize_Log	(10 + Word_SizeInBytes_Log)		// k, from above, depends on word size (?good thing or not?)
#define MM_Pages_MinSize		(1 << MM_Pages_MinSize_Log)

#define MM_Pages_MaxSize_Log	30								// maximum allocatable size, currently 1GB
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
typedef Ptr  MM_Pages_Data ;
typedef BPtr MM_Page ;

typedef uint16_t MM_Pages_Buddy_FreePages_Inx ;

typedef struct MM_Pages {
	// private data
  	MM_Pages_Data 	data ;
  	
  	// setup
  	void		 	(*init)( struct MM_Pages* ) ;
  	
  	// (de)allocation
  	MM_Page 		(*allocPage)( struct MM_Pages*, MM_Pages_Buddy_FreePages_Inx szPagesLog ) ;		// size in log(nr of pages)
  	void 			(*freePage)( struct MM_Pages*, MM_Page pg ) ;
  	
  	// user data
  	Word*			(*getUserData)( struct MM_Pages*, MM_Page pg ) ;
} MM_Pages ;
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Pages default
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
extern MM_Pages mm_pages ;
// extern MM_Pages_Data mm_pagesData ;
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
#define MM_BuddyPage_ExtlDataTag_Free		0		// free
#define MM_BuddyPage_ExtlDataTag_Alloced	1		// allocated
#define MM_BuddyPage_ExtlDataTag_Unusable	2		// cannot be used, because in between groups
#define MM_BuddyPage_ExtlDataTag_PartOf		3		// part of larger buddy

typedef uint16_t MM_BuddyPage_GroupId ;

// additional metadata for each page, to be kept when allocated.
// It consists of system + user data, in total 2 words.
typedef struct MM_BuddyPage_ExtlData {
	union {
		struct {
			uint8_t							tag ;
			uint8_t							sizeLog ;	// relative to MM_Pages_MinSize_Log
			MM_BuddyPage_GroupId			groupId ;	// id of group of buddies
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
static inline Bool mm_pages_Buddy_FreePage_IsEmpty( MM_Pages_Buddy_FreePage* fpg ) {
	return mm_dll_IsEmpty( &(fpg->dllPages) ) ;
}
%%]

%%[8
#define MM_Pages_Buddy_InitialGroupSize				(1024 * 1024 * 4)	// (10 * 1024 * 4) // 

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
static inline MM_Pages_Buddy_FreePage* mm_pages_Buddy_FreePage_At( MM_FlexArray* a, MM_Pages_Buddy_FreePages_Inx i ) {
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
extern MM_Pages_Data mm_pages_Buddy_New(  ) ;
extern MM_Page mm_pages_Buddy_AllocPage( MM_Pages* buddyPages, MM_Pages_Buddy_FreePages_Inx szPagesLog ) ;
extern void mm_pages_Buddy_FreePage( MM_Pages* buddyPages, MM_Page pg ) ;
%%]

%%[8
static inline Word* mm_pages_Buddy_GetUserData( MM_Pages* buddyPages, MM_Page pg ) {
	MM_Pages_Buddy_Data* pgs = (MM_Pages_Buddy_Data*)buddyPages->data ;
	return &(MM_Pages_Buddy_ExtlDataOfPage( pgs, pg )->user) ;
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

