%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Memory management: plan: semi space (SS)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

A semi space plan uses two spaces of which one is used to allocate, the other to copy surviving
objects into.
See XXX.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  defs & types
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
// state of gc progress
#define	MM_PLAN_SS_GCPROGRESS_COLLECTING			(1<<0)		// busy with collecting
#define	MM_PLAN_SS_GCPROGRESS_POSTCOLLECTING		(1<<1)		// busy with gc postprocessing
%%]

%%[8
// the administration
typedef struct MM_Plan_SS_Data {
	MM_Space			fragSpace0 ; 		// the 2 lower level spaces
	MM_Space			fragSpace1 ;
	MM_Space			space0 ; 			// the 2 semi spaces
	MM_Space			space1 ;
	MM_Space*			toSpace ;			// the active allocation space
	MM_Space*			fromSpace ;			// the collected space
	MM_Allocator		ssAllocator ;		// and its allocator
	MM_Allocator		residentAllocator ;	// for allocations remaining resident/nonrelocated
	MM_Collector		collector ;
	MM_Malloc			memMgt ;
	MM_TraceSupply*		queTraceSupply ;	// trace request queue
	MM_TraceSupply		allTraceSupply ;	// all trace supplies grouped together
	MM_Trace			gbmTrace ;			// GBM specific
	MM_Module			gbmModule ;			// GBM specific module info
%%[[94
  	MM_DEQue 			weakPtrFinalizeQue ;	// queue of weakptrs to be finalized
	MM_WeakPtr*			weakPtr ;				// weak ptr admin
%%]]
	Word				gcProgress ;		// gc state
} MM_Plan_SS_Data ;
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  interface
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
extern void mm_plan_SS_Init( MM_Plan* ) ;
#if MM_BYPASS_PLAN
extern void mm_plan_SS_InitBypass( MM_Plan* plan ) ;
#endif
%%]
extern Bool mm_plan_SS_PollForGC( MM_Plan*, Bool isSpaceFull, Word gcInfo ) ;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  interface object
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
extern MM_Plan mm_plan_SS ;
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  test
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#ifdef TRACE
extern void mm_plan_SS_Test() ;
#endif
%%]

