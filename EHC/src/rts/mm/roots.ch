%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Memory management: roots
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Roots hold addresses of locations holding GC managed objects, to be used
as roots for liveness tracing. Two root sets are maintained, one global,
one local organized as a stack, to be used parallel to the C stack for
primitives. The local stack groups entries and allows to push inside a
group, but pop the group as a whole.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Defs
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Global roots

%%[8
typedef struct MM_Roots_Entry {
	WPtr	 					ptrToObj ;		// ptr to live object
	HalfWord	 				nrObjs ;		// nr of objects rooted at ptrToObj
} MM_Roots_Entry ;

%%]

Local roots, LIFO structure which runs parallel with the C stack

%%[8
typedef struct MM_LclRoot_One {
	struct MM_LclRoot_One*		next ;			// next in the stack
	WPtr						ptrToObj ;		// ptr to live object
} MM_LclRoot_One ;

typedef struct MM_LclRoot_Grp {
	struct MM_LclRoot_Grp*		next ;			// next in the stack
	MM_LclRoot_One*				ones ;			// the individual entries
} MM_LclRoot_Grp ;
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Interface for global roots
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
extern MM_FlexArray			mm_Roots ;
%%]

%%[8
extern void mm_Roots_RegisterNWithFlag( WPtr toObj, HalfWord nr ) ;

static inline void mm_Roots_RegisterN( WPtr toObj, HalfWord nr ) {
	mm_Roots_RegisterNWithFlag( toObj, 1 ) ;
}

static inline void mm_Roots_Register1( WPtr toObj ) {
	mm_Roots_RegisterN( toObj, 1 ) ;
}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Interface for local roots, which follow call nesting of C functions, to be used in C functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#define MM_LclRoot_EnterGrp								MM_LclRoot_Grp _mm_lclRoot_Grp = { mm_LclRoots, NULL } ; mm_LclRoots = &_mm_lclRoot_Grp
#define MM_LclRoot_LeaveGrp								mm_LclRoots = _mm_lclRoot_Grp.next

#define MM_LclRoot_EnterOne(n,nm)						MM_LclRoot_One _##n##_MM_LclRoot_One = { mm_LclRoots->ones, (WPtr)(&(nm)) } ; mm_LclRoots->ones = &_##n##_MM_LclRoot_One
#define MM_LclRoot_EnterOne_Zeroed(n,nm)				MM_LclRoot_EnterOne(n,nm) ; { WPtr pnm = (WPtr)(&(nm)) ; *pnm = 0 ; }
#define MM_LclRoot_EnterOne1(nm1)						MM_LclRoot_EnterOne(1,nm1)
#define MM_LclRoot_EnterOne2(nm1,nm2)					MM_LclRoot_EnterOne1(nm1) ; MM_LclRoot_EnterOne(2,nm2)
#define MM_LclRoot_EnterOne3(nm1,nm2,nm3)				MM_LclRoot_EnterOne2(nm1,nm2) ; MM_LclRoot_EnterOne(3,nm3)
#define MM_LclRoot_EnterOne4(nm1,nm2,nm3,nm4)			MM_LclRoot_EnterOne3(nm1,nm2,nm3) ; MM_LclRoot_EnterOne(4,nm4)
#define MM_LclRoot_EnterOne1_Zeroed(nm1)				MM_LclRoot_EnterOne_Zeroed(1z,nm1)
#define MM_LclRoot_EnterOne2_Zeroed(nm1,nm2)			MM_LclRoot_EnterOne1_Zeroed(nm1) ; MM_LclRoot_EnterOne_Zeroed(2z,nm2)
#define MM_LclRoot_EnterOne3_Zeroed(nm1,nm2,nm3)		MM_LclRoot_EnterOne2_Zeroed(nm1,nm2) ; MM_LclRoot_EnterOne_Zeroed(3z,nm3)
#define MM_LclRoot_EnterOne4_Zeroed(nm1,nm2,nm3,nm4)	MM_LclRoot_EnterOne3_Zeroed(nm1,nm2,nm3) ; MM_LclRoot_EnterOne_Zeroed(4,nm4)
%%]

%%[8
extern MM_LclRoot_Grp* 		mm_LclRoots ;
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Initialization
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
extern void mm_init_roots() ;
%%]
