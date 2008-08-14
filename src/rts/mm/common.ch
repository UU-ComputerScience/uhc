%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Memory management: basic/common definitions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Checking wrapper around malloc etc
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
extern Ptr mm_malloc( size_t size ) ;
extern Ptr mm_realloc( Ptr ptr, size_t size ) ;
extern void mm_free( Ptr ptr ) ;
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Lowest level flexible size ptr array mem managed via malloc/realloc, basically for bootstrapping
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
typedef uint32_t MM_FlexArray_Inx ;

typedef struct MM_FlexArray {
	MM_FlexArray_Inx		sizeElt ;	// size of elements
	BPtr	 				ptr	;
	MM_FlexArray_Inx		size ;		// overall size, expressed in sizeof(Ptr)
	MM_FlexArray_Inx		free ;		// first free slot, free <= size
} MM_FlexArray ;

%%]


%%[8
// return new array in a, return a
// when a == NULL allocate descriptor in a as well.
// entries uptil free are zeroed
extern MM_FlexArray* mm_flexArray_New( MM_FlexArray* a, MM_FlexArray_Inx szElt, MM_FlexArray_Inx sz ) ;

// extend array by sz elements, realloc if necessary, bump free ptr.
// additional entries uptil free are zeroed
extern void mm_flexArray_ExtendBy( MM_FlexArray* a, MM_FlexArray_Inx sz ) ;

// new slot in array, at end
extern MM_FlexArray_Inx mm_flexArray_NewSlot( MM_FlexArray* a ) ;

// ensure availability of slot
extern void mm_flexArray_EnsureSlot( MM_FlexArray* a, MM_FlexArray_Inx i ) ;

// indexing
static inline BPtr mm_flexArray_At( MM_FlexArray* a, MM_FlexArray_Inx i ) {
	return &(a->ptr[ i * a->sizeElt ]) ;
}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Doubly linked list
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
typedef struct MM_DLL {
	struct MM_DLL*		next ;
	struct MM_DLL*		prev ;
} MM_DLL ;
%%]

%%[8
// dll empty?
extern Bool mm_dll_IsEmpty( MM_DLL* dll ) ;

// init
extern void mm_dll_Init( MM_DLL* dll ) ;

// insert at right
extern void mm_dll_InsertRight( MM_DLL* dllNew, MM_DLL* dll ) ;

// insert at left
extern void mm_dll_InsertLeft( MM_DLL* dllNew, MM_DLL* dll ) ;

// delete from dll, dll itself is unmodified
extern void mm_dll_Delete( MM_DLL* dll ) ;
%%]



