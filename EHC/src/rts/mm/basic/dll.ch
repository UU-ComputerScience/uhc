%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Memory management: Doubly linked list
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

DLL provides doubly linked list functionality, but only the part dealing
with insertion/deletion. A DLL is supposed to be part of a larger
structure, to be casted from and to the larger structure.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% DLL
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
extern void mm_dll_InsertNext( MM_DLL* dllNew, MM_DLL* dll ) ;

// insert at left
extern void mm_dll_InsertPrev( MM_DLL* dllNew, MM_DLL* dll ) ;

// delete from dll, dll itself is unmodified
extern void mm_dll_Delete( MM_DLL* dll ) ;
%%]

