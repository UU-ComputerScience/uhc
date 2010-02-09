%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Memory management: Doubly linked list
%%% see associated .ch file for more info.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#include "../../rts.h"
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% DLL
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
Bool mm_dll_IsEmpty( MM_DLL* dll ) {
	return dll->next == dll ;
}

void mm_dll_Init( MM_DLL* dll ) {
	dll->next = dll->prev = dll ;
}

void mm_dll_InsertNext( MM_DLL* dllNew, MM_DLL* dll ) {
	dllNew->prev = dll ;
	dllNew->next = dll->next ;
	dll->next->prev = dllNew ;
	dll->next = dllNew ;
}

void mm_dll_InsertPrev( MM_DLL* dllNew, MM_DLL* dll ) {
	mm_dll_InsertNext( dllNew, dll->prev ) ;
}

void mm_dll_Delete( MM_DLL* dll ) {
	dll->next->prev = dll->prev ;
	dll->prev->next = dll->next ;
}

%%]

