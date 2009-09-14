%%[8
#ifndef __BASE_SYSALLOC_H__
#define __BASE_SYSALLOC_H__
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Low level system mem allocation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Checking wrapper around malloc etc
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
extern Ptr sys_malloc( size_t size ) ;
extern Ptr sys_realloc( Ptr ptr, size_t size ) ;
extern void sys_free( Ptr ptr ) ;
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Abstraction interface around allocation,
%%% to be able to bootstrap with malloc as well as use other malloc alike impls
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
typedef struct Sys_Malloc {
	Ptr 	(*malloc)( size_t size ) ;
	Ptr 	(*realloc)( Ptr ptr, size_t size ) ;
	void 	(*free)( Ptr ptr ) ;
} Sys_Malloc ;
%%]

%%[8
// really the system malloc
extern Sys_Malloc 	sys_malloc_Sys ;
%%]

%%[8
// either the system malloc or any other built on top of that by EHC's RTS
extern Sys_Malloc* 	sys_malloc_EHC ;
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% EOF
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#endif /* __BASE_SYSALLOC_H__ */
%%]
