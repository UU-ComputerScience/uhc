%%[8
#ifndef __MM_COMMON_H__
#define __MM_COMMON_H__
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Memory management: basic/common definitions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Hard constants
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

The size of a page, basic unit of contiguous mem allocation.

%%[8
#define MM_Page_Size_Log						(10 + Word_SizeInBytes_Log)
#define MM_Page_Size							(1 << MM_Page_Size_Log)
%%]

For Fragments as used by GC allocators

%%[8
#define MM_GC_CopySpace_FragmentSize_Log		(4 + MM_Page_Size_Log)
#define MM_GC_CopySpace_FragmentSize_HiMask		Bits_Size2HiMask(Word,MM_GC_CopySpace_FragmentSize_Log)
#define MM_GC_CopySpace_FragmentSize_LoMask		Bits_Size2LoMask(Word,MM_GC_CopySpace_FragmentSize_Log)
#define MM_GC_CopySpace_FragmentSize			(1 << MM_GC_CopySpace_FragmentSize_Log)

#define MM_Allocator_GC_FragmentInitialMax		4
%%]
#define MM_GC_CopySpace_FragmentSize_Log		(4 + MM_Page_Size_Log)
#define MM_Allocator_GC_FragmentInitialMax		4

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Checking wrapper around malloc etc
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
%%]
extern Ptr mm_malloc( size_t size ) ;
extern Ptr mm_realloc( Ptr ptr, size_t size ) ;
extern void mm_free( Ptr ptr ) ;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Abstraction interface around allocation,
%%% to be able to bootstrap with malloc as well as use other malloc alike impls
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
typedef Sys_Malloc	MM_Malloc ;
%%]

%%[8
// really the system malloc
extern MM_Malloc* 	mm_malloc_Sys ;
%%]

%%[8
// either the system malloc or any other built on top of that by EHC's RTS
extern MM_Malloc* 	mm_malloc_EHC ;
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Undefined
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#define MM_Undefined		((Ptr)(&mm_undefined))
%%]

%%[8
extern void mm_undefined(void) ;
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% EOF
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#endif /* __MM_COMMON_H__ */
%%]
