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
#if GB_DEBUG
#	define MM_Page_Size_Log__					6
#else
#	define MM_Page_Size_Log__					10
#endif
#define MM_Page_Size_Log						(MM_Page_Size_Log__ + Word_SizeInBytes_Log)
#define MM_Page_Size							(1 << MM_Page_Size_Log)
%%]

For Fragments as used by GC allocators

%%[8
#if GB_DEBUG
#	define MM_GC_CopySpace_FragmentSize_Log__	4
#else
#	define MM_GC_CopySpace_FragmentSize_Log__	4
#endif
#define MM_GC_CopySpace_FragmentSize_Log		(MM_GC_CopySpace_FragmentSize_Log__ + MM_Page_Size_Log)
#define MM_GC_CopySpace_FragmentSize_HiMask		Bits_Size2HiMask(Word,MM_GC_CopySpace_FragmentSize_Log)
#define MM_GC_CopySpace_FragmentSize_LoMask		Bits_Size2LoMask(Word,MM_GC_CopySpace_FragmentSize_Log)
#define MM_GC_CopySpace_FragmentSize			(1 << MM_GC_CopySpace_FragmentSize_Log)

#define MM_Allocator_GC_FragmentInitialMax		4
%%]

Magic byte for marking fresh mem

%%[8
#if TRACE
#define MM_GC_FreshMem_Pattern_Byte				0xaaL		// will correspond to an address in upper 2GB of mem
#define MM_GC_UsedMem_Pattern_Byte				0x55L		// and its bitwise complement

#define MM_GC_FreshMem_Pattern_Word32			(MM_GC_FreshMem_Pattern_Byte | (MM_GC_FreshMem_Pattern_Byte << 8) | (MM_GC_FreshMem_Pattern_Byte << 16) | (MM_GC_FreshMem_Pattern_Byte << 24))
#define MM_GC_FreshMem_Pattern_Word64			(MM_GC_FreshMem_Pattern_Word32 | (MM_GC_FreshMem_Pattern_Word32 << 32))

#if USE_64_BITS
#	define MM_GC_FreshMem_Pattern_Word			MM_GC_FreshMem_Pattern_Word64
#else
#	define MM_GC_FreshMem_Pattern_Word			MM_GC_FreshMem_Pattern_Word32
#endif

#define MM_GC_UsedMem_Pattern_Word				(~MM_GC_FreshMem_Pattern_Word)

#endif
%%]

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
