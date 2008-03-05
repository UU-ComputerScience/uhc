%%[8
#include <inttypes.h>
#include <stdio.h>

#include "config.h"
#include "gc.h"

inline void llvmgc_init()
{
  #if USE_BOEHM_GC
  GC_INIT();
  #endif
}

inline void* llvmgc_malloc( size_t nbytes )
{
  return GC_MALLOC( nbytes );
}

inline void* llvmgc_malloc_uncollectable( size_t nbytes )
{
  return GC_MALLOC_UNCOLLECTABLE( nbytes );
}

inline void llvmc_print_statistics( )
{
  #if USE_BOEHM_GC
    fprintf( stderr, "-----Memory usage statistics-----\n" );
    fprintf( stderr, "Bytes on the heap:                %5zu MiB\n", GC_get_heap_size()   / (1024 * 1024) );
    fprintf( stderr, "Free bytes on the heap:           %5zu MiB\n", GC_get_free_bytes()  / (1024 * 1024) );
    fprintf( stderr, "Total bytes allocated by program: %5zu MiB\n", GC_get_total_bytes() / (1024 * 1024) );
  #endif
}

%%]
