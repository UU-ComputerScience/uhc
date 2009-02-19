%%[8
#include <inttypes.h>
#include <stdio.h>
#include <locale.h>
//#include <unistd.h>

#include "config.h"
#include "gc/gc.h"
#include "timing.h"

void llvmgc_init()
{
  #if USE_BOEHM_GC
    // Find the 'cold' stack bottom. Useful when runned under control ofvalgrind.
    // The environment pointer passed to main is at the bottom of the stack, so
    // we use this as 'cold' stack bottom.
    //
    //GC_stackbottom = (char*) __environ;
    
    // Format output by local settings of the machine.
    // 
    setlocale (LC_ALL, "");

    // Initialize the GC, needed on a Mac, on other platforms
    // it translates to an no-op.
    //
    GC_INIT();
    #if GC_TIMING
      // Begin the timing of the program
      //
      timing_initialize();
    #endif /* GC_TIMING */
  #endif /* USE_BOEHM_GC */
}

void* llvmgc_malloc( size_t nbytes )
{
  #if GC_TIMING
    //switch_to_gc();
    void* ret = GC_MALLOC( nbytes );
    //switch_to_user_code();
    return ret;
  #else
    return GC_MALLOC( nbytes );
  #endif /* GC_TIMING */
}

void* llvmgc_malloc_uncollectable( size_t nbytes )
{
  #if GC_TIMING
    //switch_to_gc();
    void* ret = GC_MALLOC_UNCOLLECTABLE( nbytes );
    //switch_to_user_code();
    return ret;
  #else
    return GC_MALLOC_UNCOLLECTABLE( nbytes );
  #endif /* GC_TIMING */
}

void llvmc_print_statistics( )
{
  #if USE_BOEHM_GC
    #if GC_TIMING
      timing_finalize();
      fprintf( stderr, "-----Timing statistics-----\n" );
      fprintf( stderr, "User code time:                   %4.3f seconds\n", prog_time );
      fprintf( stderr, "Garbage collector time:           %4.3f seconds\n", gc_time ); 
      fprintf( stderr, "Total:                            %4.3f seconds\n", prog_time + gc_time );
  #endif /* GC_TIMING */
  
    fprintf( stderr, "-----Memory usage statistics-----\n" );
    fprintf( stderr, "Bytes on the heap:                %'15zu bytes\n", GC_get_heap_size()  );
    fprintf( stderr, "Free bytes on the heap:           %'15zu bytes\n", GC_get_free_bytes() );
    fprintf( stderr, "Total bytes allocated by program: %'15zu bytes\n", GC_get_total_bytes());
    
    #if GC_TIMING
      fprintf( stderr, "Bytes allocated per second:       %'15.0f bytes/s\n"
        , ((double) GC_get_total_bytes()) / (prog_time + gc_time) );
    #endif /* GC_TIMING */
  #endif /* USE_BOEHM_GC */
}

%%]
