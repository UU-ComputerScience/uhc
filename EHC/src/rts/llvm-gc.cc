%%[8
#include <inttypes.h>

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

%%]
