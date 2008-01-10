%%[8
#include "gc.h"

void llvmgc_init()
{

  GC_INIT();
}

void* llvmgc_malloc( size_t nbytes )
{

  return GC_MALLOC( nbytes );
}

void* llvmgc_malloc_uncollectable( size_t nbytes )
{

  return GC_MALLOC_UNCOLLECTABLE( nbytes );
}

%%]
