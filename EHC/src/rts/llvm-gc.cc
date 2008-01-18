%%[8
#include <inttypes.h>

#include "config.h"
#include "gc.h"

void llvmgc_init()
{
  #if USE_BOEHM_GC
  int dummy;
  uintptr_t stack_bottom = (uintptr_t)&dummy;
  //stack_bottom += 4095;
  //stack_bottom &= ~4095;
  GC_stackbottom = (char*)stack_bottom;
  GC_INIT();
  #endif
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
