%%[8

/* 20110913: replaced by rts.h include hereafter, just to make it compile
#include <inttypes.h>
#include <stdint.h>
#include <stdio.h>
#include <locale.h>
//#include <unistd.h>

#include "config.h"
#if USE_BOEHM_GC
#include "gc/gc.h"
#endif
*/
#include "rts.h"

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

// -----------------------------------------------------------------------------
// Shadow-stack semispace collector
// -----------------------------------------------------------------------------

// LLVM shadow stack definitions

typedef unsigned char byte;
typedef unsigned short ushort;

struct FrameMap
{
    int32_t NumRoots;               //< Number of roots in stack frame.
    int32_t NumMeta;                //< Number of metadata entries. May be < NumRoots.
    const void *Meta[0];            //< Metadata for each root.
};

struct StackEntry
{
    struct StackEntry *Next;        //< Link to next stack entry (the caller's).
    const struct FrameMap *Map;     //< Pointer to constant FrameMap.
    void *Roots[0];                 //< Stack roots (in-place array).
};

struct StackEntry *llvm_gc_root_chain;

struct FDescr 
{
   int32_t num_fields;
   int32_t max_fields;
   unsigned char is_prim;
};

// -----------------------------------------------------------------------------
// semispace code 

#if SIZEOF_INTPTR_T == 8
typedef int64_t payload_field;
#else
typedef int32_t payload_field;
#endif



static byte *f_heap, *f_limit;
static byte *t_heap, *t_limit;
static byte *t_alloc;

static int32_t fdescr_sz;
static struct FDescr *fdescr;

void printFDescr() {
    
    // static int32_t fdescr_sz;
    // static struct FDescr *fdescr;

    if (fdescr){
        printf("************************************************\n"); 
        printf("*** FDescr table: \n"); 

        int32_t i = 0;    
        struct FDescr cur;
        while (i < fdescr_sz){
            cur = fdescr[i];
            
            printf("Con: %i | fields: %i | isPrim: %i \n", i, cur.num_fields , cur.is_prim);
            
            i++;
        }
        printf("************************************************\n"); 
    }

}

void gc_ss_init(int32_t fsz, struct FDescr *fd) { //  struct FDescr *fd

    #if SIZEOF_INTPTR_T == 8
    printf("Garbage collector running in 64 bit mode \n");
    #else
    printf("Garbage collector running in 32 bit mode \n");
    #endif

    fdescr_sz = fsz;
    fdescr    = fd;

    printFDescr();    
        
    
    unsigned int heapsz = 1024;

    printf("Initializing semi-space heap (%d bytes per space)\n", heapsz);

    f_heap = (byte*)malloc(heapsz);
    memset(f_heap, 0, heapsz);
    f_limit = f_heap + heapsz - 1;

    t_heap = (byte*)malloc(heapsz);
    memset(t_heap, 0, heapsz);
    t_limit = t_heap + heapsz - 1;
    t_alloc = t_heap;
}

void gc_collect(){
    return;
}

void* gc_ss_alloc(unsigned int sz) {

    // printf("gc_alloc(%d)", sz);

    byte *res;

    if (t_alloc + sz > t_limit)
    {
        // Need to collect
        printf(" - not enough free heap space, forcing a collection ...\n");
        gc_collect();

        if (t_alloc + sz > t_limit)
        {
            printf("Fatal: not enough heap space after collection. Available space is %d bytes, need %d bytes\n", t_limit-t_alloc+1, sz);
            exit(-1);
        }
    }

    res = t_alloc;
    t_alloc += sz;
    // printf(" - new object at 0x%08x, heap size now %d bytes\n", (unsigned int)res, t_alloc-t_heap);

    return res;
}

// Shadow-stack walker function
void sswalker() {

    int32_t             i, num_roots;
    payload_field       *root;
    payload_field       con;
    struct StackEntry   *entry = llvm_gc_root_chain;

    printf("|*****************************************************************\n");
    printf("|** Running a stack walk \n");
    printf("| [%p] llvm_gc_root_chain\n", entry);
    
    while (entry)
    {
        num_roots = entry->Map->NumRoots;
        printf("| [%p] %d root(s)\n", entry, num_roots);
        for (i = 0; i < num_roots; i++)
        {
            root = (payload_field *) entry->Roots[i];
   
            if (root == NULL) {
                printf("| ... [%d] %p\n", i, root );

            } else {
                printf("| ... [%d] %p, con: %lld \n", i, root, *root );
            }

        }
        printf("\n");


        entry = entry->Next;
    }

    printf("|*****************************************************************\n");
    

}

void rawheapwalker() {

    printf("|*****************************************************************\n");
    printf("|** Running a RAW heap walk \n");

    payload_field * scanptr = (payload_field *)t_heap;
    
    while ((void*)scanptr < (void*)t_alloc) {
        printf("| [%p]\n", scanptr);
        scanptr++;
    }

    printf("|*****************************************************************\n");
}

void heapwalker() {

/*
    printf("|*****************************************************************\n");
    printf("|** Running a heap walk \n");

    payload_field * scanptr = t_heap;
    
    while (scanptr < t_alloc) {
        // printf("| [0x%08x]: 0x%08x\n", scanptr, *scanptr);

        struct FDescr cur_descr = fdescr[*scanptr];
        
        printf("Con: %lld, fields: %i, isPrim: %i \n", *scanptr, cur_descr.num_fields, cur_descr.is_prim );

        scanptr++;

        int field_counter;
        for(field_counter = 0; field_counter < cur_descr.num_fields; field_counter++){
            printf("TEST\n");
            // printf("Field: 0x%08x\n", *scanptr);
            scanptr++;
        }
        

    }

    printf("|*****************************************************************\n");

*/

    rawheapwalker();

}




%%]
