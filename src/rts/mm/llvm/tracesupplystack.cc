%%[8
#include "../../rts.h"
%%]



%%[8

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

MM_Trace * mmtrace;

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Internal functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Interface functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
void mm_traceSupplyStack_llvm_Init( MM_TraceSupply* traceSupply, MM_Malloc* memmgt, MM_Mutator* mutator ) 
{
    //printf("mm_traceSupplyStack_llvm_Init\n");

    mmtrace = mutator->trace ;

}

void mm_traceSupplyStack_llvm_Reset( MM_TraceSupply* traceSupply, Word gcStackInfo ) 
{
    //printf("mm_traceSupplyStack_llvm_Reset\n");


}

void mm_traceSupplyStack_llvm_Run( MM_TraceSupply* traceSupply )
{

    //printf("mm_traceSupplyStack_llvm_Run\n");



    int32_t    i, num_roots;
    Word       *root;
    Word       con;
    struct StackEntry   *entry = llvm_gc_root_chain;

	
    //printf("|*****************************************************************\n");
    //printf("|** Running a stack walk \n");
    //printf("| [0x%016llx] llvm_gc_root_chain\n", (unsigned int)entry);
    
    while (entry)
    {
        num_roots = entry->Map->NumRoots;
        //printf("| [0x%016llx] %d root(s)\n", (unsigned int)entry, num_roots);
        for (i = 0; i < num_roots; i++)
        {
            root = (Word *) entry->Roots[i];
   
            if (root == NULL) {
                //printf("| ... [%d] 0x%016llx\n", i, (unsigned int)root );

            } else {
          	    //printf("check? p: %016llx v: %d \n", &root, root);

                //printf("| ... [%d] 0x%016llx, con: %lld \n", i, (unsigned int)root, *root );
           	    Word * rootUpd = mm_Trace_TraceObject( mmtrace, root );
           	    //printf("            stackroot: 0x%016llx changed to: 0x%016llx con: %i \n", root, rootUpd, *rootUpd);
           	    entry->Roots[i] = rootUpd;

            }

        }
        //printf("\n");


        entry = entry->Next;
    }

    //printf("|*****************************************************************\n");


    return;
    //exit(1);
}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% MM_TraceSupply interface object
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
MM_TraceSupply mm_traceSupplyStack_llvm =
	{ NULL
	, &mm_traceSupplyStack_llvm_Init
	, MM_Undefined
	, &mm_traceSupplyStack_llvm_Reset
	, &mm_traceSupplyStack_llvm_Run
	, MM_Undefined
	} ;
%%]
