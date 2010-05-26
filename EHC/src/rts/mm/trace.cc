%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Memory management: Trace
%%% see associated .ch file for more info.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#include "../rts.h"
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Default Trace
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
%%]
MM_Trace mm_trace ;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Combined functionality
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
void mm_trace_TraceObjects( MM_Trace* trace, Word* objs, Word nrObjs ) {
	for ( ; nrObjs > 0 ; nrObjs--, objs++ ) {
		*objs = mm_Trace_TraceObject( trace, *objs ) ;
	}
}

void mm_trace_TraceObjects2( MM_Trace* trace, Word* objs, Word nrObjs ) {

    Word nrObjs2 = 3;
    for ( ; nrObjs2 > 0 ; nrObjs2--, objs++ ) {
        Word * objs2 = (Word*) *objs;
        Word * objs3 = (Word*) *objs2;
        printf("nr: %i p1: %016llx p2: %016llx p3: %016llx v: %i \n", nrObjs2, objs, objs2, objs3, *objs3);
        objs3 = mm_Trace_TraceObject( trace, objs3 ) ;
	}
    
    	
}

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Initialization
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
void mm_init_trace() {
	// mm_trace_GBM.init( &mm_trace_GBM ) ;
}
%%]
