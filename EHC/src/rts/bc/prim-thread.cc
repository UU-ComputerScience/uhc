%%[8
#include "../rts.h"
%%]

%%[99
%%]

%%[8.dummyForLinker
int dummy_thread ;
%%]

%%[99 -8.dummyForLinker
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Primitives + plain functions for grin bytecode interpreter, those related to threads
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Just telling the outside world we do not have threads

%%[99
PRIM Word rtsSupportsBoundThreads()
{
	return gb_False ;
}
%%]

