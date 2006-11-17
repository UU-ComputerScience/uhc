%%[8
#include "rts.h"
%%]

%%[8
void panic( char* msg, int i )
{
	fprintf( stderr, "panic: %s: 0x%x\n", msg, i ) ;
	exit( 1 ) ;
}
%%]
