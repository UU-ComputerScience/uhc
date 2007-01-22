%%[8
#include "rts.h"
%%]

%%[8
void rts_error( char* msg )
{
	fprintf( stderr, "error: %s\n", msg ) ;
	exit( 1 ) ;
}

void gb_error( char* msg )
{
	fprintf( stderr, "error: %s\n", msg ) ;
	gb_exit( 1 ) ;
}

void gb_panic( char* msg )
{
	fprintf( stderr, "grinbc: panic: %s\n", msg ) ;
	gb_exit( 1 ) ;
}

void gb_panic1_1( char* msg, int i )
{
	fprintf( stderr, "grinbc: panic: %s: 0x%x\n", msg, i ) ;
	gb_exit( 1 ) ;
}

void gb_panic2( char* msg1, char* msg2 )
{
	fprintf( stderr, "grinbc: %s panic: %s\n", msg1, msg2 ) ;
	gb_exit( 1 ) ;
}

void gb_panic2_1( char* msg1, char* msg2, int i )
{
	fprintf( stderr, "grinbc: %s panic: %s: 0x%x\n", msg1, msg2, i ) ;
	gb_exit( 1 ) ;
}

void gb_panic2_2( char* msg1, char* msg2, int i1, int i2 )
{
	fprintf( stderr, "grinbc: %s panic: %s: 0x%x: 0x%x\n", msg1, msg2, i1, i2 ) ;
	gb_exit( 1 ) ;
}

%%]
