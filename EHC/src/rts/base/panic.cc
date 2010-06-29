%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Utilities for error/panic halting
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#include "../rts.h"
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Exit & panic
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
void rts_exit( int exitCode )
{
	exit( exitCode ) ;
}

void rts_error( char* msg )
{
	fprintf( stderr, "error: %s\n", msg ) ;
	rts_exit( 1 ) ;
}

void rts_panic1_0( char* msg )
{
	fprintf( stderr, "panic: %s\n", msg ) ;
	rts_exit( 1 ) ;
}

void rts_panic1_1( char* msg, int i )
{
	fprintf( stderr, "panic: %s: 0x%x\n", msg, i ) ;
	rts_exit( 1 ) ;
}

void rts_panic2_1( char* msg1, char* msg2, int i )
{
	fprintf( stderr, "%s panic: %s: 0x%x\n", msg1, msg2, i ) ;
	rts_exit( 1 ) ;
}

%%]

