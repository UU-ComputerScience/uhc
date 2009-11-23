%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Utilities
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#include "../rts.h"
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Exit * panic
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8

#ifdef __UHC_TARGET_BC__

void gb_exit(int);

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
#endif
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% First non-zero bit, higher power of 2
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
int firstNonZeroBit( Word w ) {
	int i ;
	for ( i = 0 ; i < Word_SizeInBits && (! (w & ((Word)1L))) ; i++, w >>= 1 ) ;
	return i ;
}
%%]

%%[8
int firstNonZeroMsBit( Word w, int startAt ) {
	// IF_GB_TR_ON(3,{printf("firstNonZeroMsBit w=%llx startAt=%x\n", w, startAt);}) ;
	int i ;
	for ( i = startAt ; i >= 0 && (! (w & (((Word)1L)<<i))) ; i-- ) ;
	// IF_GB_TR_ON(3,{printf("firstNonZeroMsBit i=%x\n", i);}) ;
	return i ;
}
%%]

%%[8
int firstHigherPower2( Word w ) {
	int i ;
	i = firstNonZeroMsBit( w, Word_SizeInBits-1 ) ;
	return ( w ^ (((Word)1L)<<i) ? i+1 : i ) ;
}
%%]

