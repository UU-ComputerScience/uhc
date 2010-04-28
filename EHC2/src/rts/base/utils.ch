%%[8
#ifndef __BASE_UTILS_H__
#define __BASE_UTILS_H__
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Utilities
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Cast
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#define Cast(ty,val)		((ty)(val))
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Exit & panic
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
extern void gb_error( char* msg ) ;
extern void gb_panic( char* msg ) ;
extern void gb_panic1_1( char* msg, int i ) ;
extern void gb_panic2( char* msg1, char* msg2 ) ;
extern void gb_panic2_1( char* msg1, char* msg2, int i ) ;
extern void gb_panic2_2( char* msg1, char* msg2, int i1, int i2 ) ;
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Copying
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Forward and backward variant of copying.
Copying is done from frPtr to toPtr, updating both in the process. Stops at frEndPtr.
Backwards startingpoint starts after the first location (so predecrement is/can be used).

%%[8
#define MemCopyForward(frPtr,frEndPtr,toPtr) \
									for ( ; (frPtr) < (frEndPtr) ; ) { \
										*((toPtr)++) = *((frPtr)++) ; \
									}

#define MemCopyBackward(frPtr,frEndPtr,toPtr) \
									for ( ; (frPtr) > (frEndPtr) ; ) { \
										*(--(toPtr)) = *(--(frPtr)) ; \
									}

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Swap
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#define SwapPtr( ty, x, y ) { ty _xy = x ; x = y ; y = _xy ; }
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Max, Min
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#define MAX(x,y)			((x) > (y) ? (x) : (y))
#define MIN(x,y)			((x) < (y) ? (x) : (y))
%%]

%%[8
static inline Word maxWord( Word x, Word y ) {
	if ( x > y ) return x ;
	return y ;
}

static inline Word minWord( Word x, Word y ) {
	if ( x < y ) return x ;
	return y ;
}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% First non-zero bit, higher power of 2
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
// from lsb.
// return Word_SizeInBits when all bits are zero, otherwise the corresponding position counted from the lsb
extern int firstNonZeroBit( Word w ) ;

// from msb.
// return -1 when all bits are zero, otherwise the corresponding position counted from the lsb
extern int firstNonZeroMsBit( Word w, int startAt ) ;

// first power of 2 higher or equal
// return its log2
extern int firstHigherPower2( Word w ) ;
%%]


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% EOF
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#endif /* __BASE_UTILS_H__ */
%%]

