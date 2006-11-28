%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Very basic types
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
typedef int Bool ;

#define True		1
#define False		0
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Utilities
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#define Cast(ty,val)		((ty)(val))
%%]

%%[8
extern void error( char* msg ) ;
extern void panic( char* msg ) ;
extern void panic1_1( char* msg, int i ) ;
extern void panic2_1( char* msg1, char* msg2, int i ) ;
extern void panic2_2( char* msg1, char* msg2, int i1, int i2 ) ;
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
#define MemCopyForward(ty,frPtr,frEndPtr,toPtr) \
									for ( ; Cast(ty*,frPtr) < Cast(ty*,frEndPtr) ; ) { \
										*((Cast(ty*,toPtr))++) = *((Cast(ty*,frPtr))++) ; \
									}

#define MemCopyBackward(ty,frPtr,frEndPtr,toPtr) \
									for ( ; Cast(ty*,frPtr) >= Cast(ty*,frEndPtr) ; ) { \
										*(--(Cast(ty*,toPtr))) = *(--(Cast(ty*,frPtr))) ; \
									}
