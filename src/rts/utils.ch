%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Utilities
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#define Cast(ty,val)		((ty)(val))
%%]

%%[8
extern void panic( char* msg, int i ) ;
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Copying
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Forward and backward variant of copying

%%[8
#define MemCopyForward(ty,frPtr,frEndPtr,toPtr) \
									for ( ; Cast(ty*,frPtr) < Cast(ty*,frEndPtr) ; ) { \
										*((Cast(ty*,toPtr))++) = *((Cast(ty*,frPtr))++) ; \
									}

#define MemCopyBackward(ty,frPtr,frEndPtr,toPtr) \
									for ( ; Cast(ty*,frPtr) >= Cast(ty*,frEndPtr) ; ) { \
										*(--(Cast(ty*,toPtr))) = *(--(Cast(ty*,frPtr))) ; \
									}
%%]
