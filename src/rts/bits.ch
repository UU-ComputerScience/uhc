%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Bit fiddling
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#define Bits_Pow2(ty,s)				((Cast(ty,1)) << (s))
#define Bits_Size2LoMask(ty,s)		(Bits_Pow2(ty,s) - 1)
#define Bits_Size2HiMask(ty,s)		(~ Bits_Size2LoMask(ty,s))

#define Bits_ExtrFromTo(ty,x,f,t)	((Bits_Size2LoMask(ty,(t)-1) & Bits_Size2HiMask(ty,f) & (x)) >> (t))
#define Bits_ExtrFromToSh(ty,x,f,t)	(Bits_ExtrFromTo(ty,x,f,t) >> (t))
%%]

