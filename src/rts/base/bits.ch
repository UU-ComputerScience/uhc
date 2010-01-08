%%[8
#ifndef __BASE_BITS_H__
#define __BASE_BITS_H__
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Bit size info
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#define BitsPerByte						8
#define BitsPerInt64					(BitsPerByte * sizeof(int64_t))
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Bit fiddling
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#define Bits_Pow2(ty,s)					(((ty)1) << (s))
#define Bits_Size2LoMask(ty,s)			(Bits_Pow2(ty,s) - 1)
#define Bits_Size2HiMask(ty,s)			(~ Bits_Size2LoMask(ty,s))

#define Bits_MaskFrom(ty,f)				Bits_Size2HiMask(ty,f)
#define Bits_MaskTo(ty,t)				Bits_Size2LoMask(ty,(t)+1)
#define Bits_MaskFromTo(ty,f,t)			(Bits_MaskTo(ty,t) & Bits_MaskFrom(ty,f))

#define Bits_ExtrTo(ty,x,t)				(Bits_MaskTo(ty,t) & (x))
#define Bits_ExtrFrom(ty,x,f)			(Bits_MaskFrom(ty,f) & (x))
#define Bits_ExtrFromTo(ty,x,f,t)		(Bits_MaskFromTo(ty,f,t) & (x))
#define Bits_ExtrFromSh(ty,x,f)			((x) >> (f))
#define Bits_ExtrFromToSh(ty,x,f,t)		(Bits_ExtrTo(ty,x,t) >> (f))

%%]

%%[8
#define EntierLogUpShrBy(x,m)			((((x)-1)>>(m))+1)
#define EntierLogUpBy(x,m)				(EntierLogUpShrBy(x,m)<<(m))
#define EntierLogDownShrBy(x,m)			((x)>>(m))
#define EntierLogDownBy(x,m)			(EntierLogDownShrBy(x,m)<<(m))

#define EntierUpDivBy(x,m)				(((x)-1)/(m)+1)
#define EntierUpBy(x,m)					(EntierUpDivBy(x,m)*(m))
#define EntierDownDivBy(x,m)			((x)/(m))
#define EntierDownBy(x,m)				(EntierDownDivBy(x,m)*(m))
%%]

%%[8
#define Bits_MaxSInt(ty,szBits,nBits)	((~ Cast(ty,0)) >> ((szBits) - (nBits)+1))
#define Bits_MinSInt(ty,szBits,nBits)	((~ Cast(ty,0)) & Bits_Size2HiMask(ty,(nBits)-1))

#define Bits_MaxUInt(ty,szBits,nBits)	((~ Cast(ty,0)) >> ((szBits) - (nBits)))
#define Bits_MinUInt(ty,szBits,nBits)	Cast(ty,0)

%%]

%%[8
#endif /* __BASE_BITS_H__ */
%%]

