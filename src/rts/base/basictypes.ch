%%[8
#ifndef __BASE_BASICTYPES_H__
#define __BASE_BASICTYPES_H__
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Low level and basic types
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
typedef int Bool ;

#define True		1
#define False		0
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Signed & unsigned ints of explicit sizes
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
typedef uint64_t Word64  ;
typedef  int64_t SWord64 ;
typedef  int64_t Int64   ;

typedef uint32_t Word32  ;
typedef  int32_t SWord32 ;
typedef  int32_t Int32   ;

typedef uint16_t Word16  ;
typedef  int16_t SWord16 ;
typedef  int16_t Int16   ;

typedef uint8_t  Word8   ;
typedef  int8_t  SWord8  ;
typedef  int8_t  Int8    ;

typedef float    Float   ;
typedef double   Double  ;
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Unions for allowing to pass float/double as pure bitpatterns, no conversion
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[97
typedef union {
  Float			f ;
  Word32		w ;
} Float_Word32 ;

typedef union {
  Double		d ;
  Word64		w ;
} Double_Word64 ;
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Word, with relative sizes encoded in their varieties
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Requirement: pointers should fit into a Word

%%[8

#if USE_64_BITS
typedef Word64   Word  ;
typedef SWord64  SWord ;

typedef Word32   HalfWord  ;
typedef SWord32  SHalfWord ;

typedef Word16   QuartWord  ;
typedef SWord16  SQuartWord ;

#define Word_SizeInBits			Word64_SizeInBits		
#define Word_SizeInBits_Log		Word64_SizeInBits_Log

#define Word_SizeInBytes		Word64_SizeInBytes		
#define Word_SizeInBytes_Log	Word64_SizeInBytes_Log

#else

typedef Word32   Word  ;
typedef SWord32  SWord ;

typedef Word16   HalfWord  ;
typedef SWord16  SHalfWord ;

typedef Word8    QuartWord  ;
typedef SWord8   SQuartWord ;

#define Word_SizeInBits			Word32_SizeInBits		
#define Word_SizeInBits_Log		Word32_SizeInBits_Log	

#define Word_SizeInBytes		Word32_SizeInBytes		
#define Word_SizeInBytes_Log	Word32_SizeInBytes_Log

#endif
%%]

%%[8
typedef SWord    Int ;
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Pointer
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
typedef	void*		Ptr  ;
typedef	Word8*		BPtr ;
typedef	Word*		WPtr ;
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% EOF
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#endif /* __BASE_BASICTYPES_H__ */
%%]

