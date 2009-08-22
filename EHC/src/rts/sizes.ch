%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Hardcoded / configured sizes + types
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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

%%[8
#define Word64_SizeInBits			64
#define Word64_SizeInBits_Log		6

#define Word64_SizeInBytes			8
#define Word64_SizeInBytes_Log		3

#define Word32_SizeInBits			32
#define Word32_SizeInBits_Log		5

#define Word32_SizeInBytes			4
#define Word32_SizeInBytes_Log		2

#define Word16_SizeInBits			16
#define Word16_SizeInBits_Log		4

#define Word16_SizeInBytes			2
#define Word16_SizeInBytes_Log		1

#define Word8_SizeInBits			8
#define Word8_SizeInBits_Log		3

#define Word8_SizeInBytes			1
#define Word8_SizeInBytes_Log		0

%%]

%%[8
#define Int64_SizeInBits			Word64_SizeInBits
#define Int32_SizeInBits			Word32_SizeInBits
#define Int16_SizeInBits			Word16_SizeInBits
#define Int8_SizeInBits				Word8_SizeInBits
%%]

%%[8
#define Int64_MinValue				INT64_MIN
#define Int64_MaxValue				INT64_MAX
#define Int32_MinValue				INT32_MIN
#define Int32_MaxValue				INT32_MAX
#define Int16_MinValue				INT16_MIN
#define Int16_MaxValue				INT16_MAX
#define Int8_MinValue				INT8_MIN
#define Int8_MaxValue				INT8_MAX

#define Word64_MinValue				0
#define Word64_MaxValue				UINT64_MAX
#define Word32_MinValue				0
#define Word32_MaxValue				UINT32_MAX
#define Word16_MinValue				0
#define Word16_MaxValue				UINT16_MAX
#define Word8_MinValue				0
#define Word8_MaxValue				UINT8_MAX
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

%%[8
#define Byte_SizeInBits			8
#define Byte_SizeInBits_Log		3
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Pointer
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
typedef	void*		Ptr  ;
typedef	Word8*		BPtr ;
typedef	Word*		WPtr ;
%%]
