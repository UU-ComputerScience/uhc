%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Hardcoded / configured sizes + types
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Word
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Requirement: pointers should fit into a Word

%%[8
#if USE_64_BITS
typedef uint64_t Word  ;
typedef  int64_t SWord ;

typedef uint32_t HalfWord  ;
typedef  int32_t SHalfWord ;

typedef uint16_t QuartWord  ;
typedef  int16_t SQuartWord ;

#define Word_SizeInBits			64
#define Word_SizeInBits_Log		6

#define Word_SizeInBytes		8
#define Word_SizeInBytes_Log	3

#else

typedef uint32_t Word  ;
typedef  int32_t SWord ;

typedef uint16_t HalfWord  ;
typedef  int16_t SHalfWord ;

typedef uint8_t QuartWord  ;
typedef  int8_t SQuartWord ;

#define Word_SizeInBits			32
#define Word_SizeInBits_Log		5

#define Word_SizeInBytes		4
#define Word_SizeInBytes_Log	2

#endif
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
typedef	uint8_t*	BPtr ;
typedef	Word*		WPtr ;
%%]
