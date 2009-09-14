%%[8
#ifndef __BASE_SIZES_H__
#define __BASE_SIZES_H__
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Hardcoded / configured sizes + types
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Signed & unsigned ints of explicit sizes
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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

%%[8
#define Byte_SizeInBits			8
#define Byte_SizeInBits_Log		3
%%]


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% EOF
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#endif /* __BASE_SIZES_H__ */
%%]
