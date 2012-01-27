%%[8
#ifndef __BC_PRIMCONST_H__
#define __BC_PRIMCONST_H__
%%]

%%[8
#define RTS_Unit   gb_Unit
#define RTS_False  gb_False
#define RTS_True   gb_True
#define RTS_Nil    gb_Nil
#define RTS_EQ     gb_EQ
#define RTS_GT     gb_GT
#define RTS_LT     gb_LT
%%]

%%[8
#define RTS_MkBool(b)		((b) ? RTS_True : RTS_False)
%%]

%%[98
#define CAppendBinaryMode    gb_AppendBinaryMode
#define CAppendMode          gb_AppendMode
#define CReadBinaryMode      gb_ReadBinaryMode
#define CReadMode            gb_ReadMode
#define CReadWriteBinaryMode gb_ReadWriteBinaryMode
#define CReadWriteMode       gb_ReadWriteMode
#define CWriteBinaryMode     gb_WriteBinaryMode
#define CWriteMode           gb_WriteMode
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% EOF
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#endif /* __BC_PRIMCONST_H__ */
%%]
