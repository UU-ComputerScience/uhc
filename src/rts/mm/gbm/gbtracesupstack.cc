%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Memory management: TraceSupply: GBStack
%%% see associated .ch file for more info.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#include "../../rts.h"
#include "../../bc/interpreter.h"
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% GBStack internal defs
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% GBStack internal functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% GBStack internal functions, but still externally visible (because of inlining)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% GBStack interface
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
void mm_traceSupply_GBStack_Init( MM_TraceSupply* traceSupply, MM_Malloc* memmgt, MM_Mutator* mutator ) {
	MM_TraceSupply_GBStack_Data* trsup = memmgt->malloc( sizeof(MM_TraceSupply_GBStack_Data) ) ;
	trsup->trace = mutator->trace ;
	traceSupply->data = (MM_TraceSupply_Data_Priv*)trsup ;
}

void mm_traceSupply_GBStack_Reset( MM_TraceSupply* traceSupply, Word gcStackInfo ) {
	MM_TraceSupply_GBStack_Data* trsup = (MM_TraceSupply_GBStack_Data*)traceSupply->data ;
	trsup->gcStackInfo = (GB_GCStackInfo*)gcStackInfo ;
	// trsup->gcInfo = (GB_GCInfo*)gcStackInfo ;
}
%%]

%%[8
%%]
void mm_traceSupply_GBStack_Run( MM_TraceSupply* traceSupply ) {
	MM_TraceSupply_GBStack_Data* trsup = (MM_TraceSupply_GBStack_Data*)traceSupply->data ;
	
	Word off = (trsup->gcInfo ? trsup->gcInfo->nrOfTOS_No_GCTrace : 0) ;
	// stackpointer
	WPtr s = GB_RegRelCast(Word,sp,off) ;
	// basepointer
	WPtr b = bp ;
	IF_GB_TR_ON(3,{printf("mm_traceSupply_GBStack_Run A nrOfTOS_No_GCTrace=%d (gcInfo=%p) sp=%p s=%p *s=%x\n",off,trsup->gcInfo,sp,s,*s);}) ;
	for ( ; s < (WPtr)StackAreaHigh ; s++ ) {
		if ( b == s ) {
			// meta info of stackframe needs to be skipped
			b = (WPtr)*b ;
			s += 2 ;
		}
		IF_GB_TR_ON(3,{printf("mm_traceSupply_GBStack_Run B sp=%p *s=%x\n",s,*s);}) ;
		*s = mm_Trace_TraceObject( trsup->trace, *s ) ;
	}
	IF_GB_TR_ON(3,{printf("mm_traceSupply_GBStack_Run C\n");}) ;
}

%%[8
%%]
void mm_traceSupply_GBStack_Run2( MM_TraceSupply* traceSupply ) {
	MM_TraceSupply_GBStack_Data* trsup = (MM_TraceSupply_GBStack_Data*)traceSupply->data ;
	
	Word off = (trsup->gcInfo ? trsup->gcInfo->nrOfTOS_No_GCTrace : 0) ;
	WPtr s = GB_RegRelCast(Word,sp,off) ;
	WPtr b = bp ;
	for ( ; b != NULL && b < (WPtr)StackAreaHigh ; ) {
		GB_CallInfo* ci = GB_FromBPToCallInfo(b) ;
		WPtr bptr = b ;
		// deal with part covered/ruled by gc stack info
		if ( ci->gcStackInfo != NULL ) {
			GB_GCStackInfo* info = ci->gcStackInfo ;
			Word descrInx ;
			for ( descrInx = 0 ; descrInx < info->nrDescrs ; descrInx++ ) {
				Word8 descr = info->descrs[ descrInx ] ;
				Bool mustGC = descr & 1 ;
				Word8 sz = (descr >> 1) + 1 ;
				if ( mustGC ) {
					for ( ; sz > 0 ; sz-- ) {
						bptr-- ;
						*bptr = mm_Trace_TraceObject( trsup->trace, *bptr ) ;
					}
				} else {
					bptr -= sz ;
				}
			}
		} else {
		}
		// deal with remaining part of stackframe, on top of the stack
		for ( ; s < bptr ; s++ ) {
			*s = mm_Trace_TraceObject( trsup->trace, *s ) ;
		}
		// to next frame, skipping ret info
		s = b + 2 ;
		b = (WPtr)*b ;
	}
	// remaining part
	for ( ; s < (WPtr)StackAreaHigh ; s++ ) {
		*s = mm_Trace_TraceObject( trsup->trace, *s ) ;
	}
}

%%[8
%%]
void mm_traceSupply_GBStack_Run3( MM_TraceSupply* traceSupply ) {
	MM_TraceSupply_GBStack_Data* trsup = (MM_TraceSupply_GBStack_Data*)traceSupply->data ;
	
	Word off = (trsup->gcInfo ? trsup->gcInfo->nrOfTOS_No_GCTrace : 0) ;
	WPtr s = GB_RegRelCast(Word,sp,off) ;
	WPtr b = bp ;
	IF_GB_TR_ON(3,{printf("mm_traceSupply_GBStack_Run3 BEF s=%p(lo=%p, hi=%p, used=%x, unused=%x) b=%p\n",s,(WPtr)StackAreaLow,(WPtr)StackAreaHigh,(BPtr)StackAreaHigh-(BPtr)s,(BPtr)s-(BPtr)StackAreaLow,b);}) ;
	// initial part
	for ( ; s < b ; s++ ) {
		*s = mm_Trace_TraceObject( trsup->trace, *s ) ;
	}
	for ( ; b != NULL && b < (WPtr)StackAreaHigh ; ) {
		GB_CallInfo* ci = GB_FromBPToCallInfo(b) ;
		IF_GB_TR_ON(3,{printf("mm_traceSupply_GBStack_Run3 A callinfo=%p(%s) s=%p b=%p\n",ci,ci->name,s,b);}) ;
		// skip ret info
		s = b + 2 ;
		WPtr bptr = (WPtr)*b ;
		// deal with part covered/ruled by gc stack info
		if ( ci->gcStackInfo != NULL ) {
			GB_GCStackInfo* info = ci->gcStackInfo ;
			IF_GB_TR_ON(3,{printf("mm_traceSupply_GBStack_Run3 B gcStackInfo=%p, sz=%x\n",info,info->sz);}) ;
			Word descrInx ;
			for ( descrInx = 0 ; descrInx < info->nrDescrs ; descrInx++ ) {
				Word descr = info->descrs[ descrInx ] ;
				Word sz = descr >> 1 ;
				if ( descr & 1 ) {
					for ( ; sz > 0 ; sz-- ) {
						bptr-- ;
						*bptr = mm_Trace_TraceObject( trsup->trace, *bptr ) ;
					}
				} else {
					bptr -= sz ;
				}
			}
		} else {
		}
		// deal with remaining part of stackframe, on top of the stack
		for ( ; s < bptr ; s++ ) {
			*s = mm_Trace_TraceObject( trsup->trace, *s ) ;
		}
		// to next frame
		b = (WPtr)*b ;
	}
	// remaining part
	for ( ; s < (WPtr)StackAreaHigh ; s++ ) {
		*s = mm_Trace_TraceObject( trsup->trace, *s ) ;
	}
	IF_GB_TR_ON(3,{printf("mm_traceSupply_GBStack_Run3 AFT\n");}) ;
}

%%[8
%%]
// deal with part covered/ruled by gc stack info
static inline WPtr mm_traceSupply_GBStack_RunWithStackInfo( MM_TraceSupply_GBStack_Data* trsup, GB_GCStackInfo* info, WPtr bptr ) {
	// printf("mm_traceSupply_GBStack_RunWithStackInfo BEF gcStackInfo=%p b=%p\n",info,bptr);
	if ( info != NULL ) {
		printf("mm_traceSupply_GBStack_RunWithStackInfo A gcStackInfo=%p, sz=%x ndesc=%x\n",info,info->sz,info->nrDescrs);
		Word descrInx ;
		for ( descrInx = 0 ; descrInx < info->nrDescrs ; descrInx++ ) {
			Word descr = info->descrs[ descrInx ] ;
			Word sz = descr >> 1 ;
			int i ;
			int szzz = sz ;
			WPtr bbb = bptr ;
			for ( i = -1 ; i >= -szzz ; i-- ) {
				printf( "%x ", bbb[i] ) ;
			}
			printf( "\n" ) ;
			if ( descr & 1 ) {
				printf("mm_traceSupply_GBStack_RunWithStackInfo B gcStackInfo=%p, descr[%x]=%x, tracesz=%x bptr=%x(-1=%x)\n",info,descrInx,descr,sz,bptr,*(bptr-1));
				for ( ; sz > 0 ; sz-- ) {
					bptr-- ;
					*bptr = mm_Trace_TraceObject( trsup->trace, *bptr ) ;
				}
			} else {
				printf("mm_traceSupply_GBStack_RunWithStackInfo C gcStackInfo=%p, descr[%x]=%x, skipsz=%x bptr=%x(-1=%x)\n",info,descrInx,descr,sz,bptr,*(bptr-1));
				bptr -= sz ;
			}
			for ( i = -1 ; i >= -szzz ; i-- ) {
				printf( "%x ", bbb[i] ) ;
			}
			printf( "\n" ) ;
		}
	}
	return bptr ;
	// printf("mm_traceSupply_GBStack_RunWithStackInfo AFT gcStackInfo=%p b=%p\n",info,bptr);
}

%%[8
// deal with part covered/ruled by gc stack info
static inline WPtr mm_traceSupply_GBStack_RunWithStackInfo( MM_TraceSupply_GBStack_Data* trsup, GB_GCStackInfo* info, WPtr bptr ) {
	IF_GB_TR_ON(3,{printf("mm_traceSupply_GBStack_RunWithStackInfo BEF gcStackInfo=%p b=%p\n",info,bptr);}) ;
	if ( bptr != NULL && info != NULL ) {
		IF_GB_TR_ON(3,{printf("mm_traceSupply_GBStack_RunWithStackInfo A gcStackInfo=%p, sz=%x ndesc=%x\n",info,info->sz,info->nrDescrs);}) ;
		Word descrInx ;
		for ( descrInx = 0 ; descrInx < info->nrDescrs ; descrInx++ ) {
			Word descr = info->descrs[ descrInx ] ;
			Word sz = descr >> 1 ;
			if ( descr & 1 ) {
				IF_GB_TR_ON(3,{printf("mm_traceSupply_GBStack_RunWithStackInfo B gcStackInfo=%p, descr[%x]=%x, tracesz=%x bptr=%x(-1=%x)\n",info,descrInx,descr,sz,bptr,*(bptr-1));}) ;
				for ( ; sz > 0 ; sz-- ) {
					bptr-- ;
					*bptr = mm_Trace_TraceObject( trsup->trace, *bptr ) ;
				}
			} else {
				IF_GB_TR_ON(3,{printf("mm_traceSupply_GBStack_RunWithStackInfo C gcStackInfo=%p, descr[%x]=%x, skipsz=%x bptr=%x(-1=%x)\n",info,descrInx,descr,sz,bptr,*(bptr-1));}) ;
				bptr -= sz ;
			}
		}
	}
	return bptr ;
	IF_GB_TR_ON(3,{printf("mm_traceSupply_GBStack_RunWithStackInfo AFT gcStackInfo=%p b=%p\n",info,bptr);}) ;
}
%%]

%%[8
static inline WPtr mm_traceSupply_GBStack_RunStack( MM_TraceSupply_GBStack_Data* trsup, WPtr s, WPtr bptr ) {
	for ( ; s < bptr ; s++ ) {
		*s = mm_Trace_TraceObject( trsup->trace, *s ) ;
	}
	return s ;
}
%%]

%%[8
%%]
static inline WPtr mm_traceSupply_GBStack_RunStack( MM_TraceSupply_GBStack_Data* trsup, WPtr s, WPtr bptr ) {
	printf("mm_traceSupply_GBStack_RunStack BEF s=%p b=%p\n",s,bptr);
	WPtr ssssss = s ;
	WPtr sss ;
	for ( sss = ssssss ; sss < bptr ; sss++ ) {
		printf( "%x ", *sss ) ;
	}
	printf( "\n" ) ;
	for ( ; s < bptr ; s++ ) {
		*s = mm_Trace_TraceObject( trsup->trace, *s ) ;
	}
	for ( sss = ssssss ; sss < bptr ; sss++ ) {
		printf( "%x ", *sss ) ;
	}
	printf( "\n" ) ;
	printf("mm_traceSupply_GBStack_RunStack AFT s=%p b=%p\n",s,bptr);
	return s ;
}

%%[8
%%]
void mm_traceSupply_GBStack_Run4( MM_TraceSupply* traceSupply ) {
	MM_TraceSupply_GBStack_Data* trsup = (MM_TraceSupply_GBStack_Data*)traceSupply->data ;
	
	WPtr s = sp ;
	WPtr b = bp ;
	IF_GB_TR_ON(3,{printf("mm_traceSupply_GBStack_Run3 BEF s=%p(lo=%p, hi=%p, used=%x, unused=%x) b=%p\n",s,(WPtr)StackAreaLow,(WPtr)StackAreaHigh,(BPtr)StackAreaHigh-(BPtr)s,(BPtr)s-(BPtr)StackAreaLow,b);}) ;
	// initial part, todo: check for b == NULL
	WPtr bptr = mm_traceSupply_GBStack_RunWithStackInfo( trsup, trsup->gcStackInfo, b ) ;
	s = mm_traceSupply_GBStack_RunStack( trsup, s, bptr ) ;
	for ( ; b != NULL && b < (WPtr)StackAreaHigh ; ) {
		GB_CallInfo* ci = GB_FromBPToCallInfo(b) ;
		IF_GB_TR_ON(3,{printf("mm_traceSupply_GBStack_Run3 A callinfo=%p(%s) s=%p b=%p\n",ci,ci->name,s,b);}) ;
		// skip ret info
		WPtr sss = s ;
		s = b + 2 ;
		printf("inc s=%p->%p b=%p *b=%x\n",sss,s,b,*b) ;
		bptr = (WPtr)*b ;
		bptr = mm_traceSupply_GBStack_RunWithStackInfo( trsup, ci->gcStackInfo, bptr ) ;
		// deal with remaining part of stackframe, on top of the stack
		s = mm_traceSupply_GBStack_RunStack( trsup, s, bptr ) ;
		// to next frame
		b = (WPtr)*b ;
	}
	// remaining part
	s = mm_traceSupply_GBStack_RunStack( trsup, s, (WPtr)StackAreaHigh ) ;
	IF_GB_TR_ON(3,{printf("mm_traceSupply_GBStack_Run3 AFT\n");}) ;
}

%%[8
void mm_traceSupply_GBStack_Run4( MM_TraceSupply* traceSupply ) {
	MM_TraceSupply_GBStack_Data* trsup = (MM_TraceSupply_GBStack_Data*)traceSupply->data ;
	
	WPtr s = sp ;
	WPtr b = bp ;
	IF_GB_TR_ON(3,{printf("mm_traceSupply_GBStack_Run3 BEF s=%p(lo=%p, hi=%p, used=%x, unused=%x) b=%p\n",s,(WPtr)StackAreaLow,(WPtr)StackAreaHigh,(BPtr)StackAreaHigh-(BPtr)s,(BPtr)s-(BPtr)StackAreaLow,b);}) ;
	// initial part, todo: check for b == NULL
	WPtr bptr = mm_traceSupply_GBStack_RunWithStackInfo( trsup, trsup->gcStackInfo, b ) ;
	s = mm_traceSupply_GBStack_RunStack( trsup, s, bptr ) ;
	for ( ; b != NULL && b < (WPtr)StackAreaHigh ; ) {
		GB_CallInfo* ci = GB_FromBPToCallInfo(b) ;
		IF_GB_TR_ON(3,{printf("mm_traceSupply_GBStack_Run3 A callinfo=%p(%s) s=%p b=%p\n",ci,ci->name,s,b);}) ;
		// skip ret info
		s = b + 2 ;
		bptr = (WPtr)*b ;
		bptr = mm_traceSupply_GBStack_RunWithStackInfo( trsup, ci->gcStackInfo, bptr ) ;
		// deal with remaining part of stackframe, on top of the stack
		s = mm_traceSupply_GBStack_RunStack( trsup, s, bptr ) ;
		// to next frame
		b = (WPtr)*b ;
	}
	// remaining part
	s = mm_traceSupply_GBStack_RunStack( trsup, s, (WPtr)StackAreaHigh ) ;
	IF_GB_TR_ON(3,{printf("mm_traceSupply_GBStack_Run3 AFT\n");}) ;
}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% GBStack interface object
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
MM_TraceSupply mm_traceSupply_GBStack =
	{ NULL
	, &mm_traceSupply_GBStack_Init
	, MM_Undefined
	, &mm_traceSupply_GBStack_Reset
	, &mm_traceSupply_GBStack_Run4
	, MM_Undefined
	} ;
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% GBStack dump
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#ifdef TRACE
void mm_traceSupply_GBStack_Dump( MM_TraceSupply* traceSupply ) {
}
#endif
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% GBStack test
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#ifdef TRACE
void mm_traceSupply_GBStack_Test() {
}
#endif
%%]

