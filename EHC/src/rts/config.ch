/* src/rts/config.ch.  Generated from config.ch.in by configure.  */
%%[8
#ifndef __CONFIG_H__
#define __CONFIG_H__

#include "MachDeps.h"
%%]


%%[8
/* use Boehm's GC */
#define USE_BOEHM_GC 0

/* size of Pointer */
#define SIZEOF_POINTER	SIZEOF_INTPTR_T

/* size of GrWord */
#define SIZEOF_GRWORD	SIZEOF_INTPTR_T

%%]

%%[97
// Mutually exclusive use a MP (Multiple Precision) library.

/* use GMP */
#define USE_GMP 0

/* use LTM */
#define USE_LTM 1
%%]

%%[8
/* endianness */

#define BIGENDIAN 0
#define LITTLEENDIAN 1

%%]

%%[8
/* Timing of GC */
#define GC_TIMING 0
%%]



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Misc
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
// the empty PRIM define is used to mark exported functions from prim.c, used to automatically generate prim.h
#define PRIM
%%]



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% EOF
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#endif /* __CONFIG_H__ */
%%]
