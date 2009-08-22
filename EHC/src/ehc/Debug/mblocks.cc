From HighWaterMark by Bernie Pope.

%%[102

#include "stdio.h"
#include <Stg.h>
#include <RtsTypes.h>
#include <RtsAPI.h>
#include <Constants.h>
#include <Block.h>

extern lnat mblocks_allocated;

lnat get_mblocks_allocated (void)
{
   return (mblocks_allocated);
}

int get_mblock_size (void)
{
   return (MBLOCK_SIZE);
}

%%]

