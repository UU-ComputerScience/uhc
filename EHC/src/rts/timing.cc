%%[8
#include <stdio.h>

#include "timing.h"

void timing_initialize()
{
	prog_base = clock();
	gc_base   = (clock_t) -1;	
	
	prog_time = 0;
	gc_time   = 0;
}

void switch_to_gc()
{
	clock_t now = clock();
	prog_time  += difftime(now, prog_base) / CLOCKS_PER_SEC;
	gc_base     = clock();	
}

void switch_to_user_code()
{
	clock_t now = clock();
	gc_time    += difftime(now, gc_base) / CLOCKS_PER_SEC;	
	prog_base   = clock();	
}

void timing_finalize()
{
	prog_time += difftime(clock(),prog_base) / CLOCKS_PER_SEC;
}
%%]