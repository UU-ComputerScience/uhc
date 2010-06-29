%%[8
#ifndef __TIMING_H__
#define __TIMING_H__
%%]

%%[8
/**
 * Module to measure the amount of time spend by the program and time 
 * spend by the garbage collector.
 *
 * The idea is to start by initializing prog_base with clock(). When a context
 * switch occurs between the user code and the GC, we take the difference
 * between the current clock() and the prog_base. This difference is stored in
 * prog_time. Furthermore we initialize gc_base with the current clock().
 *
 * A context switch from GC to user code the process is repeated by storing
 * the difference of the current clock() with gc_base and initializing 
 * prog_base again. 
 */
#include <time.h>

/**
 * Bases of measuring time in user code and garbage collection, initialized with
 * clock() on context changes.
 */
clock_t prog_base;
clock_t gc_base;

/**
 * Doubles to store the accumulated time in.
 */
double prog_time;
double gc_time;

/**
 * Call to initialize the timing module. This starts the timing of the user
 * code.
 */
void timing_initialize();

/**
 * Stop the user code timer and start the gc timer.
 */
void switch_to_gc();

/**
 * Stop the gc timer and start the user code timer.
 */
void switch_to_user_code();

/**
 * Finish the timing.
 */
void timing_finalize();
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% EOF
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#endif /* __TIMING_H__ */
%%]
