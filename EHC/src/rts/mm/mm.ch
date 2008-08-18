%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Memory management
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

The design is inspired by:
- http://jikesrvm.org/MMTk,
  see the (somewhat sketchy) guide for an overview, otherwise the code in the jikes distribution itself
- http://cs.anu.edu.au/people/Steve.Blackburn/,
  the main creator of MMTk, with a lot of relevant papers

However, it deviates sufficiently to warrant a sketch of the overall design:

Layering
--------
from bottom to top layer:
(0) memory mgt provided by OS, currently malloc is used, as it is most widely available
(1) Pages, groupwise aligned/sized of 2^k, abstracting from malloc, multiple mallocs allowed,
    currently buddy system
(2) arbitrary sized allocation, managed by some policy: GC, free list, fixed, ...
    Uses areas of contiguously allocated pages.
    
Layer (2) is further subdivided into sublayers, each with a particular responsibility:
(2.a) Fragment, directly based on pages, offering identification based on its start address a:
      a >> fragment size.
      This is to be used for 'remembered sets' required for generational GC.
(2.b) Space, a set of Fragments, offering flexibility in size, granularity.
      Each Space is treated as a unit for the GC wrt tracing & collection
(2.c) Increment, a set of ordered Spaces, each following the same GC policy.
      The ordering provides generations, but withing the one policy.
(2.d) Belt, a set of ordered Increments, each with a (possibly) different GC policy.
      This is the toplevel.

Efficiency
----------
Fragment, Space, Increment and Belt offer the same interface
'Allocator'. The implementation is OO like organized in that an
interface type exists, with multiple implementations and private data.
As this conflicts with efficiency each implementation provides fast path
access by means of inlining, macros or a combination of these. The
consequence is that for the system as a whole only one combination of
policies etc is configured. Non inlined access always remains possible,
but globally one combination must be configured.

Current implementations
-----------------------
as of 20080817.

Pages:
- Buddy pages

Fragment:

Space:

Increment:

Belt:

%%[8
#include "config.h"
#include "common.h"
#include "pages.h"
#include "allocator.h"
#include "allocator/listoffree.h"
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Initialization
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
extern void mm_init() ;
%%]
