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
(1) pages, groupwise aligned/sized of 2^k, abstracting from malloc, multiple mallocs allowed,
    currently buddy system
(2) arbitrary sized allocation, managed by some policy: GC, free list, fixed, ...
    Uses areas of contiguously allocated pages.

%%[8
#include "common.h"
#include "pages.h"
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Initialization
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
extern void mm_init() ;
%%]
