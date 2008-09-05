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
      The ordering provides generations, within the one policy.
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
as of 20080819.

Pages:
- Buddy pages, on top of system malloc/free

Fragment:
- Implicitly as Pages part of a Space

Space:
- A multiple Fragment (discontiguous Pages) implementation, offering growing+shrinking only
- CopySpace

Increment:

Belt:

Other Allocators:
- List of Free (LOF), on top of Pages


Flexibility
-----------
The design allows for a fair degree of flexibility. All building blocks
are accessible by means of structures, in almost OO alike style.
Replacement with different implementations (for a given interface) is
therefore easy.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Imports
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Order of imports is important because of usage dependencies between types.

%%[8
#include "config.h"
#include "common.h"
#include "basic/flexarray.h"
#include "basic/dll.h"
#include "basic/deque.h"
#include "basic/rangemap.h"
#include "pages.h"
#include "space.h"
#include "allocator.h"
#include "roots.h"
#include "collector.h"
#include "trace.h"
#include "tracesupply.h"
#include "mutator.h"
#include "plan.h"
#include "pages/buddy.h"
#include "space/fragment.h"
#include "space/copyspace.h"
#include "allocator/listoffree.h"
#include "allocator/bump.h"
#include "tracesupply/buffer.h"
#include "tracesupply/group.h"
#include "tracesupply/supplyroots.h"
#include "gbm/gbmutator.h"
#include "gbm/gbtrace.h"
#include "gbm/gbtracesupregs.h"
#include "gbm/gbtracesupstack.h"
#include "semispace/sscollector.h"
#include "semispace/ss.h"
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Initialization
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
extern void mm_init() ;
%%]
