\chapter*{Abstract}
Type classes are perhaps the most exciting feature of Haskell's type system.
Although type classes were only proposed as a solution for overloading identifiers, they have become an active research topic where experimental type class extensions are still being proposed.
On top of that, the underlying principles are also used to encode various other extensions, such as extensible records, implicit parameters, and subtyping. 
However, implementing type classes and type class extensions is not a trivial task.
It is not only a matter of type checking to resolve overloading, but also evidence for overloaded identifiers has to be inserted.
A uniform approach to easily formulate type class extensions side by side is missing.
In addition, error messages concerning type classes are difficult to understand or sometimes not present at all.

We propose a constraint-based framework for the resolution of overloading.
Assumptions and proof obligations are explicitly encoded into the constraint language of this framework.
Furthermore, type class extensions can be easily formulated using Constraint Handling Rules (CHRs).
The confluence requirement is circumvented by using only propagation CHRs, and by specifying design decisions in the form of heuristics.
Using the resulting framework, we show how various context reduction strategies can be specified side by side.
Furthermore, we explain how scoped instances and overlapping instances are naturally supported. 
We also show how functional dependencies can be supported using the existing translation into CHRs.
