#include "rts.h"

/* Test primitive
*/

int primXXXInt(int x, int y, int z)
{   return x+y+z;
}

/* Int related primitives
*/

int primAddInt(int x, int y)
{   return x+y;
}

int primSubInt(int x, int y)
{   return x-y;
}
int primMulInt(int x, int y)
{   return x*y;
}
int primDivInt(int x, int y)
{   return x/y;
}
int primModInt(int x, int y)
{   return x%y;
}


/* In the following 3 functions, only the constructor of Bool is returned.

   The arity of the constructor should also be returned, but we can return only one value.
   So, the arity will obtain a random value.
   Luckily, the arity is never used anywhere.
   (So I wonder why it is stored at all!)
*/

/* Ord Int related primitives
*/

int primGtInt(int x, int y)
{   if (x>y)
        return ((Pointer)global_True)[0];
    return ((Pointer)global_False)[0];
}
int primLtInt(int x, int y)
{   if (x<y)
        return ((Pointer)global_True)[0];
    return ((Pointer)global_False)[0];
}

/* Eq Int related primitives
*/

int primEqInt(int x, int y)
{   if (x==y)
        return ((Pointer)global_True)[0];
    return ((Pointer)global_False)[0];
}

/* Misc primitives
*/

GrWord primUnsafeId(GrWord x)
{   return x ;
}

int primUndefined()
{
    printf("attempt tot evaluate undefined\n");
    exit(1);
    return 0;
}
/*
int primCmpInt(int x, int y)
{   if (x>y)
        return global_GT[0];
    if (x==y)
        return global_EQ[0];
    return global_LT[0];
}
*/
