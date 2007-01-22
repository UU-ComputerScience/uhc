%%[8
#include "rts.h"
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Test primitive
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
PRIM int primXXXInt(int x, int y, int z)
{   return x+y+z;
}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Int related primitives
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
PRIM int primAddInt(int x, int y)
{   
	//printf("add %d %d\n", x, y );
	return x+y;
}

PRIM int primSubInt(int x, int y)
{   
	//printf("sub %d %d\n", x, y );
	return x-y;
}

PRIM int primMulInt(int x, int y)
{   
	//printf("mul %d %d\n", x, y );
	return x*y;
}

PRIM int primDivInt(int x, int y)
{   
	//printf("div %d %d\n", x, y );
	return x/y;
}

PRIM int primModInt(int x, int y)
{   
	//printf("mod %d %d\n", x, y );
	return x%y;
}
%%]

%%[8
/* In the following 3 functions, only the constructor of Bool is returned.

   The arity of the constructor should also be returned, but we can return only one value.
   So, the arity will obtain a random value.
   Luckily, the arity is never used anywhere.
   (So I wonder why it is stored at all!)
*/
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Ord Int related primitives
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
PRIM int primGtInt(int x, int y)
{   if (x>y)
    { //  printf ("%d is groter dan %d\n", x, y );
        return ((Pointer)global_True)[0];
    }
    //printf ("%d is niet groter dan %d\n", x, y );
    return ((Pointer)global_False)[0];
}

PRIM int primLtInt(int x, int y)
{   if (x<y)
        return ((Pointer)global_True)[0];
    return ((Pointer)global_False)[0];
}
%%]

%%[8
PRIM GrWord primCmpInt(int x, int y)
{   if (x>y)
        return ((Pointer)global_GT)[0];
    if (x==y)
        return ((Pointer)global_EQ)[0];
    return ((Pointer)global_LT)[0];
}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Eq Int related primitives
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
PRIM int primEqInt(int x, int y)
{
	 //printf("eq %d %d\n", x, y );
	
	   if (x==y)
        return ((Pointer)global_True)[0];
    return ((Pointer)global_False)[0];
}
%%]

%%[8
PRIM int primNeInt(int x, int y)
{
	 //printf("neq %d %d\n", x, y );
	
	   if (x!=y)
        return ((Pointer)global_True)[0];
    return ((Pointer)global_False)[0];
}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Misc primitives
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
PRIM GrWord primUnsafeId(GrWord x)
{   return x ;
}

PRIM int primUndefined()
{
    printf("attempt tot evaluate undefined\n");
    gb_exit(1);
    return 0;
}

%%]
