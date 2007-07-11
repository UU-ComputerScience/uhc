%%[8
#include "rts.h"
%%]


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Int related primitives
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8


PRIM int primNegateInt(int x)
{
	return -x;	
}

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Ord Int related primitives
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8

/* The Boolean functions below only return the constructor */


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

PRIM int primOrd(int x)
{
	return x;	
}

PRIM int primChr(int x)
{
	return x;	
}

PRIM int primOdd(int x)
{
    if (x&1)
        return ((Pointer)global_True)[0];
    return ((Pointer)global_False)[0];
}


PRIM GrWord primPackedStringNull(GrWord s)
{
	if (*  ((char*)s) )	
    	return ((Pointer)global_False)[0];	
    return ((Pointer)global_True)[0];
}

PRIM GrWord primPackedStringTail(GrWord s)
{
	return  (GrWord)(((char*)s)+1);
}

PRIM GrWord primPackedStringHead(GrWord s)
{
	return (GrWord)(*((char*)s));
}




%%]
