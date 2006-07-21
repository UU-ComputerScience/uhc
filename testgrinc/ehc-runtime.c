typedef int GrWord;
typedef GrWord* Pointer;

#define CFalse 3
#define CTrue 4


#define HEAPSIZE 10000
#define STACKSIZE 100000
#define RETURNSIZE 100

Pointer SP, HP, RP;
Pointer Stack, Heap, ReturnArea;
Pointer HeapEndCAF, HeapLimit;


extern GrWord False$global;
extern GrWord True$global;
extern int fun_main();

int main(int argc, char** argv)
{
    Heap = (Pointer)malloc(sizeof(GrWord)*HEAPSIZE);
    Stack = (Pointer)malloc(sizeof(GrWord)*STACKSIZE);
    ReturnArea = (Pointer)malloc(sizeof(GrWord)*RETURNSIZE);

    HeapLimit = Heap + HEAPSIZE;

    HP = Heap;
    SP = Stack;
    RP = ReturnArea;

    initialize();
    HeapEndCAF = HP;
    fun_main();

/*
    typedef int (*continuation)(void);
    continuation cont;
    int res;
    res = 1;
    cont = fun_main;
    while (res)
    {
        res = (*cont)();
        printf("intermediate res=%d SP-offset=%d tag=%d arity=%d value=%d\n", res, SP-Stack, SP[0], SP[1], SP[2] );
        cont = (continuation) res;
    }
 */

    printf("result SP-offset=%d HP-offset=%d tag=%d arity=%d value=%d\n", SP-Stack, HP-Heap, RP[0], RP[1], RP[2] );
    return 0;
}

GrWord heapalloc(int n)
{
    GrWord res = (GrWord) HP;
    HP += n;
    if (HP>=HeapLimit)
    {
    	printf("heap overflow\n");
    	exit(1);
    }

    return res;
}

int primAddInt(int x, int y)
{	return x+y;
}

int primSubInt(int x, int y)
{	return x-y;
}
int primMulInt(int x, int y)
{	return x*y;
}
int primDivInt(int x, int y)
{	return x/y;
}
int primModInt(int x, int y)
{	return x%y;
}


/* In the following 3 functions, only the constructor of Bool is returned.
   The arity of the constructor should also be returned, but we can return only one value.
   So, the arity will obtain a random value.
   Luckily, the arity is never used anywhere.
   (So I wonder why it is stored at all!)
*/

int primGtInt(int x, int y)
{	if (x>y)
	    return CTrue;
	return CFalse;
}
int primLtInt(int x, int y)
{	if (x<y)
	    return CTrue;
	return CFalse;
}
int primEqInt(int x, int y)
{	if (x==y)
	    return CTrue;
	return CFalse;
}
