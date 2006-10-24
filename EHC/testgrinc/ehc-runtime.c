typedef int GrWord;
typedef GrWord* Pointer;

#define HEAPSIZE 100000
#define STACKSIZE 100000
#define RETURNSIZE 100

Pointer SP, HP, RP;
Pointer Stack, Heap, ReturnArea;
Pointer HeapEndCAF, HeapLimit;


extern Pointer global_False;
extern Pointer global_True;
extern Pointer global_LT;
extern Pointer global_GT;
extern Pointer global_EQ;

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
    int i;
    for (i=0; i<10; i++)
    {
        printf("RP[%d] = %d\n", i, RP[i] );
    }
    for (i=0; i<10; i++)
    {
        printf("%d: St[%d] = %d\n", Stack+i, i, Stack[i] );
    }
    for (i=0; i<HP-Heap; i++)
    {
        printf("%d: Heap[%d] = %d\n", Heap+i, i, Heap[i] );
    }
*/

/*    printf("result SP-offset=%d HP-offset=%d tag=%d arity=%d value=%d\n", SP-Stack, HP-Heap, RP[0], RP[1], RP[2] ); */
     printf("result SP-offset=%d HP-offset=%d tag=%d value=%d\n", SP-Stack, HP-Heap, RP[0], RP[1] );

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

int primGtInt(int x, int y)
{   if (x>y)
        return global_True[0];
    return global_False[0];
}
int primLtInt(int x, int y)
{   if (x<y)
        return global_True[0];
    return global_False[0];
}
int primEqInt(int x, int y)
{   if (x==y)
        return global_True[0];
    return global_False[0];
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
