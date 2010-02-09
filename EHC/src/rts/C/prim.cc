%%[8
#include "../rts.h"
%%]


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Primitives for C backend
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8

PRIM void primPatternMatchFailure()
{
    printf("Pattern match failure\n");
    exit(1);
}

PRIM Word primOrd(Word x)
{
	return x;	
}

PRIM Word primChr(Word x)
{
	return x;	
}

PRIM Word primOdd(Word x)
{
    if (x&1)
        return RTS_True;
    return RTS_False;
}


PRIM Word primError(Word s)
{
	Word c;
	char x;

	printf("\nError function called from Haskell with message: ");
	fflush(stdout);
	
	while (  ((Word*)s)[0] == RTS_Cons )
	{
		c = ((Word*)s)[1];	
		x = ((Word*)c)[1];
		putc(x,stdout);
		s = ((Word*)s)[2];	
	}
	putc('\n', stdout);
	fflush(stdout);
	
	exit(1);
	return 0;	
}


PRIM Word primMinInt()
{
	return 0x10000000;
}
PRIM Word primMaxInt()
{
	return 0x0FFFFFFF;
}


%%]


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Exiting
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[96

PRIM Word primExitWith(Word n)
{
	exit(n);
  	return 0;
}

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Program arguments
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[99
PRIM void getProgArgv( Word argc, Word argv )
{
	*((int*)argc) = rtsArgC ;
	*((char***)argv) = rtsArgV ;
}

PRIM void setProgArgv( Word argc, Word argv )
{
	rtsArgC = (int)argc ;
	rtsArgV = (char**) argv ;
}

%%]







%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% IO
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[98

PRIM Word primStdin()
{
  	return (Word)stdin;
}

PRIM Word primStdout()
{
  	return (Word)stdout;
}

PRIM Word primStderr()
{
  	return (Word)stderr;
}

PRIM Word primHFileno(Word chan)
{
	return fileno((FILE*)chan);
}


PRIM Word primOpenFile(Word str, Word mode)
{
	char filename[1024];
	char *d, *modestring;
	Word c;
	char x;
	FILE *f;

	d = filename;
	while (  ((Word*)str)[0] == RTS_Cons )
	{
		c = ((Word*)str)[1];	
		x = ((Word*)c)[1];
		*d++ = x;
		str = ((Word*)str)[2];	
	}
	*d = 0;

	
	switch(mode - CAppendBinaryMode)
	{
	case 0: modestring = "ab"; break;
	case 1: modestring = "a"; break;
	case 2: modestring = "rb"; break;
	case 3: modestring = "r"; break;
	case 4: modestring = "r+b"; break;
	case 5: modestring = "r+"; break;
	case 6: modestring = "wb"; break;
	case 7: modestring = "w"; break;
	default:  printf("primOpenFile: illegal mode %d\n", (int)mode); fflush(stdout);
	          return 0;	
}

	//printf("try to open [%s] with mode [%s]\n", filename, modestring );  fflush(stdout);
	f = fopen(filename, modestring);
	return (Word) f;	
}

PRIM Word primHClose(Word chan)
{
	fclose( (FILE*)chan );
	return RTS_Unit;	
}

PRIM Word primHFlush(Word chan)
{
	fflush( (FILE*)chan );
	return RTS_Unit;	
}

PRIM Word primHGetChar(Word h)
{
	int c;
	c = getc( (FILE*)h );
	//printf ("character read: %c\n", c );
	return c;
}

PRIM Word primHPutChar(Word h, Word c)
{
	putc(c, (FILE*)h );
	return RTS_Unit;
}

PRIM Word primHIsEOF(Word h)
{
	int c;
	c = getc( (FILE*)h );
	if (c==EOF)
		return RTS_True;
	
	ungetc( c, (FILE*)h );
	return RTS_False;
}
%%]




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Integer related primitives
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8

PRIM Word packedStringToInteger(Word w)
{
	char *s = (char*) w;
	Word res;
    res = heapalloc(1);
    ((WPtr)res)[0] = atoi( (char*)s );
    return res;
}


PRIM Word primIntToInteger(Word n)
{
	Word res;
    res = heapalloc(1);
    ((WPtr)res)[0] = n;
    return res;
}

PRIM Word primInt32ToInteger(Word32 n)
{
	Word res;
    res = heapalloc(1);
    ((WPtr)res)[0] = n;
    return res;
}

PRIM Word primIntegerToInt(Word p)
{
	Word res;
    res = ((WPtr)p)[0];
    return res;
}

PRIM Word32 primIntegerToWord32(Word p)
{
	Word32 res;
    res = ((WPtr)p)[0];
    return res;
}


PRIM Word primCmpInteger(Word x, Word y)
{   if (((WPtr)x)[0] > ((WPtr)y)[0])
        return RTS_GT;
    if (((WPtr)x)[0] == ((WPtr)y)[0])
        return RTS_EQ;
    return RTS_LT;
}

PRIM Word primEqInteger(Word x, Word y)
{
    if (((WPtr)x)[0] == ((WPtr)y)[0])
        return RTS_True;
    return RTS_False;
}

PRIM Word primAddInteger(Word x, Word y)
{   
	Word res;
    res = heapalloc(1);
    ((WPtr)res)[0] = ((WPtr)x)[0] + ((WPtr)y)[0];
    return res;
}

PRIM Word primSubInteger(Word x, Word y)
{   
	Word res;
    res = heapalloc(1);
    ((WPtr)res)[0] = ((WPtr)x)[0] - ((WPtr)y)[0];
    return res;
}

PRIM Word primMulInteger(Word x, Word y)
{   
	Word res;
    res = heapalloc(1);
    ((WPtr)res)[0] = ((WPtr)x)[0] * ((WPtr)y)[0];
    return res;
}

PRIM Word primNegInteger(Word x)
{   
	Word res;
    res = heapalloc(1);
    ((WPtr)res)[0] = -((WPtr)x)[0];
    return res;
}

PRIM Word primQuotInteger(Word x, Word y)
{   
	Word res;
    res = heapalloc(1);
    ((WPtr)res)[0] = ((WPtr)x)[0] / ((WPtr)y)[0];
    return res;
}

PRIM Word primRemInteger(Word x, Word y)
{   
	Word res;
    res = heapalloc(1);
    ((WPtr)res)[0] = ((WPtr)x)[0] % ((WPtr)y)[0];
    return res;
}

PRIM Word primDivInteger(Word x, Word y)
{   
	Word res;
    res = heapalloc(1);
    ((WPtr)res)[0] = ((WPtr)x)[0] / ((WPtr)y)[0];
    return res;
}

PRIM Word primModInteger(Word x, Word y)
{   
	Word res;
    res = heapalloc(1);
    ((WPtr)res)[0] = ((WPtr)x)[0] % ((WPtr)y)[0];
    return res;
}


%%]
