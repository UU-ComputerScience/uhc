#include <stdlib.h>
#include <stdio.h>

int fib( int n )
{
  if( n == 0 )
    return 0;
  else if( n == 1 )
    return 1;
  else
    return fib( n-1 ) + fib( n-2 );
}

int main( )
{
  printf( "%d\n", fib( 33 ) );
  exit( 0 );
}
