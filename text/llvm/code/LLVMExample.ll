@str.res = internal constant [ 4 x i8 ] c"%d\0a\00"

declare i32 @printf( i8* %str, ... )

define fastcc i32 @fib( i32 %n ) nounwind
{
  switch i32 %n, label %default [ i32 0, label %nEq0
                                  i32 1, label %nEq1 ]

  nEq0:
    ret i32 0

  nEq1:
    ret i32 1

  default:
    %tmp1 = sub i32 %n, 1
    %tmp2 = call fastcc i32 @fib( i32 %tmp1 ) 
    %tmp3 = sub i32 %n, 2
    %tmp4 = call fastcc i32 @fib( i32 %tmp3 )
    %tmp5 = add i32 %tmp2, %tmp4
    ret i32 %tmp5
}

define i32 @main( ) nounwind
{
  %fib.res = call fastcc i32 @fib( i32 33 )
  %cast.str.res = getelementptr [ 4 x i8]* @str.res, i32 0, i32 0
  tail call i32 ( i8*, ... )* @printf( i8* %cast.str.res, i32 %fib.res )
  ret i32 0
}
