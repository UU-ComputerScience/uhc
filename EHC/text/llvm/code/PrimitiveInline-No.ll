define internal void @fun_main(  ) nounwind
{
    %fresh1 = call i32 @primMulInt( i32 6, i32 9 )

    %fresh2 = load i32** @RP
    %fresh3 = getelementptr i32* %fresh2, i32 0
    store i32 9, i32* %fresh3
    %fresh4 = load i32** @RP
    %fresh5 = getelementptr i32* %fresh4, i32 1
    store i32 %fresh1, i32* %fresh5
    
    ret void
}

