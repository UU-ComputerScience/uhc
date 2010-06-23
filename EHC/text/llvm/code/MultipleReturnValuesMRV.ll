define i32 @main(  ) nounwind
{
    %vr1 = call {i32, i32} @fun_main(  )
    ret i32 0 
}
define internal {i32, i32} @fun_main(  ) nounwind
{
    %RP   = alloca {i32, i32}, i32 1
    %RP.0 = getelementptr {i32, i32}* %RP, i32 0, i32 0
    %RP.1 = getelementptr {i32, i32}* %RP, i32 0, i32 1

    store i32 9, i32* %RP.0
    store i32 42, i32* %RP.1
    
    %vr1 = load i32* %RP.0
    %vr2 = load i32* %RP.1
    ret i32 %vr1, i32 %vr2
}

