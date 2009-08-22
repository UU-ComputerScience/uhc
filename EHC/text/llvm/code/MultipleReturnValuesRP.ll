@RP = internal global i32* zeroinitializer

define i32 @main(  ) nounwind
{
    %rpArr = call i32* @llvmgc_malloc_uncollectable( i32 16 )
    store i32* %rpArr, i32** @RP
    call void @fun_main(  )
    ret i32 0
}
define internal void @fun_main(  ) nounwind
{
    %vr1 = load i32** @RP
    %vr2 = getelementptr i32* %vr1, i32 0
    store i32 9, i32* %vr2
    %vr3 = load i32** @RP
    %vr4 = getelementptr i32* %vr3, i32 1
    store i32 42, i32* %vr4
    ret void
}

