

@__llvm_uhc_globals = internal global [ 3 x i64*] zeroinitializer

@RP = internal global i64* zeroinitializer
@global_y3_38 = internal global i64* zeroinitializer
@global_y2_65_0 = internal global i64* zeroinitializer

declare i32 @printf( i8* %str, ... )
@fresh0 = constant [ 6 x i8] c"\25lld\0a\00"

define  i32 @main(  ) nounwind 
{

    %rpArr = malloc i64, i32 3
    %rpArrPtr = getelementptr [ 3 x i64*]* @__llvm_uhc_globals, i32 0, i32 0
    store i64* %rpArr, i64** %rpArrPtr

    %fresh34 = getelementptr [ 3 x i64*]* @__llvm_uhc_globals, i32 0, i32 0
    %fresh41 = load i64** %fresh34
    %fresh35 = getelementptr i64* %fresh41, i32 1

    ;%fresh56 = inttoptr i64 5 to i64*
    store i64 5, i64* %fresh35
    


    %rp.1 = getelementptr i64* %rpArr, i32 1
    %rp.1.val = load i64* %rp.1
    %cast.str.res = getelementptr [ 6 x i8]* @fresh0, i32 0, i32 0
    tail call i32 ( i8*, ... )* @printf( i8* %cast.str.res, i64 %rp.1.val )

    ret i32 0
}

