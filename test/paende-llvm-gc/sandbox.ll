

@__llvm_uhc_globals = internal global [ 3 x i64*] zeroinitializer

@RP = internal global i64* zeroinitializer
@global_y3_38 = internal global i64* zeroinitializer
@global_y2_65_0 = internal global i64* zeroinitializer

declare i32 @printf( i8* %str, ... )
@fresh0 = constant [ 6 x i8] c"\25lld\0a\00"

define  i32 @main(  ) nounwind 
{

    ; Gglobal_y3_38 := ALLOC 3 ( NotManaged );
    ;
    ; %fresh1 = call i64* @mm_itf_allocResident_ext( i64 24 )
    ; store i64* %fresh1, i64** @global_y3_38


    %fresh1 = malloc i64, i32 3
    ;%fresh1 = call i64* @mm_itf_allocResident_ext( i64 24 )                      ; yields i64*

    %fresh2 = getelementptr [ 3 x i64*]* @__llvm_uhc_globals, i32 0, i32 1       ; yields i64**
    %fresh3 = load i64** %fresh2                                                 ; yields i64*
    ; %fresh4 = ptrtoint i64* %fresh1 to i64
    store i64* %fresh1, i64** %fresh3
   
    ; %fresh11 = getelementptr [ 3 x i64*]* @__llvm_uhc_globals, i32 1
    ; %fresh12 = load i64** %fresh11
    ; %fresh13 = getelementptr i64* %fresh12, i32 0
    ; store i64 14, i64* %fresh13

    ret i32 0
}

