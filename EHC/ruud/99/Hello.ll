; Ccomma2->16
; CChar->17
; CInt->18
; CUHC_Base_PackedString->19
; P1_UHC_Base_primretIO->20
; P1_UHC_Base_ioFromPrim->21
; P1_UHC_Base_y103_0_1954_0->22
; P1_UHC_Base_primbindIO->23
; P1_UHC_OldIO_y103_0_32_0->24
; P1_UHC_OldIO_y103_0_35_0->25
; UNBOXED->26
; HOLE->27
; FUHC_Base_primCharToInt->28
; FUHC_Base_packedStringHead->29
; FUHC_Base_packedStringHeadtildespec1->30
; FUHC_Base_packedStringHeadtildespec14->31
; FUHC_Base_packedStringTail->32
; FUHC_Base_packedStringTailtildespec1->33
; FUHC_Base_packedStringTailtildespec14->34
; FUHC_Base_errortildespec1->35
; FUHC_Base_stringSum->36
; FUHC_Base_packedStringToString->37
; FUHC_Base_packedStringToStringtildespec1->38
; FUHC_Base_packedStringToStringtildespec14->39
; FUHC_Base_head->40
; FUHC_Base_headtildespec1->41
; FUHC_Base_tail->42
; FUHC_Base_tailtildespec1->43
; FUHC_OldIO_fun0tildeprimStdout->44
; FUHC_OldIO_hPutChartildespec1->45
; FUHC_OldIO_hPutStrtildespec2->46
; FUHC_OldIO_hPutStrtildespec1->47
; FUHC_OldIO_hPutStrLntildespec1->48
; FUHC_Run_ehcRunMaintildespec1->49
; Afun_app2_1->50
; Afun_app2_58->51
; Afun_app2_59->52

@fresh0 = internal constant [ 6 x i8] c"\25lld\0a\00"
@fresh316 = internal constant [ 14 x i8] c"Hello\2c\20World\21\00"
@fresh409 = internal constant [ 18 x i8] c"Prelude\2eundefined\00"
@fresh642 = internal constant [ 14 x i8] c"Hello\2c\20World\21\00"
@fresh650 = internal constant [ 18 x i8] c"Prelude\2eundefined\00"
@fresh915 = internal constant [ 14 x i8] c"Hello\2c\20World\21\00"
@fresh952 = internal constant [ 18 x i8] c"Prelude\2eundefined\00"
@RP = internal global i64* zeroinitializer
@global_UHC_Base_undefined = internal global i64* zeroinitializer
@global_UHC_Base_y103_0_459 = internal global i64* zeroinitializer
@global_UHC_OldIO_primStdout = internal global i64* zeroinitializer
@global_Main_y13_0_2 = internal global i64* zeroinitializer
@global_Main_main = internal global i64* zeroinitializer
@global_main = internal global i64* zeroinitializer
@global_UHC_OldIO_y110_234_1 = internal global i64* zeroinitializer
@global_UHC_OldIO_y110_242_1 = internal global i64* zeroinitializer

declare i32 @printf( i8* %str, ... )
declare void @llvmgc_init(  )
declare void @llvmc_print_statistics(  )
declare i64* @llvmgc_malloc( i64 %x )
declare i64* @llvmgc_malloc_uncollectable( i64 %x )
declare void @primPatternMatchFailure(  )
declare i64 @primStdout(  )
declare i64 @primPackedStringHead( i64 %fresh318 )
declare i64 @primHPutChar( i64 %fresh366, i64 %fresh365 )
declare i64 @primPackedStringHead( i64 %fresh411 )
declare i64 @primUnsafeId( i64 %fresh423 )
declare i64 @primAddInt( i64 %fresh524, i64 %fresh523 )
declare i64 @primPackedStringHead( i64 %fresh580 )
declare i64 @primPackedStringTail( i64 %fresh636 )
declare i64 @primPackedStringTail( i64 %fresh644 )
declare i64 @primPackedStringTail( i64 %fresh652 )
declare i64 @primError( i64 %fresh818 )
declare i64 @primPackedStringNull( i64 %fresh874 )
declare i64 @primPackedStringNull( i64 %fresh917 )
declare i64 @primPackedStringNull( i64 %fresh954 )

define  i32 @main(  ) nounwind
{
    call void @llvmgc_init(  )
    %rpArr = call i64* @llvmgc_malloc_uncollectable( i64 24 )
    store i64* %rpArr, i64** @RP
    call void @initialize(  )
    call void @fun_mainFullProg(  )
    call void @llvmc_print_statistics(  )
    ret i32 0
}

define internal void @initialize(  ) nounwind
{
    %fresh1 = call i64* @llvmgc_malloc_uncollectable( i64 16 )
    store i64* %fresh1, i64** @global_UHC_Base_undefined
    %fresh2 = call i64* @llvmgc_malloc_uncollectable( i64 24 )
    store i64* %fresh2, i64** @global_UHC_Base_y103_0_459
    %fresh3 = call i64* @llvmgc_malloc_uncollectable( i64 16 )
    store i64* %fresh3, i64** @global_UHC_OldIO_primStdout
    %fresh4 = call i64* @llvmgc_malloc_uncollectable( i64 24 )
    store i64* %fresh4, i64** @global_Main_y13_0_2
    %fresh5 = call i64* @llvmgc_malloc_uncollectable( i64 24 )
    store i64* %fresh5, i64** @global_Main_main
    %fresh6 = call i64* @llvmgc_malloc_uncollectable( i64 24 )
    store i64* %fresh6, i64** @global_main
    %fresh7 = call i64* @llvmgc_malloc_uncollectable( i64 8 )
    store i64* %fresh7, i64** @global_UHC_OldIO_y110_234_1
    %fresh8 = call i64* @llvmgc_malloc_uncollectable( i64 16 )
    store i64* %fresh8, i64** @global_UHC_OldIO_y110_242_1
    %fresh9 = load i64** @global_UHC_Base_undefined
    %fresh10 = getelementptr i64* %fresh9, i32 0
    store i64 35, i64* %fresh10
    %fresh11 = load i64** @global_UHC_Base_y103_0_459
    %fresh12 = getelementptr i64* %fresh11, i32 0
    store i64 39, i64* %fresh12
    %fresh13 = load i64** @global_UHC_OldIO_primStdout
    %fresh14 = getelementptr i64* %fresh13, i32 0
    store i64 44, i64* %fresh14
    %fresh15 = load i64** @global_Main_y13_0_2
    %fresh16 = getelementptr i64* %fresh15, i32 0
    store i64 38, i64* %fresh16
    %fresh17 = load i64** @global_Main_main
    %fresh18 = getelementptr i64* %fresh17, i32 0
    store i64 48, i64* %fresh18
    %fresh19 = load i64** @global_main
    %fresh20 = getelementptr i64* %fresh19, i32 0
    store i64 49, i64* %fresh20
    %fresh21 = load i64** @global_UHC_OldIO_y110_234_1
    %fresh22 = getelementptr i64* %fresh21, i32 0
    store i64 0, i64* %fresh22
    %fresh23 = load i64** @global_UHC_OldIO_y110_242_1
    %fresh24 = getelementptr i64* %fresh23, i32 0
    store i64 17, i64* %fresh24
    %fresh25 = load i64** @global_UHC_OldIO_y110_242_1
    %fresh26 = getelementptr i64* %fresh25, i32 1
    store i64 10, i64* %fresh26
    ret void
}

define internal void @fun_mainFullProg(  ) nounwind
{
    %x16 = alloca i64, i32 1
    %x229 = alloca i64, i32 1
    %x230 = alloca i64, i32 1
    %x231 = alloca i64, i32 1
    %x485 = alloca i64, i32 1
    %x486 = alloca i64, i32 1
    %x487 = alloca i64, i32 1
    %x489 = alloca i64, i32 1
    %x490 = alloca i64, i32 1
    ; Store
    %fresh27 = call i64* @llvmgc_malloc( i64 8 )
    %fresh28 = ptrtoint i64* %fresh27 to i64
    store i64 %fresh28, i64* %x16
    %fresh29 = load i64* %x16
    %fresh30 = inttoptr i64 %fresh29 to i64*
    %fresh31 = getelementptr i64* %fresh30, i32 0
    store i64 27, i64* %fresh31
    ; Fetch
    ;  (inlined)Variable_Unembedded x_476 = Value_Var (Variable_Subs (Variable_Unembedded global_x_11_main) 0)
    ; Case
    %fresh32 = load i64** @global_main
    %fresh33 = getelementptr i64* %fresh32, i32 0
    %fresh34 = load i64* %fresh33
    switch i64 %fresh34, label %fresh107 [ i64 23, label %fresh35
                                           i64 49, label %fresh42 ]
    fresh35:
    ; Fetch
    ;  (inlined)Variable_Unembedded x_690 = Value_Var (Variable_Subs (Variable_Unembedded global_x_11_main) 1)
    ; Fetch
    ;  (inlined)Variable_Unembedded x_689 = Value_Var (Variable_Subs (Variable_Unembedded global_x_11_main) 2)
    ; Unit
    %fresh36 = load i64** @global_main
    %fresh37 = getelementptr i64* %fresh36, i32 1
    %fresh38 = load i64* %fresh37
    store i64 %fresh38, i64* %x489
    %fresh39 = load i64** @global_main
    %fresh40 = getelementptr i64* %fresh39, i32 2
    %fresh41 = load i64* %fresh40
    store i64 %fresh41, i64* %x490
    br label %fresh108
    fresh42:
    ; Fetch
    ;  (inlined)Variable_Unembedded x_479 = Value_Var (Variable_Subs (Variable_Unembedded global_x_10_Main.main) 0)
    ; Case
    %fresh43 = load i64** @global_Main_main
    %fresh44 = getelementptr i64* %fresh43, i32 0
    %fresh45 = load i64* %fresh44
    switch i64 %fresh45, label %fresh96 [ i64 23, label %fresh46
                                          i64 48, label %fresh53 ]
    fresh46:
    ; Fetch
    ;  (inlined)Variable_Unembedded x_692 = Value_Var (Variable_Subs (Variable_Unembedded global_x_10_Main.main) 1)
    ; Fetch
    ;  (inlined)Variable_Unembedded x_691 = Value_Var (Variable_Subs (Variable_Unembedded global_x_10_Main.main) 2)
    ; Unit
    store i64 23, i64* %x485
    %fresh47 = load i64** @global_Main_main
    %fresh48 = getelementptr i64* %fresh47, i32 1
    %fresh49 = load i64* %fresh48
    store i64 %fresh49, i64* %x486
    %fresh50 = load i64** @global_Main_main
    %fresh51 = getelementptr i64* %fresh50, i32 2
    %fresh52 = load i64* %fresh51
    store i64 %fresh52, i64* %x487
    br label %fresh97
    fresh53:
    ; Store
    %fresh54 = call i64* @llvmgc_malloc( i64 16 )
    %fresh55 = ptrtoint i64* %fresh54 to i64
    store i64 %fresh55, i64* %x229
    %fresh56 = load i64* %x229
    %fresh57 = inttoptr i64 %fresh56 to i64*
    %fresh58 = getelementptr i64* %fresh57, i32 0
    store i64 45, i64* %fresh58
    ; Store
    %fresh59 = call i64* @llvmgc_malloc( i64 24 )
    %fresh60 = ptrtoint i64* %fresh59 to i64
    store i64 %fresh60, i64* %x230
    %fresh61 = load i64* %x230
    %fresh62 = inttoptr i64 %fresh61 to i64*
    %fresh63 = getelementptr i64* %fresh62, i32 0
    store i64 52, i64* %fresh63
    %fresh64 = load i64* %x229
    %fresh65 = load i64* %x230
    %fresh66 = inttoptr i64 %fresh65 to i64*
    %fresh67 = getelementptr i64* %fresh66, i32 1
    store i64 %fresh64, i64* %fresh67
    %fresh68 = load i64** @global_UHC_OldIO_y110_242_1
    %fresh69 = load i64* %x230
    %fresh70 = inttoptr i64 %fresh69 to i64*
    %fresh71 = getelementptr i64* %fresh70, i32 2
    %fresh72 = ptrtoint i64* %fresh68 to i64
    store i64 %fresh72, i64* %fresh71
    ; Store
    %fresh73 = call i64* @llvmgc_malloc( i64 24 )
    %fresh74 = ptrtoint i64* %fresh73 to i64
    store i64 %fresh74, i64* %x231
    %fresh75 = load i64* %x231
    %fresh76 = inttoptr i64 %fresh75 to i64*
    %fresh77 = getelementptr i64* %fresh76, i32 0
    store i64 47, i64* %fresh77
    ; Call (Normal or Tail)
    %fresh78 = load i64* %x231
    %fresh79 = load i64* %x230
    call void @fun_UHC_Base_y91_43_0_class_MonadtildeUHC_Base_gtgttildespec1( i64 %fresh78, i64 %fresh79 )
    ; Result
    ;  (inlined)Variable_Unembedded x_482 = Value_Var (Variable_Subs Variable_RP 0)
    ;  (inlined)Variable_Unembedded x_483 = Value_Var (Variable_Subs Variable_RP 1)
    ;  (inlined)Variable_Unembedded x_484 = Value_Var (Variable_Subs Variable_RP 2)
    ; UpdateUnit
    %fresh81 = load i64** @RP
    %fresh82 = getelementptr i64* %fresh81, i32 0
    %fresh83 = load i64* %fresh82
    store i64 %fresh83, i64* %x485
    %fresh84 = load i64** @global_Main_main
    %fresh85 = getelementptr i64* %fresh84, i32 0
    store i64 %fresh83, i64* %fresh85
    %fresh86 = load i64** @RP
    %fresh87 = getelementptr i64* %fresh86, i32 1
    %fresh88 = load i64* %fresh87
    store i64 %fresh88, i64* %x486
    %fresh89 = load i64** @global_Main_main
    %fresh90 = getelementptr i64* %fresh89, i32 1
    store i64 %fresh88, i64* %fresh90
    %fresh91 = load i64** @RP
    %fresh92 = getelementptr i64* %fresh91, i32 2
    %fresh93 = load i64* %fresh92
    store i64 %fresh93, i64* %x487
    %fresh94 = load i64** @global_Main_main
    %fresh95 = getelementptr i64* %fresh94, i32 2
    store i64 %fresh93, i64* %fresh95
    br label %fresh97
    fresh96:
    call void @primPatternMatchFailure(  )
    unreachable
    br label %fresh97
    fresh97:
    ; UpdateUnit
    %fresh98 = load i64* %x485
    %fresh99 = load i64** @global_main
    %fresh100 = getelementptr i64* %fresh99, i32 0
    store i64 %fresh98, i64* %fresh100
    %fresh101 = load i64* %x486
    store i64 %fresh101, i64* %x489
    %fresh102 = load i64** @global_main
    %fresh103 = getelementptr i64* %fresh102, i32 1
    store i64 %fresh101, i64* %fresh103
    %fresh104 = load i64* %x487
    store i64 %fresh104, i64* %x490
    %fresh105 = load i64** @global_main
    %fresh106 = getelementptr i64* %fresh105, i32 2
    store i64 %fresh104, i64* %fresh106
    br label %fresh108
    fresh107:
    call void @primPatternMatchFailure(  )
    unreachable
    br label %fresh108
    fresh108:
    ; Call (Normal or Tail)
    %fresh109 = load i64* %x489
    %fresh110 = load i64* %x490
    %fresh111 = load i64* %x16
    tail call void @fun_UHC_Base_primbindIO( i64 %fresh109, i64 %fresh110, i64 %fresh111 )
    ; Result
    ret void
}

define internal void @fun_app2_1( i64 %x20, i64 %x21 ) nounwind
{
    %x53 = alloca i64, i32 1
    %x178 = alloca i64, i32 1
    %x197 = alloca i64, i32 1
    %x198 = alloca i64, i32 1
    %x199 = alloca i64, i32 1
    %x492 = alloca i64, i32 1
    %x493 = alloca i64, i32 1
    %x503 = alloca i64, i32 1
    %x520 = alloca i64, i32 1
    %x521 = alloca i64, i32 1
    %x522 = alloca i64, i32 1
    %x536 = alloca i64, i32 1
    %x537 = alloca i64, i32 1
    %x695 = alloca i64, i32 1
    %x697 = alloca i64, i32 1
    %x701 = alloca i64, i32 1
    ; Fetch
    %fresh113 = inttoptr i64 %x20 to i64*
    %fresh114 = getelementptr i64* %fresh113, i32 1
    %fresh115 = load i64* %fresh114
    store i64 %fresh115, i64* %x492
    ; Fetch
    %fresh116 = inttoptr i64 %x20 to i64*
    %fresh117 = getelementptr i64* %fresh116, i32 2
    %fresh118 = load i64* %fresh117
    store i64 %fresh118, i64* %x493
    ; Fetch
    ;  (inlined)Variable_Unembedded x_497 = Value_Var (Variable_Subs (Variable_Unembedded x_492) 0)
    ; Case
    %fresh119 = load i64* %x492
    %fresh120 = inttoptr i64 %fresh119 to i64*
    %fresh121 = getelementptr i64* %fresh120, i32 0
    %fresh122 = load i64* %fresh121
    switch i64 %fresh122, label %fresh137 [ i64 26, label %fresh123
                                            i64 44, label %fresh128 ]
    fresh123:
    ; Fetch
    ;  (inlined)Variable_Unembedded x_693 = Value_Var (Variable_Subs (Variable_Unembedded x_492) 1)
    ; Unit
    %fresh124 = load i64* %x492
    %fresh125 = inttoptr i64 %fresh124 to i64*
    %fresh126 = getelementptr i64* %fresh125, i32 1
    %fresh127 = load i64* %fresh126
    store i64 %fresh127, i64* %x197
    br label %fresh138
    fresh128:
    ; FFI
    %fresh129 = call i64 @primStdout(  )
    store i64 %fresh129, i64* %x178
    ; Unit
    ;  (inlined)Variable_Unembedded x_499 = Value_Con (Constant_Alias "UNBOXED")
    ;  (inlined)Variable_Unembedded x_500 = Value_Var (Variable_Unembedded x_178_UHC.OldIO.110_0_0)
    ; UpdateUnit
    %fresh130 = load i64* %x492
    %fresh131 = inttoptr i64 %fresh130 to i64*
    %fresh132 = getelementptr i64* %fresh131, i32 0
    store i64 26, i64* %fresh132
    %fresh133 = load i64* %x178
    store i64 %fresh133, i64* %x197
    %fresh134 = load i64* %x492
    %fresh135 = inttoptr i64 %fresh134 to i64*
    %fresh136 = getelementptr i64* %fresh135, i32 1
    store i64 %fresh133, i64* %fresh136
    br label %fresh138
    fresh137:
    call void @primPatternMatchFailure(  )
    unreachable
    br label %fresh138
    fresh138:
    ; Fetch
    ;  (inlined)Variable_Unembedded x_501 = Value_Var (Variable_Subs (Variable_Unembedded x_493) 0)
    ; Case
    %fresh139 = load i64* %x493
    %fresh140 = inttoptr i64 %fresh139 to i64*
    %fresh141 = getelementptr i64* %fresh140, i32 0
    %fresh142 = load i64* %fresh141
    switch i64 %fresh142, label %fresh360 [ i64 17, label %fresh143
                                            i64 40, label %fresh148
                                            i64 41, label %fresh276 ]
    fresh143:
    ; Fetch
    ;  (inlined)Variable_Unembedded x_694 = Value_Var (Variable_Subs (Variable_Unembedded x_493) 1)
    ; Unit
    %fresh144 = load i64* %x493
    %fresh145 = inttoptr i64 %fresh144 to i64*
    %fresh146 = getelementptr i64* %fresh145, i32 1
    %fresh147 = load i64* %fresh146
    store i64 %fresh147, i64* %x198
    br label %fresh361
    fresh148:
    ; Fetch
    %fresh149 = load i64* %x493
    %fresh150 = inttoptr i64 %fresh149 to i64*
    %fresh151 = getelementptr i64* %fresh150, i32 1
    %fresh152 = load i64* %fresh151
    store i64 %fresh152, i64* %x695
    ; Fetch
    %fresh153 = load i64* %x695
    %fresh154 = inttoptr i64 %fresh153 to i64*
    %fresh155 = getelementptr i64* %fresh154, i32 0
    %fresh156 = load i64* %fresh155
    store i64 %fresh156, i64* %x503
    ; Case
    %fresh157 = load i64* %x503
    %fresh158 = icmp sgt i64 %fresh157, 27
    br i1 %fresh158, label %fresh159, label %fresh207
    fresh159:
    %fresh160 = load i64* %x503
    switch i64 %fresh160, label %fresh205 [ i64 42, label %fresh161
                                            i64 43, label %fresh185 ]
    fresh161:
    ; Fetch
    ;  (inlined)Variable_Unembedded x_696 = Value_Var (Variable_Subs (Variable_Unembedded x_695) 1)
    ; Call (Normal or Tail)
    %fresh162 = load i64* %x695
    %fresh163 = inttoptr i64 %fresh162 to i64*
    %fresh164 = getelementptr i64* %fresh163, i32 1
    %fresh165 = load i64* %fresh164
    call void @fun_UHC_Base_tail( i64 %fresh165 )
    ; Result
    ;  (inlined)Variable_Unembedded x_506 = Value_Var (Variable_Subs Variable_RP 0)
    ;  (inlined)Variable_Unembedded x_507 = Value_Var (Variable_Subs Variable_RP 1)
    ;  (inlined)Variable_Unembedded x_508 = Value_Var (Variable_Subs Variable_RP 2)
    ; UpdateUnit
    %fresh167 = load i64** @RP
    %fresh168 = getelementptr i64* %fresh167, i32 0
    %fresh169 = load i64* %fresh168
    store i64 %fresh169, i64* %x503
    %fresh170 = load i64* %x695
    %fresh171 = inttoptr i64 %fresh170 to i64*
    %fresh172 = getelementptr i64* %fresh171, i32 0
    store i64 %fresh169, i64* %fresh172
    %fresh173 = load i64** @RP
    %fresh174 = getelementptr i64* %fresh173, i32 1
    %fresh175 = load i64* %fresh174
    %fresh176 = load i64* %x695
    %fresh177 = inttoptr i64 %fresh176 to i64*
    %fresh178 = getelementptr i64* %fresh177, i32 1
    store i64 %fresh175, i64* %fresh178
    %fresh179 = load i64** @RP
    %fresh180 = getelementptr i64* %fresh179, i32 2
    %fresh181 = load i64* %fresh180
    %fresh182 = load i64* %x695
    %fresh183 = inttoptr i64 %fresh182 to i64*
    %fresh184 = getelementptr i64* %fresh183, i32 2
    store i64 %fresh181, i64* %fresh184
    br label %fresh206
    fresh185:
    ; Call (Normal or Tail)
    call void @fun_UHC_Base_tailtildespec1(  )
    ; Result
    ;  (inlined)Variable_Unembedded x_509 = Value_Var (Variable_Subs Variable_RP 0)
    ;  (inlined)Variable_Unembedded x_510 = Value_Var (Variable_Subs Variable_RP 1)
    ;  (inlined)Variable_Unembedded x_511 = Value_Var (Variable_Subs Variable_RP 2)
    ; UpdateUnit
    %fresh187 = load i64** @RP
    %fresh188 = getelementptr i64* %fresh187, i32 0
    %fresh189 = load i64* %fresh188
    store i64 %fresh189, i64* %x503
    %fresh190 = load i64* %x695
    %fresh191 = inttoptr i64 %fresh190 to i64*
    %fresh192 = getelementptr i64* %fresh191, i32 0
    store i64 %fresh189, i64* %fresh192
    %fresh193 = load i64** @RP
    %fresh194 = getelementptr i64* %fresh193, i32 1
    %fresh195 = load i64* %fresh194
    %fresh196 = load i64* %x695
    %fresh197 = inttoptr i64 %fresh196 to i64*
    %fresh198 = getelementptr i64* %fresh197, i32 1
    store i64 %fresh195, i64* %fresh198
    %fresh199 = load i64** @RP
    %fresh200 = getelementptr i64* %fresh199, i32 2
    %fresh201 = load i64* %fresh200
    %fresh202 = load i64* %x695
    %fresh203 = inttoptr i64 %fresh202 to i64*
    %fresh204 = getelementptr i64* %fresh203, i32 2
    store i64 %fresh201, i64* %fresh204
    br label %fresh206
    fresh205:
    call void @primPatternMatchFailure(  )
    unreachable
    br label %fresh206
    fresh206:
    br label %fresh208
    fresh207:
    br label %fresh208
    fresh208:
    %fresh209 = load i64* %x503
    switch i64 %fresh209, label %fresh266 [ i64 3, label %fresh210
                                            i64 4, label %fresh244 ]
    fresh210:
    ; Fetch
    %fresh211 = load i64* %x695
    %fresh212 = inttoptr i64 %fresh211 to i64*
    %fresh213 = getelementptr i64* %fresh212, i32 1
    %fresh214 = load i64* %fresh213
    store i64 %fresh214, i64* %x697
    ; Fetch
    ;  (inlined)Variable_Unembedded x_512 = Value_Var (Variable_Subs (Variable_Unembedded x_697) 0)
    ; Case
    %fresh215 = load i64* %x697
    %fresh216 = inttoptr i64 %fresh215 to i64*
    %fresh217 = getelementptr i64* %fresh216, i32 0
    %fresh218 = load i64* %fresh217
    switch i64 %fresh218, label %fresh242 [ i64 17, label %fresh219
                                            i64 29, label %fresh224 ]
    fresh219:
    ; Fetch
    ;  (inlined)Variable_Unembedded x_698 = Value_Var (Variable_Subs (Variable_Unembedded x_697) 1)
    ; Unit
    store i64 17, i64* %x520
    %fresh220 = load i64* %x697
    %fresh221 = inttoptr i64 %fresh220 to i64*
    %fresh222 = getelementptr i64* %fresh221, i32 1
    %fresh223 = load i64* %fresh222
    store i64 %fresh223, i64* %x521
    br label %fresh243
    fresh224:
    ; Fetch
    ;  (inlined)Variable_Unembedded x_699 = Value_Var (Variable_Subs (Variable_Unembedded x_697) 1)
    ; Call (Normal or Tail)
    %fresh225 = load i64* %x697
    %fresh226 = inttoptr i64 %fresh225 to i64*
    %fresh227 = getelementptr i64* %fresh226, i32 1
    %fresh228 = load i64* %fresh227
    call void @fun_UHC_Base_packedStringHead( i64 %fresh228 )
    ; Result
    ;  (inlined)Variable_Unembedded x_514 = Value_Var (Variable_Subs Variable_RP 0)
    ;  (inlined)Variable_Unembedded x_515 = Value_Var (Variable_Subs Variable_RP 1)
    ; UpdateUnit
    %fresh230 = load i64** @RP
    %fresh231 = getelementptr i64* %fresh230, i32 0
    %fresh232 = load i64* %fresh231
    store i64 %fresh232, i64* %x520
    %fresh233 = load i64* %x697
    %fresh234 = inttoptr i64 %fresh233 to i64*
    %fresh235 = getelementptr i64* %fresh234, i32 0
    store i64 %fresh232, i64* %fresh235
    %fresh236 = load i64** @RP
    %fresh237 = getelementptr i64* %fresh236, i32 1
    %fresh238 = load i64* %fresh237
    store i64 %fresh238, i64* %x521
    %fresh239 = load i64* %x697
    %fresh240 = inttoptr i64 %fresh239 to i64*
    %fresh241 = getelementptr i64* %fresh240, i32 1
    store i64 %fresh238, i64* %fresh241
    br label %fresh243
    fresh242:
    call void @primPatternMatchFailure(  )
    unreachable
    br label %fresh243
    fresh243:
    br label %fresh267
    fresh244:
    ; Fetch
    ;  (inlined)Variable_Unembedded x_516 = Value_Var (Variable_Subs (Variable_Unembedded global_x_6_UHC.Base.undefined) 0)
    ; Case
    %fresh245 = load i64** @global_UHC_Base_undefined
    %fresh246 = getelementptr i64* %fresh245, i32 0
    %fresh247 = load i64* %fresh246
    switch i64 %fresh247, label %fresh264 [ i64 26, label %fresh248
                                            i64 35, label %fresh252 ]
    fresh248:
    ; Fetch
    ;  (inlined)Variable_Unembedded x_700 = Value_Var (Variable_Subs (Variable_Unembedded global_x_6_UHC.Base.undefined) 1)
    ; Unit
    store i64 26, i64* %x520
    %fresh249 = load i64** @global_UHC_Base_undefined
    %fresh250 = getelementptr i64* %fresh249, i32 1
    %fresh251 = load i64* %fresh250
    store i64 %fresh251, i64* %x521
    br label %fresh265
    fresh252:
    ; Call (Normal or Tail)
    call void @fun_UHC_Base_errortildespec1(  )
    ; Result
    ;  (inlined)Variable_Unembedded x_518 = Value_Var (Variable_Subs Variable_RP 0)
    ;  (inlined)Variable_Unembedded x_519 = Value_Var (Variable_Subs Variable_RP 1)
    ; UpdateUnit
    %fresh254 = load i64** @RP
    %fresh255 = getelementptr i64* %fresh254, i32 0
    %fresh256 = load i64* %fresh255
    store i64 %fresh256, i64* %x520
    %fresh257 = load i64** @global_UHC_Base_undefined
    %fresh258 = getelementptr i64* %fresh257, i32 0
    store i64 %fresh256, i64* %fresh258
    %fresh259 = load i64** @RP
    %fresh260 = getelementptr i64* %fresh259, i32 1
    %fresh261 = load i64* %fresh260
    store i64 %fresh261, i64* %x521
    %fresh262 = load i64** @global_UHC_Base_undefined
    %fresh263 = getelementptr i64* %fresh262, i32 1
    store i64 %fresh261, i64* %fresh263
    br label %fresh265
    fresh264:
    call void @primPatternMatchFailure(  )
    unreachable
    br label %fresh265
    fresh265:
    br label %fresh267
    fresh266:
    call void @primPatternMatchFailure(  )
    unreachable
    br label %fresh267
    fresh267:
    ; UpdateUnit
    %fresh268 = load i64* %x520
    %fresh269 = load i64* %x493
    %fresh270 = inttoptr i64 %fresh269 to i64*
    %fresh271 = getelementptr i64* %fresh270, i32 0
    store i64 %fresh268, i64* %fresh271
    %fresh272 = load i64* %x521
    store i64 %fresh272, i64* %x198
    %fresh273 = load i64* %x493
    %fresh274 = inttoptr i64 %fresh273 to i64*
    %fresh275 = getelementptr i64* %fresh274, i32 1
    store i64 %fresh272, i64* %fresh275
    br label %fresh361
    fresh276:
    ; Fetch
    %fresh277 = load i64** @global_Main_y13_0_2
    %fresh278 = getelementptr i64* %fresh277, i32 0
    %fresh279 = load i64* %fresh278
    store i64 %fresh279, i64* %x522
    ; Case
    %fresh280 = load i64* %x522
    %fresh281 = icmp sgt i64 %fresh280, 27
    br i1 %fresh281, label %fresh282, label %fresh299
    fresh282:
    ; Call (Normal or Tail)
    call void @fun_UHC_Base_packedStringToStringtildespec1(  )
    ; Result
    ;  (inlined)Variable_Unembedded x_525 = Value_Var (Variable_Subs Variable_RP 0)
    ;  (inlined)Variable_Unembedded x_526 = Value_Var (Variable_Subs Variable_RP 1)
    ;  (inlined)Variable_Unembedded x_527 = Value_Var (Variable_Subs Variable_RP 2)
    ; UpdateUnit
    %fresh284 = load i64** @RP
    %fresh285 = getelementptr i64* %fresh284, i32 0
    %fresh286 = load i64* %fresh285
    store i64 %fresh286, i64* %x522
    %fresh287 = load i64** @global_Main_y13_0_2
    %fresh288 = getelementptr i64* %fresh287, i32 0
    store i64 %fresh286, i64* %fresh288
    %fresh289 = load i64** @RP
    %fresh290 = getelementptr i64* %fresh289, i32 1
    %fresh291 = load i64* %fresh290
    %fresh292 = load i64** @global_Main_y13_0_2
    %fresh293 = getelementptr i64* %fresh292, i32 1
    store i64 %fresh291, i64* %fresh293
    %fresh294 = load i64** @RP
    %fresh295 = getelementptr i64* %fresh294, i32 2
    %fresh296 = load i64* %fresh295
    %fresh297 = load i64** @global_Main_y13_0_2
    %fresh298 = getelementptr i64* %fresh297, i32 2
    store i64 %fresh296, i64* %fresh298
    br label %fresh300
    fresh299:
    br label %fresh300
    fresh300:
    %fresh301 = load i64* %x522
    switch i64 %fresh301, label %fresh350 [ i64 3, label %fresh302
                                            i64 4, label %fresh328 ]
    fresh302:
    ; Fetch
    %fresh303 = load i64** @global_Main_y13_0_2
    %fresh304 = getelementptr i64* %fresh303, i32 1
    %fresh305 = load i64* %fresh304
    store i64 %fresh305, i64* %x701
    ; Fetch
    ;  (inlined)Variable_Unembedded x_528 = Value_Var (Variable_Subs (Variable_Unembedded x_701) 0)
    ; Case
    %fresh306 = load i64* %x701
    %fresh307 = inttoptr i64 %fresh306 to i64*
    %fresh308 = getelementptr i64* %fresh307, i32 0
    %fresh309 = load i64* %fresh308
    switch i64 %fresh309, label %fresh326 [ i64 17, label %fresh310
                                            i64 30, label %fresh315 ]
    fresh310:
    ; Fetch
    ;  (inlined)Variable_Unembedded x_702 = Value_Var (Variable_Subs (Variable_Unembedded x_701) 1)
    ; Unit
    store i64 17, i64* %x536
    %fresh311 = load i64* %x701
    %fresh312 = inttoptr i64 %fresh311 to i64*
    %fresh313 = getelementptr i64* %fresh312, i32 1
    %fresh314 = load i64* %fresh313
    store i64 %fresh314, i64* %x537
    br label %fresh327
    fresh315:
    ; Unit
    ;  (inlined)Variable_Unembedded x_52_UHC.Base._110_73_2 = Value_Con (Constant_LiteralStr "Hello, World!")
    ; FFI
    %fresh316 = ptrtoint [ 14 x i8]* @fresh316 to i64
    %fresh317 = call i64 @primPackedStringHead( i64 %fresh316 )
    store i64 %fresh317, i64* %x53
    ; Unit
    ;  (inlined)Variable_Unembedded x_530 = Value_Con (Constant_Alias "CChar")
    ;  (inlined)Variable_Unembedded x_531 = Value_Var (Variable_Unembedded x_53_UHC.Base.110_73_0)
    ; UpdateUnit
    store i64 17, i64* %x536
    %fresh319 = load i64* %x701
    %fresh320 = inttoptr i64 %fresh319 to i64*
    %fresh321 = getelementptr i64* %fresh320, i32 0
    store i64 17, i64* %fresh321
    %fresh322 = load i64* %x53
    store i64 %fresh322, i64* %x537
    %fresh323 = load i64* %x701
    %fresh324 = inttoptr i64 %fresh323 to i64*
    %fresh325 = getelementptr i64* %fresh324, i32 1
    store i64 %fresh322, i64* %fresh325
    br label %fresh327
    fresh326:
    call void @primPatternMatchFailure(  )
    unreachable
    br label %fresh327
    fresh327:
    br label %fresh351
    fresh328:
    ; Fetch
    ;  (inlined)Variable_Unembedded x_532 = Value_Var (Variable_Subs (Variable_Unembedded global_x_6_UHC.Base.undefined) 0)
    ; Case
    %fresh329 = load i64** @global_UHC_Base_undefined
    %fresh330 = getelementptr i64* %fresh329, i32 0
    %fresh331 = load i64* %fresh330
    switch i64 %fresh331, label %fresh348 [ i64 26, label %fresh332
                                            i64 35, label %fresh336 ]
    fresh332:
    ; Fetch
    ;  (inlined)Variable_Unembedded x_703 = Value_Var (Variable_Subs (Variable_Unembedded global_x_6_UHC.Base.undefined) 1)
    ; Unit
    store i64 26, i64* %x536
    %fresh333 = load i64** @global_UHC_Base_undefined
    %fresh334 = getelementptr i64* %fresh333, i32 1
    %fresh335 = load i64* %fresh334
    store i64 %fresh335, i64* %x537
    br label %fresh349
    fresh336:
    ; Call (Normal or Tail)
    call void @fun_UHC_Base_errortildespec1(  )
    ; Result
    ;  (inlined)Variable_Unembedded x_534 = Value_Var (Variable_Subs Variable_RP 0)
    ;  (inlined)Variable_Unembedded x_535 = Value_Var (Variable_Subs Variable_RP 1)
    ; UpdateUnit
    %fresh338 = load i64** @RP
    %fresh339 = getelementptr i64* %fresh338, i32 0
    %fresh340 = load i64* %fresh339
    store i64 %fresh340, i64* %x536
    %fresh341 = load i64** @global_UHC_Base_undefined
    %fresh342 = getelementptr i64* %fresh341, i32 0
    store i64 %fresh340, i64* %fresh342
    %fresh343 = load i64** @RP
    %fresh344 = getelementptr i64* %fresh343, i32 1
    %fresh345 = load i64* %fresh344
    store i64 %fresh345, i64* %x537
    %fresh346 = load i64** @global_UHC_Base_undefined
    %fresh347 = getelementptr i64* %fresh346, i32 1
    store i64 %fresh345, i64* %fresh347
    br label %fresh349
    fresh348:
    call void @primPatternMatchFailure(  )
    unreachable
    br label %fresh349
    fresh349:
    br label %fresh351
    fresh350:
    call void @primPatternMatchFailure(  )
    unreachable
    br label %fresh351
    fresh351:
    ; UpdateUnit
    %fresh352 = load i64* %x536
    %fresh353 = load i64* %x493
    %fresh354 = inttoptr i64 %fresh353 to i64*
    %fresh355 = getelementptr i64* %fresh354, i32 0
    store i64 %fresh352, i64* %fresh355
    %fresh356 = load i64* %x537
    store i64 %fresh356, i64* %x198
    %fresh357 = load i64* %x493
    %fresh358 = inttoptr i64 %fresh357 to i64*
    %fresh359 = getelementptr i64* %fresh358, i32 1
    store i64 %fresh356, i64* %fresh359
    br label %fresh361
    fresh360:
    call void @primPatternMatchFailure(  )
    unreachable
    br label %fresh361
    fresh361:
    ; FFI
    %fresh362 = load i64* %x197
    %fresh363 = load i64* %x198
    %fresh364 = call i64 @primHPutChar( i64 %fresh362, i64 %fresh363 )
    store i64 %fresh364, i64* %x199
    ; Unit (Yielding)
    %fresh367 = load i64* %x199
    %fresh368 = load i64** @RP
    %fresh369 = getelementptr i64* %fresh368, i32 0
    store i64 %fresh367, i64* %fresh369
    ret void
}

define internal void @fun_UHC_Base_primAddInt( i64 %x40, i64 %x41 ) nounwind
{
    %x36 = alloca i64, i32 1
    %x37 = alloca i64, i32 1
    %x42 = alloca i64, i32 1
    %x43 = alloca i64, i32 1
    %x44 = alloca i64, i32 1
    %x57 = alloca i64, i32 1
    %x94 = alloca i64, i32 1
    %x95 = alloca i64, i32 1
    %x550 = alloca i64, i32 1
    %x556 = alloca i64, i32 1
    %x557 = alloca i64, i32 1
    %x705 = alloca i64, i32 1
    %x709 = alloca i64, i32 1
    ; Fetch
    ;  (inlined)Variable_Unembedded x_538 = Value_Var (Variable_Subs (Variable_Unembedded x_40_UHC.Base.110_69_2) 0)
    ; Case
    %fresh370 = inttoptr i64 %x40 to i64*
    %fresh371 = getelementptr i64* %fresh370, i32 0
    %fresh372 = load i64* %fresh371
    switch i64 %fresh372, label %fresh429 [ i64 18, label %fresh373
                                            i64 28, label %fresh377 ]
    fresh373:
    ; Fetch
    ;  (inlined)Variable_Unembedded x_704 = Value_Var (Variable_Subs (Variable_Unembedded x_40_UHC.Base.110_69_2) 1)
    ; Unit
    %fresh374 = inttoptr i64 %x40 to i64*
    %fresh375 = getelementptr i64* %fresh374, i32 1
    %fresh376 = load i64* %fresh375
    store i64 %fresh376, i64* %x42
    br label %fresh430
    fresh377:
    ; Fetch
    %fresh378 = inttoptr i64 %x40 to i64*
    %fresh379 = getelementptr i64* %fresh378, i32 1
    %fresh380 = load i64* %fresh379
    store i64 %fresh380, i64* %x705
    ; Fetch
    ;  (inlined)Variable_Unembedded x_540 = Value_Var (Variable_Subs (Variable_Unembedded x_705) 0)
    ; Case
    %fresh381 = load i64* %x705
    %fresh382 = inttoptr i64 %fresh381 to i64*
    %fresh383 = getelementptr i64* %fresh382, i32 0
    %fresh384 = load i64* %fresh383
    switch i64 %fresh384, label %fresh419 [ i64 17, label %fresh385
                                            i64 29, label %fresh390
                                            i64 31, label %fresh408 ]
    fresh385:
    ; Fetch
    ;  (inlined)Variable_Unembedded x_706 = Value_Var (Variable_Subs (Variable_Unembedded x_705) 1)
    ; Unit
    %fresh386 = load i64* %x705
    %fresh387 = inttoptr i64 %fresh386 to i64*
    %fresh388 = getelementptr i64* %fresh387, i32 1
    %fresh389 = load i64* %fresh388
    store i64 %fresh389, i64* %x36
    br label %fresh420
    fresh390:
    ; Fetch
    ;  (inlined)Variable_Unembedded x_707 = Value_Var (Variable_Subs (Variable_Unembedded x_705) 1)
    ; Call (Normal or Tail)
    %fresh391 = load i64* %x705
    %fresh392 = inttoptr i64 %fresh391 to i64*
    %fresh393 = getelementptr i64* %fresh392, i32 1
    %fresh394 = load i64* %fresh393
    call void @fun_UHC_Base_packedStringHead( i64 %fresh394 )
    ; Result
    ;  (inlined)Variable_Unembedded x_542 = Value_Var (Variable_Subs Variable_RP 0)
    ;  (inlined)Variable_Unembedded x_543 = Value_Var (Variable_Subs Variable_RP 1)
    ; UpdateUnit
    %fresh396 = load i64** @RP
    %fresh397 = getelementptr i64* %fresh396, i32 0
    %fresh398 = load i64* %fresh397
    %fresh399 = load i64* %x705
    %fresh400 = inttoptr i64 %fresh399 to i64*
    %fresh401 = getelementptr i64* %fresh400, i32 0
    store i64 %fresh398, i64* %fresh401
    %fresh402 = load i64** @RP
    %fresh403 = getelementptr i64* %fresh402, i32 1
    %fresh404 = load i64* %fresh403
    store i64 %fresh404, i64* %x36
    %fresh405 = load i64* %x705
    %fresh406 = inttoptr i64 %fresh405 to i64*
    %fresh407 = getelementptr i64* %fresh406, i32 1
    store i64 %fresh404, i64* %fresh407
    br label %fresh420
    fresh408:
    ; Unit
    ;  (inlined)Variable_Unembedded x_56_UHC.Base._110_73_2 = Value_Con (Constant_LiteralStr "Prelude.undefined")
    ; FFI
    %fresh409 = ptrtoint [ 18 x i8]* @fresh409 to i64
    %fresh410 = call i64 @primPackedStringHead( i64 %fresh409 )
    store i64 %fresh410, i64* %x57
    ; Unit
    ;  (inlined)Variable_Unembedded x_544 = Value_Con (Constant_Alias "CChar")
    ;  (inlined)Variable_Unembedded x_545 = Value_Var (Variable_Unembedded x_57_UHC.Base.110_73_0)
    ; UpdateUnit
    %fresh412 = load i64* %x705
    %fresh413 = inttoptr i64 %fresh412 to i64*
    %fresh414 = getelementptr i64* %fresh413, i32 0
    store i64 17, i64* %fresh414
    %fresh415 = load i64* %x57
    store i64 %fresh415, i64* %x36
    %fresh416 = load i64* %x705
    %fresh417 = inttoptr i64 %fresh416 to i64*
    %fresh418 = getelementptr i64* %fresh417, i32 1
    store i64 %fresh415, i64* %fresh418
    br label %fresh420
    fresh419:
    call void @primPatternMatchFailure(  )
    unreachable
    br label %fresh420
    fresh420:
    ; FFI
    %fresh421 = load i64* %x36
    %fresh422 = call i64 @primUnsafeId( i64 %fresh421 )
    store i64 %fresh422, i64* %x37
    ; Unit
    ;  (inlined)Variable_Unembedded x_546 = Value_Con (Constant_Alias "CInt")
    ;  (inlined)Variable_Unembedded x_547 = Value_Var (Variable_Unembedded x_37_UHC.Base.110_65_0)
    ; UpdateUnit
    %fresh424 = inttoptr i64 %x40 to i64*
    %fresh425 = getelementptr i64* %fresh424, i32 0
    store i64 18, i64* %fresh425
    %fresh426 = load i64* %x37
    store i64 %fresh426, i64* %x42
    %fresh427 = inttoptr i64 %x40 to i64*
    %fresh428 = getelementptr i64* %fresh427, i32 1
    store i64 %fresh426, i64* %fresh428
    br label %fresh430
    fresh429:
    call void @primPatternMatchFailure(  )
    unreachable
    br label %fresh430
    fresh430:
    ; Fetch
    ;  (inlined)Variable_Unembedded x_548 = Value_Var (Variable_Subs (Variable_Unembedded x_41_UHC.Base.110_69_3) 0)
    ; Case
    %fresh431 = inttoptr i64 %x41 to i64*
    %fresh432 = getelementptr i64* %fresh431, i32 0
    %fresh433 = load i64* %fresh432
    switch i64 %fresh433, label %fresh518 [ i64 18, label %fresh434
                                            i64 36, label %fresh438 ]
    fresh434:
    ; Fetch
    ;  (inlined)Variable_Unembedded x_708 = Value_Var (Variable_Subs (Variable_Unembedded x_41_UHC.Base.110_69_3) 1)
    ; Unit
    %fresh435 = inttoptr i64 %x41 to i64*
    %fresh436 = getelementptr i64* %fresh435, i32 1
    %fresh437 = load i64* %fresh436
    store i64 %fresh437, i64* %x43
    br label %fresh519
    fresh438:
    ; Fetch
    %fresh439 = inttoptr i64 %x41 to i64*
    %fresh440 = getelementptr i64* %fresh439, i32 1
    %fresh441 = load i64* %fresh440
    store i64 %fresh441, i64* %x709
    ; Fetch
    %fresh442 = load i64* %x709
    %fresh443 = inttoptr i64 %fresh442 to i64*
    %fresh444 = getelementptr i64* %fresh443, i32 0
    %fresh445 = load i64* %fresh444
    store i64 %fresh445, i64* %x550
    ; Case
    %fresh446 = load i64* %x550
    %fresh447 = icmp sgt i64 %fresh446, 27
    br i1 %fresh447, label %fresh448, label %fresh472
    fresh448:
    ; Fetch
    ;  (inlined)Variable_Unembedded x_710 = Value_Var (Variable_Subs (Variable_Unembedded x_709) 1)
    ; Call (Normal or Tail)
    %fresh449 = load i64* %x709
    %fresh450 = inttoptr i64 %fresh449 to i64*
    %fresh451 = getelementptr i64* %fresh450, i32 1
    %fresh452 = load i64* %fresh451
    call void @fun_UHC_Base_packedStringToString( i64 %fresh452 )
    ; Result
    ;  (inlined)Variable_Unembedded x_553 = Value_Var (Variable_Subs Variable_RP 0)
    ;  (inlined)Variable_Unembedded x_554 = Value_Var (Variable_Subs Variable_RP 1)
    ;  (inlined)Variable_Unembedded x_555 = Value_Var (Variable_Subs Variable_RP 2)
    ; UpdateUnit
    %fresh454 = load i64** @RP
    %fresh455 = getelementptr i64* %fresh454, i32 0
    %fresh456 = load i64* %fresh455
    store i64 %fresh456, i64* %x550
    %fresh457 = load i64* %x709
    %fresh458 = inttoptr i64 %fresh457 to i64*
    %fresh459 = getelementptr i64* %fresh458, i32 0
    store i64 %fresh456, i64* %fresh459
    %fresh460 = load i64** @RP
    %fresh461 = getelementptr i64* %fresh460, i32 1
    %fresh462 = load i64* %fresh461
    %fresh463 = load i64* %x709
    %fresh464 = inttoptr i64 %fresh463 to i64*
    %fresh465 = getelementptr i64* %fresh464, i32 1
    store i64 %fresh462, i64* %fresh465
    %fresh466 = load i64** @RP
    %fresh467 = getelementptr i64* %fresh466, i32 2
    %fresh468 = load i64* %fresh467
    %fresh469 = load i64* %x709
    %fresh470 = inttoptr i64 %fresh469 to i64*
    %fresh471 = getelementptr i64* %fresh470, i32 2
    store i64 %fresh468, i64* %fresh471
    br label %fresh473
    fresh472:
    br label %fresh473
    fresh473:
    %fresh474 = load i64* %x550
    switch i64 %fresh474, label %fresh510 [ i64 3, label %fresh475
                                            i64 4, label %fresh509 ]
    fresh475:
    ; Fetch
    ;  (inlined)Variable_Unembedded x_712 = Value_Var (Variable_Subs (Variable_Unembedded x_709) 1)
    ; Fetch
    ;  (inlined)Variable_Unembedded x_711 = Value_Var (Variable_Subs (Variable_Unembedded x_709) 2)
    ; Store
    %fresh476 = call i64* @llvmgc_malloc( i64 16 )
    %fresh477 = ptrtoint i64* %fresh476 to i64
    store i64 %fresh477, i64* %x94
    %fresh478 = load i64* %x94
    %fresh479 = inttoptr i64 %fresh478 to i64*
    %fresh480 = getelementptr i64* %fresh479, i32 0
    store i64 36, i64* %fresh480
    %fresh481 = load i64* %x709
    %fresh482 = inttoptr i64 %fresh481 to i64*
    %fresh483 = getelementptr i64* %fresh482, i32 2
    %fresh484 = load i64* %fresh483
    %fresh485 = load i64* %x94
    %fresh486 = inttoptr i64 %fresh485 to i64*
    %fresh487 = getelementptr i64* %fresh486, i32 1
    store i64 %fresh484, i64* %fresh487
    ; Store
    %fresh488 = call i64* @llvmgc_malloc( i64 16 )
    %fresh489 = ptrtoint i64* %fresh488 to i64
    store i64 %fresh489, i64* %x95
    %fresh490 = load i64* %x95
    %fresh491 = inttoptr i64 %fresh490 to i64*
    %fresh492 = getelementptr i64* %fresh491, i32 0
    store i64 28, i64* %fresh492
    %fresh493 = load i64* %x709
    %fresh494 = inttoptr i64 %fresh493 to i64*
    %fresh495 = getelementptr i64* %fresh494, i32 1
    %fresh496 = load i64* %fresh495
    %fresh497 = load i64* %x95
    %fresh498 = inttoptr i64 %fresh497 to i64*
    %fresh499 = getelementptr i64* %fresh498, i32 1
    store i64 %fresh496, i64* %fresh499
    ; Call (Normal or Tail)
    %fresh500 = load i64* %x95
    %fresh501 = load i64* %x94
    call void @fun_UHC_Base_primAddInt( i64 %fresh500, i64 %fresh501 )
    ; Result
    %fresh503 = load i64** @RP
    %fresh504 = getelementptr i64* %fresh503, i32 0
    %fresh505 = load i64* %fresh504
    store i64 %fresh505, i64* %x556
    %fresh506 = load i64** @RP
    %fresh507 = getelementptr i64* %fresh506, i32 1
    %fresh508 = load i64* %fresh507
    store i64 %fresh508, i64* %x557
    br label %fresh511
    fresh509:
    ; Unit
    store i64 18, i64* %x556
    store i64 0, i64* %x557
    br label %fresh511
    fresh510:
    call void @primPatternMatchFailure(  )
    unreachable
    br label %fresh511
    fresh511:
    ; UpdateUnit
    %fresh512 = load i64* %x556
    %fresh513 = inttoptr i64 %x41 to i64*
    %fresh514 = getelementptr i64* %fresh513, i32 0
    store i64 %fresh512, i64* %fresh514
    %fresh515 = load i64* %x557
    store i64 %fresh515, i64* %x43
    %fresh516 = inttoptr i64 %x41 to i64*
    %fresh517 = getelementptr i64* %fresh516, i32 1
    store i64 %fresh515, i64* %fresh517
    br label %fresh519
    fresh518:
    call void @primPatternMatchFailure(  )
    unreachable
    br label %fresh519
    fresh519:
    ; FFI
    %fresh520 = load i64* %x42
    %fresh521 = load i64* %x43
    %fresh522 = call i64 @primAddInt( i64 %fresh520, i64 %fresh521 )
    store i64 %fresh522, i64* %x44
    ; Unit (Yielding)
    %fresh525 = load i64** @RP
    %fresh526 = getelementptr i64* %fresh525, i32 0
    store i64 18, i64* %fresh526
    %fresh527 = load i64* %x44
    %fresh528 = load i64** @RP
    %fresh529 = getelementptr i64* %fresh528, i32 1
    store i64 %fresh527, i64* %fresh529
    ret void
}

define internal void @fun_UHC_Base_packedStringHead( i64 %x47 ) nounwind
{
    %x48 = alloca i64, i32 1
    %x49 = alloca i64, i32 1
    ; Fetch
    ;  (inlined)Variable_Unembedded x_558 = Value_Var (Variable_Subs (Variable_Unembedded x_47_UHC.Base.110_73_2) 0)
    ; Case
    %fresh530 = inttoptr i64 %x47 to i64*
    %fresh531 = getelementptr i64* %fresh530, i32 0
    %fresh532 = load i64* %fresh531
    switch i64 %fresh532, label %fresh576 [ i64 19, label %fresh533
                                            i64 32, label %fresh537
                                            i64 33, label %fresh552
                                            i64 34, label %fresh564 ]
    fresh533:
    ; Fetch
    ;  (inlined)Variable_Unembedded x_713 = Value_Var (Variable_Subs (Variable_Unembedded x_47_UHC.Base.110_73_2) 1)
    ; Unit
    %fresh534 = inttoptr i64 %x47 to i64*
    %fresh535 = getelementptr i64* %fresh534, i32 1
    %fresh536 = load i64* %fresh535
    store i64 %fresh536, i64* %x48
    br label %fresh577
    fresh537:
    ; Fetch
    ;  (inlined)Variable_Unembedded x_714 = Value_Var (Variable_Subs (Variable_Unembedded x_47_UHC.Base.110_73_2) 1)
    ; Call (Normal or Tail)
    %fresh538 = inttoptr i64 %x47 to i64*
    %fresh539 = getelementptr i64* %fresh538, i32 1
    %fresh540 = load i64* %fresh539
    call void @fun_UHC_Base_packedStringTail( i64 %fresh540 )
    ; Result
    ;  (inlined)Variable_Unembedded x_560 = Value_Var (Variable_Subs Variable_RP 0)
    ;  (inlined)Variable_Unembedded x_561 = Value_Var (Variable_Subs Variable_RP 1)
    ; UpdateUnit
    %fresh542 = load i64** @RP
    %fresh543 = getelementptr i64* %fresh542, i32 0
    %fresh544 = load i64* %fresh543
    %fresh545 = inttoptr i64 %x47 to i64*
    %fresh546 = getelementptr i64* %fresh545, i32 0
    store i64 %fresh544, i64* %fresh546
    %fresh547 = load i64** @RP
    %fresh548 = getelementptr i64* %fresh547, i32 1
    %fresh549 = load i64* %fresh548
    store i64 %fresh549, i64* %x48
    %fresh550 = inttoptr i64 %x47 to i64*
    %fresh551 = getelementptr i64* %fresh550, i32 1
    store i64 %fresh549, i64* %fresh551
    br label %fresh577
    fresh552:
    ; Call (Normal or Tail)
    call void @fun_UHC_Base_packedStringTailtildespec1(  )
    ; Result
    ;  (inlined)Variable_Unembedded x_562 = Value_Var (Variable_Subs Variable_RP 0)
    ;  (inlined)Variable_Unembedded x_563 = Value_Var (Variable_Subs Variable_RP 1)
    ; UpdateUnit
    %fresh554 = load i64** @RP
    %fresh555 = getelementptr i64* %fresh554, i32 0
    %fresh556 = load i64* %fresh555
    %fresh557 = inttoptr i64 %x47 to i64*
    %fresh558 = getelementptr i64* %fresh557, i32 0
    store i64 %fresh556, i64* %fresh558
    %fresh559 = load i64** @RP
    %fresh560 = getelementptr i64* %fresh559, i32 1
    %fresh561 = load i64* %fresh560
    store i64 %fresh561, i64* %x48
    %fresh562 = inttoptr i64 %x47 to i64*
    %fresh563 = getelementptr i64* %fresh562, i32 1
    store i64 %fresh561, i64* %fresh563
    br label %fresh577
    fresh564:
    ; Call (Normal or Tail)
    call void @fun_UHC_Base_packedStringTailtildespec14(  )
    ; Result
    ;  (inlined)Variable_Unembedded x_564 = Value_Var (Variable_Subs Variable_RP 0)
    ;  (inlined)Variable_Unembedded x_565 = Value_Var (Variable_Subs Variable_RP 1)
    ; UpdateUnit
    %fresh566 = load i64** @RP
    %fresh567 = getelementptr i64* %fresh566, i32 0
    %fresh568 = load i64* %fresh567
    %fresh569 = inttoptr i64 %x47 to i64*
    %fresh570 = getelementptr i64* %fresh569, i32 0
    store i64 %fresh568, i64* %fresh570
    %fresh571 = load i64** @RP
    %fresh572 = getelementptr i64* %fresh571, i32 1
    %fresh573 = load i64* %fresh572
    store i64 %fresh573, i64* %x48
    %fresh574 = inttoptr i64 %x47 to i64*
    %fresh575 = getelementptr i64* %fresh574, i32 1
    store i64 %fresh573, i64* %fresh575
    br label %fresh577
    fresh576:
    call void @primPatternMatchFailure(  )
    unreachable
    br label %fresh577
    fresh577:
    ; FFI
    %fresh578 = load i64* %x48
    %fresh579 = call i64 @primPackedStringHead( i64 %fresh578 )
    store i64 %fresh579, i64* %x49
    ; Unit (Yielding)
    %fresh581 = load i64** @RP
    %fresh582 = getelementptr i64* %fresh581, i32 0
    store i64 17, i64* %fresh582
    %fresh583 = load i64* %x49
    %fresh584 = load i64** @RP
    %fresh585 = getelementptr i64* %fresh584, i32 1
    store i64 %fresh583, i64* %fresh585
    ret void
}

define internal void @fun_UHC_Base_packedStringTail( i64 %x60 ) nounwind
{
    %x61 = alloca i64, i32 1
    %x62 = alloca i64, i32 1
    ; Fetch
    ;  (inlined)Variable_Unembedded x_566 = Value_Var (Variable_Subs (Variable_Unembedded x_60_UHC.Base.110_75_2) 0)
    ; Case
    %fresh586 = inttoptr i64 %x60 to i64*
    %fresh587 = getelementptr i64* %fresh586, i32 0
    %fresh588 = load i64* %fresh587
    switch i64 %fresh588, label %fresh632 [ i64 19, label %fresh589
                                            i64 32, label %fresh593
                                            i64 33, label %fresh608
                                            i64 34, label %fresh620 ]
    fresh589:
    ; Fetch
    ;  (inlined)Variable_Unembedded x_715 = Value_Var (Variable_Subs (Variable_Unembedded x_60_UHC.Base.110_75_2) 1)
    ; Unit
    %fresh590 = inttoptr i64 %x60 to i64*
    %fresh591 = getelementptr i64* %fresh590, i32 1
    %fresh592 = load i64* %fresh591
    store i64 %fresh592, i64* %x61
    br label %fresh633
    fresh593:
    ; Fetch
    ;  (inlined)Variable_Unembedded x_716 = Value_Var (Variable_Subs (Variable_Unembedded x_60_UHC.Base.110_75_2) 1)
    ; Call (Normal or Tail)
    %fresh594 = inttoptr i64 %x60 to i64*
    %fresh595 = getelementptr i64* %fresh594, i32 1
    %fresh596 = load i64* %fresh595
    call void @fun_UHC_Base_packedStringTail( i64 %fresh596 )
    ; Result
    ;  (inlined)Variable_Unembedded x_568 = Value_Var (Variable_Subs Variable_RP 0)
    ;  (inlined)Variable_Unembedded x_569 = Value_Var (Variable_Subs Variable_RP 1)
    ; UpdateUnit
    %fresh598 = load i64** @RP
    %fresh599 = getelementptr i64* %fresh598, i32 0
    %fresh600 = load i64* %fresh599
    %fresh601 = inttoptr i64 %x60 to i64*
    %fresh602 = getelementptr i64* %fresh601, i32 0
    store i64 %fresh600, i64* %fresh602
    %fresh603 = load i64** @RP
    %fresh604 = getelementptr i64* %fresh603, i32 1
    %fresh605 = load i64* %fresh604
    store i64 %fresh605, i64* %x61
    %fresh606 = inttoptr i64 %x60 to i64*
    %fresh607 = getelementptr i64* %fresh606, i32 1
    store i64 %fresh605, i64* %fresh607
    br label %fresh633
    fresh608:
    ; Call (Normal or Tail)
    call void @fun_UHC_Base_packedStringTailtildespec1(  )
    ; Result
    ;  (inlined)Variable_Unembedded x_570 = Value_Var (Variable_Subs Variable_RP 0)
    ;  (inlined)Variable_Unembedded x_571 = Value_Var (Variable_Subs Variable_RP 1)
    ; UpdateUnit
    %fresh610 = load i64** @RP
    %fresh611 = getelementptr i64* %fresh610, i32 0
    %fresh612 = load i64* %fresh611
    %fresh613 = inttoptr i64 %x60 to i64*
    %fresh614 = getelementptr i64* %fresh613, i32 0
    store i64 %fresh612, i64* %fresh614
    %fresh615 = load i64** @RP
    %fresh616 = getelementptr i64* %fresh615, i32 1
    %fresh617 = load i64* %fresh616
    store i64 %fresh617, i64* %x61
    %fresh618 = inttoptr i64 %x60 to i64*
    %fresh619 = getelementptr i64* %fresh618, i32 1
    store i64 %fresh617, i64* %fresh619
    br label %fresh633
    fresh620:
    ; Call (Normal or Tail)
    call void @fun_UHC_Base_packedStringTailtildespec14(  )
    ; Result
    ;  (inlined)Variable_Unembedded x_572 = Value_Var (Variable_Subs Variable_RP 0)
    ;  (inlined)Variable_Unembedded x_573 = Value_Var (Variable_Subs Variable_RP 1)
    ; UpdateUnit
    %fresh622 = load i64** @RP
    %fresh623 = getelementptr i64* %fresh622, i32 0
    %fresh624 = load i64* %fresh623
    %fresh625 = inttoptr i64 %x60 to i64*
    %fresh626 = getelementptr i64* %fresh625, i32 0
    store i64 %fresh624, i64* %fresh626
    %fresh627 = load i64** @RP
    %fresh628 = getelementptr i64* %fresh627, i32 1
    %fresh629 = load i64* %fresh628
    store i64 %fresh629, i64* %x61
    %fresh630 = inttoptr i64 %x60 to i64*
    %fresh631 = getelementptr i64* %fresh630, i32 1
    store i64 %fresh629, i64* %fresh631
    br label %fresh633
    fresh632:
    call void @primPatternMatchFailure(  )
    unreachable
    br label %fresh633
    fresh633:
    ; FFI
    %fresh634 = load i64* %x61
    %fresh635 = call i64 @primPackedStringTail( i64 %fresh634 )
    store i64 %fresh635, i64* %x62
    ; Unit (Yielding)
    %fresh637 = load i64** @RP
    %fresh638 = getelementptr i64* %fresh637, i32 0
    store i64 19, i64* %fresh638
    %fresh639 = load i64* %x62
    %fresh640 = load i64** @RP
    %fresh641 = getelementptr i64* %fresh640, i32 1
    store i64 %fresh639, i64* %fresh641
    ret void
}

define internal void @fun_UHC_Base_packedStringTailtildespec1(  ) nounwind
{
    %x66 = alloca i64, i32 1
    ; Unit
    ;  (inlined)Variable_Unembedded x_65_UHC.Base._110_75_2 = Value_Con (Constant_LiteralStr "Hello, World!")
    ; FFI
    %fresh642 = ptrtoint [ 14 x i8]* @fresh642 to i64
    %fresh643 = call i64 @primPackedStringTail( i64 %fresh642 )
    store i64 %fresh643, i64* %x66
    ; Unit (Yielding)
    %fresh645 = load i64** @RP
    %fresh646 = getelementptr i64* %fresh645, i32 0
    store i64 19, i64* %fresh646
    %fresh647 = load i64* %x66
    %fresh648 = load i64** @RP
    %fresh649 = getelementptr i64* %fresh648, i32 1
    store i64 %fresh647, i64* %fresh649
    ret void
}

define internal void @fun_UHC_Base_packedStringTailtildespec14(  ) nounwind
{
    %x70 = alloca i64, i32 1
    ; Unit
    ;  (inlined)Variable_Unembedded x_69_UHC.Base._110_75_2 = Value_Con (Constant_LiteralStr "Prelude.undefined")
    ; FFI
    %fresh650 = ptrtoint [ 18 x i8]* @fresh650 to i64
    %fresh651 = call i64 @primPackedStringTail( i64 %fresh650 )
    store i64 %fresh651, i64* %x70
    ; Unit (Yielding)
    %fresh653 = load i64** @RP
    %fresh654 = getelementptr i64* %fresh653, i32 0
    store i64 19, i64* %fresh654
    %fresh655 = load i64* %x70
    %fresh656 = load i64** @RP
    %fresh657 = getelementptr i64* %fresh656, i32 1
    store i64 %fresh655, i64* %fresh657
    ret void
}

define internal void @fun_UHC_Base_primretIO( i64 %x73, i64 %x74 ) nounwind
{
    ; Unit (Yielding)
    %fresh658 = load i64** @RP
    %fresh659 = getelementptr i64* %fresh658, i32 0
    store i64 16, i64* %fresh659
    %fresh660 = load i64** @RP
    %fresh661 = getelementptr i64* %fresh660, i32 1
    store i64 %x74, i64* %fresh661
    %fresh662 = load i64** @RP
    %fresh663 = getelementptr i64* %fresh662, i32 2
    store i64 %x73, i64* %fresh663
    ret void
}

define internal void @fun_UHC_Base_ioFromPrim( i64 %x77, i64 %x78 ) nounwind
{
    %x79 = alloca i64, i32 1
    ; Store
    %fresh664 = call i64* @llvmgc_malloc( i64 24 )
    %fresh665 = ptrtoint i64* %fresh664 to i64
    store i64 %fresh665, i64* %x79
    %fresh666 = load i64* %x79
    %fresh667 = inttoptr i64 %fresh666 to i64*
    %fresh668 = getelementptr i64* %fresh667, i32 0
    store i64 50, i64* %fresh668
    %fresh669 = load i64* %x79
    %fresh670 = inttoptr i64 %fresh669 to i64*
    %fresh671 = getelementptr i64* %fresh670, i32 1
    store i64 %x77, i64* %fresh671
    %fresh672 = load i64* %x79
    %fresh673 = inttoptr i64 %fresh672 to i64*
    %fresh674 = getelementptr i64* %fresh673, i32 2
    store i64 %x78, i64* %fresh674
    ; Fetch
    ;  (inlined)Variable_Unembedded x_574 = Value_Var (Variable_Subs (Variable_Unembedded x_79_UHC.Base.x__161_0) 0)
    ; Case
    %fresh675 = load i64* %x79
    %fresh676 = inttoptr i64 %fresh675 to i64*
    %fresh677 = getelementptr i64* %fresh676, i32 0
    %fresh678 = load i64* %fresh677
    switch i64 %fresh678, label %fresh696 [ i64 0, label %fresh679
                                            i64 50, label %fresh680 ]
    fresh679:
    ; Unit
    br label %fresh697
    fresh680:
    ; Fetch
    ;  (inlined)Variable_Unembedded x_718 = Value_Var (Variable_Subs (Variable_Unembedded x_79_UHC.Base.x__161_0) 1)
    ; Fetch
    ;  (inlined)Variable_Unembedded x_717 = Value_Var (Variable_Subs (Variable_Unembedded x_79_UHC.Base.x__161_0) 2)
    ; Call (Normal or Tail)
    %fresh681 = load i64* %x79
    %fresh682 = inttoptr i64 %fresh681 to i64*
    %fresh683 = getelementptr i64* %fresh682, i32 1
    %fresh684 = load i64* %fresh683
    %fresh685 = load i64* %x79
    %fresh686 = inttoptr i64 %fresh685 to i64*
    %fresh687 = getelementptr i64* %fresh686, i32 2
    %fresh688 = load i64* %fresh687
    call void @fun_app2_1( i64 %fresh684, i64 %fresh688 )
    ; Result
    ;  (inlined)Variable_Unembedded x_577 = Value_Var (Variable_Subs Variable_RP 0)
    ; UpdateUnit
    %fresh690 = load i64** @RP
    %fresh691 = getelementptr i64* %fresh690, i32 0
    %fresh692 = load i64* %fresh691
    %fresh693 = load i64* %x79
    %fresh694 = inttoptr i64 %fresh693 to i64*
    %fresh695 = getelementptr i64* %fresh694, i32 0
    store i64 %fresh692, i64* %fresh695
    br label %fresh697
    fresh696:
    call void @primPatternMatchFailure(  )
    unreachable
    br label %fresh697
    fresh697:
    ; Unit (Yielding)
    %fresh698 = load i64** @RP
    %fresh699 = getelementptr i64* %fresh698, i32 0
    store i64 16, i64* %fresh699
    %fresh700 = load i64** @RP
    %fresh701 = getelementptr i64* %fresh700, i32 1
    store i64 %x78, i64* %fresh701
    %fresh702 = load i64* %x79
    %fresh703 = load i64** @RP
    %fresh704 = getelementptr i64* %fresh703, i32 2
    store i64 %fresh702, i64* %fresh704
    ret void
}

define internal void @fun_UHC_Base_errortildespec1(  ) nounwind
{
    %x83 = alloca i64, i32 1
    %x84 = alloca i64, i32 1
    %x102 = alloca i64, i32 1
    %x103 = alloca i64, i32 1
    %x578 = alloca i64, i32 1
    ; Fetch
    %fresh705 = load i64** @global_UHC_Base_y103_0_459
    %fresh706 = getelementptr i64* %fresh705, i32 0
    %fresh707 = load i64* %fresh706
    store i64 %fresh707, i64* %x578
    ; Case
    %fresh708 = load i64* %x578
    %fresh709 = icmp sgt i64 %fresh708, 27
    br i1 %fresh709, label %fresh710, label %fresh727
    fresh710:
    ; Call (Normal or Tail)
    call void @fun_UHC_Base_packedStringToStringtildespec14(  )
    ; Result
    ;  (inlined)Variable_Unembedded x_581 = Value_Var (Variable_Subs Variable_RP 0)
    ;  (inlined)Variable_Unembedded x_582 = Value_Var (Variable_Subs Variable_RP 1)
    ;  (inlined)Variable_Unembedded x_583 = Value_Var (Variable_Subs Variable_RP 2)
    ; UpdateUnit
    %fresh712 = load i64** @RP
    %fresh713 = getelementptr i64* %fresh712, i32 0
    %fresh714 = load i64* %fresh713
    store i64 %fresh714, i64* %x578
    %fresh715 = load i64** @global_UHC_Base_y103_0_459
    %fresh716 = getelementptr i64* %fresh715, i32 0
    store i64 %fresh714, i64* %fresh716
    %fresh717 = load i64** @RP
    %fresh718 = getelementptr i64* %fresh717, i32 1
    %fresh719 = load i64* %fresh718
    %fresh720 = load i64** @global_UHC_Base_y103_0_459
    %fresh721 = getelementptr i64* %fresh720, i32 1
    store i64 %fresh719, i64* %fresh721
    %fresh722 = load i64** @RP
    %fresh723 = getelementptr i64* %fresh722, i32 2
    %fresh724 = load i64* %fresh723
    %fresh725 = load i64** @global_UHC_Base_y103_0_459
    %fresh726 = getelementptr i64* %fresh725, i32 2
    store i64 %fresh724, i64* %fresh726
    br label %fresh728
    fresh727:
    br label %fresh728
    fresh728:
    %fresh729 = load i64* %x578
    switch i64 %fresh729, label %fresh757 [ i64 3, label %fresh730
                                            i64 4, label %fresh756 ]
    fresh730:
    ; Fetch
    ;  (inlined)Variable_Unembedded x_720 = Value_Var (Variable_Subs (Variable_Unembedded global_x_7_UHC.Base.103_0_459) 1)
    ; Fetch
    ;  (inlined)Variable_Unembedded x_719 = Value_Var (Variable_Subs (Variable_Unembedded global_x_7_UHC.Base.103_0_459) 2)
    ; Store
    %fresh731 = call i64* @llvmgc_malloc( i64 16 )
    %fresh732 = ptrtoint i64* %fresh731 to i64
    store i64 %fresh732, i64* %x102
    %fresh733 = load i64* %x102
    %fresh734 = inttoptr i64 %fresh733 to i64*
    %fresh735 = getelementptr i64* %fresh734, i32 0
    store i64 36, i64* %fresh735
    %fresh736 = load i64** @global_UHC_Base_y103_0_459
    %fresh737 = getelementptr i64* %fresh736, i32 2
    %fresh738 = load i64* %fresh737
    %fresh739 = load i64* %x102
    %fresh740 = inttoptr i64 %fresh739 to i64*
    %fresh741 = getelementptr i64* %fresh740, i32 1
    store i64 %fresh738, i64* %fresh741
    ; Store
    %fresh742 = call i64* @llvmgc_malloc( i64 16 )
    %fresh743 = ptrtoint i64* %fresh742 to i64
    store i64 %fresh743, i64* %x103
    %fresh744 = load i64* %x103
    %fresh745 = inttoptr i64 %fresh744 to i64*
    %fresh746 = getelementptr i64* %fresh745, i32 0
    store i64 28, i64* %fresh746
    %fresh747 = load i64** @global_UHC_Base_y103_0_459
    %fresh748 = getelementptr i64* %fresh747, i32 1
    %fresh749 = load i64* %fresh748
    %fresh750 = load i64* %x103
    %fresh751 = inttoptr i64 %fresh750 to i64*
    %fresh752 = getelementptr i64* %fresh751, i32 1
    store i64 %fresh749, i64* %fresh752
    ; Call (Normal or Tail)
    %fresh753 = load i64* %x103
    %fresh754 = load i64* %x102
    call void @fun_UHC_Base_primAddInt( i64 %fresh753, i64 %fresh754 )
    ; Result
    br label %fresh758
    fresh756:
    ; Unit
    br label %fresh758
    fresh757:
    call void @primPatternMatchFailure(  )
    unreachable
    br label %fresh758
    fresh758:
    ; Fetch
    ;  (inlined)Variable_Unembedded x_584 = Value_Var (Variable_Subs (Variable_Unembedded global_x_7_UHC.Base.103_0_459) 0)
    ; Case
    %fresh759 = load i64** @global_UHC_Base_y103_0_459
    %fresh760 = getelementptr i64* %fresh759, i32 0
    %fresh761 = load i64* %fresh760
    switch i64 %fresh761, label %fresh814 [ i64 3, label %fresh762
                                            i64 4, label %fresh780
                                            i64 39, label %fresh786 ]
    fresh762:
    ; Fetch
    ;  (inlined)Variable_Unembedded x_722 = Value_Var (Variable_Subs (Variable_Unembedded global_x_7_UHC.Base.103_0_459) 1)
    ; Fetch
    ;  (inlined)Variable_Unembedded x_721 = Value_Var (Variable_Subs (Variable_Unembedded global_x_7_UHC.Base.103_0_459) 2)
    ; Unit to PtrNode
    %fresh763 = call i64* @llvmgc_malloc( i64 24 )
    %fresh764 = ptrtoint i64* %fresh763 to i64
    store i64 %fresh764, i64* %x83
    %fresh765 = load i64* %x83
    %fresh766 = inttoptr i64 %fresh765 to i64*
    %fresh767 = getelementptr i64* %fresh766, i32 0
    store i64 3, i64* %fresh767
    %fresh768 = load i64** @global_UHC_Base_y103_0_459
    %fresh769 = getelementptr i64* %fresh768, i32 1
    %fresh770 = load i64* %fresh769
    %fresh771 = load i64* %x83
    %fresh772 = inttoptr i64 %fresh771 to i64*
    %fresh773 = getelementptr i64* %fresh772, i32 1
    store i64 %fresh770, i64* %fresh773
    %fresh774 = load i64** @global_UHC_Base_y103_0_459
    %fresh775 = getelementptr i64* %fresh774, i32 2
    %fresh776 = load i64* %fresh775
    %fresh777 = load i64* %x83
    %fresh778 = inttoptr i64 %fresh777 to i64*
    %fresh779 = getelementptr i64* %fresh778, i32 2
    store i64 %fresh776, i64* %fresh779
    br label %fresh815
    fresh780:
    ; Unit to PtrNode
    %fresh781 = call i64* @llvmgc_malloc( i64 8 )
    %fresh782 = ptrtoint i64* %fresh781 to i64
    store i64 %fresh782, i64* %x83
    %fresh783 = load i64* %x83
    %fresh784 = inttoptr i64 %fresh783 to i64*
    %fresh785 = getelementptr i64* %fresh784, i32 0
    store i64 4, i64* %fresh785
    br label %fresh815
    fresh786:
    ; Call (Normal or Tail)
    call void @fun_UHC_Base_packedStringToStringtildespec14(  )
    ; Result
    ;  (inlined)Variable_Unembedded x_587 = Value_Var (Variable_Subs Variable_RP 0)
    ;  (inlined)Variable_Unembedded x_588 = Value_Var (Variable_Subs Variable_RP 1)
    ;  (inlined)Variable_Unembedded x_589 = Value_Var (Variable_Subs Variable_RP 2)
    ; Updateunit to PtrNode
    %fresh788 = call i64* @llvmgc_malloc( i64 24 )
    %fresh789 = ptrtoint i64* %fresh788 to i64
    store i64 %fresh789, i64* %x83
    %fresh790 = load i64** @RP
    %fresh791 = getelementptr i64* %fresh790, i32 0
    %fresh792 = load i64* %fresh791
    %fresh793 = load i64** @global_UHC_Base_y103_0_459
    %fresh794 = getelementptr i64* %fresh793, i32 0
    store i64 %fresh792, i64* %fresh794
    %fresh795 = load i64* %x83
    %fresh796 = inttoptr i64 %fresh795 to i64*
    %fresh797 = getelementptr i64* %fresh796, i32 0
    store i64 %fresh792, i64* %fresh797
    %fresh798 = load i64** @RP
    %fresh799 = getelementptr i64* %fresh798, i32 1
    %fresh800 = load i64* %fresh799
    %fresh801 = load i64** @global_UHC_Base_y103_0_459
    %fresh802 = getelementptr i64* %fresh801, i32 1
    store i64 %fresh800, i64* %fresh802
    %fresh803 = load i64* %x83
    %fresh804 = inttoptr i64 %fresh803 to i64*
    %fresh805 = getelementptr i64* %fresh804, i32 1
    store i64 %fresh800, i64* %fresh805
    %fresh806 = load i64** @RP
    %fresh807 = getelementptr i64* %fresh806, i32 2
    %fresh808 = load i64* %fresh807
    %fresh809 = load i64** @global_UHC_Base_y103_0_459
    %fresh810 = getelementptr i64* %fresh809, i32 2
    store i64 %fresh808, i64* %fresh810
    %fresh811 = load i64* %x83
    %fresh812 = inttoptr i64 %fresh811 to i64*
    %fresh813 = getelementptr i64* %fresh812, i32 2
    store i64 %fresh808, i64* %fresh813
    br label %fresh815
    fresh814:
    call void @primPatternMatchFailure(  )
    unreachable
    br label %fresh815
    fresh815:
    ; FFI
    %fresh816 = load i64* %x83
    %fresh817 = call i64 @primError( i64 %fresh816 )
    store i64 %fresh817, i64* %x84
    ; Unit (Yielding)
    %fresh819 = load i64** @RP
    %fresh820 = getelementptr i64* %fresh819, i32 0
    store i64 26, i64* %fresh820
    %fresh821 = load i64* %x84
    %fresh822 = load i64** @RP
    %fresh823 = getelementptr i64* %fresh822, i32 1
    store i64 %fresh821, i64* %fresh823
    ret void
}

define internal void @fun_UHC_Base_packedStringToString( i64 %x109 ) nounwind
{
    %x110 = alloca i64, i32 1
    %x111 = alloca i64, i32 1
    %x112 = alloca i64, i32 1
    %x113 = alloca i64, i32 1
    %x114 = alloca i64, i32 1
    ; Fetch
    ;  (inlined)Variable_Unembedded x_590 = Value_Var (Variable_Subs (Variable_Unembedded x_109_UHC.Base.p__297) 0)
    ; Case
    %fresh824 = inttoptr i64 %x109 to i64*
    %fresh825 = getelementptr i64* %fresh824, i32 0
    %fresh826 = load i64* %fresh825
    switch i64 %fresh826, label %fresh870 [ i64 19, label %fresh827
                                            i64 32, label %fresh831
                                            i64 33, label %fresh846
                                            i64 34, label %fresh858 ]
    fresh827:
    ; Fetch
    ;  (inlined)Variable_Unembedded x_723 = Value_Var (Variable_Subs (Variable_Unembedded x_109_UHC.Base.p__297) 1)
    ; Unit
    %fresh828 = inttoptr i64 %x109 to i64*
    %fresh829 = getelementptr i64* %fresh828, i32 1
    %fresh830 = load i64* %fresh829
    store i64 %fresh830, i64* %x110
    br label %fresh871
    fresh831:
    ; Fetch
    ;  (inlined)Variable_Unembedded x_724 = Value_Var (Variable_Subs (Variable_Unembedded x_109_UHC.Base.p__297) 1)
    ; Call (Normal or Tail)
    %fresh832 = inttoptr i64 %x109 to i64*
    %fresh833 = getelementptr i64* %fresh832, i32 1
    %fresh834 = load i64* %fresh833
    call void @fun_UHC_Base_packedStringTail( i64 %fresh834 )
    ; Result
    ;  (inlined)Variable_Unembedded x_592 = Value_Var (Variable_Subs Variable_RP 0)
    ;  (inlined)Variable_Unembedded x_593 = Value_Var (Variable_Subs Variable_RP 1)
    ; UpdateUnit
    %fresh836 = load i64** @RP
    %fresh837 = getelementptr i64* %fresh836, i32 0
    %fresh838 = load i64* %fresh837
    %fresh839 = inttoptr i64 %x109 to i64*
    %fresh840 = getelementptr i64* %fresh839, i32 0
    store i64 %fresh838, i64* %fresh840
    %fresh841 = load i64** @RP
    %fresh842 = getelementptr i64* %fresh841, i32 1
    %fresh843 = load i64* %fresh842
    store i64 %fresh843, i64* %x110
    %fresh844 = inttoptr i64 %x109 to i64*
    %fresh845 = getelementptr i64* %fresh844, i32 1
    store i64 %fresh843, i64* %fresh845
    br label %fresh871
    fresh846:
    ; Call (Normal or Tail)
    call void @fun_UHC_Base_packedStringTailtildespec1(  )
    ; Result
    ;  (inlined)Variable_Unembedded x_594 = Value_Var (Variable_Subs Variable_RP 0)
    ;  (inlined)Variable_Unembedded x_595 = Value_Var (Variable_Subs Variable_RP 1)
    ; UpdateUnit
    %fresh848 = load i64** @RP
    %fresh849 = getelementptr i64* %fresh848, i32 0
    %fresh850 = load i64* %fresh849
    %fresh851 = inttoptr i64 %x109 to i64*
    %fresh852 = getelementptr i64* %fresh851, i32 0
    store i64 %fresh850, i64* %fresh852
    %fresh853 = load i64** @RP
    %fresh854 = getelementptr i64* %fresh853, i32 1
    %fresh855 = load i64* %fresh854
    store i64 %fresh855, i64* %x110
    %fresh856 = inttoptr i64 %x109 to i64*
    %fresh857 = getelementptr i64* %fresh856, i32 1
    store i64 %fresh855, i64* %fresh857
    br label %fresh871
    fresh858:
    ; Call (Normal or Tail)
    call void @fun_UHC_Base_packedStringTailtildespec14(  )
    ; Result
    ;  (inlined)Variable_Unembedded x_596 = Value_Var (Variable_Subs Variable_RP 0)
    ;  (inlined)Variable_Unembedded x_597 = Value_Var (Variable_Subs Variable_RP 1)
    ; UpdateUnit
    %fresh860 = load i64** @RP
    %fresh861 = getelementptr i64* %fresh860, i32 0
    %fresh862 = load i64* %fresh861
    %fresh863 = inttoptr i64 %x109 to i64*
    %fresh864 = getelementptr i64* %fresh863, i32 0
    store i64 %fresh862, i64* %fresh864
    %fresh865 = load i64** @RP
    %fresh866 = getelementptr i64* %fresh865, i32 1
    %fresh867 = load i64* %fresh866
    store i64 %fresh867, i64* %x110
    %fresh868 = inttoptr i64 %x109 to i64*
    %fresh869 = getelementptr i64* %fresh868, i32 1
    store i64 %fresh867, i64* %fresh869
    br label %fresh871
    fresh870:
    call void @primPatternMatchFailure(  )
    unreachable
    br label %fresh871
    fresh871:
    ; FFI
    %fresh872 = load i64* %x110
    %fresh873 = call i64 @primPackedStringNull( i64 %fresh872 )
    store i64 %fresh873, i64* %x111
    ; Case
    %fresh875 = load i64* %x111
    switch i64 %fresh875, label %fresh913 [ i64 1, label %fresh876
                                            i64 2, label %fresh910 ]
    fresh876:
    ; Store
    %fresh877 = call i64* @llvmgc_malloc( i64 16 )
    %fresh878 = ptrtoint i64* %fresh877 to i64
    store i64 %fresh878, i64* %x112
    %fresh879 = load i64* %x112
    %fresh880 = inttoptr i64 %fresh879 to i64*
    %fresh881 = getelementptr i64* %fresh880, i32 0
    store i64 32, i64* %fresh881
    %fresh882 = load i64* %x112
    %fresh883 = inttoptr i64 %fresh882 to i64*
    %fresh884 = getelementptr i64* %fresh883, i32 1
    store i64 %x109, i64* %fresh884
    ; Store
    %fresh885 = call i64* @llvmgc_malloc( i64 24 )
    %fresh886 = ptrtoint i64* %fresh885 to i64
    store i64 %fresh886, i64* %x113
    %fresh887 = load i64* %x113
    %fresh888 = inttoptr i64 %fresh887 to i64*
    %fresh889 = getelementptr i64* %fresh888, i32 0
    store i64 37, i64* %fresh889
    %fresh890 = load i64* %x112
    %fresh891 = load i64* %x113
    %fresh892 = inttoptr i64 %fresh891 to i64*
    %fresh893 = getelementptr i64* %fresh892, i32 1
    store i64 %fresh890, i64* %fresh893
    ; Store
    %fresh894 = call i64* @llvmgc_malloc( i64 16 )
    %fresh895 = ptrtoint i64* %fresh894 to i64
    store i64 %fresh895, i64* %x114
    %fresh896 = load i64* %x114
    %fresh897 = inttoptr i64 %fresh896 to i64*
    %fresh898 = getelementptr i64* %fresh897, i32 0
    store i64 29, i64* %fresh898
    %fresh899 = load i64* %x114
    %fresh900 = inttoptr i64 %fresh899 to i64*
    %fresh901 = getelementptr i64* %fresh900, i32 1
    store i64 %x109, i64* %fresh901
    ; Unit (Yielding)
    %fresh902 = load i64** @RP
    %fresh903 = getelementptr i64* %fresh902, i32 0
    store i64 3, i64* %fresh903
    %fresh904 = load i64* %x114
    %fresh905 = load i64** @RP
    %fresh906 = getelementptr i64* %fresh905, i32 1
    store i64 %fresh904, i64* %fresh906
    %fresh907 = load i64* %x113
    %fresh908 = load i64** @RP
    %fresh909 = getelementptr i64* %fresh908, i32 2
    store i64 %fresh907, i64* %fresh909
    br label %fresh914
    fresh910:
    ; Unit (Yielding)
    %fresh911 = load i64** @RP
    %fresh912 = getelementptr i64* %fresh911, i32 0
    store i64 4, i64* %fresh912
    br label %fresh914
    fresh913:
    call void @primPatternMatchFailure(  )
    unreachable
    br label %fresh914
    fresh914:
    ret void
}

define internal void @fun_UHC_Base_packedStringToStringtildespec1(  ) nounwind
{
    %x118 = alloca i64, i32 1
    %x119 = alloca i64, i32 1
    %x120 = alloca i64, i32 1
    %x121 = alloca i64, i32 1
    ; Unit
    ;  (inlined)Variable_Unembedded x_117_UHC.Base._110_201_28 = Value_Con (Constant_LiteralStr "Hello, World!")
    ; FFI
    %fresh915 = ptrtoint [ 14 x i8]* @fresh915 to i64
    %fresh916 = call i64 @primPackedStringNull( i64 %fresh915 )
    store i64 %fresh916, i64* %x118
    ; Case
    %fresh918 = load i64* %x118
    switch i64 %fresh918, label %fresh950 [ i64 1, label %fresh919
                                            i64 2, label %fresh947 ]
    fresh919:
    ; Store
    %fresh920 = call i64* @llvmgc_malloc( i64 16 )
    %fresh921 = ptrtoint i64* %fresh920 to i64
    store i64 %fresh921, i64* %x119
    %fresh922 = load i64* %x119
    %fresh923 = inttoptr i64 %fresh922 to i64*
    %fresh924 = getelementptr i64* %fresh923, i32 0
    store i64 33, i64* %fresh924
    ; Store
    %fresh925 = call i64* @llvmgc_malloc( i64 24 )
    %fresh926 = ptrtoint i64* %fresh925 to i64
    store i64 %fresh926, i64* %x120
    %fresh927 = load i64* %x120
    %fresh928 = inttoptr i64 %fresh927 to i64*
    %fresh929 = getelementptr i64* %fresh928, i32 0
    store i64 37, i64* %fresh929
    %fresh930 = load i64* %x119
    %fresh931 = load i64* %x120
    %fresh932 = inttoptr i64 %fresh931 to i64*
    %fresh933 = getelementptr i64* %fresh932, i32 1
    store i64 %fresh930, i64* %fresh933
    ; Store
    %fresh934 = call i64* @llvmgc_malloc( i64 16 )
    %fresh935 = ptrtoint i64* %fresh934 to i64
    store i64 %fresh935, i64* %x121
    %fresh936 = load i64* %x121
    %fresh937 = inttoptr i64 %fresh936 to i64*
    %fresh938 = getelementptr i64* %fresh937, i32 0
    store i64 30, i64* %fresh938
    ; Unit (Yielding)
    %fresh939 = load i64** @RP
    %fresh940 = getelementptr i64* %fresh939, i32 0
    store i64 3, i64* %fresh940
    %fresh941 = load i64* %x121
    %fresh942 = load i64** @RP
    %fresh943 = getelementptr i64* %fresh942, i32 1
    store i64 %fresh941, i64* %fresh943
    %fresh944 = load i64* %x120
    %fresh945 = load i64** @RP
    %fresh946 = getelementptr i64* %fresh945, i32 2
    store i64 %fresh944, i64* %fresh946
    br label %fresh951
    fresh947:
    ; Unit (Yielding)
    %fresh948 = load i64** @RP
    %fresh949 = getelementptr i64* %fresh948, i32 0
    store i64 4, i64* %fresh949
    br label %fresh951
    fresh950:
    call void @primPatternMatchFailure(  )
    unreachable
    br label %fresh951
    fresh951:
    ret void
}

define internal void @fun_UHC_Base_packedStringToStringtildespec14(  ) nounwind
{
    %x125 = alloca i64, i32 1
    %x126 = alloca i64, i32 1
    %x127 = alloca i64, i32 1
    %x128 = alloca i64, i32 1
    ; Unit
    ;  (inlined)Variable_Unembedded x_124_UHC.Base._110_201_28 = Value_Con (Constant_LiteralStr "Prelude.undefined")
    ; FFI
    %fresh952 = ptrtoint [ 18 x i8]* @fresh952 to i64
    %fresh953 = call i64 @primPackedStringNull( i64 %fresh952 )
    store i64 %fresh953, i64* %x125
    ; Case
    %fresh955 = load i64* %x125
    switch i64 %fresh955, label %fresh987 [ i64 1, label %fresh956
                                            i64 2, label %fresh984 ]
    fresh956:
    ; Store
    %fresh957 = call i64* @llvmgc_malloc( i64 16 )
    %fresh958 = ptrtoint i64* %fresh957 to i64
    store i64 %fresh958, i64* %x126
    %fresh959 = load i64* %x126
    %fresh960 = inttoptr i64 %fresh959 to i64*
    %fresh961 = getelementptr i64* %fresh960, i32 0
    store i64 34, i64* %fresh961
    ; Store
    %fresh962 = call i64* @llvmgc_malloc( i64 24 )
    %fresh963 = ptrtoint i64* %fresh962 to i64
    store i64 %fresh963, i64* %x127
    %fresh964 = load i64* %x127
    %fresh965 = inttoptr i64 %fresh964 to i64*
    %fresh966 = getelementptr i64* %fresh965, i32 0
    store i64 37, i64* %fresh966
    %fresh967 = load i64* %x126
    %fresh968 = load i64* %x127
    %fresh969 = inttoptr i64 %fresh968 to i64*
    %fresh970 = getelementptr i64* %fresh969, i32 1
    store i64 %fresh967, i64* %fresh970
    ; Store
    %fresh971 = call i64* @llvmgc_malloc( i64 16 )
    %fresh972 = ptrtoint i64* %fresh971 to i64
    store i64 %fresh972, i64* %x128
    %fresh973 = load i64* %x128
    %fresh974 = inttoptr i64 %fresh973 to i64*
    %fresh975 = getelementptr i64* %fresh974, i32 0
    store i64 31, i64* %fresh975
    ; Unit (Yielding)
    %fresh976 = load i64** @RP
    %fresh977 = getelementptr i64* %fresh976, i32 0
    store i64 3, i64* %fresh977
    %fresh978 = load i64* %x128
    %fresh979 = load i64** @RP
    %fresh980 = getelementptr i64* %fresh979, i32 1
    store i64 %fresh978, i64* %fresh980
    %fresh981 = load i64* %x127
    %fresh982 = load i64** @RP
    %fresh983 = getelementptr i64* %fresh982, i32 2
    store i64 %fresh981, i64* %fresh983
    br label %fresh988
    fresh984:
    ; Unit (Yielding)
    %fresh985 = load i64** @RP
    %fresh986 = getelementptr i64* %fresh985, i32 0
    store i64 4, i64* %fresh986
    br label %fresh988
    fresh987:
    call void @primPatternMatchFailure(  )
    unreachable
    br label %fresh988
    fresh988:
    ret void
}

define internal void @fun_UHC_Base_y91_43_0_class_MonadtildeUHC_Base_gtgttildespec1( i64 %x133, i64 %x134 ) nounwind
{
    %x135 = alloca i64, i32 1
    ; Store
    %fresh989 = call i64* @llvmgc_malloc( i64 16 )
    %fresh990 = ptrtoint i64* %fresh989 to i64
    store i64 %fresh990, i64* %x135
    %fresh991 = load i64* %x135
    %fresh992 = inttoptr i64 %fresh991 to i64*
    %fresh993 = getelementptr i64* %fresh992, i32 0
    store i64 22, i64* %fresh993
    %fresh994 = load i64* %x135
    %fresh995 = inttoptr i64 %fresh994 to i64*
    %fresh996 = getelementptr i64* %fresh995, i32 1
    store i64 %x134, i64* %fresh996
    ; Unit (Yielding)
    %fresh997 = load i64** @RP
    %fresh998 = getelementptr i64* %fresh997, i32 0
    store i64 23, i64* %fresh998
    %fresh999 = load i64** @RP
    %fresh1000 = getelementptr i64* %fresh999, i32 1
    store i64 %x133, i64* %fresh1000
    %fresh1001 = load i64* %x135
    %fresh1002 = load i64** @RP
    %fresh1003 = getelementptr i64* %fresh1002, i32 2
    store i64 %fresh1001, i64* %fresh1003
    ret void
}

define internal void @fun_UHC_Base_primbindIO( i64 %x153, i64 %x154, i64 %x155 ) nounwind
{
    %x207 = alloca i64, i32 1
    %x208 = alloca i64, i32 1
    %x209 = alloca i64, i32 1
    %x210 = alloca i64, i32 1
    %x211 = alloca i64, i32 1
    %x220 = alloca i64, i32 1
    %x221 = alloca i64, i32 1
    %x222 = alloca i64, i32 1
    %x223 = alloca i64, i32 1
    %x224 = alloca i64, i32 1
    %x598 = alloca i64, i32 1
    %x601 = alloca i64, i32 1
    %x607 = alloca i64, i32 1
    %x608 = alloca i64, i32 1
    %x609 = alloca i64, i32 1
    %x616 = alloca i64, i32 1
    %x620 = alloca i64, i32 1
    %x621 = alloca i64, i32 1
    %x627 = alloca i64, i32 1
    %x630 = alloca i64, i32 1
    %x633 = alloca i64, i32 1
    %x642 = alloca i64, i32 1
    %x643 = alloca i64, i32 1
    %x644 = alloca i64, i32 1
    %x651 = alloca i64, i32 1
    %x726 = alloca i64, i32 1
    %x735 = alloca i64, i32 1
    %x739 = alloca i64, i32 1
    ; Fetch
    %fresh1004 = inttoptr i64 %x153 to i64*
    %fresh1005 = getelementptr i64* %fresh1004, i32 0
    %fresh1006 = load i64* %fresh1005
    store i64 %fresh1006, i64* %x598
    ; Case
    %fresh1007 = load i64* %x598
    %fresh1008 = icmp sgt i64 %fresh1007, 27
    br i1 %fresh1008, label %fresh1009, label %fresh1156
    fresh1009:
    %fresh1010 = load i64* %x598
    switch i64 %fresh1010, label %fresh1154 [ i64 47, label %fresh1011
                                              i64 51, label %fresh1105 ]
    fresh1011:
    ; Fetch
    %fresh1012 = load i64** @global_Main_y13_0_2
    %fresh1013 = getelementptr i64* %fresh1012, i32 0
    %fresh1014 = load i64* %fresh1013
    store i64 %fresh1014, i64* %x601
    ; Case
    %fresh1015 = load i64* %x601
    %fresh1016 = icmp sgt i64 %fresh1015, 27
    br i1 %fresh1016, label %fresh1017, label %fresh1034
    fresh1017:
    ; Call (Normal or Tail)
    call void @fun_UHC_Base_packedStringToStringtildespec1(  )
    ; Result
    ;  (inlined)Variable_Unembedded x_604 = Value_Var (Variable_Subs Variable_RP 0)
    ;  (inlined)Variable_Unembedded x_605 = Value_Var (Variable_Subs Variable_RP 1)
    ;  (inlined)Variable_Unembedded x_606 = Value_Var (Variable_Subs Variable_RP 2)
    ; UpdateUnit
    %fresh1019 = load i64** @RP
    %fresh1020 = getelementptr i64* %fresh1019, i32 0
    %fresh1021 = load i64* %fresh1020
    store i64 %fresh1021, i64* %x601
    %fresh1022 = load i64** @global_Main_y13_0_2
    %fresh1023 = getelementptr i64* %fresh1022, i32 0
    store i64 %fresh1021, i64* %fresh1023
    %fresh1024 = load i64** @RP
    %fresh1025 = getelementptr i64* %fresh1024, i32 1
    %fresh1026 = load i64* %fresh1025
    %fresh1027 = load i64** @global_Main_y13_0_2
    %fresh1028 = getelementptr i64* %fresh1027, i32 1
    store i64 %fresh1026, i64* %fresh1028
    %fresh1029 = load i64** @RP
    %fresh1030 = getelementptr i64* %fresh1029, i32 2
    %fresh1031 = load i64* %fresh1030
    %fresh1032 = load i64** @global_Main_y13_0_2
    %fresh1033 = getelementptr i64* %fresh1032, i32 2
    store i64 %fresh1031, i64* %fresh1033
    br label %fresh1035
    fresh1034:
    br label %fresh1035
    fresh1035:
    %fresh1036 = load i64* %x601
    switch i64 %fresh1036, label %fresh1039 [ i64 3, label %fresh1037
                                              i64 4, label %fresh1038 ]
    fresh1037:
    ; Unit
    store i64 1, i64* %x607
    br label %fresh1040
    fresh1038:
    ; Unit
    store i64 2, i64* %x607
    br label %fresh1040
    fresh1039:
    call void @primPatternMatchFailure(  )
    unreachable
    br label %fresh1040
    fresh1040:
    ; Case
    %fresh1041 = load i64* %x607
    switch i64 %fresh1041, label %fresh1092 [ i64 1, label %fresh1042
                                              i64 2, label %fresh1089 ]
    fresh1042:
    ; Store
    %fresh1043 = call i64* @llvmgc_malloc( i64 24 )
    %fresh1044 = ptrtoint i64* %fresh1043 to i64
    store i64 %fresh1044, i64* %x220
    %fresh1045 = load i64* %x220
    %fresh1046 = inttoptr i64 %fresh1045 to i64*
    %fresh1047 = getelementptr i64* %fresh1046, i32 0
    store i64 43, i64* %fresh1047
    ; Store
    %fresh1048 = call i64* @llvmgc_malloc( i64 24 )
    %fresh1049 = ptrtoint i64* %fresh1048 to i64
    store i64 %fresh1049, i64* %x221
    %fresh1050 = load i64* %x221
    %fresh1051 = inttoptr i64 %fresh1050 to i64*
    %fresh1052 = getelementptr i64* %fresh1051, i32 0
    store i64 46, i64* %fresh1052
    %fresh1053 = load i64* %x220
    %fresh1054 = load i64* %x221
    %fresh1055 = inttoptr i64 %fresh1054 to i64*
    %fresh1056 = getelementptr i64* %fresh1055, i32 1
    store i64 %fresh1053, i64* %fresh1056
    ; Store
    %fresh1057 = call i64* @llvmgc_malloc( i64 16 )
    %fresh1058 = ptrtoint i64* %fresh1057 to i64
    store i64 %fresh1058, i64* %x222
    %fresh1059 = load i64* %x222
    %fresh1060 = inttoptr i64 %fresh1059 to i64*
    %fresh1061 = getelementptr i64* %fresh1060, i32 0
    store i64 41, i64* %fresh1061
    ; Store
    %fresh1062 = call i64* @llvmgc_malloc( i64 16 )
    %fresh1063 = ptrtoint i64* %fresh1062 to i64
    store i64 %fresh1063, i64* %x223
    %fresh1064 = load i64* %x223
    %fresh1065 = inttoptr i64 %fresh1064 to i64*
    %fresh1066 = getelementptr i64* %fresh1065, i32 0
    store i64 45, i64* %fresh1066
    ; Store
    %fresh1067 = call i64* @llvmgc_malloc( i64 24 )
    %fresh1068 = ptrtoint i64* %fresh1067 to i64
    store i64 %fresh1068, i64* %x224
    %fresh1069 = load i64* %x224
    %fresh1070 = inttoptr i64 %fresh1069 to i64*
    %fresh1071 = getelementptr i64* %fresh1070, i32 0
    store i64 51, i64* %fresh1071
    %fresh1072 = load i64* %x223
    %fresh1073 = load i64* %x224
    %fresh1074 = inttoptr i64 %fresh1073 to i64*
    %fresh1075 = getelementptr i64* %fresh1074, i32 1
    store i64 %fresh1072, i64* %fresh1075
    %fresh1076 = load i64* %x222
    %fresh1077 = load i64* %x224
    %fresh1078 = inttoptr i64 %fresh1077 to i64*
    %fresh1079 = getelementptr i64* %fresh1078, i32 2
    store i64 %fresh1076, i64* %fresh1079
    ; Call (Normal or Tail)
    %fresh1080 = load i64* %x224
    %fresh1081 = load i64* %x221
    call void @fun_UHC_Base_y91_43_0_class_MonadtildeUHC_Base_gtgttildespec1( i64 %fresh1080, i64 %fresh1081 )
    ; Result
    %fresh1083 = load i64** @RP
    %fresh1084 = getelementptr i64* %fresh1083, i32 0
    %fresh1085 = load i64* %fresh1084
    store i64 %fresh1085, i64* %x608
    %fresh1086 = load i64** @RP
    %fresh1087 = getelementptr i64* %fresh1086, i32 1
    %fresh1088 = load i64* %fresh1087
    store i64 %fresh1088, i64* %x609
    ;  (inlined)Variable_Unembedded x_610 = Value_Var (Variable_Subs Variable_RP 2)
    br label %fresh1093
    fresh1089:
    ; Unit
    store i64 20, i64* %x608
    %fresh1090 = load i64** @global_UHC_OldIO_y110_234_1
    %fresh1091 = ptrtoint i64* %fresh1090 to i64
    store i64 %fresh1091, i64* %x609
    br label %fresh1093
    fresh1092:
    call void @primPatternMatchFailure(  )
    unreachable
    br label %fresh1093
    fresh1093:
    ; UpdateUnit
    %fresh1094 = load i64* %x608
    store i64 %fresh1094, i64* %x598
    %fresh1095 = inttoptr i64 %x153 to i64*
    %fresh1096 = getelementptr i64* %fresh1095, i32 0
    store i64 %fresh1094, i64* %fresh1096
    %fresh1097 = load i64* %x609
    %fresh1098 = inttoptr i64 %x153 to i64*
    %fresh1099 = getelementptr i64* %fresh1098, i32 1
    store i64 %fresh1097, i64* %fresh1099
    %fresh1100 = load i64** @RP
    %fresh1101 = getelementptr i64* %fresh1100, i32 2
    %fresh1102 = load i64* %fresh1101
    %fresh1103 = inttoptr i64 %x153 to i64*
    %fresh1104 = getelementptr i64* %fresh1103, i32 2
    store i64 %fresh1102, i64* %fresh1104
    br label %fresh1155
    fresh1105:
    ; Fetch
    %fresh1106 = inttoptr i64 %x153 to i64*
    %fresh1107 = getelementptr i64* %fresh1106, i32 1
    %fresh1108 = load i64* %fresh1107
    store i64 %fresh1108, i64* %x726
    ; Fetch
    ;  (inlined)Variable_Unembedded x_725 = Value_Var (Variable_Subs (Variable_Unembedded x_153_UHC.Base.93_1896_0__165) 2)
    ; Fetch
    ;  (inlined)Variable_Unembedded x_611 = Value_Var (Variable_Subs (Variable_Unembedded x_726) 0)
    ; Case
    %fresh1109 = load i64* %x726
    %fresh1110 = inttoptr i64 %fresh1109 to i64*
    %fresh1111 = getelementptr i64* %fresh1110, i32 0
    %fresh1112 = load i64* %fresh1111
    switch i64 %fresh1112, label %fresh1137 [ i64 24, label %fresh1113
                                              i64 26, label %fresh1118
                                              i64 45, label %fresh1123 ]
    fresh1113:
    ; Fetch
    ;  (inlined)Variable_Unembedded x_727 = Value_Var (Variable_Subs (Variable_Unembedded x_726) 1)
    ; Unit
    %fresh1114 = load i64* %x726
    %fresh1115 = inttoptr i64 %fresh1114 to i64*
    %fresh1116 = getelementptr i64* %fresh1115, i32 1
    %fresh1117 = load i64* %fresh1116
    store i64 %fresh1117, i64* %x616
    br label %fresh1138
    fresh1118:
    ; Fetch
    ;  (inlined)Variable_Unembedded x_728 = Value_Var (Variable_Subs (Variable_Unembedded x_726) 1)
    ; Unit
    %fresh1119 = load i64* %x726
    %fresh1120 = inttoptr i64 %fresh1119 to i64*
    %fresh1121 = getelementptr i64* %fresh1120, i32 1
    %fresh1122 = load i64* %fresh1121
    store i64 %fresh1122, i64* %x616
    br label %fresh1138
    fresh1123:
    ; Call (Normal or Tail)
    call void @fun_UHC_OldIO_hPutChartildespec1(  )
    ; Result
    ;  (inlined)Variable_Unembedded x_613 = Value_Var (Variable_Subs Variable_RP 0)
    ;  (inlined)Variable_Unembedded x_614 = Value_Var (Variable_Subs Variable_RP 1)
    ; UpdateUnit
    %fresh1125 = load i64** @RP
    %fresh1126 = getelementptr i64* %fresh1125, i32 0
    %fresh1127 = load i64* %fresh1126
    %fresh1128 = load i64* %x726
    %fresh1129 = inttoptr i64 %fresh1128 to i64*
    %fresh1130 = getelementptr i64* %fresh1129, i32 0
    store i64 %fresh1127, i64* %fresh1130
    %fresh1131 = load i64** @RP
    %fresh1132 = getelementptr i64* %fresh1131, i32 1
    %fresh1133 = load i64* %fresh1132
    store i64 %fresh1133, i64* %x616
    %fresh1134 = load i64* %x726
    %fresh1135 = inttoptr i64 %fresh1134 to i64*
    %fresh1136 = getelementptr i64* %fresh1135, i32 1
    store i64 %fresh1133, i64* %fresh1136
    br label %fresh1138
    fresh1137:
    call void @primPatternMatchFailure(  )
    unreachable
    br label %fresh1138
    fresh1138:
    ; Call (Normal or Tail)
    %fresh1139 = load i64* %x616
    %fresh1140 = inttoptr i64 %x153 to i64*
    %fresh1141 = getelementptr i64* %fresh1140, i32 2
    %fresh1142 = load i64* %fresh1141
    call void @fun_UHC_OldIO_y103_0_32_0( i64 %fresh1139, i64 %fresh1142 )
    ; Result
    ;  (inlined)Variable_Unembedded x_617 = Value_Var (Variable_Subs Variable_RP 0)
    ;  (inlined)Variable_Unembedded x_618 = Value_Var (Variable_Subs Variable_RP 1)
    ; UpdateUnit
    %fresh1144 = load i64** @RP
    %fresh1145 = getelementptr i64* %fresh1144, i32 0
    %fresh1146 = load i64* %fresh1145
    store i64 %fresh1146, i64* %x598
    %fresh1147 = inttoptr i64 %x153 to i64*
    %fresh1148 = getelementptr i64* %fresh1147, i32 0
    store i64 %fresh1146, i64* %fresh1148
    %fresh1149 = load i64** @RP
    %fresh1150 = getelementptr i64* %fresh1149, i32 1
    %fresh1151 = load i64* %fresh1150
    %fresh1152 = inttoptr i64 %x153 to i64*
    %fresh1153 = getelementptr i64* %fresh1152, i32 1
    store i64 %fresh1151, i64* %fresh1153
    br label %fresh1155
    fresh1154:
    call void @primPatternMatchFailure(  )
    unreachable
    br label %fresh1155
    fresh1155:
    br label %fresh1157
    fresh1156:
    br label %fresh1157
    fresh1157:
    %fresh1158 = load i64* %x598
    switch i64 %fresh1158, label %fresh1195 [ i64 20, label %fresh1159
                                              i64 21, label %fresh1170
                                              i64 23, label %fresh1181 ]
    fresh1159:
    ; Fetch
    ;  (inlined)Variable_Unembedded x_729 = Value_Var (Variable_Subs (Variable_Unembedded x_153_UHC.Base.93_1896_0__165) 1)
    ; Call (Normal or Tail)
    %fresh1160 = inttoptr i64 %x153 to i64*
    %fresh1161 = getelementptr i64* %fresh1160, i32 1
    %fresh1162 = load i64* %fresh1161
    call void @fun_UHC_Base_primretIO( i64 %fresh1162, i64 %x155 )
    ; Result
    %fresh1164 = load i64** @RP
    %fresh1165 = getelementptr i64* %fresh1164, i32 1
    %fresh1166 = load i64* %fresh1165
    store i64 %fresh1166, i64* %x620
    %fresh1167 = load i64** @RP
    %fresh1168 = getelementptr i64* %fresh1167, i32 2
    %fresh1169 = load i64* %fresh1168
    store i64 %fresh1169, i64* %x621
    br label %fresh1196
    fresh1170:
    ; Fetch
    ;  (inlined)Variable_Unembedded x_730 = Value_Var (Variable_Subs (Variable_Unembedded x_153_UHC.Base.93_1896_0__165) 1)
    ; Call (Normal or Tail)
    %fresh1171 = inttoptr i64 %x153 to i64*
    %fresh1172 = getelementptr i64* %fresh1171, i32 1
    %fresh1173 = load i64* %fresh1172
    call void @fun_UHC_Base_ioFromPrim( i64 %fresh1173, i64 %x155 )
    ; Result
    %fresh1175 = load i64** @RP
    %fresh1176 = getelementptr i64* %fresh1175, i32 1
    %fresh1177 = load i64* %fresh1176
    store i64 %fresh1177, i64* %x620
    %fresh1178 = load i64** @RP
    %fresh1179 = getelementptr i64* %fresh1178, i32 2
    %fresh1180 = load i64* %fresh1179
    store i64 %fresh1180, i64* %x621
    br label %fresh1196
    fresh1181:
    ; Fetch
    ;  (inlined)Variable_Unembedded x_732 = Value_Var (Variable_Subs (Variable_Unembedded x_153_UHC.Base.93_1896_0__165) 1)
    ; Fetch
    ;  (inlined)Variable_Unembedded x_731 = Value_Var (Variable_Subs (Variable_Unembedded x_153_UHC.Base.93_1896_0__165) 2)
    ; Call (Normal or Tail)
    %fresh1182 = inttoptr i64 %x153 to i64*
    %fresh1183 = getelementptr i64* %fresh1182, i32 1
    %fresh1184 = load i64* %fresh1183
    %fresh1185 = inttoptr i64 %x153 to i64*
    %fresh1186 = getelementptr i64* %fresh1185, i32 2
    %fresh1187 = load i64* %fresh1186
    call void @fun_UHC_Base_primbindIO( i64 %fresh1184, i64 %fresh1187, i64 %x155 )
    ; Result
    %fresh1189 = load i64** @RP
    %fresh1190 = getelementptr i64* %fresh1189, i32 1
    %fresh1191 = load i64* %fresh1190
    store i64 %fresh1191, i64* %x620
    %fresh1192 = load i64** @RP
    %fresh1193 = getelementptr i64* %fresh1192, i32 2
    %fresh1194 = load i64* %fresh1193
    store i64 %fresh1194, i64* %x621
    br label %fresh1196
    fresh1195:
    call void @primPatternMatchFailure(  )
    unreachable
    br label %fresh1196
    fresh1196:
    ; Fetch
    ;  (inlined)Variable_Unembedded x_622 = Value_Var (Variable_Subs (Variable_Unembedded x_621) 0)
    ; Case
    %fresh1197 = load i64* %x621
    %fresh1198 = inttoptr i64 %fresh1197 to i64*
    %fresh1199 = getelementptr i64* %fresh1198, i32 0
    %fresh1200 = load i64* %fresh1199
    switch i64 %fresh1200, label %fresh1218 [ i64 0, label %fresh1201
                                              i64 50, label %fresh1202 ]
    fresh1201:
    ; Unit
    br label %fresh1219
    fresh1202:
    ; Fetch
    ;  (inlined)Variable_Unembedded x_734 = Value_Var (Variable_Subs (Variable_Unembedded x_621) 1)
    ; Fetch
    ;  (inlined)Variable_Unembedded x_733 = Value_Var (Variable_Subs (Variable_Unembedded x_621) 2)
    ; Call (Normal or Tail)
    %fresh1203 = load i64* %x621
    %fresh1204 = inttoptr i64 %fresh1203 to i64*
    %fresh1205 = getelementptr i64* %fresh1204, i32 1
    %fresh1206 = load i64* %fresh1205
    %fresh1207 = load i64* %x621
    %fresh1208 = inttoptr i64 %fresh1207 to i64*
    %fresh1209 = getelementptr i64* %fresh1208, i32 2
    %fresh1210 = load i64* %fresh1209
    call void @fun_app2_1( i64 %fresh1206, i64 %fresh1210 )
    ; Result
    ;  (inlined)Variable_Unembedded x_625 = Value_Var (Variable_Subs Variable_RP 0)
    ; UpdateUnit
    %fresh1212 = load i64** @RP
    %fresh1213 = getelementptr i64* %fresh1212, i32 0
    %fresh1214 = load i64* %fresh1213
    %fresh1215 = load i64* %x621
    %fresh1216 = inttoptr i64 %fresh1215 to i64*
    %fresh1217 = getelementptr i64* %fresh1216, i32 0
    store i64 %fresh1214, i64* %fresh1217
    br label %fresh1219
    fresh1218:
    call void @primPatternMatchFailure(  )
    unreachable
    br label %fresh1219
    fresh1219:
    ; Fetch
    %fresh1220 = inttoptr i64 %x154 to i64*
    %fresh1221 = getelementptr i64* %fresh1220, i32 1
    %fresh1222 = load i64* %fresh1221
    store i64 %fresh1222, i64* %x627
    ; Fetch
    %fresh1223 = load i64* %x627
    %fresh1224 = inttoptr i64 %fresh1223 to i64*
    %fresh1225 = getelementptr i64* %fresh1224, i32 0
    %fresh1226 = load i64* %fresh1225
    store i64 %fresh1226, i64* %x630
    ; Case
    %fresh1227 = load i64* %x630
    %fresh1228 = icmp sgt i64 %fresh1227, 27
    br i1 %fresh1228, label %fresh1229, label %fresh1427
    fresh1229:
    %fresh1230 = load i64* %x630
    switch i64 %fresh1230, label %fresh1425 [ i64 46, label %fresh1231
                                              i64 52, label %fresh1372 ]
    fresh1231:
    ; Fetch
    %fresh1232 = load i64* %x627
    %fresh1233 = inttoptr i64 %fresh1232 to i64*
    %fresh1234 = getelementptr i64* %fresh1233, i32 1
    %fresh1235 = load i64* %fresh1234
    store i64 %fresh1235, i64* %x735
    ; Fetch
    %fresh1236 = load i64* %x735
    %fresh1237 = inttoptr i64 %fresh1236 to i64*
    %fresh1238 = getelementptr i64* %fresh1237, i32 0
    %fresh1239 = load i64* %fresh1238
    store i64 %fresh1239, i64* %x633
    ; Case
    %fresh1240 = load i64* %x633
    %fresh1241 = icmp sgt i64 %fresh1240, 27
    br i1 %fresh1241, label %fresh1242, label %fresh1290
    fresh1242:
    %fresh1243 = load i64* %x633
    switch i64 %fresh1243, label %fresh1288 [ i64 42, label %fresh1244
                                              i64 43, label %fresh1268 ]
    fresh1244:
    ; Fetch
    ;  (inlined)Variable_Unembedded x_736 = Value_Var (Variable_Subs (Variable_Unembedded x_735) 1)
    ; Call (Normal or Tail)
    %fresh1245 = load i64* %x735
    %fresh1246 = inttoptr i64 %fresh1245 to i64*
    %fresh1247 = getelementptr i64* %fresh1246, i32 1
    %fresh1248 = load i64* %fresh1247
    call void @fun_UHC_Base_tail( i64 %fresh1248 )
    ; Result
    ;  (inlined)Variable_Unembedded x_636 = Value_Var (Variable_Subs Variable_RP 0)
    ;  (inlined)Variable_Unembedded x_637 = Value_Var (Variable_Subs Variable_RP 1)
    ;  (inlined)Variable_Unembedded x_638 = Value_Var (Variable_Subs Variable_RP 2)
    ; UpdateUnit
    %fresh1250 = load i64** @RP
    %fresh1251 = getelementptr i64* %fresh1250, i32 0
    %fresh1252 = load i64* %fresh1251
    store i64 %fresh1252, i64* %x633
    %fresh1253 = load i64* %x735
    %fresh1254 = inttoptr i64 %fresh1253 to i64*
    %fresh1255 = getelementptr i64* %fresh1254, i32 0
    store i64 %fresh1252, i64* %fresh1255
    %fresh1256 = load i64** @RP
    %fresh1257 = getelementptr i64* %fresh1256, i32 1
    %fresh1258 = load i64* %fresh1257
    %fresh1259 = load i64* %x735
    %fresh1260 = inttoptr i64 %fresh1259 to i64*
    %fresh1261 = getelementptr i64* %fresh1260, i32 1
    store i64 %fresh1258, i64* %fresh1261
    %fresh1262 = load i64** @RP
    %fresh1263 = getelementptr i64* %fresh1262, i32 2
    %fresh1264 = load i64* %fresh1263
    %fresh1265 = load i64* %x735
    %fresh1266 = inttoptr i64 %fresh1265 to i64*
    %fresh1267 = getelementptr i64* %fresh1266, i32 2
    store i64 %fresh1264, i64* %fresh1267
    br label %fresh1289
    fresh1268:
    ; Call (Normal or Tail)
    call void @fun_UHC_Base_tailtildespec1(  )
    ; Result
    ;  (inlined)Variable_Unembedded x_639 = Value_Var (Variable_Subs Variable_RP 0)
    ;  (inlined)Variable_Unembedded x_640 = Value_Var (Variable_Subs Variable_RP 1)
    ;  (inlined)Variable_Unembedded x_641 = Value_Var (Variable_Subs Variable_RP 2)
    ; UpdateUnit
    %fresh1270 = load i64** @RP
    %fresh1271 = getelementptr i64* %fresh1270, i32 0
    %fresh1272 = load i64* %fresh1271
    store i64 %fresh1272, i64* %x633
    %fresh1273 = load i64* %x735
    %fresh1274 = inttoptr i64 %fresh1273 to i64*
    %fresh1275 = getelementptr i64* %fresh1274, i32 0
    store i64 %fresh1272, i64* %fresh1275
    %fresh1276 = load i64** @RP
    %fresh1277 = getelementptr i64* %fresh1276, i32 1
    %fresh1278 = load i64* %fresh1277
    %fresh1279 = load i64* %x735
    %fresh1280 = inttoptr i64 %fresh1279 to i64*
    %fresh1281 = getelementptr i64* %fresh1280, i32 1
    store i64 %fresh1278, i64* %fresh1281
    %fresh1282 = load i64** @RP
    %fresh1283 = getelementptr i64* %fresh1282, i32 2
    %fresh1284 = load i64* %fresh1283
    %fresh1285 = load i64* %x735
    %fresh1286 = inttoptr i64 %fresh1285 to i64*
    %fresh1287 = getelementptr i64* %fresh1286, i32 2
    store i64 %fresh1284, i64* %fresh1287
    br label %fresh1289
    fresh1288:
    call void @primPatternMatchFailure(  )
    unreachable
    br label %fresh1289
    fresh1289:
    br label %fresh1291
    fresh1290:
    br label %fresh1291
    fresh1291:
    %fresh1292 = load i64* %x633
    switch i64 %fresh1292, label %fresh1295 [ i64 3, label %fresh1293
                                              i64 4, label %fresh1294 ]
    fresh1293:
    ; Unit
    store i64 1, i64* %x642
    br label %fresh1296
    fresh1294:
    ; Unit
    store i64 2, i64* %x642
    br label %fresh1296
    fresh1295:
    call void @primPatternMatchFailure(  )
    unreachable
    br label %fresh1296
    fresh1296:
    ; Case
    %fresh1297 = load i64* %x642
    switch i64 %fresh1297, label %fresh1356 [ i64 1, label %fresh1298
                                              i64 2, label %fresh1353 ]
    fresh1298:
    ; Store
    %fresh1299 = call i64* @llvmgc_malloc( i64 24 )
    %fresh1300 = ptrtoint i64* %fresh1299 to i64
    store i64 %fresh1300, i64* %x207
    %fresh1301 = load i64* %x207
    %fresh1302 = inttoptr i64 %fresh1301 to i64*
    %fresh1303 = getelementptr i64* %fresh1302, i32 0
    store i64 42, i64* %fresh1303
    %fresh1304 = load i64* %x735
    %fresh1305 = load i64* %x207
    %fresh1306 = inttoptr i64 %fresh1305 to i64*
    %fresh1307 = getelementptr i64* %fresh1306, i32 1
    store i64 %fresh1304, i64* %fresh1307
    ; Store
    %fresh1308 = call i64* @llvmgc_malloc( i64 24 )
    %fresh1309 = ptrtoint i64* %fresh1308 to i64
    store i64 %fresh1309, i64* %x208
    %fresh1310 = load i64* %x208
    %fresh1311 = inttoptr i64 %fresh1310 to i64*
    %fresh1312 = getelementptr i64* %fresh1311, i32 0
    store i64 46, i64* %fresh1312
    %fresh1313 = load i64* %x207
    %fresh1314 = load i64* %x208
    %fresh1315 = inttoptr i64 %fresh1314 to i64*
    %fresh1316 = getelementptr i64* %fresh1315, i32 1
    store i64 %fresh1313, i64* %fresh1316
    ; Store
    %fresh1317 = call i64* @llvmgc_malloc( i64 16 )
    %fresh1318 = ptrtoint i64* %fresh1317 to i64
    store i64 %fresh1318, i64* %x209
    %fresh1319 = load i64* %x209
    %fresh1320 = inttoptr i64 %fresh1319 to i64*
    %fresh1321 = getelementptr i64* %fresh1320, i32 0
    store i64 40, i64* %fresh1321
    %fresh1322 = load i64* %x735
    %fresh1323 = load i64* %x209
    %fresh1324 = inttoptr i64 %fresh1323 to i64*
    %fresh1325 = getelementptr i64* %fresh1324, i32 1
    store i64 %fresh1322, i64* %fresh1325
    ; Store
    %fresh1326 = call i64* @llvmgc_malloc( i64 16 )
    %fresh1327 = ptrtoint i64* %fresh1326 to i64
    store i64 %fresh1327, i64* %x210
    %fresh1328 = load i64* %x210
    %fresh1329 = inttoptr i64 %fresh1328 to i64*
    %fresh1330 = getelementptr i64* %fresh1329, i32 0
    store i64 45, i64* %fresh1330
    ; Store
    %fresh1331 = call i64* @llvmgc_malloc( i64 24 )
    %fresh1332 = ptrtoint i64* %fresh1331 to i64
    store i64 %fresh1332, i64* %x211
    %fresh1333 = load i64* %x211
    %fresh1334 = inttoptr i64 %fresh1333 to i64*
    %fresh1335 = getelementptr i64* %fresh1334, i32 0
    store i64 51, i64* %fresh1335
    %fresh1336 = load i64* %x210
    %fresh1337 = load i64* %x211
    %fresh1338 = inttoptr i64 %fresh1337 to i64*
    %fresh1339 = getelementptr i64* %fresh1338, i32 1
    store i64 %fresh1336, i64* %fresh1339
    %fresh1340 = load i64* %x209
    %fresh1341 = load i64* %x211
    %fresh1342 = inttoptr i64 %fresh1341 to i64*
    %fresh1343 = getelementptr i64* %fresh1342, i32 2
    store i64 %fresh1340, i64* %fresh1343
    ; Call (Normal or Tail)
    %fresh1344 = load i64* %x211
    %fresh1345 = load i64* %x208
    call void @fun_UHC_Base_y91_43_0_class_MonadtildeUHC_Base_gtgttildespec1( i64 %fresh1344, i64 %fresh1345 )
    ; Result
    %fresh1347 = load i64** @RP
    %fresh1348 = getelementptr i64* %fresh1347, i32 0
    %fresh1349 = load i64* %fresh1348
    store i64 %fresh1349, i64* %x643
    %fresh1350 = load i64** @RP
    %fresh1351 = getelementptr i64* %fresh1350, i32 1
    %fresh1352 = load i64* %fresh1351
    store i64 %fresh1352, i64* %x644
    ;  (inlined)Variable_Unembedded x_645 = Value_Var (Variable_Subs Variable_RP 2)
    br label %fresh1357
    fresh1353:
    ; Unit
    store i64 20, i64* %x643
    %fresh1354 = load i64** @global_UHC_OldIO_y110_234_1
    %fresh1355 = ptrtoint i64* %fresh1354 to i64
    store i64 %fresh1355, i64* %x644
    br label %fresh1357
    fresh1356:
    call void @primPatternMatchFailure(  )
    unreachable
    br label %fresh1357
    fresh1357:
    ; UpdateUnit
    %fresh1358 = load i64* %x643
    store i64 %fresh1358, i64* %x630
    %fresh1359 = load i64* %x627
    %fresh1360 = inttoptr i64 %fresh1359 to i64*
    %fresh1361 = getelementptr i64* %fresh1360, i32 0
    store i64 %fresh1358, i64* %fresh1361
    %fresh1362 = load i64* %x644
    %fresh1363 = load i64* %x627
    %fresh1364 = inttoptr i64 %fresh1363 to i64*
    %fresh1365 = getelementptr i64* %fresh1364, i32 1
    store i64 %fresh1362, i64* %fresh1365
    %fresh1366 = load i64** @RP
    %fresh1367 = getelementptr i64* %fresh1366, i32 2
    %fresh1368 = load i64* %fresh1367
    %fresh1369 = load i64* %x627
    %fresh1370 = inttoptr i64 %fresh1369 to i64*
    %fresh1371 = getelementptr i64* %fresh1370, i32 2
    store i64 %fresh1368, i64* %fresh1371
    br label %fresh1426
    fresh1372:
    ; Fetch
    %fresh1373 = load i64* %x627
    %fresh1374 = inttoptr i64 %fresh1373 to i64*
    %fresh1375 = getelementptr i64* %fresh1374, i32 1
    %fresh1376 = load i64* %fresh1375
    store i64 %fresh1376, i64* %x739
    ; Fetch
    ;  (inlined)Variable_Unembedded x_738 = Value_Var (Variable_Subs (Variable_Unembedded x_627) 2)
    ; Fetch
    ;  (inlined)Variable_Unembedded x_646 = Value_Var (Variable_Subs (Variable_Unembedded x_739) 0)
    ; Case
    %fresh1377 = load i64* %x739
    %fresh1378 = inttoptr i64 %fresh1377 to i64*
    %fresh1379 = getelementptr i64* %fresh1378, i32 0
    %fresh1380 = load i64* %fresh1379
    switch i64 %fresh1380, label %fresh1405 [ i64 24, label %fresh1381
                                              i64 26, label %fresh1386
                                              i64 45, label %fresh1391 ]
    fresh1381:
    ; Fetch
    ;  (inlined)Variable_Unembedded x_740 = Value_Var (Variable_Subs (Variable_Unembedded x_739) 1)
    ; Unit
    %fresh1382 = load i64* %x739
    %fresh1383 = inttoptr i64 %fresh1382 to i64*
    %fresh1384 = getelementptr i64* %fresh1383, i32 1
    %fresh1385 = load i64* %fresh1384
    store i64 %fresh1385, i64* %x651
    br label %fresh1406
    fresh1386:
    ; Fetch
    ;  (inlined)Variable_Unembedded x_741 = Value_Var (Variable_Subs (Variable_Unembedded x_739) 1)
    ; Unit
    %fresh1387 = load i64* %x739
    %fresh1388 = inttoptr i64 %fresh1387 to i64*
    %fresh1389 = getelementptr i64* %fresh1388, i32 1
    %fresh1390 = load i64* %fresh1389
    store i64 %fresh1390, i64* %x651
    br label %fresh1406
    fresh1391:
    ; Call (Normal or Tail)
    call void @fun_UHC_OldIO_hPutChartildespec1(  )
    ; Result
    ;  (inlined)Variable_Unembedded x_648 = Value_Var (Variable_Subs Variable_RP 0)
    ;  (inlined)Variable_Unembedded x_649 = Value_Var (Variable_Subs Variable_RP 1)
    ; UpdateUnit
    %fresh1393 = load i64** @RP
    %fresh1394 = getelementptr i64* %fresh1393, i32 0
    %fresh1395 = load i64* %fresh1394
    %fresh1396 = load i64* %x739
    %fresh1397 = inttoptr i64 %fresh1396 to i64*
    %fresh1398 = getelementptr i64* %fresh1397, i32 0
    store i64 %fresh1395, i64* %fresh1398
    %fresh1399 = load i64** @RP
    %fresh1400 = getelementptr i64* %fresh1399, i32 1
    %fresh1401 = load i64* %fresh1400
    store i64 %fresh1401, i64* %x651
    %fresh1402 = load i64* %x739
    %fresh1403 = inttoptr i64 %fresh1402 to i64*
    %fresh1404 = getelementptr i64* %fresh1403, i32 1
    store i64 %fresh1401, i64* %fresh1404
    br label %fresh1406
    fresh1405:
    call void @primPatternMatchFailure(  )
    unreachable
    br label %fresh1406
    fresh1406:
    ; Call (Normal or Tail)
    %fresh1407 = load i64* %x651
    %fresh1408 = load i64* %x627
    %fresh1409 = inttoptr i64 %fresh1408 to i64*
    %fresh1410 = getelementptr i64* %fresh1409, i32 2
    %fresh1411 = load i64* %fresh1410
    call void @fun_UHC_OldIO_y103_0_32_0( i64 %fresh1407, i64 %fresh1411 )
    ; Result
    ;  (inlined)Variable_Unembedded x_652 = Value_Var (Variable_Subs Variable_RP 0)
    ;  (inlined)Variable_Unembedded x_653 = Value_Var (Variable_Subs Variable_RP 1)
    ; UpdateUnit
    %fresh1413 = load i64** @RP
    %fresh1414 = getelementptr i64* %fresh1413, i32 0
    %fresh1415 = load i64* %fresh1414
    store i64 %fresh1415, i64* %x630
    %fresh1416 = load i64* %x627
    %fresh1417 = inttoptr i64 %fresh1416 to i64*
    %fresh1418 = getelementptr i64* %fresh1417, i32 0
    store i64 %fresh1415, i64* %fresh1418
    %fresh1419 = load i64** @RP
    %fresh1420 = getelementptr i64* %fresh1419, i32 1
    %fresh1421 = load i64* %fresh1420
    %fresh1422 = load i64* %x627
    %fresh1423 = inttoptr i64 %fresh1422 to i64*
    %fresh1424 = getelementptr i64* %fresh1423, i32 1
    store i64 %fresh1421, i64* %fresh1424
    br label %fresh1426
    fresh1425:
    call void @primPatternMatchFailure(  )
    unreachable
    br label %fresh1426
    fresh1426:
    br label %fresh1428
    fresh1427:
    br label %fresh1428
    fresh1428:
    %fresh1429 = load i64* %x630
    switch i64 %fresh1429, label %fresh1455 [ i64 20, label %fresh1430
                                              i64 21, label %fresh1437
                                              i64 23, label %fresh1444 ]
    fresh1430:
    ; Fetch
    ;  (inlined)Variable_Unembedded x_742 = Value_Var (Variable_Subs (Variable_Unembedded x_627) 1)
    ; Call (Normal or Tail)
    %fresh1431 = load i64* %x627
    %fresh1432 = inttoptr i64 %fresh1431 to i64*
    %fresh1433 = getelementptr i64* %fresh1432, i32 1
    %fresh1434 = load i64* %fresh1433
    %fresh1435 = load i64* %x620
    tail call void @fun_UHC_Base_primretIO( i64 %fresh1434, i64 %fresh1435 )
    ; Result
    br label %fresh1456
    fresh1437:
    ; Fetch
    ;  (inlined)Variable_Unembedded x_743 = Value_Var (Variable_Subs (Variable_Unembedded x_627) 1)
    ; Call (Normal or Tail)
    %fresh1438 = load i64* %x627
    %fresh1439 = inttoptr i64 %fresh1438 to i64*
    %fresh1440 = getelementptr i64* %fresh1439, i32 1
    %fresh1441 = load i64* %fresh1440
    %fresh1442 = load i64* %x620
    tail call void @fun_UHC_Base_ioFromPrim( i64 %fresh1441, i64 %fresh1442 )
    ; Result
    br label %fresh1456
    fresh1444:
    ; Fetch
    ;  (inlined)Variable_Unembedded x_745 = Value_Var (Variable_Subs (Variable_Unembedded x_627) 1)
    ; Fetch
    ;  (inlined)Variable_Unembedded x_744 = Value_Var (Variable_Subs (Variable_Unembedded x_627) 2)
    ; Call (Normal or Tail)
    %fresh1445 = load i64* %x627
    %fresh1446 = inttoptr i64 %fresh1445 to i64*
    %fresh1447 = getelementptr i64* %fresh1446, i32 1
    %fresh1448 = load i64* %fresh1447
    %fresh1449 = load i64* %x627
    %fresh1450 = inttoptr i64 %fresh1449 to i64*
    %fresh1451 = getelementptr i64* %fresh1450, i32 2
    %fresh1452 = load i64* %fresh1451
    %fresh1453 = load i64* %x620
    tail call void @fun_UHC_Base_primbindIO( i64 %fresh1448, i64 %fresh1452, i64 %fresh1453 )
    ; Result
    br label %fresh1456
    fresh1455:
    call void @primPatternMatchFailure(  )
    unreachable
    br label %fresh1456
    fresh1456:
    ret void
}

define internal void @fun_UHC_Base_tail( i64 %x167 ) nounwind
{
    %x654 = alloca i64, i32 1
    %x666 = alloca i64, i32 1
    %x667 = alloca i64, i32 1
    %x668 = alloca i64, i32 1
    %x671 = alloca i64, i32 1
    %x672 = alloca i64, i32 1
    %x747 = alloca i64, i32 1
    ; Fetch
    %fresh1457 = inttoptr i64 %x167 to i64*
    %fresh1458 = getelementptr i64* %fresh1457, i32 0
    %fresh1459 = load i64* %fresh1458
    store i64 %fresh1459, i64* %x654
    ; Case
    %fresh1460 = load i64* %x654
    %fresh1461 = icmp sgt i64 %fresh1460, 27
    br i1 %fresh1461, label %fresh1462, label %fresh1503
    fresh1462:
    %fresh1463 = load i64* %x654
    switch i64 %fresh1463, label %fresh1501 [ i64 42, label %fresh1464
                                              i64 43, label %fresh1484 ]
    fresh1464:
    ; Fetch
    ;  (inlined)Variable_Unembedded x_746 = Value_Var (Variable_Subs (Variable_Unembedded x_167_UHC.Base.93_6226_0__347) 1)
    ; Call (Normal or Tail)
    %fresh1465 = inttoptr i64 %x167 to i64*
    %fresh1466 = getelementptr i64* %fresh1465, i32 1
    %fresh1467 = load i64* %fresh1466
    call void @fun_UHC_Base_tail( i64 %fresh1467 )
    ; Result
    ;  (inlined)Variable_Unembedded x_657 = Value_Var (Variable_Subs Variable_RP 0)
    ;  (inlined)Variable_Unembedded x_658 = Value_Var (Variable_Subs Variable_RP 1)
    ;  (inlined)Variable_Unembedded x_659 = Value_Var (Variable_Subs Variable_RP 2)
    ; UpdateUnit
    %fresh1469 = load i64** @RP
    %fresh1470 = getelementptr i64* %fresh1469, i32 0
    %fresh1471 = load i64* %fresh1470
    store i64 %fresh1471, i64* %x654
    %fresh1472 = inttoptr i64 %x167 to i64*
    %fresh1473 = getelementptr i64* %fresh1472, i32 0
    store i64 %fresh1471, i64* %fresh1473
    %fresh1474 = load i64** @RP
    %fresh1475 = getelementptr i64* %fresh1474, i32 1
    %fresh1476 = load i64* %fresh1475
    %fresh1477 = inttoptr i64 %x167 to i64*
    %fresh1478 = getelementptr i64* %fresh1477, i32 1
    store i64 %fresh1476, i64* %fresh1478
    %fresh1479 = load i64** @RP
    %fresh1480 = getelementptr i64* %fresh1479, i32 2
    %fresh1481 = load i64* %fresh1480
    %fresh1482 = inttoptr i64 %x167 to i64*
    %fresh1483 = getelementptr i64* %fresh1482, i32 2
    store i64 %fresh1481, i64* %fresh1483
    br label %fresh1502
    fresh1484:
    ; Call (Normal or Tail)
    call void @fun_UHC_Base_tailtildespec1(  )
    ; Result
    ;  (inlined)Variable_Unembedded x_660 = Value_Var (Variable_Subs Variable_RP 0)
    ;  (inlined)Variable_Unembedded x_661 = Value_Var (Variable_Subs Variable_RP 1)
    ;  (inlined)Variable_Unembedded x_662 = Value_Var (Variable_Subs Variable_RP 2)
    ; UpdateUnit
    %fresh1486 = load i64** @RP
    %fresh1487 = getelementptr i64* %fresh1486, i32 0
    %fresh1488 = load i64* %fresh1487
    store i64 %fresh1488, i64* %x654
    %fresh1489 = inttoptr i64 %x167 to i64*
    %fresh1490 = getelementptr i64* %fresh1489, i32 0
    store i64 %fresh1488, i64* %fresh1490
    %fresh1491 = load i64** @RP
    %fresh1492 = getelementptr i64* %fresh1491, i32 1
    %fresh1493 = load i64* %fresh1492
    %fresh1494 = inttoptr i64 %x167 to i64*
    %fresh1495 = getelementptr i64* %fresh1494, i32 1
    store i64 %fresh1493, i64* %fresh1495
    %fresh1496 = load i64** @RP
    %fresh1497 = getelementptr i64* %fresh1496, i32 2
    %fresh1498 = load i64* %fresh1497
    %fresh1499 = inttoptr i64 %x167 to i64*
    %fresh1500 = getelementptr i64* %fresh1499, i32 2
    store i64 %fresh1498, i64* %fresh1500
    br label %fresh1502
    fresh1501:
    call void @primPatternMatchFailure(  )
    unreachable
    br label %fresh1502
    fresh1502:
    br label %fresh1504
    fresh1503:
    br label %fresh1504
    fresh1504:
    %fresh1505 = load i64* %x654
    switch i64 %fresh1505, label %fresh1604 [ i64 3, label %fresh1506
                                              i64 4, label %fresh1570 ]
    fresh1506:
    ; Fetch
    %fresh1507 = inttoptr i64 %x167 to i64*
    %fresh1508 = getelementptr i64* %fresh1507, i32 2
    %fresh1509 = load i64* %fresh1508
    store i64 %fresh1509, i64* %x747
    ; Fetch
    ;  (inlined)Variable_Unembedded x_663 = Value_Var (Variable_Subs (Variable_Unembedded x_747) 0)
    ; Case
    %fresh1510 = load i64* %x747
    %fresh1511 = inttoptr i64 %fresh1510 to i64*
    %fresh1512 = getelementptr i64* %fresh1511, i32 0
    %fresh1513 = load i64* %fresh1512
    switch i64 %fresh1513, label %fresh1568 [ i64 3, label %fresh1514
                                              i64 4, label %fresh1529
                                              i64 37, label %fresh1532 ]
    fresh1514:
    ; Fetch
    ;  (inlined)Variable_Unembedded x_750 = Value_Var (Variable_Subs (Variable_Unembedded x_747) 1)
    ; Fetch
    ;  (inlined)Variable_Unembedded x_749 = Value_Var (Variable_Subs (Variable_Unembedded x_747) 2)
    ; Unit (Yielding)
    %fresh1515 = load i64** @RP
    %fresh1516 = getelementptr i64* %fresh1515, i32 0
    store i64 3, i64* %fresh1516
    %fresh1517 = load i64* %x747
    %fresh1518 = inttoptr i64 %fresh1517 to i64*
    %fresh1519 = getelementptr i64* %fresh1518, i32 1
    %fresh1520 = load i64* %fresh1519
    %fresh1521 = load i64** @RP
    %fresh1522 = getelementptr i64* %fresh1521, i32 1
    store i64 %fresh1520, i64* %fresh1522
    %fresh1523 = load i64* %x747
    %fresh1524 = inttoptr i64 %fresh1523 to i64*
    %fresh1525 = getelementptr i64* %fresh1524, i32 2
    %fresh1526 = load i64* %fresh1525
    %fresh1527 = load i64** @RP
    %fresh1528 = getelementptr i64* %fresh1527, i32 2
    store i64 %fresh1526, i64* %fresh1528
    br label %fresh1569
    fresh1529:
    ; Unit (Yielding)
    %fresh1530 = load i64** @RP
    %fresh1531 = getelementptr i64* %fresh1530, i32 0
    store i64 4, i64* %fresh1531
    br label %fresh1569
    fresh1532:
    ; Fetch
    ;  (inlined)Variable_Unembedded x_751 = Value_Var (Variable_Subs (Variable_Unembedded x_747) 1)
    ; Call (Normal or Tail)
    %fresh1533 = load i64* %x747
    %fresh1534 = inttoptr i64 %fresh1533 to i64*
    %fresh1535 = getelementptr i64* %fresh1534, i32 1
    %fresh1536 = load i64* %fresh1535
    call void @fun_UHC_Base_packedStringToString( i64 %fresh1536 )
    ; Result
    %fresh1538 = load i64** @RP
    %fresh1539 = getelementptr i64* %fresh1538, i32 0
    %fresh1540 = load i64* %fresh1539
    store i64 %fresh1540, i64* %x666
    %fresh1541 = load i64** @RP
    %fresh1542 = getelementptr i64* %fresh1541, i32 1
    %fresh1543 = load i64* %fresh1542
    store i64 %fresh1543, i64* %x667
    %fresh1544 = load i64** @RP
    %fresh1545 = getelementptr i64* %fresh1544, i32 2
    %fresh1546 = load i64* %fresh1545
    store i64 %fresh1546, i64* %x668
    ; Update
    %fresh1547 = load i64* %x666
    %fresh1548 = load i64* %x747
    %fresh1549 = inttoptr i64 %fresh1548 to i64*
    %fresh1550 = getelementptr i64* %fresh1549, i32 0
    store i64 %fresh1547, i64* %fresh1550
    %fresh1551 = load i64* %x667
    %fresh1552 = load i64* %x747
    %fresh1553 = inttoptr i64 %fresh1552 to i64*
    %fresh1554 = getelementptr i64* %fresh1553, i32 1
    store i64 %fresh1551, i64* %fresh1554
    %fresh1555 = load i64* %x668
    %fresh1556 = load i64* %x747
    %fresh1557 = inttoptr i64 %fresh1556 to i64*
    %fresh1558 = getelementptr i64* %fresh1557, i32 2
    store i64 %fresh1555, i64* %fresh1558
    ; Unit (Yielding)
    %fresh1559 = load i64* %x666
    %fresh1560 = load i64** @RP
    %fresh1561 = getelementptr i64* %fresh1560, i32 0
    store i64 %fresh1559, i64* %fresh1561
    %fresh1562 = load i64* %x667
    %fresh1563 = load i64** @RP
    %fresh1564 = getelementptr i64* %fresh1563, i32 1
    store i64 %fresh1562, i64* %fresh1564
    %fresh1565 = load i64* %x668
    %fresh1566 = load i64** @RP
    %fresh1567 = getelementptr i64* %fresh1566, i32 2
    store i64 %fresh1565, i64* %fresh1567
    br label %fresh1569
    fresh1568:
    call void @primPatternMatchFailure(  )
    unreachable
    br label %fresh1569
    fresh1569:
    br label %fresh1605
    fresh1570:
    ; Fetch
    ;  (inlined)Variable_Unembedded x_669 = Value_Var (Variable_Subs (Variable_Unembedded global_x_6_UHC.Base.undefined) 0)
    ; Case
    %fresh1571 = load i64** @global_UHC_Base_undefined
    %fresh1572 = getelementptr i64* %fresh1571, i32 0
    %fresh1573 = load i64* %fresh1572
    switch i64 %fresh1573, label %fresh1602 [ i64 26, label %fresh1574
                                              i64 35, label %fresh1582 ]
    fresh1574:
    ; Fetch
    ;  (inlined)Variable_Unembedded x_752 = Value_Var (Variable_Subs (Variable_Unembedded global_x_6_UHC.Base.undefined) 1)
    ; Unit (Yielding)
    %fresh1575 = load i64** @RP
    %fresh1576 = getelementptr i64* %fresh1575, i32 0
    store i64 26, i64* %fresh1576
    %fresh1577 = load i64** @global_UHC_Base_undefined
    %fresh1578 = getelementptr i64* %fresh1577, i32 1
    %fresh1579 = load i64* %fresh1578
    %fresh1580 = load i64** @RP
    %fresh1581 = getelementptr i64* %fresh1580, i32 1
    store i64 %fresh1579, i64* %fresh1581
    br label %fresh1603
    fresh1582:
    ; Call (Normal or Tail)
    call void @fun_UHC_Base_errortildespec1(  )
    ; Result
    %fresh1584 = load i64** @RP
    %fresh1585 = getelementptr i64* %fresh1584, i32 0
    %fresh1586 = load i64* %fresh1585
    store i64 %fresh1586, i64* %x671
    %fresh1587 = load i64** @RP
    %fresh1588 = getelementptr i64* %fresh1587, i32 1
    %fresh1589 = load i64* %fresh1588
    store i64 %fresh1589, i64* %x672
    ; Update
    %fresh1590 = load i64* %x671
    %fresh1591 = load i64** @global_UHC_Base_undefined
    %fresh1592 = getelementptr i64* %fresh1591, i32 0
    store i64 %fresh1590, i64* %fresh1592
    %fresh1593 = load i64* %x672
    %fresh1594 = load i64** @global_UHC_Base_undefined
    %fresh1595 = getelementptr i64* %fresh1594, i32 1
    store i64 %fresh1593, i64* %fresh1595
    ; Unit (Yielding)
    %fresh1596 = load i64* %x671
    %fresh1597 = load i64** @RP
    %fresh1598 = getelementptr i64* %fresh1597, i32 0
    store i64 %fresh1596, i64* %fresh1598
    %fresh1599 = load i64* %x672
    %fresh1600 = load i64** @RP
    %fresh1601 = getelementptr i64* %fresh1600, i32 1
    store i64 %fresh1599, i64* %fresh1601
    br label %fresh1603
    fresh1602:
    call void @primPatternMatchFailure(  )
    unreachable
    br label %fresh1603
    fresh1603:
    br label %fresh1605
    fresh1604:
    call void @primPatternMatchFailure(  )
    unreachable
    br label %fresh1605
    fresh1605:
    ret void
}

define internal void @fun_UHC_Base_tailtildespec1(  ) nounwind
{
    %x673 = alloca i64, i32 1
    %x682 = alloca i64, i32 1
    %x683 = alloca i64, i32 1
    %x684 = alloca i64, i32 1
    %x687 = alloca i64, i32 1
    %x688 = alloca i64, i32 1
    %x753 = alloca i64, i32 1
    ; Fetch
    %fresh1606 = load i64** @global_Main_y13_0_2
    %fresh1607 = getelementptr i64* %fresh1606, i32 0
    %fresh1608 = load i64* %fresh1607
    store i64 %fresh1608, i64* %x673
    ; Case
    %fresh1609 = load i64* %x673
    %fresh1610 = icmp sgt i64 %fresh1609, 27
    br i1 %fresh1610, label %fresh1611, label %fresh1628
    fresh1611:
    ; Call (Normal or Tail)
    call void @fun_UHC_Base_packedStringToStringtildespec1(  )
    ; Result
    ;  (inlined)Variable_Unembedded x_676 = Value_Var (Variable_Subs Variable_RP 0)
    ;  (inlined)Variable_Unembedded x_677 = Value_Var (Variable_Subs Variable_RP 1)
    ;  (inlined)Variable_Unembedded x_678 = Value_Var (Variable_Subs Variable_RP 2)
    ; UpdateUnit
    %fresh1613 = load i64** @RP
    %fresh1614 = getelementptr i64* %fresh1613, i32 0
    %fresh1615 = load i64* %fresh1614
    store i64 %fresh1615, i64* %x673
    %fresh1616 = load i64** @global_Main_y13_0_2
    %fresh1617 = getelementptr i64* %fresh1616, i32 0
    store i64 %fresh1615, i64* %fresh1617
    %fresh1618 = load i64** @RP
    %fresh1619 = getelementptr i64* %fresh1618, i32 1
    %fresh1620 = load i64* %fresh1619
    %fresh1621 = load i64** @global_Main_y13_0_2
    %fresh1622 = getelementptr i64* %fresh1621, i32 1
    store i64 %fresh1620, i64* %fresh1622
    %fresh1623 = load i64** @RP
    %fresh1624 = getelementptr i64* %fresh1623, i32 2
    %fresh1625 = load i64* %fresh1624
    %fresh1626 = load i64** @global_Main_y13_0_2
    %fresh1627 = getelementptr i64* %fresh1626, i32 2
    store i64 %fresh1625, i64* %fresh1627
    br label %fresh1629
    fresh1628:
    br label %fresh1629
    fresh1629:
    %fresh1630 = load i64* %x673
    switch i64 %fresh1630, label %fresh1729 [ i64 3, label %fresh1631
                                              i64 4, label %fresh1695 ]
    fresh1631:
    ; Fetch
    %fresh1632 = load i64** @global_Main_y13_0_2
    %fresh1633 = getelementptr i64* %fresh1632, i32 2
    %fresh1634 = load i64* %fresh1633
    store i64 %fresh1634, i64* %x753
    ; Fetch
    ;  (inlined)Variable_Unembedded x_679 = Value_Var (Variable_Subs (Variable_Unembedded x_753) 0)
    ; Case
    %fresh1635 = load i64* %x753
    %fresh1636 = inttoptr i64 %fresh1635 to i64*
    %fresh1637 = getelementptr i64* %fresh1636, i32 0
    %fresh1638 = load i64* %fresh1637
    switch i64 %fresh1638, label %fresh1693 [ i64 3, label %fresh1639
                                              i64 4, label %fresh1654
                                              i64 37, label %fresh1657 ]
    fresh1639:
    ; Fetch
    ;  (inlined)Variable_Unembedded x_755 = Value_Var (Variable_Subs (Variable_Unembedded x_753) 1)
    ; Fetch
    ;  (inlined)Variable_Unembedded x_754 = Value_Var (Variable_Subs (Variable_Unembedded x_753) 2)
    ; Unit (Yielding)
    %fresh1640 = load i64** @RP
    %fresh1641 = getelementptr i64* %fresh1640, i32 0
    store i64 3, i64* %fresh1641
    %fresh1642 = load i64* %x753
    %fresh1643 = inttoptr i64 %fresh1642 to i64*
    %fresh1644 = getelementptr i64* %fresh1643, i32 1
    %fresh1645 = load i64* %fresh1644
    %fresh1646 = load i64** @RP
    %fresh1647 = getelementptr i64* %fresh1646, i32 1
    store i64 %fresh1645, i64* %fresh1647
    %fresh1648 = load i64* %x753
    %fresh1649 = inttoptr i64 %fresh1648 to i64*
    %fresh1650 = getelementptr i64* %fresh1649, i32 2
    %fresh1651 = load i64* %fresh1650
    %fresh1652 = load i64** @RP
    %fresh1653 = getelementptr i64* %fresh1652, i32 2
    store i64 %fresh1651, i64* %fresh1653
    br label %fresh1694
    fresh1654:
    ; Unit (Yielding)
    %fresh1655 = load i64** @RP
    %fresh1656 = getelementptr i64* %fresh1655, i32 0
    store i64 4, i64* %fresh1656
    br label %fresh1694
    fresh1657:
    ; Fetch
    ;  (inlined)Variable_Unembedded x_756 = Value_Var (Variable_Subs (Variable_Unembedded x_753) 1)
    ; Call (Normal or Tail)
    %fresh1658 = load i64* %x753
    %fresh1659 = inttoptr i64 %fresh1658 to i64*
    %fresh1660 = getelementptr i64* %fresh1659, i32 1
    %fresh1661 = load i64* %fresh1660
    call void @fun_UHC_Base_packedStringToString( i64 %fresh1661 )
    ; Result
    %fresh1663 = load i64** @RP
    %fresh1664 = getelementptr i64* %fresh1663, i32 0
    %fresh1665 = load i64* %fresh1664
    store i64 %fresh1665, i64* %x682
    %fresh1666 = load i64** @RP
    %fresh1667 = getelementptr i64* %fresh1666, i32 1
    %fresh1668 = load i64* %fresh1667
    store i64 %fresh1668, i64* %x683
    %fresh1669 = load i64** @RP
    %fresh1670 = getelementptr i64* %fresh1669, i32 2
    %fresh1671 = load i64* %fresh1670
    store i64 %fresh1671, i64* %x684
    ; Update
    %fresh1672 = load i64* %x682
    %fresh1673 = load i64* %x753
    %fresh1674 = inttoptr i64 %fresh1673 to i64*
    %fresh1675 = getelementptr i64* %fresh1674, i32 0
    store i64 %fresh1672, i64* %fresh1675
    %fresh1676 = load i64* %x683
    %fresh1677 = load i64* %x753
    %fresh1678 = inttoptr i64 %fresh1677 to i64*
    %fresh1679 = getelementptr i64* %fresh1678, i32 1
    store i64 %fresh1676, i64* %fresh1679
    %fresh1680 = load i64* %x684
    %fresh1681 = load i64* %x753
    %fresh1682 = inttoptr i64 %fresh1681 to i64*
    %fresh1683 = getelementptr i64* %fresh1682, i32 2
    store i64 %fresh1680, i64* %fresh1683
    ; Unit (Yielding)
    %fresh1684 = load i64* %x682
    %fresh1685 = load i64** @RP
    %fresh1686 = getelementptr i64* %fresh1685, i32 0
    store i64 %fresh1684, i64* %fresh1686
    %fresh1687 = load i64* %x683
    %fresh1688 = load i64** @RP
    %fresh1689 = getelementptr i64* %fresh1688, i32 1
    store i64 %fresh1687, i64* %fresh1689
    %fresh1690 = load i64* %x684
    %fresh1691 = load i64** @RP
    %fresh1692 = getelementptr i64* %fresh1691, i32 2
    store i64 %fresh1690, i64* %fresh1692
    br label %fresh1694
    fresh1693:
    call void @primPatternMatchFailure(  )
    unreachable
    br label %fresh1694
    fresh1694:
    br label %fresh1730
    fresh1695:
    ; Fetch
    ;  (inlined)Variable_Unembedded x_685 = Value_Var (Variable_Subs (Variable_Unembedded global_x_6_UHC.Base.undefined) 0)
    ; Case
    %fresh1696 = load i64** @global_UHC_Base_undefined
    %fresh1697 = getelementptr i64* %fresh1696, i32 0
    %fresh1698 = load i64* %fresh1697
    switch i64 %fresh1698, label %fresh1727 [ i64 26, label %fresh1699
                                              i64 35, label %fresh1707 ]
    fresh1699:
    ; Fetch
    ;  (inlined)Variable_Unembedded x_757 = Value_Var (Variable_Subs (Variable_Unembedded global_x_6_UHC.Base.undefined) 1)
    ; Unit (Yielding)
    %fresh1700 = load i64** @RP
    %fresh1701 = getelementptr i64* %fresh1700, i32 0
    store i64 26, i64* %fresh1701
    %fresh1702 = load i64** @global_UHC_Base_undefined
    %fresh1703 = getelementptr i64* %fresh1702, i32 1
    %fresh1704 = load i64* %fresh1703
    %fresh1705 = load i64** @RP
    %fresh1706 = getelementptr i64* %fresh1705, i32 1
    store i64 %fresh1704, i64* %fresh1706
    br label %fresh1728
    fresh1707:
    ; Call (Normal or Tail)
    call void @fun_UHC_Base_errortildespec1(  )
    ; Result
    %fresh1709 = load i64** @RP
    %fresh1710 = getelementptr i64* %fresh1709, i32 0
    %fresh1711 = load i64* %fresh1710
    store i64 %fresh1711, i64* %x687
    %fresh1712 = load i64** @RP
    %fresh1713 = getelementptr i64* %fresh1712, i32 1
    %fresh1714 = load i64* %fresh1713
    store i64 %fresh1714, i64* %x688
    ; Update
    %fresh1715 = load i64* %x687
    %fresh1716 = load i64** @global_UHC_Base_undefined
    %fresh1717 = getelementptr i64* %fresh1716, i32 0
    store i64 %fresh1715, i64* %fresh1717
    %fresh1718 = load i64* %x688
    %fresh1719 = load i64** @global_UHC_Base_undefined
    %fresh1720 = getelementptr i64* %fresh1719, i32 1
    store i64 %fresh1718, i64* %fresh1720
    ; Unit (Yielding)
    %fresh1721 = load i64* %x687
    %fresh1722 = load i64** @RP
    %fresh1723 = getelementptr i64* %fresh1722, i32 0
    store i64 %fresh1721, i64* %fresh1723
    %fresh1724 = load i64* %x688
    %fresh1725 = load i64** @RP
    %fresh1726 = getelementptr i64* %fresh1725, i32 1
    store i64 %fresh1724, i64* %fresh1726
    br label %fresh1728
    fresh1727:
    call void @primPatternMatchFailure(  )
    unreachable
    br label %fresh1728
    fresh1728:
    br label %fresh1730
    fresh1729:
    call void @primPatternMatchFailure(  )
    unreachable
    br label %fresh1730
    fresh1730:
    ret void
}

define internal void @fun_UHC_OldIO_hPutChartildespec1(  ) nounwind
{
    ; Unit (Yielding)
    %fresh1731 = load i64** @RP
    %fresh1732 = getelementptr i64* %fresh1731, i32 0
    store i64 24, i64* %fresh1732
    %fresh1733 = load i64** @global_UHC_OldIO_primStdout
    %fresh1734 = load i64** @RP
    %fresh1735 = getelementptr i64* %fresh1734, i32 1
    %fresh1736 = ptrtoint i64* %fresh1733 to i64
    store i64 %fresh1736, i64* %fresh1735
    ret void
}

define internal void @fun_UHC_OldIO_y103_0_32_0( i64 %x189, i64 %x190 ) nounwind
{
    %x191 = alloca i64, i32 1
    ; Store
    %fresh1737 = call i64* @llvmgc_malloc( i64 24 )
    %fresh1738 = ptrtoint i64* %fresh1737 to i64
    store i64 %fresh1738, i64* %x191
    %fresh1739 = load i64* %x191
    %fresh1740 = inttoptr i64 %fresh1739 to i64*
    %fresh1741 = getelementptr i64* %fresh1740, i32 0
    store i64 25, i64* %fresh1741
    %fresh1742 = load i64* %x191
    %fresh1743 = inttoptr i64 %fresh1742 to i64*
    %fresh1744 = getelementptr i64* %fresh1743, i32 1
    store i64 %x189, i64* %fresh1744
    %fresh1745 = load i64* %x191
    %fresh1746 = inttoptr i64 %fresh1745 to i64*
    %fresh1747 = getelementptr i64* %fresh1746, i32 2
    store i64 %x190, i64* %fresh1747
    ; Unit (Yielding)
    %fresh1748 = load i64** @RP
    %fresh1749 = getelementptr i64* %fresh1748, i32 0
    store i64 21, i64* %fresh1749
    %fresh1750 = load i64* %x191
    %fresh1751 = load i64** @RP
    %fresh1752 = getelementptr i64* %fresh1751, i32 1
    store i64 %fresh1750, i64* %fresh1752
    ret void
}

