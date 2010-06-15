; CInt->8
; HOLE->9
; FprimSubInttildespec1->10
; FprimSubInttildespec1tildespec1->11

@fresh0 = internal constant [ 6 x i8] c"\25lld\0a\00"
@RP = internal global i64* zeroinitializer

declare i32 @printf( i8* %str, ... )
declare void @llvmgc_init(  )
declare void @llvmc_print_statistics(  )
declare i64* @llvmgc_malloc( i64 %x )
declare i64* @llvmgc_malloc_uncollectable( i64 %x )
declare void @primPatternMatchFailure(  )
declare i64 @primSubInt( i64 %fresh40, i64 %fresh39 )
declare i64 @primSubInt( i64 %fresh48, i64 %fresh47 )
declare i64 @primEqInt( i64 %fresh93, i64 %fresh92 )
declare i64 @primMulInt( i64 %fresh148, i64 %fresh147 )
declare i64 @primEqInt( i64 %fresh163, i64 %fresh162 )
declare i64 @primMulInt( i64 %fresh178, i64 %fresh177 )

define  i32 @main(  ) nounwind
{
    call void @llvmgc_init(  )
    %rpArr = call i64* @llvmgc_malloc_uncollectable( i64 16 )
    store i64* %rpArr, i64** @RP
    call void @initialize(  )
    call void @fun_fun0tildemain(  )
    %rp.1 = getelementptr i64* %rpArr, i32 1
    %rp.1.val = load i64* %rp.1
    %cast.str.res = getelementptr [ 6 x i8]* @fresh0, i32 0, i32 0
    tail call i32 ( i8*, ... )* @printf( i8* %cast.str.res, i64 %rp.1.val )
    call void @llvmc_print_statistics(  )
    ret i32 0
}

define internal void @initialize(  ) nounwind
{
    ret void
}

define internal void @fun_primSubInttildespec1( i64 %y17_0_2 ) nounwind
{
    %y_17_0_2 = alloca i64, i32 1
    %y17_0_0 = alloca i64, i32 1
    ; Fetch
    ;  (inlined)Variable_Unembedded x_59 = Value_Var (Variable_Subs (Variable_Unembedded x_9_17_0_2) 0)
    ; Case
    %fresh1 = inttoptr i64 %y17_0_2 to i64*
    %fresh2 = getelementptr i64* %fresh1, i32 0
    %fresh3 = load i64* %fresh2
    switch i64 %fresh3, label %fresh35 [ i64 8, label %fresh4
                                         i64 10, label %fresh8
                                         i64 11, label %fresh23 ]
    fresh4:
    ; Fetch
    ;  (inlined)Variable_Unembedded x_77 = Value_Var (Variable_Subs (Variable_Unembedded x_9_17_0_2) 1)
    ; Unit
    %fresh5 = inttoptr i64 %y17_0_2 to i64*
    %fresh6 = getelementptr i64* %fresh5, i32 1
    %fresh7 = load i64* %fresh6
    store i64 %fresh7, i64* %y_17_0_2
    br label %fresh36
    fresh8:
    ; Fetch
    ;  (inlined)Variable_Unembedded x_78 = Value_Var (Variable_Subs (Variable_Unembedded x_9_17_0_2) 1)
    ; Call (Normal or Tail)
    %fresh9 = inttoptr i64 %y17_0_2 to i64*
    %fresh10 = getelementptr i64* %fresh9, i32 1
    %fresh11 = load i64* %fresh10
    call void @fun_primSubInttildespec1( i64 %fresh11 )
    ; Result
    ;  (inlined)Variable_Unembedded x_61 = Value_Var (Variable_Subs Variable_RP 0)
    ;  (inlined)Variable_Unembedded x_62 = Value_Var (Variable_Subs Variable_RP 1)
    ; UpdateUnit
    %fresh13 = load i64** @RP
    %fresh14 = getelementptr i64* %fresh13, i32 0
    %fresh15 = load i64* %fresh14
    %fresh16 = inttoptr i64 %y17_0_2 to i64*
    %fresh17 = getelementptr i64* %fresh16, i32 0
    store i64 %fresh15, i64* %fresh17
    %fresh18 = load i64** @RP
    %fresh19 = getelementptr i64* %fresh18, i32 1
    %fresh20 = load i64* %fresh19
    store i64 %fresh20, i64* %y_17_0_2
    %fresh21 = inttoptr i64 %y17_0_2 to i64*
    %fresh22 = getelementptr i64* %fresh21, i32 1
    store i64 %fresh20, i64* %fresh22
    br label %fresh36
    fresh23:
    ; Call (Normal or Tail)
    call void @fun_primSubInttildespec1tildespec1(  )
    ; Result
    ;  (inlined)Variable_Unembedded x_63 = Value_Var (Variable_Subs Variable_RP 0)
    ;  (inlined)Variable_Unembedded x_64 = Value_Var (Variable_Subs Variable_RP 1)
    ; UpdateUnit
    %fresh25 = load i64** @RP
    %fresh26 = getelementptr i64* %fresh25, i32 0
    %fresh27 = load i64* %fresh26
    %fresh28 = inttoptr i64 %y17_0_2 to i64*
    %fresh29 = getelementptr i64* %fresh28, i32 0
    store i64 %fresh27, i64* %fresh29
    %fresh30 = load i64** @RP
    %fresh31 = getelementptr i64* %fresh30, i32 1
    %fresh32 = load i64* %fresh31
    store i64 %fresh32, i64* %y_17_0_2
    %fresh33 = inttoptr i64 %y17_0_2 to i64*
    %fresh34 = getelementptr i64* %fresh33, i32 1
    store i64 %fresh32, i64* %fresh34
    br label %fresh36
    fresh35:
    call void @primPatternMatchFailure(  )
    unreachable
    br label %fresh36
    fresh36:
    ; Unit
    ;  (inlined)Variable_Unembedded x_11__17_0_3 = Value_Con (Constant_LiteralInt 1)
    ; FFI
    %fresh37 = load i64* %y_17_0_2
    %fresh38 = call i64 @primSubInt( i64 %fresh37, i64 1 )
    store i64 %fresh38, i64* %y17_0_0
    ; Unit (Yielding)
    %fresh41 = load i64** @RP
    %fresh42 = getelementptr i64* %fresh41, i32 0
    store i64 8, i64* %fresh42
    %fresh43 = load i64* %y17_0_0
    %fresh44 = load i64** @RP
    %fresh45 = getelementptr i64* %fresh44, i32 1
    store i64 %fresh43, i64* %fresh45
    ret void
}

define internal void @fun_primSubInttildespec1tildespec1(  ) nounwind
{
    %y17_0_0 = alloca i64, i32 1
    ; Unit
    ;  (inlined)Variable_Unembedded x_15__17_0_2 = Value_Con (Constant_LiteralInt 10)
    ; Unit
    ;  (inlined)Variable_Unembedded x_16__17_0_3 = Value_Con (Constant_LiteralInt 1)
    ; FFI
    %fresh46 = call i64 @primSubInt( i64 10, i64 1 )
    store i64 %fresh46, i64* %y17_0_0
    ; Unit (Yielding)
    %fresh49 = load i64** @RP
    %fresh50 = getelementptr i64* %fresh49, i32 0
    store i64 8, i64* %fresh50
    %fresh51 = load i64* %y17_0_0
    %fresh52 = load i64** @RP
    %fresh53 = getelementptr i64* %fresh52, i32 1
    store i64 %fresh51, i64* %fresh53
    ret void
}

define internal void @fun_fac( i64 %n__510 ) nounwind
{
    %y_17_12_20 = alloca i64, i32 1
    %y17_12_00 = alloca i64, i32 1
    %y11_0_937 = alloca i64, i32 1
    %y_17_4_21 = alloca i64, i32 1
    %y17_4_01 = alloca i64, i32 1
    ; Fetch
    ;  (inlined)Variable_Unembedded x_65 = Value_Var (Variable_Subs (Variable_Unembedded x_20_n__510) 0)
    ; Case
    %fresh54 = inttoptr i64 %n__510 to i64*
    %fresh55 = getelementptr i64* %fresh54, i32 0
    %fresh56 = load i64* %fresh55
    switch i64 %fresh56, label %fresh88 [ i64 8, label %fresh57
                                          i64 10, label %fresh61
                                          i64 11, label %fresh76 ]
    fresh57:
    ; Fetch
    ;  (inlined)Variable_Unembedded x_79 = Value_Var (Variable_Subs (Variable_Unembedded x_20_n__510) 1)
    ; Unit
    %fresh58 = inttoptr i64 %n__510 to i64*
    %fresh59 = getelementptr i64* %fresh58, i32 1
    %fresh60 = load i64* %fresh59
    store i64 %fresh60, i64* %y_17_12_20
    br label %fresh89
    fresh61:
    ; Fetch
    ;  (inlined)Variable_Unembedded x_80 = Value_Var (Variable_Subs (Variable_Unembedded x_20_n__510) 1)
    ; Call (Normal or Tail)
    %fresh62 = inttoptr i64 %n__510 to i64*
    %fresh63 = getelementptr i64* %fresh62, i32 1
    %fresh64 = load i64* %fresh63
    call void @fun_primSubInttildespec1( i64 %fresh64 )
    ; Result
    ;  (inlined)Variable_Unembedded x_67 = Value_Var (Variable_Subs Variable_RP 0)
    ;  (inlined)Variable_Unembedded x_68 = Value_Var (Variable_Subs Variable_RP 1)
    ; UpdateUnit
    %fresh66 = load i64** @RP
    %fresh67 = getelementptr i64* %fresh66, i32 0
    %fresh68 = load i64* %fresh67
    %fresh69 = inttoptr i64 %n__510 to i64*
    %fresh70 = getelementptr i64* %fresh69, i32 0
    store i64 %fresh68, i64* %fresh70
    %fresh71 = load i64** @RP
    %fresh72 = getelementptr i64* %fresh71, i32 1
    %fresh73 = load i64* %fresh72
    store i64 %fresh73, i64* %y_17_12_20
    %fresh74 = inttoptr i64 %n__510 to i64*
    %fresh75 = getelementptr i64* %fresh74, i32 1
    store i64 %fresh73, i64* %fresh75
    br label %fresh89
    fresh76:
    ; Call (Normal or Tail)
    call void @fun_primSubInttildespec1tildespec1(  )
    ; Result
    ;  (inlined)Variable_Unembedded x_69 = Value_Var (Variable_Subs Variable_RP 0)
    ;  (inlined)Variable_Unembedded x_70 = Value_Var (Variable_Subs Variable_RP 1)
    ; UpdateUnit
    %fresh78 = load i64** @RP
    %fresh79 = getelementptr i64* %fresh78, i32 0
    %fresh80 = load i64* %fresh79
    %fresh81 = inttoptr i64 %n__510 to i64*
    %fresh82 = getelementptr i64* %fresh81, i32 0
    store i64 %fresh80, i64* %fresh82
    %fresh83 = load i64** @RP
    %fresh84 = getelementptr i64* %fresh83, i32 1
    %fresh85 = load i64* %fresh84
    store i64 %fresh85, i64* %y_17_12_20
    %fresh86 = inttoptr i64 %n__510 to i64*
    %fresh87 = getelementptr i64* %fresh86, i32 1
    store i64 %fresh85, i64* %fresh87
    br label %fresh89
    fresh88:
    call void @primPatternMatchFailure(  )
    unreachable
    br label %fresh89
    fresh89:
    ; Unit
    ;  (inlined)Variable_Unembedded x_22__17_12_30 = Value_Con (Constant_LiteralInt 1)
    ; FFI
    %fresh90 = load i64* %y_17_12_20
    %fresh91 = call i64 @primEqInt( i64 %fresh90, i64 1 )
    store i64 %fresh91, i64* %y17_12_00
    ; Case
    %fresh94 = load i64* %y17_12_00
    switch i64 %fresh94, label %fresh159 [ i64 1, label %fresh95
                                           i64 2, label %fresh154 ]
    fresh95:
    ; Store
    %fresh96 = call i64* @llvmgc_malloc( i64 16 )
    %fresh97 = ptrtoint i64* %fresh96 to i64
    store i64 %fresh97, i64* %y11_0_937
    %fresh98 = load i64* %y11_0_937
    %fresh99 = inttoptr i64 %fresh98 to i64*
    %fresh100 = getelementptr i64* %fresh99, i32 0
    store i64 10, i64* %fresh100
    %fresh101 = load i64* %y11_0_937
    %fresh102 = inttoptr i64 %fresh101 to i64*
    %fresh103 = getelementptr i64* %fresh102, i32 1
    store i64 %n__510, i64* %fresh103
    ; Fetch
    ;  (inlined)Variable_Unembedded x_71 = Value_Var (Variable_Subs (Variable_Unembedded x_20_n__510) 0)
    ; Case
    %fresh104 = inttoptr i64 %n__510 to i64*
    %fresh105 = getelementptr i64* %fresh104, i32 0
    %fresh106 = load i64* %fresh105
    switch i64 %fresh106, label %fresh138 [ i64 8, label %fresh107
                                            i64 10, label %fresh111
                                            i64 11, label %fresh126 ]
    fresh107:
    ; Fetch
    ;  (inlined)Variable_Unembedded x_81 = Value_Var (Variable_Subs (Variable_Unembedded x_20_n__510) 1)
    ; Unit
    %fresh108 = inttoptr i64 %n__510 to i64*
    %fresh109 = getelementptr i64* %fresh108, i32 1
    %fresh110 = load i64* %fresh109
    store i64 %fresh110, i64* %y_17_4_21
    br label %fresh139
    fresh111:
    ; Fetch
    ;  (inlined)Variable_Unembedded x_82 = Value_Var (Variable_Subs (Variable_Unembedded x_20_n__510) 1)
    ; Call (Normal or Tail)
    %fresh112 = inttoptr i64 %n__510 to i64*
    %fresh113 = getelementptr i64* %fresh112, i32 1
    %fresh114 = load i64* %fresh113
    call void @fun_primSubInttildespec1( i64 %fresh114 )
    ; Result
    ;  (inlined)Variable_Unembedded x_73 = Value_Var (Variable_Subs Variable_RP 0)
    ;  (inlined)Variable_Unembedded x_74 = Value_Var (Variable_Subs Variable_RP 1)
    ; UpdateUnit
    %fresh116 = load i64** @RP
    %fresh117 = getelementptr i64* %fresh116, i32 0
    %fresh118 = load i64* %fresh117
    %fresh119 = inttoptr i64 %n__510 to i64*
    %fresh120 = getelementptr i64* %fresh119, i32 0
    store i64 %fresh118, i64* %fresh120
    %fresh121 = load i64** @RP
    %fresh122 = getelementptr i64* %fresh121, i32 1
    %fresh123 = load i64* %fresh122
    store i64 %fresh123, i64* %y_17_4_21
    %fresh124 = inttoptr i64 %n__510 to i64*
    %fresh125 = getelementptr i64* %fresh124, i32 1
    store i64 %fresh123, i64* %fresh125
    br label %fresh139
    fresh126:
    ; Call (Normal or Tail)
    call void @fun_primSubInttildespec1tildespec1(  )
    ; Result
    ;  (inlined)Variable_Unembedded x_75 = Value_Var (Variable_Subs Variable_RP 0)
    ;  (inlined)Variable_Unembedded x_76 = Value_Var (Variable_Subs Variable_RP 1)
    ; UpdateUnit
    %fresh128 = load i64** @RP
    %fresh129 = getelementptr i64* %fresh128, i32 0
    %fresh130 = load i64* %fresh129
    %fresh131 = inttoptr i64 %n__510 to i64*
    %fresh132 = getelementptr i64* %fresh131, i32 0
    store i64 %fresh130, i64* %fresh132
    %fresh133 = load i64** @RP
    %fresh134 = getelementptr i64* %fresh133, i32 1
    %fresh135 = load i64* %fresh134
    store i64 %fresh135, i64* %y_17_4_21
    %fresh136 = inttoptr i64 %n__510 to i64*
    %fresh137 = getelementptr i64* %fresh136, i32 1
    store i64 %fresh135, i64* %fresh137
    br label %fresh139
    fresh138:
    call void @primPatternMatchFailure(  )
    unreachable
    br label %fresh139
    fresh139:
    ; Call (Normal or Tail)
    %fresh140 = load i64* %y11_0_937
    call void @fun_fac( i64 %fresh140 )
    ; Result
    ;  (inlined)Variable_Unembedded x_26__17_4_31 = Value_Var (Variable_Subs Variable_RP 1)
    ; FFI
    %fresh142 = load i64* %y_17_4_21
    %fresh143 = load i64** @RP
    %fresh144 = getelementptr i64* %fresh143, i32 1
    %fresh145 = load i64* %fresh144
    %fresh146 = call i64 @primMulInt( i64 %fresh142, i64 %fresh145 )
    store i64 %fresh146, i64* %y17_4_01
    ; Unit (Yielding)
    %fresh149 = load i64** @RP
    %fresh150 = getelementptr i64* %fresh149, i32 0
    store i64 8, i64* %fresh150
    %fresh151 = load i64* %y17_4_01
    %fresh152 = load i64** @RP
    %fresh153 = getelementptr i64* %fresh152, i32 1
    store i64 %fresh151, i64* %fresh153
    br label %fresh160
    fresh154:
    ; Unit (Yielding)
    %fresh155 = load i64** @RP
    %fresh156 = getelementptr i64* %fresh155, i32 0
    store i64 8, i64* %fresh156
    %fresh157 = load i64** @RP
    %fresh158 = getelementptr i64* %fresh157, i32 1
    store i64 1, i64* %fresh158
    br label %fresh160
    fresh159:
    call void @primPatternMatchFailure(  )
    unreachable
    br label %fresh160
    fresh160:
    ret void
}

define internal void @fun_fun0tildemain(  ) nounwind
{
    %y17_12_00 = alloca i64, i32 1
    %y11_0_937 = alloca i64, i32 1
    %y17_4_01 = alloca i64, i32 1
    ; Unit
    ;  (inlined)Variable_Unembedded x_30__17_12_20 = Value_Con (Constant_LiteralInt 10)
    ; Unit
    ;  (inlined)Variable_Unembedded x_31__17_12_30 = Value_Con (Constant_LiteralInt 1)
    ; FFI
    %fresh161 = call i64 @primEqInt( i64 10, i64 1 )
    store i64 %fresh161, i64* %y17_12_00
    ; Case
    %fresh164 = load i64* %y17_12_00
    switch i64 %fresh164, label %fresh189 [ i64 1, label %fresh165
                                            i64 2, label %fresh184 ]
    fresh165:
    ; Store
    %fresh166 = call i64* @llvmgc_malloc( i64 16 )
    %fresh167 = ptrtoint i64* %fresh166 to i64
    store i64 %fresh167, i64* %y11_0_937
    %fresh168 = load i64* %y11_0_937
    %fresh169 = inttoptr i64 %fresh168 to i64*
    %fresh170 = getelementptr i64* %fresh169, i32 0
    store i64 11, i64* %fresh170
    ; Unit
    ;  (inlined)Variable_Unembedded x_34__17_4_21 = Value_Con (Constant_LiteralInt 10)
    ; Call (Normal or Tail)
    %fresh171 = load i64* %y11_0_937
    call void @fun_fac( i64 %fresh171 )
    ; Result
    ;  (inlined)Variable_Unembedded x_35__17_4_31 = Value_Var (Variable_Subs Variable_RP 1)
    ; FFI
    %fresh173 = load i64** @RP
    %fresh174 = getelementptr i64* %fresh173, i32 1
    %fresh175 = load i64* %fresh174
    %fresh176 = call i64 @primMulInt( i64 10, i64 %fresh175 )
    store i64 %fresh176, i64* %y17_4_01
    ; Unit (Yielding)
    %fresh179 = load i64** @RP
    %fresh180 = getelementptr i64* %fresh179, i32 0
    store i64 8, i64* %fresh180
    %fresh181 = load i64* %y17_4_01
    %fresh182 = load i64** @RP
    %fresh183 = getelementptr i64* %fresh182, i32 1
    store i64 %fresh181, i64* %fresh183
    br label %fresh190
    fresh184:
    ; Unit (Yielding)
    %fresh185 = load i64** @RP
    %fresh186 = getelementptr i64* %fresh185, i32 0
    store i64 8, i64* %fresh186
    %fresh187 = load i64** @RP
    %fresh188 = getelementptr i64* %fresh187, i32 1
    store i64 1, i64* %fresh188
    br label %fresh190
    fresh189:
    call void @primPatternMatchFailure(  )
    unreachable
    br label %fresh190
    fresh190:
    ret void
}

