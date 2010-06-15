	.file	"Hello_opt.bc"


	.text
	.align	16
	.globl	main
	.type	main,@function
main:                                                       # @main
.LBB1_0:
	pushq	%r15
	pushq	%r14
	pushq	%rbx
	call	llvmgc_init
	movl	$24, %ebx
	movl	$24, %edi
	call	llvmgc_malloc_uncollectable
	movq	%rax, RP(%rip)
	movl	$16, %r14d
	movl	$16, %edi
	call	llvmgc_malloc_uncollectable
	movq	%rax, global_UHC_Base_undefined(%rip)
	movl	$24, %edi
	call	llvmgc_malloc_uncollectable
	movq	%rax, global_UHC_Base_y103_0_459(%rip)
	movl	$16, %edi
	call	llvmgc_malloc_uncollectable
	movq	%rax, global_UHC_OldIO_primStdout(%rip)
	movl	$24, %edi
	call	llvmgc_malloc_uncollectable
	movq	%rax, global_Main_y13_0_2(%rip)
	movl	$24, %edi
	call	llvmgc_malloc_uncollectable
	movq	%rax, global_Main_main(%rip)
	movq	%rbx, %rdi
	call	llvmgc_malloc_uncollectable
	movq	%rax, global_main(%rip)
	movl	$8, %edi
	call	llvmgc_malloc_uncollectable
	movq	%rax, global_UHC_OldIO_y110_234_1(%rip)
	movq	%r14, %rdi
	call	llvmgc_malloc_uncollectable
	movq	%rax, global_UHC_OldIO_y110_242_1(%rip)
	movq	global_UHC_Base_undefined(%rip), %rax
	movq	$35, (%rax)
	movq	global_UHC_Base_y103_0_459(%rip), %rax
	movq	$39, (%rax)
	movq	global_UHC_OldIO_primStdout(%rip), %rax
	movq	$44, (%rax)
	movq	global_Main_y13_0_2(%rip), %rax
	movq	$38, (%rax)
	movq	global_Main_main(%rip), %rax
	movq	$48, (%rax)
	movq	global_main(%rip), %rax
	movq	$49, (%rax)
	movq	global_UHC_OldIO_y110_234_1(%rip), %rax
	movq	$0, (%rax)
	movq	global_UHC_OldIO_y110_242_1(%rip), %rax
	movq	$17, (%rax)
	movq	global_UHC_OldIO_y110_242_1(%rip), %rax
	movq	$10, 8(%rax)
	movl	$8, %edi
	call	llvmgc_malloc
	movq	$27, (%rax)
	movq	global_main(%rip), %rcx
	movq	(%rcx), %rdx
	cmpq	$49, %rdx
	movq	%rax, %rbx
	je	.LBB1_3
.LBB1_1:
	cmpq	$23, %rdx
	jne	.LBB1_9
.LBB1_2:                                                    # %fresh35.i
	movq	16(%rcx), %rsi
	movq	8(%rcx), %rdi
	jmp	.LBB1_8
.LBB1_3:                                                    # %fresh42.i
	movq	global_Main_main(%rip), %rax
	movq	(%rax), %rdx
	cmpq	$48, %rdx
	je	.LBB1_6
.LBB1_4:                                                    # %fresh42.i
	cmpq	$23, %rdx
	jne	.LBB1_9
.LBB1_5:                                                    # %fresh46.i
	movq	16(%rax), %rsi
	movq	8(%rax), %rdi
	movl	$23, %eax
	jmp	.LBB1_7
.LBB1_6:                                                    # %fresh53.i
	movl	$16, %edi
	call	llvmgc_malloc
	movq	%rax, %r14
	movq	$45, (%r14)
	movl	$24, %edi
	call	llvmgc_malloc
	movq	%rax, %r15
	movq	$52, (%r15)
	movq	%r14, 8(%r15)
	movq	global_UHC_OldIO_y110_242_1(%rip), %rax
	movq	%rax, 16(%r15)
	movl	$24, %edi
	call	llvmgc_malloc
	movq	%rax, %r14
	movq	$47, (%r14)
	movl	$16, %edi
	call	llvmgc_malloc
	movq	$22, (%rax)
	movq	%r15, 8(%rax)
	movq	RP(%rip), %rcx
	movq	$23, (%rcx)
	movq	RP(%rip), %rcx
	movq	%r14, 8(%rcx)
	movq	RP(%rip), %rcx
	movq	%rax, 16(%rcx)
	movq	RP(%rip), %rax
	movq	(%rax), %rax
	movq	global_Main_main(%rip), %rcx
	movq	%rax, (%rcx)
	movq	RP(%rip), %rcx
	movq	8(%rcx), %rdi
	movq	global_Main_main(%rip), %rcx
	movq	%rdi, 8(%rcx)
	movq	RP(%rip), %rcx
	movq	16(%rcx), %rsi
	movq	global_Main_main(%rip), %rcx
	movq	%rsi, 16(%rcx)
	movq	global_main(%rip), %rcx
.LBB1_7:                                                    # %fresh97.i
	movq	%rax, (%rcx)
	movq	global_main(%rip), %rax
	movq	%rdi, 8(%rax)
	movq	global_main(%rip), %rax
	movq	%rsi, 16(%rax)
.LBB1_8:                                                    # %fun_mainFullProg.exit
	movq	%rbx, %rdx
	call	fun_UHC_Base_primbindIO
	call	llvmc_print_statistics
	xorl	%eax, %eax
	popq	%rbx
	popq	%r14
	popq	%r15
	ret
.LBB1_9:                                                    # %fresh96.i
	call	primPatternMatchFailure
	.size	main, .-main


	.align	16
	.type	fun_app2_1,@function
fun_app2_1:                                                 # @fun_app2_1
.LBB2_0:
	pushq	%r15
	pushq	%r14
	pushq	%rbx
	movq	8(%rdi), %rbx
	movq	(%rbx), %rax
	cmpq	$44, %rax
	movq	16(%rdi), %r14
	je	.LBB2_3
.LBB2_1:
	cmpq	$26, %rax
	jne	.LBB2_39
.LBB2_2:                                                    # %fresh123
	movq	8(%rbx), %rax
	jmp	.LBB2_4
.LBB2_3:                                                    # %fresh128
	call	primStdout
	movq	$26, (%rbx)
	movq	%rax, 8(%rbx)
.LBB2_4:                                                    # %fresh138
	movq	%rax, %rbx
	movq	(%r14), %rax
	cmpq	$41, %rax
	je	.LBB2_25
.LBB2_5:                                                    # %fresh138
	cmpq	$40, %rax
	je	.LBB2_8
.LBB2_6:                                                    # %fresh138
	cmpq	$17, %rax
	jne	.LBB2_39
.LBB2_7:                                                    # %fresh143
	movq	8(%r14), %rax
	jmp	.LBB2_38
.LBB2_8:                                                    # %fresh148
	movq	8(%r14), %r15
	movq	(%r15), %rax
	cmpq	$27, %rax
	jle	.LBB2_14
.LBB2_9:                                                    # %fresh159
	cmpq	$43, %rax
	je	.LBB2_12
.LBB2_10:                                                   # %fresh159
	cmpq	$42, %rax
	jne	.LBB2_39
.LBB2_11:                                                   # %fresh161
	movq	8(%r15), %rdi
	call	fun_UHC_Base_tail
	jmp	.LBB2_13
.LBB2_12:                                                   # %fresh185
	call	fun_UHC_Base_tailtildespec1
.LBB2_13:                                                   # %fresh185
	movq	RP(%rip), %rax
	movq	(%rax), %rax
	movq	%rax, (%r15)
	movq	RP(%rip), %rcx
	movq	8(%rcx), %rcx
	movq	%rcx, 8(%r15)
	movq	RP(%rip), %rcx
	movq	16(%rcx), %rcx
	movq	%rcx, 16(%r15)
.LBB2_14:                                                   # %fresh208
	cmpq	$4, %rax
	je	.LBB2_19
.LBB2_15:                                                   # %fresh208
	cmpq	$3, %rax
	jne	.LBB2_39
.LBB2_16:                                                   # %fresh210
	movq	8(%r15), %r15
	movq	(%r15), %rax
	cmpq	$29, %rax
	je	.LBB2_23
.LBB2_17:                                                   # %fresh210
	cmpq	$17, %rax
	jne	.LBB2_39
.LBB2_18:                                                   # %fresh219
	movq	8(%r15), %rax
	movl	$17, %ecx
	jmp	.LBB2_24
.LBB2_19:                                                   # %fresh244
	movq	global_UHC_Base_undefined(%rip), %rcx
	movq	(%rcx), %rax
	cmpq	$35, %rax
	je	.LBB2_22
.LBB2_20:                                                   # %fresh244
	cmpq	$26, %rax
	jne	.LBB2_39
.LBB2_21:                                                   # %fresh248
	movq	8(%rcx), %rax
	movl	$26, %ecx
	jmp	.LBB2_24
.LBB2_22:                                                   # %fresh252
	call	fun_UHC_Base_errortildespec1
	movq	RP(%rip), %rcx
	movq	(%rcx), %rcx
	movq	global_UHC_Base_undefined(%rip), %rax
	movq	%rcx, (%rax)
	movq	RP(%rip), %rax
	movq	8(%rax), %rax
	movq	global_UHC_Base_undefined(%rip), %rdx
	movq	%rax, 8(%rdx)
	jmp	.LBB2_24
.LBB2_23:                                                   # %fresh224
	movq	8(%r15), %rdi
	call	fun_UHC_Base_packedStringHead
	movq	RP(%rip), %rcx
	movq	(%rcx), %rcx
	movq	%rcx, (%r15)
	movq	RP(%rip), %rax
	movq	8(%rax), %rax
	movq	%rax, 8(%r15)
.LBB2_24:                                                   # %fresh267
	movq	%rcx, (%r14)
	movq	%rax, 8(%r14)
	jmp	.LBB2_38
.LBB2_25:                                                   # %fresh276
	movq	global_Main_y13_0_2(%rip), %rax
	movq	(%rax), %rax
	cmpq	$27, %rax
	jle	.LBB2_27
.LBB2_26:                                                   # %fresh282
	call	fun_UHC_Base_packedStringToStringtildespec1
	movq	RP(%rip), %rax
	movq	(%rax), %rax
	movq	global_Main_y13_0_2(%rip), %rcx
	movq	%rax, (%rcx)
	movq	RP(%rip), %rcx
	movq	8(%rcx), %rcx
	movq	global_Main_y13_0_2(%rip), %rdx
	movq	%rcx, 8(%rdx)
	movq	RP(%rip), %rcx
	movq	16(%rcx), %rcx
	movq	global_Main_y13_0_2(%rip), %rdx
	movq	%rcx, 16(%rdx)
.LBB2_27:                                                   # %fresh300
	cmpq	$4, %rax
	je	.LBB2_32
.LBB2_28:                                                   # %fresh300
	cmpq	$3, %rax
	jne	.LBB2_39
.LBB2_29:                                                   # %fresh302
	movq	global_Main_y13_0_2(%rip), %rax
	movq	8(%rax), %r15
	movq	(%r15), %rax
	cmpq	$30, %rax
	je	.LBB2_36
.LBB2_30:                                                   # %fresh302
	cmpq	$17, %rax
	jne	.LBB2_39
.LBB2_31:                                                   # %fresh310
	movq	8(%r15), %rax
	movl	$17, %r15d
	jmp	.LBB2_37
.LBB2_32:                                                   # %fresh328
	movq	global_UHC_Base_undefined(%rip), %r15
	movq	(%r15), %rax
	cmpq	$35, %rax
	je	.LBB2_35
.LBB2_33:                                                   # %fresh328
	cmpq	$26, %rax
	jne	.LBB2_39
.LBB2_34:                                                   # %fresh332
	movq	8(%r15), %rax
	movl	$26, %r15d
	jmp	.LBB2_37
.LBB2_35:                                                   # %fresh336
	call	fun_UHC_Base_errortildespec1
	movq	RP(%rip), %r15
	movq	(%r15), %r15
	movq	global_UHC_Base_undefined(%rip), %rax
	movq	%r15, (%rax)
	movq	RP(%rip), %rax
	movq	8(%rax), %rax
	movq	global_UHC_Base_undefined(%rip), %rcx
	movq	%rax, 8(%rcx)
	jmp	.LBB2_37
.LBB2_36:                                                   # %fresh315
	movl	$fresh316, %edi
	call	primPackedStringHead
	movq	$17, (%r15)
	movq	%rax, 8(%r15)
	movl	$17, %r15d
.LBB2_37:                                                   # %fresh351
	movq	%r15, (%r14)
	movq	%rax, 8(%r14)
.LBB2_38:                                                   # %fresh361
	movq	%rbx, %rdi
	movq	%rax, %rsi
	call	primHPutChar
	movq	RP(%rip), %rcx
	movq	%rax, (%rcx)
	popq	%rbx
	popq	%r14
	popq	%r15
	ret
.LBB2_39:                                                   # %fresh137
	call	primPatternMatchFailure
	.size	fun_app2_1, .-fun_app2_1


	.align	16
	.type	fun_UHC_Base_primAddInt,@function
fun_UHC_Base_primAddInt:                                    # @fun_UHC_Base_primAddInt
.LBB3_0:
	pushq	%r15
	pushq	%r14
	pushq	%r12
	pushq	%rbx
	subq	$8, %rsp
	movq	(%rdi), %rax
	cmpq	$28, %rax
	movq	%rsi, %rbx
	movq	%rdi, %r14
	je	.LBB3_3
.LBB3_1:
	cmpq	$18, %rax
	jne	.LBB3_21
.LBB3_2:                                                    # %fresh373
	movq	8(%r14), %rax
	jmp	.LBB3_10
.LBB3_3:                                                    # %fresh377
	movq	8(%r14), %r15
	movq	(%r15), %rax
	cmpq	$31, %rax
	je	.LBB3_7
.LBB3_4:                                                    # %fresh377
	cmpq	$29, %rax
	je	.LBB3_8
.LBB3_5:                                                    # %fresh377
	cmpq	$17, %rax
	jne	.LBB3_21
.LBB3_6:                                                    # %fresh385
	movq	8(%r15), %rax
	jmp	.LBB3_9
.LBB3_7:                                                    # %fresh408
	movl	$fresh409, %edi
	call	primPackedStringHead
	movq	$17, (%r15)
	movq	%rax, 8(%r15)
	jmp	.LBB3_9
.LBB3_8:                                                    # %fresh390
	movq	8(%r15), %rdi
	call	fun_UHC_Base_packedStringHead
	movq	RP(%rip), %rax
	movq	(%rax), %rax
	movq	%rax, (%r15)
	movq	RP(%rip), %rax
	movq	8(%rax), %rax
	movq	%rax, 8(%r15)
.LBB3_9:                                                    # %fresh420
	movq	%rax, %rdi
	call	primUnsafeId
	movq	$18, (%r14)
	movq	%rax, 8(%r14)
.LBB3_10:                                                   # %fresh430
	movq	%rax, %r14
	movq	(%rbx), %rax
	cmpq	$36, %rax
	je	.LBB3_13
.LBB3_11:                                                   # %fresh430
	cmpq	$18, %rax
	jne	.LBB3_21
.LBB3_12:                                                   # %fresh434
	movq	8(%rbx), %rsi
	jmp	.LBB3_20
.LBB3_13:                                                   # %fresh438
	movq	8(%rbx), %r15
	movq	(%r15), %rax
	cmpq	$27, %rax
	jle	.LBB3_15
.LBB3_14:                                                   # %fresh448
	movq	8(%r15), %rdi
	call	fun_UHC_Base_packedStringToString
	movq	RP(%rip), %rax
	movq	(%rax), %rax
	movq	%rax, (%r15)
	movq	RP(%rip), %rcx
	movq	8(%rcx), %rcx
	movq	%rcx, 8(%r15)
	movq	RP(%rip), %rcx
	movq	16(%rcx), %rcx
	movq	%rcx, 16(%r15)
.LBB3_15:                                                   # %fresh473
	cmpq	$3, %rax
	je	.LBB3_18
.LBB3_16:                                                   # %fresh473
	cmpq	$4, %rax
	jne	.LBB3_21
.LBB3_17:                                                   # %fresh473.fresh511_crit_edge
	xorl	%esi, %esi
	movl	$18, %r15d
	jmp	.LBB3_19
.LBB3_18:                                                   # %fresh475
	movl	$16, %edi
	call	llvmgc_malloc
	movq	%rax, %r12
	movq	$36, (%r12)
	movq	16(%r15), %rax
	movq	%rax, 8(%r12)
	movl	$16, %edi
	call	llvmgc_malloc
	movq	$28, (%rax)
	movq	8(%r15), %r15
	movq	%r15, 8(%rax)
	movq	%rax, %rdi
	movq	%r12, %rsi
	call	fun_UHC_Base_primAddInt
	movq	RP(%rip), %r15
	movq	8(%r15), %rsi
	movq	(%r15), %r15
.LBB3_19:                                                   # %fresh511
	movq	%r15, (%rbx)
	movq	%rsi, 8(%rbx)
.LBB3_20:                                                   # %fresh519
	movq	%r14, %rdi
	call	primAddInt
	movq	RP(%rip), %rcx
	movq	$18, (%rcx)
	movq	RP(%rip), %rcx
	movq	%rax, 8(%rcx)
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	ret
.LBB3_21:                                                   # %fresh419
	call	primPatternMatchFailure
	.size	fun_UHC_Base_primAddInt, .-fun_UHC_Base_primAddInt


	.align	16
	.type	fun_UHC_Base_packedStringHead,@function
fun_UHC_Base_packedStringHead:                              # @fun_UHC_Base_packedStringHead
.LBB4_0:
	pushq	%rbx
	movq	(%rdi), %rax
	cmpq	$31, %rax
	movq	%rdi, %rbx
	jle	.LBB4_10
.LBB4_1:
	cmpq	$32, %rax
	je	.LBB4_5
.LBB4_2:
	cmpq	$33, %rax
	je	.LBB4_7
.LBB4_3:
	cmpq	$34, %rax
	jne	.LBB4_9
.LBB4_4:                                                    # %fresh564
	movl	$fresh409, %edi
	jmp	.LBB4_8
.LBB4_5:                                                    # %fresh537
	movq	8(%rbx), %rdi
	call	fun_UHC_Base_packedStringTail
.LBB4_6:                                                    # %fresh537
	movq	RP(%rip), %rax
	movq	(%rax), %rax
	movq	%rax, (%rbx)
	movq	RP(%rip), %rax
	movq	8(%rax), %rdi
	movq	%rdi, 8(%rbx)
	jmp	.LBB4_12
.LBB4_7:                                                    # %fresh552
	movl	$fresh316, %edi
.LBB4_8:                                                    # %fresh552
	call	primPackedStringTail
	movq	RP(%rip), %rcx
	movq	$19, (%rcx)
	movq	RP(%rip), %rcx
	movq	%rax, 8(%rcx)
	jmp	.LBB4_6
.LBB4_9:                                                    # %fresh576
	call	primPatternMatchFailure
.LBB4_10:
	cmpq	$19, %rax
	jne	.LBB4_9
.LBB4_11:                                                   # %fresh533
	movq	8(%rbx), %rdi
.LBB4_12:                                                   # %fresh577
	call	primPackedStringHead
	movq	RP(%rip), %rcx
	movq	$17, (%rcx)
	movq	RP(%rip), %rcx
	movq	%rax, 8(%rcx)
	popq	%rbx
	ret
	.size	fun_UHC_Base_packedStringHead, .-fun_UHC_Base_packedStringHead


	.align	16
	.type	fun_UHC_Base_packedStringTail,@function
fun_UHC_Base_packedStringTail:                              # @fun_UHC_Base_packedStringTail
.LBB5_0:
	pushq	%rbx
	movq	(%rdi), %rax
	cmpq	$31, %rax
	movq	%rdi, %rbx
	jle	.LBB5_10
.LBB5_1:
	cmpq	$32, %rax
	je	.LBB5_5
.LBB5_2:
	cmpq	$33, %rax
	je	.LBB5_7
.LBB5_3:
	cmpq	$34, %rax
	jne	.LBB5_9
.LBB5_4:                                                    # %fresh620
	movl	$fresh409, %edi
	jmp	.LBB5_8
.LBB5_5:                                                    # %fresh593
	movq	8(%rbx), %rdi
	call	fun_UHC_Base_packedStringTail
.LBB5_6:                                                    # %fresh593
	movq	RP(%rip), %rax
	movq	(%rax), %rax
	movq	%rax, (%rbx)
	movq	RP(%rip), %rax
	movq	8(%rax), %rdi
	movq	%rdi, 8(%rbx)
	jmp	.LBB5_12
.LBB5_7:                                                    # %fresh608
	movl	$fresh316, %edi
.LBB5_8:                                                    # %fresh608
	call	primPackedStringTail
	movq	RP(%rip), %rcx
	movq	$19, (%rcx)
	movq	RP(%rip), %rcx
	movq	%rax, 8(%rcx)
	jmp	.LBB5_6
.LBB5_9:                                                    # %fresh632
	call	primPatternMatchFailure
.LBB5_10:
	cmpq	$19, %rax
	jne	.LBB5_9
.LBB5_11:                                                   # %fresh589
	movq	8(%rbx), %rdi
.LBB5_12:                                                   # %fresh633
	call	primPackedStringTail
	movq	RP(%rip), %rcx
	movq	$19, (%rcx)
	movq	RP(%rip), %rcx
	movq	%rax, 8(%rcx)
	popq	%rbx
	ret
	.size	fun_UHC_Base_packedStringTail, .-fun_UHC_Base_packedStringTail


	.align	16
	.type	fun_UHC_Base_errortildespec1,@function
fun_UHC_Base_errortildespec1:                               # @fun_UHC_Base_errortildespec1
.LBB6_0:
	pushq	%rbx
	movq	global_UHC_Base_y103_0_459(%rip), %rax
	movq	(%rax), %rax
	cmpq	$27, %rax
	jle	.LBB6_2
.LBB6_1:                                                    # %fresh710
	call	fun_UHC_Base_packedStringToStringtildespec14
	movq	RP(%rip), %rax
	movq	(%rax), %rax
	movq	global_UHC_Base_y103_0_459(%rip), %rcx
	movq	%rax, (%rcx)
	movq	RP(%rip), %rcx
	movq	8(%rcx), %rcx
	movq	global_UHC_Base_y103_0_459(%rip), %rdx
	movq	%rcx, 8(%rdx)
	movq	RP(%rip), %rcx
	movq	16(%rcx), %rcx
	movq	global_UHC_Base_y103_0_459(%rip), %rdx
	movq	%rcx, 16(%rdx)
.LBB6_2:                                                    # %fresh728
	cmpq	$4, %rax
	je	.LBB6_5
.LBB6_3:                                                    # %fresh728
	cmpq	$3, %rax
	jne	.LBB6_10
.LBB6_4:                                                    # %fresh730
	movl	$16, %edi
	call	llvmgc_malloc
	movq	%rax, %rbx
	movq	$36, (%rbx)
	movq	global_UHC_Base_y103_0_459(%rip), %rax
	movq	16(%rax), %rax
	movq	%rax, 8(%rbx)
	movl	$16, %edi
	call	llvmgc_malloc
	movq	$28, (%rax)
	movq	global_UHC_Base_y103_0_459(%rip), %rcx
	movq	8(%rcx), %rcx
	movq	%rcx, 8(%rax)
	movq	%rax, %rdi
	movq	%rbx, %rsi
	call	fun_UHC_Base_primAddInt
.LBB6_5:                                                    # %fresh758
	movq	global_UHC_Base_y103_0_459(%rip), %rax
	movq	(%rax), %rax
	cmpq	$39, %rax
	je	.LBB6_9
.LBB6_6:                                                    # %fresh758
	cmpq	$4, %rax
	je	.LBB6_11
.LBB6_7:                                                    # %fresh758
	cmpq	$3, %rax
	jne	.LBB6_10
.LBB6_8:                                                    # %fresh762
	movl	$24, %edi
	call	llvmgc_malloc
	movq	$3, (%rax)
	movq	global_UHC_Base_y103_0_459(%rip), %rcx
	movq	8(%rcx), %rcx
	movq	%rcx, 8(%rax)
	movq	global_UHC_Base_y103_0_459(%rip), %rcx
	movq	16(%rcx), %rcx
	movq	%rcx, 16(%rax)
	jmp	.LBB6_12
.LBB6_9:                                                    # %fresh786
	call	fun_UHC_Base_packedStringToStringtildespec14
	movl	$24, %edi
	call	llvmgc_malloc
	movq	RP(%rip), %rcx
	movq	(%rcx), %rcx
	movq	global_UHC_Base_y103_0_459(%rip), %rdx
	movq	%rcx, (%rdx)
	movq	%rcx, (%rax)
	movq	RP(%rip), %rcx
	movq	8(%rcx), %rcx
	movq	global_UHC_Base_y103_0_459(%rip), %rdx
	movq	%rcx, 8(%rdx)
	movq	%rcx, 8(%rax)
	movq	RP(%rip), %rcx
	movq	16(%rcx), %rcx
	movq	global_UHC_Base_y103_0_459(%rip), %rdx
	movq	%rcx, 16(%rdx)
	movq	%rcx, 16(%rax)
	jmp	.LBB6_12
.LBB6_10:                                                   # %fresh757
	call	primPatternMatchFailure
.LBB6_11:                                                   # %fresh780
	movl	$8, %edi
	call	llvmgc_malloc
	movq	$4, (%rax)
.LBB6_12:                                                   # %fresh815
	movq	%rax, %rdi
	call	primError
	movq	RP(%rip), %rcx
	movq	$26, (%rcx)
	movq	RP(%rip), %rcx
	movq	%rax, 8(%rcx)
	popq	%rbx
	ret
	.size	fun_UHC_Base_errortildespec1, .-fun_UHC_Base_errortildespec1


	.align	16
	.type	fun_UHC_Base_packedStringToString,@function
fun_UHC_Base_packedStringToString:                          # @fun_UHC_Base_packedStringToString
.LBB7_0:
	pushq	%r15
	pushq	%r14
	pushq	%rbx
	movq	(%rdi), %rax
	cmpq	$31, %rax
	movq	%rdi, %rbx
	jle	.LBB7_12
.LBB7_1:
	cmpq	$32, %rax
	je	.LBB7_5
.LBB7_2:
	cmpq	$33, %rax
	je	.LBB7_7
.LBB7_3:
	cmpq	$34, %rax
	jne	.LBB7_11
.LBB7_4:                                                    # %fresh858
	movl	$fresh409, %edi
	jmp	.LBB7_8
.LBB7_5:                                                    # %fresh831
	movq	8(%rbx), %rdi
	call	fun_UHC_Base_packedStringTail
.LBB7_6:                                                    # %fresh831
	movq	RP(%rip), %rax
	movq	(%rax), %rax
	movq	%rax, (%rbx)
	movq	RP(%rip), %rax
	movq	8(%rax), %rdi
	movq	%rdi, 8(%rbx)
	jmp	.LBB7_14
.LBB7_7:                                                    # %fresh846
	movl	$fresh316, %edi
.LBB7_8:                                                    # %fresh846
	call	primPackedStringTail
	movq	RP(%rip), %rcx
	movq	$19, (%rcx)
	movq	RP(%rip), %rcx
	movq	%rax, 8(%rcx)
	jmp	.LBB7_6
.LBB7_9:                                                    # %fresh910
	movq	RP(%rip), %rax
	movq	$4, (%rax)
.LBB7_10:                                                   # %fresh910
	popq	%rbx
	popq	%r14
	popq	%r15
	ret
.LBB7_11:                                                   # %fresh870
	call	primPatternMatchFailure
.LBB7_12:
	cmpq	$19, %rax
	jne	.LBB7_11
.LBB7_13:                                                   # %fresh827
	movq	8(%rbx), %rdi
.LBB7_14:                                                   # %fresh871
	call	primPackedStringNull
	cmpq	$2, %rax
	je	.LBB7_9
.LBB7_15:                                                   # %fresh871
	cmpq	$1, %rax
	jne	.LBB7_11
.LBB7_16:                                                   # %fresh876
	movl	$16, %edi
	call	llvmgc_malloc
	movq	%rax, %r14
	movq	$32, (%r14)
	movq	%rbx, 8(%r14)
	movl	$24, %edi
	call	llvmgc_malloc
	movq	%rax, %r15
	movq	$37, (%r15)
	movq	%r14, 8(%r15)
	movl	$16, %edi
	call	llvmgc_malloc
	movq	$29, (%rax)
	movq	%rbx, 8(%rax)
	movq	RP(%rip), %rcx
	movq	$3, (%rcx)
	movq	RP(%rip), %rcx
	movq	%rax, 8(%rcx)
	movq	RP(%rip), %rax
	movq	%r15, 16(%rax)
	jmp	.LBB7_10
	.size	fun_UHC_Base_packedStringToString, .-fun_UHC_Base_packedStringToString


	.align	16
	.type	fun_UHC_Base_packedStringToStringtildespec1,@function
fun_UHC_Base_packedStringToStringtildespec1:                # @fun_UHC_Base_packedStringToStringtildespec1
.LBB8_0:
	pushq	%r14
	pushq	%rbx
	subq	$8, %rsp
	movl	$fresh316, %edi
	call	primPackedStringNull
	cmpq	$2, %rax
	je	.LBB8_3
.LBB8_1:
	cmpq	$1, %rax
	jne	.LBB8_5
.LBB8_2:                                                    # %fresh919
	movl	$16, %edi
	call	llvmgc_malloc
	movq	%rax, %rbx
	movq	$33, (%rbx)
	movl	$24, %edi
	call	llvmgc_malloc
	movq	%rax, %r14
	movq	$37, (%r14)
	movq	%rbx, 8(%r14)
	movl	$16, %edi
	call	llvmgc_malloc
	movq	$30, (%rax)
	movq	RP(%rip), %rcx
	movq	$3, (%rcx)
	movq	RP(%rip), %rcx
	movq	%rax, 8(%rcx)
	movq	RP(%rip), %rax
	movq	%r14, 16(%rax)
	jmp	.LBB8_4
.LBB8_3:                                                    # %fresh947
	movq	RP(%rip), %rax
	movq	$4, (%rax)
.LBB8_4:                                                    # %fresh947
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	ret
.LBB8_5:                                                    # %fresh950
	call	primPatternMatchFailure
	.size	fun_UHC_Base_packedStringToStringtildespec1, .-fun_UHC_Base_packedStringToStringtildespec1


	.align	16
	.type	fun_UHC_Base_packedStringToStringtildespec14,@function
fun_UHC_Base_packedStringToStringtildespec14:               # @fun_UHC_Base_packedStringToStringtildespec14
.LBB9_0:
	pushq	%r14
	pushq	%rbx
	subq	$8, %rsp
	movl	$fresh409, %edi
	call	primPackedStringNull
	cmpq	$2, %rax
	je	.LBB9_3
.LBB9_1:
	cmpq	$1, %rax
	jne	.LBB9_5
.LBB9_2:                                                    # %fresh956
	movl	$16, %edi
	call	llvmgc_malloc
	movq	%rax, %rbx
	movq	$34, (%rbx)
	movl	$24, %edi
	call	llvmgc_malloc
	movq	%rax, %r14
	movq	$37, (%r14)
	movq	%rbx, 8(%r14)
	movl	$16, %edi
	call	llvmgc_malloc
	movq	$31, (%rax)
	movq	RP(%rip), %rcx
	movq	$3, (%rcx)
	movq	RP(%rip), %rcx
	movq	%rax, 8(%rcx)
	movq	RP(%rip), %rax
	movq	%r14, 16(%rax)
	jmp	.LBB9_4
.LBB9_3:                                                    # %fresh984
	movq	RP(%rip), %rax
	movq	$4, (%rax)
.LBB9_4:                                                    # %fresh984
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	ret
.LBB9_5:                                                    # %fresh987
	call	primPatternMatchFailure
	.size	fun_UHC_Base_packedStringToStringtildespec14, .-fun_UHC_Base_packedStringToStringtildespec14


	.align	16
	.type	fun_UHC_Base_primbindIO,@function
fun_UHC_Base_primbindIO:                                    # @fun_UHC_Base_primbindIO
.LBB10_0:
	pushq	%rbp
	pushq	%r15
	pushq	%r14
	pushq	%r13
	pushq	%r12
	pushq	%rbx
	subq	$24, %rsp
	movq	%rdi, %rbx
	movq	%rsi, 8(%rsp)
	movq	%rdx, 16(%rsp)
	.align	16
.LBB10_1:                                                   # %tailrecurse
                                                            # Loop Depth 1
                                                            # Loop Header
                                                            # Inner Loop
	movq	(%rbx), %rax
	cmpq	$27, %rax
	jle	.LBB10_18
.LBB10_2:                                                   # %fresh1009
                                                            # Loop Depth 1
                                                            # Loop Header is BB10_1
                                                            # Inner Loop
	cmpq	$51, %rax
	je	.LBB10_12
.LBB10_3:                                                   # %fresh1009
                                                            # Loop Depth 1
                                                            # Loop Header is BB10_1
                                                            # Inner Loop
	cmpq	$47, %rax
	jne	.LBB10_8
.LBB10_4:                                                   # %fresh1011
                                                            # Loop Depth 1
                                                            # Loop Header is BB10_1
                                                            # Inner Loop
	movq	global_Main_y13_0_2(%rip), %rax
	movq	(%rax), %rax
	cmpq	$27, %rax
	jle	.LBB10_6
.LBB10_5:                                                   # %fresh1017
                                                            # Loop Depth 1
                                                            # Loop Header is BB10_1
                                                            # Inner Loop
	call	fun_UHC_Base_packedStringToStringtildespec1
	movq	RP(%rip), %rax
	movq	(%rax), %rax
	movq	global_Main_y13_0_2(%rip), %rcx
	movq	%rax, (%rcx)
	movq	RP(%rip), %rcx
	movq	8(%rcx), %rcx
	movq	global_Main_y13_0_2(%rip), %rdx
	movq	%rcx, 8(%rdx)
	movq	RP(%rip), %rcx
	movq	16(%rcx), %rcx
	movq	global_Main_y13_0_2(%rip), %rdx
	movq	%rcx, 16(%rdx)
.LBB10_6:                                                   # %fresh1035
                                                            # Loop Depth 1
                                                            # Loop Header is BB10_1
                                                            # Inner Loop
	cmpq	$3, %rax
	je	.LBB10_9
.LBB10_7:                                                   # %fresh1035
                                                            # Loop Depth 1
                                                            # Loop Header is BB10_1
                                                            # Inner Loop
	cmpq	$4, %rax
	je	.LBB10_10
.LBB10_8:                                                   # %fresh1039
	call	primPatternMatchFailure
.LBB10_9:                                                   # %fresh1042
                                                            # Loop Depth 1
                                                            # Loop Header is BB10_1
                                                            # Inner Loop
	movl	$24, %r14d
	movq	%r14, %rdi
	call	llvmgc_malloc
	movq	%rax, %r15
	movq	$43, (%r15)
	movq	%r14, %rdi
	call	llvmgc_malloc
	movq	%rax, %r12
	movq	$46, (%r12)
	movq	%r15, 8(%r12)
	movl	$16, %r15d
	movq	%r15, %rdi
	call	llvmgc_malloc
	movq	%rax, %r13
	movq	$41, (%r13)
	movq	%r15, %rdi
	call	llvmgc_malloc
	movq	%rax, %rbp
	movq	$45, (%rbp)
	movq	%r14, %rdi
	call	llvmgc_malloc
	movq	%rax, %r14
	movq	$51, (%r14)
	movq	%rbp, 8(%r14)
	movq	%r13, 16(%r14)
	movq	%r15, %rdi
	call	llvmgc_malloc
	movq	$22, (%rax)
	movq	%r12, 8(%rax)
	movq	RP(%rip), %rcx
	movq	$23, (%rcx)
	movq	RP(%rip), %rcx
	movq	%r14, 8(%rcx)
	movq	RP(%rip), %rcx
	movq	%rax, 16(%rcx)
	movq	RP(%rip), %rax
	movq	8(%rax), %rcx
	movq	(%rax), %rax
	jmp	.LBB10_11
.LBB10_10:                                                  # %fresh1089
                                                            # Loop Depth 1
                                                            # Loop Header is BB10_1
                                                            # Inner Loop
	movl	$20, %eax
	movq	global_UHC_OldIO_y110_234_1(%rip), %rcx
.LBB10_11:                                                  # %fresh1093
                                                            # Loop Depth 1
                                                            # Loop Header is BB10_1
                                                            # Inner Loop
	movq	%rax, (%rbx)
	movq	%rcx, 8(%rbx)
	movq	RP(%rip), %rcx
	movq	16(%rcx), %rcx
	movq	%rcx, 16(%rbx)
	jmp	.LBB10_18
.LBB10_12:                                                  # %fresh1105
                                                            # Loop Depth 1
                                                            # Loop Header is BB10_1
                                                            # Inner Loop
	movq	8(%rbx), %rax
	movq	(%rax), %rcx
	cmpq	$45, %rcx
	je	.LBB10_16
.LBB10_13:                                                  # %fresh1105
                                                            # Loop Depth 1
                                                            # Loop Header is BB10_1
                                                            # Inner Loop
	cmpq	$26, %rcx
	je	.LBB10_15
.LBB10_14:                                                  # %fresh1105
                                                            # Loop Depth 1
                                                            # Loop Header is BB10_1
                                                            # Inner Loop
	cmpq	$24, %rcx
	jne	.LBB10_8
.LBB10_15:                                                  # %fresh1113
                                                            # Loop Depth 1
                                                            # Loop Header is BB10_1
                                                            # Inner Loop
	movq	8(%rax), %r14
	jmp	.LBB10_17
.LBB10_16:                                                  # %fresh1123
                                                            # Loop Depth 1
                                                            # Loop Header is BB10_1
                                                            # Inner Loop
	movq	RP(%rip), %r14
	movq	$24, (%r14)
	movq	RP(%rip), %r14
	movq	global_UHC_OldIO_primStdout(%rip), %rcx
	movq	%rcx, 8(%r14)
	movq	RP(%rip), %r14
	movq	(%r14), %r14
	movq	%r14, (%rax)
	movq	RP(%rip), %r14
	movq	8(%r14), %r14
	movq	%r14, 8(%rax)
.LBB10_17:                                                  # %fresh1138
                                                            # Loop Depth 1
                                                            # Loop Header is BB10_1
                                                            # Inner Loop
	movq	16(%rbx), %r15
	movl	$24, %eax
	movq	%rax, %rdi
	call	llvmgc_malloc
	movq	$25, (%rax)
	movq	%r14, 8(%rax)
	movq	%r15, 16(%rax)
	movq	RP(%rip), %rcx
	movq	$21, (%rcx)
	movq	RP(%rip), %rcx
	movq	%rax, 8(%rcx)
	movq	RP(%rip), %rax
	movq	(%rax), %rax
	movq	%rax, (%rbx)
	movq	RP(%rip), %rcx
	movq	8(%rcx), %rcx
	movq	%rcx, 8(%rbx)
.LBB10_18:                                                  # %fresh1157
                                                            # Loop Depth 1
                                                            # Loop Header is BB10_1
                                                            # Inner Loop
	cmpq	$23, %rax
	je	.LBB10_34
.LBB10_19:                                                  # %fresh1157
                                                            # Loop Depth 1
                                                            # Loop Header is BB10_1
                                                            # Inner Loop
	cmpq	$21, %rax
	je	.LBB10_33
.LBB10_20:                                                  # %fresh1157
                                                            # Loop Depth 1
                                                            # Loop Header is BB10_1
                                                            # Inner Loop
	cmpq	$20, %rax
	jne	.LBB10_8
.LBB10_21:                                                  # %fresh1159
                                                            # Loop Depth 1
                                                            # Loop Header is BB10_1
                                                            # Inner Loop
	movq	8(%rbx), %rbx
	movq	RP(%rip), %rax
	movq	$16, (%rax)
	movq	RP(%rip), %rax
	movq	16(%rsp), %rcx
	movq	%rcx, 8(%rax)
	movq	RP(%rip), %rax
	movq	%rbx, 16(%rax)
.LBB10_22:                                                  # %fresh1159
                                                            # Loop Depth 1
                                                            # Loop Header is BB10_1
                                                            # Inner Loop
	movq	RP(%rip), %rbx
	movq	%rbx, %rax
.LBB10_23:                                                  # %fresh1196
                                                            # Loop Depth 1
                                                            # Loop Header is BB10_1
                                                            # Inner Loop
	movq	16(%rax), %r14
	movq	(%r14), %rax
	testq	%rax, %rax
	movq	8(%rbx), %rbx
	movq	%rbx, 16(%rsp)
	je	.LBB10_26
.LBB10_24:                                                  # %fresh1196
                                                            # Loop Depth 1
                                                            # Loop Header is BB10_1
                                                            # Inner Loop
	cmpq	$50, %rax
	jne	.LBB10_8
.LBB10_25:                                                  # %fresh1202
                                                            # Loop Depth 1
                                                            # Loop Header is BB10_1
                                                            # Inner Loop
	movq	8(%r14), %rdi
	call	fun_app2_1
	movq	RP(%rip), %rbx
	movq	(%rbx), %rbx
	movq	%rbx, (%r14)
.LBB10_26:                                                  # %fresh1219
                                                            # Loop Depth 1
                                                            # Loop Header is BB10_1
                                                            # Inner Loop
	movq	8(%rsp), %rbx
	movq	8(%rbx), %rbx
	movq	(%rbx), %rax
	cmpq	$27, %rax
	jle	.LBB10_47
.LBB10_27:                                                  # %fresh1229
                                                            # Loop Depth 1
                                                            # Loop Header is BB10_1
                                                            # Inner Loop
	cmpq	$52, %rax
	je	.LBB10_41
.LBB10_28:                                                  # %fresh1229
                                                            # Loop Depth 1
                                                            # Loop Header is BB10_1
                                                            # Inner Loop
	cmpq	$46, %rax
	jne	.LBB10_8
.LBB10_29:                                                  # %fresh1231
                                                            # Loop Depth 1
                                                            # Loop Header is BB10_1
                                                            # Inner Loop
	movq	8(%rbx), %r14
	movq	(%r14), %rax
	cmpq	$27, %rax
	jle	.LBB10_37
.LBB10_30:                                                  # %fresh1242
                                                            # Loop Depth 1
                                                            # Loop Header is BB10_1
                                                            # Inner Loop
	cmpq	$43, %rax
	je	.LBB10_35
.LBB10_31:                                                  # %fresh1242
                                                            # Loop Depth 1
                                                            # Loop Header is BB10_1
                                                            # Inner Loop
	cmpq	$42, %rax
	jne	.LBB10_8
.LBB10_32:                                                  # %fresh1244
                                                            # Loop Depth 1
                                                            # Loop Header is BB10_1
                                                            # Inner Loop
	movq	8(%r14), %rdi
	call	fun_UHC_Base_tail
	jmp	.LBB10_36
.LBB10_33:                                                  # %fresh1170
                                                            # Loop Depth 1
                                                            # Loop Header is BB10_1
                                                            # Inner Loop
	movq	8(%rbx), %rbx
	movl	$24, %eax
	movq	%rax, %rdi
	call	llvmgc_malloc
	movq	%rax, %r14
	movq	$50, (%r14)
	movq	%rbx, 8(%r14)
	movq	16(%rsp), %r15
	movq	%r15, 16(%r14)
	movq	%rbx, %rdi
	call	fun_app2_1
	movq	RP(%rip), %rbx
	movq	(%rbx), %rbx
	movq	%rbx, (%r14)
	movq	RP(%rip), %rbx
	movq	$16, (%rbx)
	movq	RP(%rip), %rbx
	movq	%r15, 8(%rbx)
	movq	RP(%rip), %rbx
	movq	%r14, 16(%rbx)
	jmp	.LBB10_22
.LBB10_34:                                                  # %fresh1181
                                                            # Loop Depth 1
                                                            # Loop Header is BB10_1
                                                            # Inner Loop
	movq	16(%rbx), %rsi
	movq	8(%rbx), %rdi
	movq	16(%rsp), %rdx
	call	fun_UHC_Base_primbindIO
	jmp	.LBB10_22
.LBB10_35:                                                  # %fresh1268
                                                            # Loop Depth 1
                                                            # Loop Header is BB10_1
                                                            # Inner Loop
	call	fun_UHC_Base_tailtildespec1
.LBB10_36:                                                  # %fresh1268
                                                            # Loop Depth 1
                                                            # Loop Header is BB10_1
                                                            # Inner Loop
	movq	RP(%rip), %rax
	movq	(%rax), %rax
	movq	%rax, (%r14)
	movq	RP(%rip), %rcx
	movq	8(%rcx), %rcx
	movq	%rcx, 8(%r14)
	movq	RP(%rip), %rcx
	movq	16(%rcx), %rcx
	movq	%rcx, 16(%r14)
.LBB10_37:                                                  # %fresh1291
                                                            # Loop Depth 1
                                                            # Loop Header is BB10_1
                                                            # Inner Loop
	cmpq	$3, %rax
	je	.LBB10_54
.LBB10_38:                                                  # %fresh1291
                                                            # Loop Depth 1
                                                            # Loop Header is BB10_1
                                                            # Inner Loop
	cmpq	$4, %rax
	jne	.LBB10_8
.LBB10_39:                                                  # %fresh1353
                                                            # Loop Depth 1
                                                            # Loop Header is BB10_1
                                                            # Inner Loop
	movl	$20, %eax
	movq	global_UHC_OldIO_y110_234_1(%rip), %rcx
.LBB10_40:                                                  # %fresh1357
                                                            # Loop Depth 1
                                                            # Loop Header is BB10_1
                                                            # Inner Loop
	movq	%rax, (%rbx)
	movq	%rcx, 8(%rbx)
	movq	RP(%rip), %rcx
	movq	16(%rcx), %rcx
	movq	%rcx, 16(%rbx)
	jmp	.LBB10_47
.LBB10_41:                                                  # %fresh1372
                                                            # Loop Depth 1
                                                            # Loop Header is BB10_1
                                                            # Inner Loop
	movq	8(%rbx), %rax
	movq	(%rax), %rcx
	cmpq	$45, %rcx
	je	.LBB10_45
.LBB10_42:                                                  # %fresh1372
                                                            # Loop Depth 1
                                                            # Loop Header is BB10_1
                                                            # Inner Loop
	cmpq	$26, %rcx
	je	.LBB10_44
.LBB10_43:                                                  # %fresh1372
                                                            # Loop Depth 1
                                                            # Loop Header is BB10_1
                                                            # Inner Loop
	cmpq	$24, %rcx
	jne	.LBB10_8
.LBB10_44:                                                  # %fresh1381
                                                            # Loop Depth 1
                                                            # Loop Header is BB10_1
                                                            # Inner Loop
	movq	8(%rax), %r14
	jmp	.LBB10_46
.LBB10_45:                                                  # %fresh1391
                                                            # Loop Depth 1
                                                            # Loop Header is BB10_1
                                                            # Inner Loop
	movq	RP(%rip), %r14
	movq	$24, (%r14)
	movq	RP(%rip), %r14
	movq	global_UHC_OldIO_primStdout(%rip), %rcx
	movq	%rcx, 8(%r14)
	movq	RP(%rip), %r14
	movq	(%r14), %r14
	movq	%r14, (%rax)
	movq	RP(%rip), %r14
	movq	8(%r14), %r14
	movq	%r14, 8(%rax)
.LBB10_46:                                                  # %fresh1406
                                                            # Loop Depth 1
                                                            # Loop Header is BB10_1
                                                            # Inner Loop
	movq	16(%rbx), %r15
	movl	$24, %eax
	movq	%rax, %rdi
	call	llvmgc_malloc
	movq	$25, (%rax)
	movq	%r14, 8(%rax)
	movq	%r15, 16(%rax)
	movq	RP(%rip), %rcx
	movq	$21, (%rcx)
	movq	RP(%rip), %rcx
	movq	%rax, 8(%rcx)
	movq	RP(%rip), %rax
	movq	(%rax), %rax
	movq	%rax, (%rbx)
	movq	RP(%rip), %rcx
	movq	8(%rcx), %rcx
	movq	%rcx, 8(%rbx)
.LBB10_47:                                                  # %fresh1428
                                                            # Loop Depth 1
                                                            # Loop Header is BB10_1
                                                            # Inner Loop
	cmpq	$23, %rax
	je	.LBB10_53
.LBB10_48:                                                  # %fresh1428
	cmpq	$21, %rax
	je	.LBB10_52
.LBB10_49:                                                  # %fresh1428
	cmpq	$20, %rax
	jne	.LBB10_8
.LBB10_50:                                                  # %fresh1430
	movq	8(%rbx), %rbx
	movq	RP(%rip), %rax
	movq	$16, (%rax)
	movq	RP(%rip), %rax
	movq	16(%rsp), %rcx
	movq	%rcx, 8(%rax)
	movq	RP(%rip), %rax
	movq	%rbx, 16(%rax)
.LBB10_51:                                                  # %fresh1430
	addq	$24, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	popq	%rbp
	ret
.LBB10_52:                                                  # %fresh1437
	movq	8(%rbx), %rbx
	movl	$24, %edi
	call	llvmgc_malloc
	movq	%rax, %r14
	movq	$50, (%r14)
	movq	%rbx, 8(%r14)
	movq	16(%rsp), %r15
	movq	%r15, 16(%r14)
	movq	%rbx, %rdi
	call	fun_app2_1
	movq	RP(%rip), %rbx
	movq	(%rbx), %rbx
	movq	%rbx, (%r14)
	movq	RP(%rip), %rbx
	movq	$16, (%rbx)
	movq	RP(%rip), %rbx
	movq	%r15, 8(%rbx)
	movq	RP(%rip), %rbx
	movq	%r14, 16(%rbx)
	jmp	.LBB10_51
.LBB10_53:                                                  # %fresh1444
                                                            # Loop Depth 1
                                                            # Loop Header is BB10_1
                                                            # Inner Loop
	movq	16(%rbx), %rax
	movq	%rax, 8(%rsp)
	movq	8(%rbx), %rbx
	jmp	.LBB10_1
.LBB10_54:                                                  # %fresh1298
                                                            # Loop Depth 1
                                                            # Loop Header is BB10_1
                                                            # Inner Loop
	movl	$24, %r15d
	movq	%r15, %rdi
	call	llvmgc_malloc
	movq	%rax, %r12
	movq	$42, (%r12)
	movq	%r14, 8(%r12)
	movq	%r15, %rdi
	call	llvmgc_malloc
	movq	%rax, %r13
	movq	$46, (%r13)
	movq	%r12, 8(%r13)
	movl	$16, %r12d
	movq	%r12, %rdi
	call	llvmgc_malloc
	movq	%rax, %rbp
	movq	$40, (%rbp)
	movq	%r14, 8(%rbp)
	movq	%r12, %rdi
	call	llvmgc_malloc
	movq	%rax, %r14
	movq	$45, (%r14)
	movq	%r15, %rdi
	call	llvmgc_malloc
	movq	%rax, %r15
	movq	$51, (%r15)
	movq	%r14, 8(%r15)
	movq	%rbp, 16(%r15)
	movq	%r12, %rdi
	call	llvmgc_malloc
	movq	$22, (%rax)
	movq	%r13, 8(%rax)
	movq	RP(%rip), %rcx
	movq	$23, (%rcx)
	movq	RP(%rip), %rcx
	movq	%r15, 8(%rcx)
	movq	RP(%rip), %rcx
	movq	%rax, 16(%rcx)
	movq	RP(%rip), %rax
	movq	8(%rax), %rcx
	movq	(%rax), %rax
	jmp	.LBB10_40
	.size	fun_UHC_Base_primbindIO, .-fun_UHC_Base_primbindIO


	.align	16
	.type	fun_UHC_Base_tail,@function
fun_UHC_Base_tail:                                          # @fun_UHC_Base_tail
.LBB11_0:
	pushq	%rbx
	movq	(%rdi), %rax
	cmpq	$27, %rax
	movq	%rdi, %rbx
	jle	.LBB11_6
.LBB11_1:                                                   # %fresh1462
	cmpq	$43, %rax
	je	.LBB11_4
.LBB11_2:                                                   # %fresh1462
	cmpq	$42, %rax
	jne	.LBB11_18
.LBB11_3:                                                   # %fresh1464
	movq	8(%rbx), %rdi
	call	fun_UHC_Base_tail
	jmp	.LBB11_5
.LBB11_4:                                                   # %fresh1484
	call	fun_UHC_Base_tailtildespec1
.LBB11_5:                                                   # %fresh1484
	movq	RP(%rip), %rax
	movq	(%rax), %rax
	movq	%rax, (%rbx)
	movq	RP(%rip), %rcx
	movq	8(%rcx), %rcx
	movq	%rcx, 8(%rbx)
	movq	RP(%rip), %rcx
	movq	16(%rcx), %rcx
	movq	%rcx, 16(%rbx)
.LBB11_6:                                                   # %fresh1504
	cmpq	$4, %rax
	je	.LBB11_14
.LBB11_7:                                                   # %fresh1504
	cmpq	$3, %rax
	jne	.LBB11_18
.LBB11_8:                                                   # %fresh1506
	movq	16(%rbx), %rbx
	movq	(%rbx), %rax
	cmpq	$37, %rax
	je	.LBB11_13
.LBB11_9:                                                   # %fresh1506
	cmpq	$4, %rax
	je	.LBB11_12
.LBB11_10:                                                  # %fresh1506
	cmpq	$3, %rax
	jne	.LBB11_18
.LBB11_11:                                                  # %fresh1514
	movq	RP(%rip), %rax
	movq	$3, (%rax)
	movq	8(%rbx), %rax
	movq	RP(%rip), %rcx
	movq	%rax, 8(%rcx)
	movq	16(%rbx), %rbx
	movq	RP(%rip), %rax
	movq	%rbx, 16(%rax)
	popq	%rbx
	ret
.LBB11_12:                                                  # %fresh1529
	movq	RP(%rip), %rbx
	movq	$4, (%rbx)
	popq	%rbx
	ret
.LBB11_13:                                                  # %fresh1532
	movq	8(%rbx), %rdi
	call	fun_UHC_Base_packedStringToString
	movq	RP(%rip), %rax
	movq	16(%rax), %rcx
	movq	8(%rax), %rdx
	movq	(%rax), %rax
	movq	%rax, (%rbx)
	movq	%rdx, 8(%rbx)
	movq	%rcx, 16(%rbx)
	movq	RP(%rip), %rsi
	movq	%rax, (%rsi)
	movq	RP(%rip), %rax
	movq	%rdx, 8(%rax)
	movq	RP(%rip), %rax
	movq	%rcx, 16(%rax)
	popq	%rbx
	ret
.LBB11_14:                                                  # %fresh1570
	movq	global_UHC_Base_undefined(%rip), %rax
	movq	(%rax), %rax
	cmpq	$35, %rax
	je	.LBB11_17
.LBB11_15:                                                  # %fresh1570
	cmpq	$26, %rax
	jne	.LBB11_18
.LBB11_16:                                                  # %fresh1574
	movq	RP(%rip), %rax
	movq	$26, (%rax)
	movq	global_UHC_Base_undefined(%rip), %rax
	movq	8(%rax), %rax
	movq	RP(%rip), %rcx
	movq	%rax, 8(%rcx)
	popq	%rbx
	ret
.LBB11_17:                                                  # %fresh1582
	call	fun_UHC_Base_errortildespec1
	movq	RP(%rip), %rax
	movq	8(%rax), %rcx
	movq	(%rax), %rax
	movq	global_UHC_Base_undefined(%rip), %rdx
	movq	%rax, (%rdx)
	movq	global_UHC_Base_undefined(%rip), %rdx
	movq	%rcx, 8(%rdx)
	movq	RP(%rip), %rdx
	movq	%rax, (%rdx)
	movq	RP(%rip), %rax
	movq	%rcx, 8(%rax)
	popq	%rbx
	ret
.LBB11_18:                                                  # %fresh1501
	call	primPatternMatchFailure
	.size	fun_UHC_Base_tail, .-fun_UHC_Base_tail


	.align	16
	.type	fun_UHC_Base_tailtildespec1,@function
fun_UHC_Base_tailtildespec1:                                # @fun_UHC_Base_tailtildespec1
.LBB12_0:
	pushq	%rbx
	movq	global_Main_y13_0_2(%rip), %rax
	movq	(%rax), %rax
	cmpq	$27, %rax
	jle	.LBB12_2
.LBB12_1:                                                   # %fresh1611
	call	fun_UHC_Base_packedStringToStringtildespec1
	movq	RP(%rip), %rax
	movq	(%rax), %rax
	movq	global_Main_y13_0_2(%rip), %rcx
	movq	%rax, (%rcx)
	movq	RP(%rip), %rcx
	movq	8(%rcx), %rcx
	movq	global_Main_y13_0_2(%rip), %rdx
	movq	%rcx, 8(%rdx)
	movq	RP(%rip), %rcx
	movq	16(%rcx), %rcx
	movq	global_Main_y13_0_2(%rip), %rdx
	movq	%rcx, 16(%rdx)
.LBB12_2:                                                   # %fresh1629
	cmpq	$4, %rax
	je	.LBB12_10
.LBB12_3:                                                   # %fresh1629
	cmpq	$3, %rax
	jne	.LBB12_14
.LBB12_4:                                                   # %fresh1631
	movq	global_Main_y13_0_2(%rip), %rax
	movq	16(%rax), %rbx
	movq	(%rbx), %rax
	cmpq	$37, %rax
	je	.LBB12_9
.LBB12_5:                                                   # %fresh1631
	cmpq	$4, %rax
	je	.LBB12_8
.LBB12_6:                                                   # %fresh1631
	cmpq	$3, %rax
	jne	.LBB12_14
.LBB12_7:                                                   # %fresh1639
	movq	RP(%rip), %rax
	movq	$3, (%rax)
	movq	8(%rbx), %rax
	movq	RP(%rip), %rcx
	movq	%rax, 8(%rcx)
	movq	16(%rbx), %rbx
	movq	RP(%rip), %rax
	movq	%rbx, 16(%rax)
	popq	%rbx
	ret
.LBB12_8:                                                   # %fresh1654
	movq	RP(%rip), %rbx
	movq	$4, (%rbx)
	popq	%rbx
	ret
.LBB12_9:                                                   # %fresh1657
	movq	8(%rbx), %rdi
	call	fun_UHC_Base_packedStringToString
	movq	RP(%rip), %rax
	movq	16(%rax), %rcx
	movq	8(%rax), %rdx
	movq	(%rax), %rax
	movq	%rax, (%rbx)
	movq	%rdx, 8(%rbx)
	movq	%rcx, 16(%rbx)
	movq	RP(%rip), %rsi
	movq	%rax, (%rsi)
	movq	RP(%rip), %rax
	movq	%rdx, 8(%rax)
	movq	RP(%rip), %rax
	movq	%rcx, 16(%rax)
	popq	%rbx
	ret
.LBB12_10:                                                  # %fresh1695
	movq	global_UHC_Base_undefined(%rip), %rax
	movq	(%rax), %rax
	cmpq	$35, %rax
	je	.LBB12_13
.LBB12_11:                                                  # %fresh1695
	cmpq	$26, %rax
	jne	.LBB12_14
.LBB12_12:                                                  # %fresh1699
	movq	RP(%rip), %rax
	movq	$26, (%rax)
	movq	global_UHC_Base_undefined(%rip), %rax
	movq	8(%rax), %rax
	movq	RP(%rip), %rcx
	movq	%rax, 8(%rcx)
	popq	%rbx
	ret
.LBB12_13:                                                  # %fresh1707
	call	fun_UHC_Base_errortildespec1
	movq	RP(%rip), %rax
	movq	8(%rax), %rcx
	movq	(%rax), %rax
	movq	global_UHC_Base_undefined(%rip), %rdx
	movq	%rax, (%rdx)
	movq	global_UHC_Base_undefined(%rip), %rdx
	movq	%rcx, 8(%rdx)
	movq	RP(%rip), %rdx
	movq	%rax, (%rdx)
	movq	RP(%rip), %rax
	movq	%rcx, 8(%rax)
	popq	%rbx
	ret
.LBB12_14:                                                  # %fresh1693
	call	primPatternMatchFailure
	.size	fun_UHC_Base_tailtildespec1, .-fun_UHC_Base_tailtildespec1
	.type	fresh316,@object
	.section	.rodata.str1.1,"aMS",@progbits,1
fresh316:                                                   # @fresh316
	.asciz	"Hello, World!"
	.size	fresh316, 14
	.type	fresh409,@object
	.section	.rodata.str1.16,"aMS",@progbits,1
	.align	16
fresh409:                                                   # @fresh409
	.asciz	"Prelude.undefined"
	.size	fresh409, 18
	.type	RP,@object
	.bss
	.local	RP
	.comm	RP,8,8                                      # @RP
	.type	global_UHC_Base_undefined,@object
	.local	global_UHC_Base_undefined
	.comm	global_UHC_Base_undefined,8,8               # @global_UHC_Base_undefined
	.type	global_UHC_Base_y103_0_459,@object
	.local	global_UHC_Base_y103_0_459
	.comm	global_UHC_Base_y103_0_459,8,8              # @global_UHC_Base_y103_0_459
	.type	global_UHC_OldIO_primStdout,@object
	.local	global_UHC_OldIO_primStdout
	.comm	global_UHC_OldIO_primStdout,8,8             # @global_UHC_OldIO_primStdout
	.type	global_Main_y13_0_2,@object
	.local	global_Main_y13_0_2
	.comm	global_Main_y13_0_2,8,8                     # @global_Main_y13_0_2
	.type	global_Main_main,@object
	.local	global_Main_main
	.comm	global_Main_main,8,8                        # @global_Main_main
	.type	global_main,@object
	.local	global_main
	.comm	global_main,8,8                             # @global_main
	.type	global_UHC_OldIO_y110_234_1,@object
	.local	global_UHC_OldIO_y110_234_1
	.comm	global_UHC_OldIO_y110_234_1,8,8             # @global_UHC_OldIO_y110_234_1
	.type	global_UHC_OldIO_y110_242_1,@object
	.local	global_UHC_OldIO_y110_242_1
	.comm	global_UHC_OldIO_y110_242_1,8,8             # @global_UHC_OldIO_y110_242_1

