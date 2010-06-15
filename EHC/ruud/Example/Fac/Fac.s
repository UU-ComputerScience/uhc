	.file	"Fac_opt.bc"


	.text
	.align	16
	.globl	main
	.type	main,@function
main:                                                       # @main
.LBB1_0:
	pushq	%rbx
	call	llvmgc_init
	movl	$16, %edi
	call	llvmgc_malloc_uncollectable
	movq	%rax, %rbx
	movq	%rbx, RP(%rip)
	movl	$10, %edi
	movl	$1, %esi
	call	primEqInt
	cmpq	$2, %rax
	je	.LBB1_4
.LBB1_1:
	cmpq	$1, %rax
	jne	.LBB1_3
.LBB1_2:                                                    # %fresh165.i
	movl	$16, %edi
	call	llvmgc_malloc
	movq	$11, (%rax)
	movq	%rax, %rdi
	call	fun_fac
	movq	RP(%rip), %rax
	movq	8(%rax), %rsi
	movl	$10, %edi
	call	primMulInt
	movq	RP(%rip), %rcx
	movq	$8, (%rcx)
	movq	RP(%rip), %rcx
	movq	%rax, 8(%rcx)
	jmp	.LBB1_5
.LBB1_3:                                                    # %fresh189.i
	call	primPatternMatchFailure
.LBB1_4:                                                    # %fresh184.i
	movq	RP(%rip), %rax
	movq	$8, (%rax)
	movq	RP(%rip), %rax
	movq	$1, 8(%rax)
.LBB1_5:                                                    # %fun_fun0tildemain.exit
	movq	8(%rbx), %rsi
	movl	$fresh0, %edi
	xorb	%al, %al
	call	printf
	call	llvmc_print_statistics
	xorl	%eax, %eax
	popq	%rbx
	ret
	.size	main, .-main


	.align	16
	.type	fun_primSubInttildespec1,@function
fun_primSubInttildespec1:                                   # @fun_primSubInttildespec1
.LBB2_0:
	pushq	%rbx
	movq	(%rdi), %rax
	cmpq	$11, %rax
	movq	%rdi, %rbx
	je	.LBB2_7
.LBB2_1:
	cmpq	$10, %rax
	je	.LBB2_5
.LBB2_2:
	cmpq	$8, %rax
	jne	.LBB2_8
.LBB2_3:                                                    # %fresh4
	movq	8(%rbx), %rdi
.LBB2_4:                                                    # %fresh36
	movl	$1, %esi
	call	primSubInt
	movq	RP(%rip), %rcx
	movq	$8, (%rcx)
	movq	RP(%rip), %rcx
	movq	%rax, 8(%rcx)
	popq	%rbx
	ret
.LBB2_5:                                                    # %fresh8
	movq	8(%rbx), %rdi
	call	fun_primSubInttildespec1
.LBB2_6:                                                    # %fresh8
	movq	RP(%rip), %rax
	movq	(%rax), %rax
	movq	%rax, (%rbx)
	movq	RP(%rip), %rax
	movq	8(%rax), %rdi
	movq	%rdi, 8(%rbx)
	jmp	.LBB2_4
.LBB2_7:                                                    # %fresh23
	movl	$10, %edi
	movl	$1, %esi
	call	primSubInt
	movq	RP(%rip), %rcx
	movq	$8, (%rcx)
	movq	RP(%rip), %rcx
	movq	%rax, 8(%rcx)
	jmp	.LBB2_6
.LBB2_8:                                                    # %fresh35
	call	primPatternMatchFailure
	.size	fun_primSubInttildespec1, .-fun_primSubInttildespec1


	.align	16
	.type	fun_fac,@function
fun_fac:                                                    # @fun_fac
.LBB3_0:
	pushq	%r15
	pushq	%r14
	pushq	%rbx
	movq	(%rdi), %rax
	cmpq	$11, %rax
	movq	%rdi, %rbx
	je	.LBB3_13
.LBB3_1:
	cmpq	$10, %rax
	je	.LBB3_11
.LBB3_2:
	cmpq	$8, %rax
	jne	.LBB3_19
.LBB3_3:                                                    # %fresh57
	movq	8(%rbx), %rdi
.LBB3_4:                                                    # %fresh89
	movl	$1, %esi
	call	primEqInt
	cmpq	$2, %rax
	je	.LBB3_17
.LBB3_5:                                                    # %fresh89
	cmpq	$1, %rax
	jne	.LBB3_19
.LBB3_6:                                                    # %fresh95
	movl	$16, %edi
	call	llvmgc_malloc
	movq	$10, (%rax)
	movq	%rbx, 8(%rax)
	movq	(%rbx), %rcx
	cmpq	$11, %rcx
	movq	%rax, %r14
	je	.LBB3_16
.LBB3_7:                                                    # %fresh95
	cmpq	$10, %rcx
	je	.LBB3_14
.LBB3_8:                                                    # %fresh95
	cmpq	$8, %rcx
	jne	.LBB3_19
.LBB3_9:                                                    # %fresh107
	movq	8(%rbx), %r15
.LBB3_10:                                                   # %fresh139
	movq	%r14, %rdi
	call	fun_fac
	movq	RP(%rip), %rax
	movq	8(%rax), %rsi
	movq	%r15, %rdi
	call	primMulInt
	movq	RP(%rip), %rcx
	movq	$8, (%rcx)
	movq	RP(%rip), %rcx
	movq	%rax, 8(%rcx)
	jmp	.LBB3_18
.LBB3_11:                                                   # %fresh61
	movq	8(%rbx), %rdi
	call	fun_primSubInttildespec1
.LBB3_12:                                                   # %fresh61
	movq	RP(%rip), %rax
	movq	(%rax), %rax
	movq	%rax, (%rbx)
	movq	RP(%rip), %rax
	movq	8(%rax), %rdi
	movq	%rdi, 8(%rbx)
	jmp	.LBB3_4
.LBB3_13:                                                   # %fresh76
	movl	$10, %edi
	movl	$1, %esi
	call	primSubInt
	movq	RP(%rip), %rcx
	movq	$8, (%rcx)
	movq	RP(%rip), %rcx
	movq	%rax, 8(%rcx)
	jmp	.LBB3_12
.LBB3_14:                                                   # %fresh111
	movq	8(%rbx), %rdi
	call	fun_primSubInttildespec1
.LBB3_15:                                                   # %fresh111
	movq	RP(%rip), %r15
	movq	(%r15), %r15
	movq	%r15, (%rbx)
	movq	RP(%rip), %r15
	movq	8(%r15), %r15
	movq	%r15, 8(%rbx)
	jmp	.LBB3_10
.LBB3_16:                                                   # %fresh126
	movl	$10, %edi
	movl	$1, %esi
	call	primSubInt
	movq	RP(%rip), %r15
	movq	$8, (%r15)
	movq	RP(%rip), %r15
	movq	%rax, 8(%r15)
	jmp	.LBB3_15
.LBB3_17:                                                   # %fresh154
	movq	RP(%rip), %rax
	movq	$8, (%rax)
	movq	RP(%rip), %rax
	movq	$1, 8(%rax)
.LBB3_18:                                                   # %fresh154
	popq	%rbx
	popq	%r14
	popq	%r15
	ret
.LBB3_19:                                                   # %fresh88
	call	primPatternMatchFailure
	.size	fun_fac, .-fun_fac
	.type	fresh0,@object
	.section	.rodata.str1.1,"aMS",@progbits,1
fresh0:                                                     # @fresh0
	.asciz	"%lld\n"
	.size	fresh0, 6
	.type	RP,@object
	.bss
	.local	RP
	.comm	RP,8,8                                      # @RP

