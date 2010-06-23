###########################################################################################
# Prefixes
###########################################################################################

# Source location
EXTLIBS_LTM_PREFIX                                  := $(EXTLIBS_PREFIX)ltm/

###########################################################################################
# LTM specific options & config
###########################################################################################

LTM_GCC_OPTS		:= -funroll-loops -fomit-frame-pointer

###########################################################################################
# Source files
###########################################################################################

LTM_SRC_C			:= \
	$(patsubst %,$(EXTLIBS_LTM_PREFIX)%.c,\
		bn_error \
		bn_fast_mp_invmod \
		bn_fast_mp_montgomery_reduce \
		bn_fast_s_mp_mul_digs \
		bn_fast_s_mp_mul_high_digs \
		bn_fast_s_mp_sqr \
		bn_mp_2expt \
		bn_mp_abs \
		bn_mp_add \
		bn_mp_add_d \
		bn_mp_addmod \
		bn_mp_and \
		bn_mp_clamp \
		bn_mp_clear \
		bn_mp_clear_multi \
		bn_mp_cmp \
		bn_mp_cmp_d \
		bn_mp_cmp_mag \
		bn_mp_cnt_lsb \
		bn_mp_copy \
		bn_mp_count_bits \
		bn_mp_div \
		bn_mp_div_2 \
		bn_mp_div_2d \
		bn_mp_div_3 \
		bn_mp_div_d \
		bn_mp_dr_is_modulus \
		bn_mp_dr_reduce \
		bn_mp_dr_setup \
		bn_mp_exch \
		bn_mp_expt_d \
		bn_mp_exptmod \
		bn_mp_exptmod_fast \
		bn_mp_exteuclid \
		bn_mp_fread \
		bn_mp_fwrite \
		bn_mp_gcd \
		bn_mp_get_int \
		bn_mp_get_uint64 \
		bn_mp_get_sint64 \
		bn_mp_get_double \
		bn_mp_grow \
		bn_mp_init \
		bn_mp_init_copy \
		bn_mp_init_multi \
		bn_mp_init_set \
		bn_mp_init_set_int \
		bn_mp_init_size \
		bn_mp_invmod \
		bn_mp_invmod_slow \
		bn_mp_is_square \
		bn_mp_jacobi \
		bn_mp_karatsuba_mul \
		bn_mp_karatsuba_sqr \
		bn_mp_lcm \
		bn_mp_lshd \
		bn_mp_mod \
		bn_mp_mod_2d \
		bn_mp_mod_d \
		bn_mp_montgomery_calc_normalization \
		bn_mp_montgomery_reduce \
		bn_mp_montgomery_setup \
		bn_mp_mul \
		bn_mp_mul_2 \
		bn_mp_mul_2d \
		bn_mp_mul_d \
		bn_mp_mulmod \
		bn_mp_n_root \
		bn_mp_neg \
		bn_mp_com \
		bn_mp_or \
		bn_mp_prime_fermat \
		bn_mp_prime_is_divisible \
		bn_mp_prime_is_prime \
		bn_mp_prime_miller_rabin \
		bn_mp_prime_next_prime \
		bn_mp_prime_rabin_miller_trials \
		bn_mp_prime_random_ex \
		bn_mp_radix_size \
		bn_mp_radix_size_estim \
		bn_mp_radix_smap \
		bn_mp_rand \
		bn_mp_read_radix \
		bn_mp_read_signed_bin \
		bn_mp_read_unsigned_bin \
		bn_mp_reduce \
		bn_mp_reduce_2k \
		bn_mp_reduce_2k_l \
		bn_mp_reduce_2k_setup \
		bn_mp_reduce_2k_setup_l \
		bn_mp_reduce_is_2k \
		bn_mp_reduce_is_2k_l \
		bn_mp_reduce_setup \
		bn_mp_rshd \
		bn_mp_set \
		bn_mp_set_int \
		bn_mp_set_uint64 \
		bn_mp_set_sint64 \
		bn_mp_set_double \
		bn_mp_set_double64 \
		bn_mp_sgn \
		bn_mp_shrink \
		bn_mp_signed_bin_size \
		bn_mp_sqr \
		bn_mp_sqrmod \
		bn_mp_sqrt \
		bn_mp_sub \
		bn_mp_sub_d \
		bn_mp_submod \
		bn_mp_to_signed_bin \
		bn_mp_to_signed_bin_n \
		bn_mp_to_unsigned_bin \
		bn_mp_to_unsigned_bin_n \
		bn_mp_toom_mul \
		bn_mp_toom_sqr \
		bn_mp_toradix \
		bn_mp_toradix_n \
		bn_mp_unsigned_bin_size \
		bn_mp_xor \
		bn_mp_zero \
		bn_prime_tab \
		bn_reverse \
		bn_s_mp_add \
		bn_s_mp_exptmod \
		bn_s_mp_mul_digs \
		bn_s_mp_mul_high_digs \
		bn_s_mp_sqr \
		bn_s_mp_sub \
		bncore \
	)

LTM_SRC_H			:= \
	$(patsubst %,$(EXTLIBS_LTM_PREFIX)%.h,\
		tommath \
		tommath_class \
		tommath_superclass \
	)
