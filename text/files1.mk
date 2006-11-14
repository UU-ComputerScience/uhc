# cfg for text, further additions in files1-*.mk

# location of text src
TEXT_SRC_PREFIX				:= $(TOP_PREFIX)text/

# this file
TEXT_MKF					:= $(wildcard $(TEXT_SRC_PREFIX)files*.mk)

# main
TEXT_MAIN					:= main

# subtext
TEXT_SUBS					:= # added to in files1-*.mk
TEXT_SUBS_ASIS				:= afp-pgf

# variant, to be configured on top level
TEXT_VARIANT				:= $(TEXT_MAIN)
TEXT_SHUFFLE_VARIANT		:= 1
TEXT_TMP_PREFIX				:= $(BLD_PREFIX)
TEXT_TMP_VARIANT_PREFIX		:= $(TEXT_TMP_PREFIX)$(TEXT_VARIANT)/

# all variants
TEXT_PUB_VARIANTS			= # added to in files1-*.mk
TEXT_PRIV_VARIANTS			= # added to in files1-*.mk
TEXT_VARIANTS				= $(TEXT_PUB_VARIANTS) $(TEXT_PRIV_VARIANTS)

# configuration of lhs2tex, to be done on top level
LHS2TEX_OPTS_TEXT_CONFIG	:= 
LHS2TEX_OPTS_VARIANT_CONFIG	:= 

# configuration of ruler, to be done on top level
TEXT_RULER_MARK_CHANGES_CFG	:= # --markchanges="E - *"

# order used by shuffle
TEXT_SHUFFLE_ORDER			:= # added to in files1-*.mk

