# location of fmt src
FMT_SRC_PREFIX	:= $(TOP_PREFIX)lhs2TeX/

# this file
FMT_MKF			:= $(FMT_SRC_PREFIX)files.mk

# all src
FMT_ALL_SRC		:= $(wildcard $(FMT_SRC_PREFIX)*.fmt)

# distribution
FMT_DIST_FILES	:= $(FMT_ALL_SRC) $(FMT_MKF)
