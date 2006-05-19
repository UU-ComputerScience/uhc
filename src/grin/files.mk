# variant, to be configured on top level

# all variants
GRIN_PUB_VARIANTS						:= 8 9 10
GRIN_VARIANTS							:= $(GRIN_PUB_VARIANTS) 11

# location of grin src
SRC_GRIN_PREFIX							:= $(SRC_PREFIX)grin/

# this file
GRIN_MKF								:= $(SRC_GRIN_PREFIX)files.mk

# distribution
GRIN_DIST_FILES							:= $(GRIN_ALL_SRC) $(GRIN_MKF)

