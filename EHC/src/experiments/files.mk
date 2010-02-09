# location of experiments src
SRC_EXPERIMENTS_PREFIX	:= $(SRC_PREFIX)experiments/

# location of experimental builds
BLD_EXPERIMENTS_PREFIX	:= $(BLD_PREFIX)experiments/

# various prefixes
BIN_EXPERIMENTS_PREFIX				:= $(BIN_PREFIX)experiments/
BINABS_EXPERIMENTS_PREFIX			:= $(BINABS_PREFIX)experiments/

# default variant, to be overridden by parameterized invocation of make
EXPERIMENTS_VARIANT					:= X
EXPERIMENTS_VARIANT_PREFIX			:= $(EXPERIMENTS_VARIANT)/
