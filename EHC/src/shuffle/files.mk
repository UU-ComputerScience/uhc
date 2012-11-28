###########################################################################################
# defines
###########################################################################################

# location of shuffle src
SRC_SHUFFLE_PREFIX	:= $(SRC_PREFIX)shuffle/

# location of shuffle build
SHUFFLE_BLD_PREFIX	:= $(BLD_PREFIX)shuffle/

# this file
SHUFFLE_MKF			:= $(SRC_SHUFFLE_PREFIX)files.mk

# main + sources + dpds
SHUFFLE_MAIN		:= Shuffle

# binary/executable
SHUFFLE_NAME		:= shuffle
SHUFFLE_BLD_EXEC	:= $(BIN_PREFIX)$(SHUFFLE_NAME)$(EXEC_SUFFIX)
SHUFFLE				:= $(SHUFFLE_BLD_EXEC)
SHUFFLE_HS			:= $(SHUFFLE) --hs --preamble=no --lhs2tex=no --line=yes --compiler=$(GHC_VERSION)
SHUFFLE_HS_PRE		:= $(SHUFFLE) --hs --preamble=yes --lhs2tex=no --line=yes --compiler=$(GHC_VERSION)
SHUFFLE_AG			:= $(SHUFFLE) --ag --preamble=no --lhs2tex=no --line=no --compiler=$(GHC_VERSION)
SHUFFLE_AG_PRE			:= $(SHUFFLE) --ag --preamble=yes --lhs2tex=no --line=no --compiler=$(GHC_VERSION)
SHUFFLE_PLAIN		:= $(SHUFFLE) --plain --preamble=no --lhs2tex=no --line=no
SHUFFLE_C			:= $(SHUFFLE_PLAIN)
SHUFFLE_JAVA		:= $(SHUFFLE_PLAIN)
SHUFFLE_JS			:= $(SHUFFLE_PLAIN)
# setting --line=yes for AG is not possible because of uuagc's weird interpretation of the layout rule

# cabal setup
SHUFFLE_SETUP_HS    := $(SRC_SHUFFLE_PREFIX)Setup.hs
SHUFFLE_SETUP       := $(SHUFFLE_BLD_PREFIX)setup$(EXEC_SUFFIX)

# distribution
SHUFFLE_DIST_FILES			:= $(SHUFFLE_ALL_SRC) $(SHUFFLE_MKF)

###########################################################################################
# targets
###########################################################################################

$(SHUFFLE_NAME): $(SHUFFLE_BLD_EXEC)

shuffle-clean:
	rm -rf $(SHUFFLE_BLD_EXEC) $(SHUFFLE_BLD_PREFIX)

###########################################################################################
# rules
###########################################################################################

$(SHUFFLE_SETUP): $(SHUFFLE_SETUP_HS)
	mkdir -p $(@D)
	$(call GHC_CABAL,$<,$@)

# Use cabal for building, pass UUAGC as environment variable to Setup.hs
$(SHUFFLE_BLD_EXEC): $(SHUFFLE_SETUP)
	@$(EXIT_IF_ABSENT_LIB_OR_TOOL)
	cd $(SRC_SHUFFLE_PREFIX) ; \
	UUAGC="$(AGC) $(UUAGC_OPTS_WHEN_EHC)" \
	$(TOPABS_PREFIX)$(SHUFFLE_SETUP) configure $(CABAL_SETUP_OPTS) $(CABAL_OPT_INSTALL_LOC) \
		--builddir=$(TOPABS_PREFIX)$(SHUFFLE_BLD_PREFIX) \
		--bindir=$(TOPABS_PREFIX)$(BIN_PREFIX) \
		--with-compiler=$(GHC) \
		--ghc-options="$(GHC_OPTS) $(GHC_OPTS_OPTIM) $(GHC_OPTS_WHEN_EHC)"; \
	UUAGC="$(AGC) $(UUAGC_OPTS_WHEN_EHC)" \
	$(TOPABS_PREFIX)$(SHUFFLE_SETUP) build --builddir=$(TOPABS_PREFIX)$(SHUFFLE_BLD_PREFIX) ; \
	UUAGC="$(AGC) $(UUAGC_OPTS_WHEN_EHC)" \
	$(TOPABS_PREFIX)$(SHUFFLE_SETUP) copy --builddir=$(TOPABS_PREFIX)$(SHUFFLE_BLD_PREFIX) ;
