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

# Use cabal for building, pass UUAGC as environment variable to Setup.hs
$(SHUFFLE_BLD_EXEC): $(LIB_EH_UTIL_INS_FLAG)
	@$(EXIT_IF_ABSENT_LIB_OR_TOOL)
	cd $(SRC_SHUFFLE_PREFIX) ; \
	UUAGC="$(AGC) $(UUAGC_OPTS_WHEN_EHC)" \
	cabal configure --builddir=$(TOPABS_PREFIX)$(SHUFFLE_BLD_PREFIX) --bindir=$(TOPABS_PREFIX)$(BIN_PREFIX) \
		--with-compiler=$(GHC) --ghc-options="$(GHC_OPTS) $(GHC_OPTS_OPTIM) $(GHC_OPTS_WHEN_EHC) -package $(LIB_EH_UTIL_PKG_NAME)"; \
	UUAGC="$(AGC) $(UUAGC_OPTS_WHEN_EHC)" \
	cabal build --builddir=$(TOPABS_PREFIX)$(SHUFFLE_BLD_PREFIX) ; \
	UUAGC="$(AGC) $(UUAGC_OPTS_WHEN_EHC)" \
	cabal copy --builddir=$(TOPABS_PREFIX)$(SHUFFLE_BLD_PREFIX) ;
