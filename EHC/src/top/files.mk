# location of library src
SRC_TOP_PREFIX				:= $(SRC_PREFIX)top/
BLD_TOP_PREFIX				:= $(BLD_PREFIX)lib-top/

# this file + other mk files
TOP_MKF						:= $(patsubst %,$(SRC_TOP_PREFIX)%.mk,files)

# Top sources



LIB_TOP_HS_SRC = \
	$(SRC_TOP_PREFIX)Constraint/Equality.hs \
	$(SRC_TOP_PREFIX)Constraint/Information.hs \
	$(SRC_TOP_PREFIX)Constraint/Polymorphism.hs \
	$(SRC_TOP_PREFIX)Constraint/Qualifier.hs \
	$(SRC_TOP_PREFIX)Constraint.hs \
	$(SRC_TOP_PREFIX)Implementation/Basic.hs \
	$(SRC_TOP_PREFIX)Implementation/FastSubstitution.hs \
	$(SRC_TOP_PREFIX)Implementation/General.hs \
	$(SRC_TOP_PREFIX)Implementation/Overloading.hs \
	$(SRC_TOP_PREFIX)Implementation/SimpleSubstitution.hs \
	$(SRC_TOP_PREFIX)Implementation/TypeGraph/ApplyHeuristics.hs \
	$(SRC_TOP_PREFIX)Implementation/TypeGraph/Basics.hs \
	$(SRC_TOP_PREFIX)Implementation/TypeGraph/Class.hs \
	$(SRC_TOP_PREFIX)Implementation/TypeGraph/ClassMonadic.hs \
	$(SRC_TOP_PREFIX)Implementation/TypeGraph/DefaultHeuristics.hs \
	$(SRC_TOP_PREFIX)Implementation/TypeGraph/EquivalenceGroup.hs \
	$(SRC_TOP_PREFIX)Implementation/TypeGraph/Heuristic.hs \
	$(SRC_TOP_PREFIX)Implementation/TypeGraph/Path.hs \
	$(SRC_TOP_PREFIX)Implementation/TypeGraph/Standard.hs \
	$(SRC_TOP_PREFIX)Implementation/TypeGraphSubstitution.hs \
	$(SRC_TOP_PREFIX)Implementation/TypeInference.hs \
	$(SRC_TOP_PREFIX)Interface/Basic.hs \
	$(SRC_TOP_PREFIX)Interface/Qualification.hs \
	$(SRC_TOP_PREFIX)Interface/Substitution.hs \
	$(SRC_TOP_PREFIX)Interface/TypeInference.hs \
	$(SRC_TOP_PREFIX)Monad/Select.hs \
	$(SRC_TOP_PREFIX)Monad/StateFix.hs \
	$(SRC_TOP_PREFIX)Ordering/Tree.hs \
	$(SRC_TOP_PREFIX)Ordering/TreeWalk.hs \
	$(SRC_TOP_PREFIX)Solver/Greedy.hs \
	$(SRC_TOP_PREFIX)Solver/PartitionCombinator.hs \
	$(SRC_TOP_PREFIX)Solver/SwitchCombinator.hs \
	$(SRC_TOP_PREFIX)Solver/TypeGraph.hs \
	$(SRC_TOP_PREFIX)Solver.hs \
	$(SRC_TOP_PREFIX)Types/Classes.hs \
	$(SRC_TOP_PREFIX)Types/Kinds.hs \
	$(SRC_TOP_PREFIX)Types/Primitive.hs \
	$(SRC_TOP_PREFIX)Types/Qualification.hs \
	$(SRC_TOP_PREFIX)Types/Quantification.hs \
	$(SRC_TOP_PREFIX)Types/Schemes.hs \
	$(SRC_TOP_PREFIX)Types/Substitution.hs \
	$(SRC_TOP_PREFIX)Types/Synonym.hs \
	$(SRC_TOP_PREFIX)Types/Unification.hs \
	$(SRC_TOP_PREFIX)Types.hs \
	$(SRC_TOP_PREFIX)Util/Embedding.hs \
	$(SRC_TOP_PREFIX)Util/Empty.hs \
	$(SRC_TOP_PREFIX)Util/IntErr.hs \
	$(SRC_TOP_PREFIX)Util/Option.hs



# lib/cabal config
LIB_TOP_QUAL				:= Top
LIB_TOP_QUAL_PREFIX			:= $(LIB_TOP_QUAL).
LIB_TOP_PKG_NAME			:= $(subst .,-,$(LIB_TOP_QUAL))
LIB_TOP_HS_PREFIX			:= $(SRC_TOP_PREFIX)$(subst .,$(PATH_SEP),$(LIB_TOP_QUAL_PREFIX))
LIB_TOP_INS_FLAG			:= $(INSABS_FLAG_PREFIX)$(LIB_TOP_PKG_NAME)

# derived stuff
LIB_TOP_CABAL_DRV			:= $(BLD_TOP_PREFIX)lib-Top.cabal
LIB_TOP_HS_DRV				:= $(patsubst $(SRC_TOP_PREFIX)%,$(BLD_TOP_PREFIX)Top/%,$(LIB_TOP_HS_SRC))
LIB_TOP_SETUP_HS_DRV		:= $(BLD_TOP_PREFIX)Setup.hs
LIB_TOP_SETUP2				:= $(BLD_TOP_PREFIX)setup$(EXEC_SUFFIX)
LIB_TOP_SETUP				:= ./setup$(EXEC_SUFFIX)

# distribution
TOP_DIST_FILES				:= $(TOP_MKF) $(LIB_TOP_HS_SRC)

# target
top: $(LIB_TOP_INS_FLAG)

# rules
$(LIB_TOP_CABAL_DRV): $(TOP_MKF) $(LIB_TOP_HS_SRC)
	mkdir -p $(@D)
	$(call GEN_CABAL \
		, $(LIB_TOP_PKG_NAME) \
		, $(EH_VERSION) \
		, mtl \
		, $(CABAL_OPT_ALLOW_UNDECIDABLE_INSTANCES) OverlappingInstances \
		, Typing Our Programs library \
		, $(subst $(PATH_SEP),.,$(patsubst $(SRC_TOP_PREFIX)%.hs,$(LIB_TOP_QUAL_PREFIX)%, $(LIB_TOP_HS_SRC) )) \
		, \
	) > $@

$(LIB_TOP_HS_DRV): $(BLD_TOP_PREFIX)Top/%.hs: $(SRC_TOP_PREFIX)%.hs
	mkdir -p $(@D)
	cp $< $@

$(LIB_TOP_SETUP_HS_DRV): $(TOP_MKF)
	mkdir -p $(@D)
	$(call GEN_CABAL_SETUP) > $@

$(LIB_TOP_SETUP2): $(LIB_TOP_SETUP_HS_DRV)
	$(call GHC_CABAL,$<,$@)

$(LIB_TOP_INS_FLAG): $(LIB_TOP_HS_DRV) $(LIB_TOP_CABAL_DRV) $(LIB_TOP_SETUP2) $(TOP_MKF)
	mkdir -p $(@D)
	cd $(BLD_TOP_PREFIX) && \
	$(LIB_TOP_SETUP) configure $(CABAL_SETUP_OPTS) --prefix=$(INSABS_PREFIX) --user && \
	$(LIB_TOP_SETUP) build && \
	$(LIB_TOP_SETUP) install --user && \
	echo $@ > $@

