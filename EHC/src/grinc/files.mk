# location of grin compiler src
SRC_GRINC_PREFIX						:= $(SRC_PREFIX)grinc/

# this file
GRINC_MKF								:= $(SRC_GRINC_PREFIX)files.mk

# end products, binary, executable, etc
GRINC_EXEC_NAME							:= grinc
GRINC_BLD_EXEC							:= $(EHC_BIN_VARIANT_PREFIX)$(GRINC_EXEC_NAME)$(EXEC_SUFFIX)
GRINC_ALL_PUB_EXECS						:= $(patsubst %,$(EHC_BIN_PREFIX)%/$(GRINC_EXEC_NAME)$(EXEC_SUFFIX),$(GRIN_PUB_VARIANTS))
GRINC_ALL_EXECS							:= $(patsubst %,$(EHC_BIN_PREFIX)%/$(GRINC_EXEC_NAME)$(EXEC_SUFFIX),$(GRIN_VARIANTS))

### BEGIN of library (will make part of remainder obsolete)
# library
# derived stuff
LIB_GRINC_CABAL_DRV						:= $(EHC_BLD_LIBGRINC_VARIANT_PREFIX)lib-$(LIB_GRINC_BASE)$(EHC_VARIANT).cabal
LIB_GRINC_SETUP_HS_DRV					:= $(EHC_BLD_LIBGRINC_VARIANT_PREFIX)Setup.hs
LIB_GRINC_SETUP2						:= $(EHC_BLD_LIBGRINC_VARIANT_PREFIX)setup$(EXEC_SUFFIX)
LIB_GRINC_SETUP							:= ./setup$(EXEC_SUFFIX)

# main + sources + dpds, for .chs
GRINC_MAIN								:= GRINC
GRINC_MAIN_HS                           := $(EHC_BLD_VARIANT_PREFIX)$(GRINC_MAIN).hs
GRINC_HS_MAIN_SRC_CHS					:= $(patsubst %,$(SRC_GRINC_PREFIX)%.chs,$(GRINC_MAIN))
GRINC_HS_MAIN_DRV_HS					:= $(patsubst $(SRC_GRINC_PREFIX)%.chs,$(EHC_BLD_VARIANT_PREFIX)%.hs,$(GRINC_HS_MAIN_SRC_CHS))

GRINC_HS_UTIL_SRC_CHS					:= $(patsubst %,$(SRC_GRINC_PREFIX)%.chs,\
													GRINCCommon \
													HeapPointsToFixpoint Primitives \
													CompilerDriver Config \
											)
GRINC_HS_UTIL_DRV_HS					:= $(patsubst $(SRC_GRINC_PREFIX)%.chs,$(GRINC_BLD_LIB_HS_VARIANT_PREFIX)%.hs,$(GRINC_HS_UTIL_SRC_CHS))

GRINC_HS_UTILCPP_SRC_CHS				:= $(patsubst %,$(SRC_GRINC_PREFIX)%.chs,\
													ConfigDefines \
											)
GRINC_HS_UTILCPP_DRV_HS					:= $(patsubst $(SRC_GRINC_PREFIX)%.chs,$(GRINC_BLD_LIB_HS_VARIANT_PREFIX)%.hs,$(GRINC_HS_UTILCPP_SRC_CHS))

GRINC_HS_ALL_SRC_CHS					:= $(GRINC_HS_MAIN_SRC_CHS) $(GRINC_HS_UTIL_SRC_CHS) $(GRINC_HS_UTILCPP_SRC_CHS)
GRINC_HS_ALL_DRV_HS						:= $(GRINC_HS_MAIN_DRV_HS) $(GRINC_HS_UTIL_DRV_HS) $(GRINC_HS_UTILCPP_DRV_HS)

# main + sources + dpds, for .cag
GRINC_AGSILLY_MAIN_SRC_CAG			:= $(patsubst %,$(SRC_GRINC_PREFIX)%.cag,Silly)
GRINC_AGSILLY_DPDS_SRC_CAG			:= $(patsubst %,$(SRC_GRINC_PREFIX)Silly/%.cag,AbsSyn)
$(patsubst $(SRC_GRINC_PREFIX)%.cag,$(GRINC_BLD_LIB_HS_VARIANT_PREFIX)%.hs,$(GRINC_AGSILLY_MAIN_SRC_CAG)) \
										: $(patsubst $(SRC_GRINC_PREFIX)%.cag,$(GRINC_BLD_LIB_HS_VARIANT_PREFIX)%.ag,$(GRINC_AGSILLY_DPDS_SRC_CAG))

GRINC_AGSILLY_PRETTYC_MAIN_SRC_CAG		:= $(patsubst %,$(SRC_GRINC_PREFIX)Silly/%.cag,PrettyC)
GRINC_AGSILLY_PRETTYC_DPDS_SRC_CAG		:= $(patsubst %,$(SRC_GRINC_PREFIX)Silly/%.cag,AbsSyn)
$(patsubst $(SRC_GRINC_PREFIX)%.cag,$(GRINC_BLD_LIB_HS_VARIANT_PREFIX)%.hs,$(GRINC_AGSILLY_PRETTYC_MAIN_SRC_CAG)) \
										: $(patsubst $(SRC_GRINC_PREFIX)%.cag,$(GRINC_BLD_LIB_HS_VARIANT_PREFIX)%.ag,$(GRINC_AGSILLY_PRETTYC_DPDS_SRC_CAG))

GRINC_AGSILLY_PRETTYLLVM_MAIN_SRC_CAG		:= $(patsubst %,$(SRC_GRINC_PREFIX)Silly/%.cag,PrettyLLVM)
GRINC_AGSILLY_PRETTYLLVM_DPDS_SRC_CAG		:= $(patsubst %,$(SRC_GRINC_PREFIX)Silly/%.cag,AbsSyn)
$(patsubst $(SRC_GRINC_PREFIX)%.cag,$(GRINC_BLD_LIB_HS_VARIANT_PREFIX)%.hs,$(GRINC_AGSILLY_PRETTYLLVM_MAIN_SRC_CAG)) \
										: $(patsubst $(SRC_GRINC_PREFIX)%.cag,$(GRINC_BLD_LIB_HS_VARIANT_PREFIX)%.ag,$(GRINC_AGSILLY_PRETTYLLVM_DPDS_SRC_CAG))


GRINC_AGGRINCODE_GENSILLY_MAIN_SRC_CAG	:= $(patsubst %,$(SRC_GRINC_PREFIX)GrinCode/%.cag,GenSilly)
GRINC_AGGRINCODE_GENSILLY_DPDS_SRC_CAG	:= $(patsubst %,$(SRC_GRINC_PREFIX)GrinCode/%.cag,ToSilly LastExpr)
$(patsubst $(SRC_GRINC_PREFIX)%.cag,$(GRINC_BLD_LIB_HS_VARIANT_PREFIX)%.hs,$(GRINC_AGGRINCODE_GENSILLY_MAIN_SRC_CAG)) \
										: $(patsubst $(SRC_GRINC_PREFIX)%.cag,$(GRINC_BLD_LIB_HS_VARIANT_PREFIX)%.ag,$(GRINC_AGGRINCODE_GENSILLY_DPDS_SRC_CAG))

GRINC_AGGRINCODE_ABSEVAL_MAIN_SRC_CAG	:= $(patsubst %,$(SRC_GRINC_PREFIX)GrinCode/%.cag,AbsEval)
GRINC_AGGRINCODE_ABSEVAL_DPDS_SRC_CAG	:= $(patsubst %,$(SRC_GRINC_PREFIX)GrinCode/%.cag,)
$(patsubst $(SRC_GRINC_PREFIX)%.cag,$(GRINC_BLD_LIB_HS_VARIANT_PREFIX)%.hs,$(GRINC_AGGRINCODE_ABSEVAL_MAIN_SRC_CAG)) \
										: $(patsubst $(SRC_GRINC_PREFIX)%.cag,$(GRINC_BLD_LIB_HS_VARIANT_PREFIX)%.ag,$(GRINC_AGGRINCODE_ABSEVAL_DPDS_SRC_CAG))

GRINC_AGGRINCODE_POINTSTO_MAIN_SRC_CAG	:= $(patsubst %,$(SRC_GRINC_PREFIX)GrinCode/%.cag,PointsToAnalysis)
GRINC_AGGRINCODE_POINTSTO_DPDS_SRC_CAG	:= $(patsubst %,$(SRC_GRINC_PREFIX)GrinCode/%.cag,)
$(patsubst $(SRC_GRINC_PREFIX)%.cag,$(GRINC_BLD_LIB_HS_VARIANT_PREFIX)%.hs,$(GRINC_AGGRINCODE_POINTSTO_MAIN_SRC_CAG)) \
										: $(patsubst $(SRC_GRINC_PREFIX)%.cag,$(GRINC_BLD_LIB_HS_VARIANT_PREFIX)%.ag,$(GRINC_AGGRINCODE_POINTSTO_DPDS_SRC_CAG))

GRINC_AGGRINCODE_ALLTRF_MAIN_SRC_CAG	:= $(patsubst %,$(SRC_GRINC_PREFIX)GrinCode/Trf/%.cag,DropUnusedExpr NameIdents SparseCase NormForHPT NumberIdents CaseElimination SplitFetch DropUnusedBindings DropUnusedTags GrInline RightSkew LowerGrin CopyPropagation BuildAppBindings ReturningCatch CleanupPass)
GRINC_AGGRINCODE_ALLTRF_DPDS_SRC_CAG	:= $(patsubst %,$(SRC_GRINC_PREFIX)GrinCode/%.cag,)
$(patsubst $(SRC_GRINC_PREFIX)%.cag,$(GRINC_BLD_LIB_HS_VARIANT_PREFIX)%.hs,$(GRINC_AGGRINCODE_ALLTRF_MAIN_SRC_CAG)) \
										: $(patsubst $(SRC_GRINC_PREFIX)%.cag,$(GRINC_BLD_LIB_HS_VARIANT_PREFIX)%.ag,$(GRINC_AGGRINCODE_ALLTRF_DPDS_SRC_CAG))

GRINC_AG_D_MAIN_SRC_CAG					:= \
											$(GRINC_AGSILLY_MAIN_SRC_CAG)

GRINC_AG_S_MAIN_SRC_CAG					:= \
											$(GRINC_AGSILLY_PRETTYC_MAIN_SRC_CAG) \
											$(GRINC_AGSILLY_PRETTYLLVM_MAIN_SRC_CAG) \
											$(GRINC_AGGRINCODE_GENSILLY_MAIN_SRC_CAG) \
											$(GRINC_AGGRINCODE_POINTSTO_MAIN_SRC_CAG) \
											$(GRINC_AGGRINCODE_ABSEVAL_MAIN_SRC_CAG) \
											$(GRINC_AGGRINCODE_ALLTRF_MAIN_SRC_CAG)

GRINC_AG_ALL_MAIN_SRC_CAG				:= $(GRINC_AG_D_MAIN_SRC_CAG) $(GRINC_AG_S_MAIN_SRC_CAG) $(GRINC_AG_DS_MAIN_SRC_CAG)

GRINC_AG_ALL_DPDS_SRC_CAG				:= $(sort \
											$(GRINC_AGSILLY_DPDS_SRC_CAG) \
											$(GRINC_AGSILLY_PRETTYC_DPDS_SRC_CAG) \
											$(GRINC_AGGRINCODE_GENSILLY_DPDS_SRC_CAG) \
											$(GRINC_AGGRINCODE_POINTSTO_DPDS_SRC_CAG) \
											$(GRINC_AGGRINCODE_ABSEVAL_DPDS_SRC_CAG) \
											$(GRINC_AGGRINCODE_ALLTRF_DPDS_SRC_CAG) \
											)

# derived
GRINC_AG_D_MAIN_DRV_AG					:= $(patsubst $(SRC_GRINC_PREFIX)%.cag,$(GRINC_BLD_LIB_HS_VARIANT_PREFIX)%.ag,$(GRINC_AG_D_MAIN_SRC_CAG))
GRINC_AG_S_MAIN_DRV_AG					:= $(patsubst $(SRC_GRINC_PREFIX)%.cag,$(GRINC_BLD_LIB_HS_VARIANT_PREFIX)%.ag,$(GRINC_AG_S_MAIN_SRC_CAG))
GRINC_AG_ALL_MAIN_DRV_AG				:= $(GRINC_AG_D_MAIN_DRV_AG) $(GRINC_AG_S_MAIN_DRV_AG) $(GRINC_AG_DS_MAIN_DRV_AG)

GRINC_AG_D_MAIN_DRV_HS					:= $(GRINC_AG_D_MAIN_DRV_AG:.ag=.hs)
GRINC_AG_S_MAIN_DRV_HS					:= $(GRINC_AG_S_MAIN_DRV_AG:.ag=.hs)
GRINC_AG_ALL_MAIN_DRV_HS				:= $(GRINC_AG_D_MAIN_DRV_HS) $(GRINC_AG_S_MAIN_DRV_HS) $(GRINC_AG_DS_MAIN_DRV_HS)

GRINC_AG_ALL_DPDS_DRV_AG				:= $(patsubst $(SRC_GRINC_PREFIX)%.cag,$(GRINC_BLD_LIB_HS_VARIANT_PREFIX)%.ag,$(GRINC_AG_ALL_DPDS_SRC_CAG))

# all dependents for a variant to kick of building
GRINC_ALL_DPDS							:= $(GRINC_HS_ALL_DRV_HS) $(GRINC_AG_ALL_MAIN_DRV_HS)

# all src
GRINC_ALL_CHUNK_SRC						:= $(GRINC_AG_ALL_MAIN_SRC_CAG) $(GRINC_AG_ALL_DPDS_SRC_CAG) $(GRINC_HS_ALL_SRC_CHS)
GRINC_ALL_SRC							:= $(GRINC_ALL_CHUNK_SRC) $(GRINC_RULES_ALL_SRC) $(GRINC_MKF)

# rules for grinc library construction
$(LIB_GRINC_CABAL_DRV): $(GRINC_ALL_DPDS) $(GRINC_MKF)
	mkdir -p $(@D)
	$(call GEN_CABAL \
		, $(LIB_GRINC_PKG_NAME) \
		, $(EH_VERSION) \
		, mtl $(LIB_EH_UTIL_PKG_NAME) $(LIB_EHC_PKG_NAME) \
		, $(CABAL_OPT_ALLOW_UNDECIDABLE_INSTANCES) \
		, Part of GRINC$(EHC_VARIANT) compiler packaged as library \
		, $(subst $(PATH_SEP),.,$(patsubst $(GRINC_BLD_LIB_HS_VARIANT_PREFIX)%.hs,$(LIB_GRINC_QUAL_PREFIX)%,$(shell $(FILTER_NONEMP_FILES) $(GRINC_HS_UTIL_DRV_HS) $(GRINC_HS_UTILCPP_DRV_HS) $(GRINC_AG_ALL_MAIN_DRV_HS)))) \
	) > $@

$(LIB_GRINC_SETUP_HS_DRV): $(GRINC_MKF)
	mkdir -p $(@D)
	$(call GEN_CABAL_SETUP) > $@

$(LIB_GRINC_SETUP2): $(LIB_GRINC_SETUP_HS_DRV)
	$(call GHC_CABAL,$<,$@)

$(LIB_GRINC_INS_FLAG): $(LIB_GRINC_CABAL_DRV) $(LIB_GRINC_SETUP2) $(INS_GRINC_LIB_ALL_AG) $(GRINC_MKF)
	mkdir -p $(@D)
	cd $(EHC_BLD_LIBGRINC_VARIANT_PREFIX) && \
	$(LIB_GRINC_SETUP) configure $(CABAL_SETUP_OPTS) --prefix=$(INSABS_PREFIX) --user && \
	$(LIB_GRINC_SETUP) build && \
	$(LIB_GRINC_SETUP) install --user && \
	echo $@ > $@

$(INS_GRINC_LIB_ALL_AG): $(INS_GRINC_LIB_AG_PREFIX)%: $(GRINC_BLD_LIB_HS_VARIANT_PREFIX)%
	mkdir -p $(@D)
	cp $< $@

# rules for ehc library sources+derived
$(GRINC_AG_ALL_MAIN_DRV_AG) $(GRINC_AG_ALL_DPDS_DRV_AG): $(GRINC_BLD_LIB_HS_VARIANT_PREFIX)%.ag: $(SRC_GRINC_PREFIX)%.cag $(SHUFFLE)
	mkdir -p $(@D)
	$(SHUFFLE_AG) $(LIB_EHC_SHUFFLE_DEFS) $(LIB_GRINC_SHUFFLE_DEFS) --gen=$(EHC_VARIANT) --base=$(*F) --order="$(EHC_SHUFFLE_ORDER)" $< > $@ && \
	touch $@

$(GRINC_AG_D_MAIN_DRV_HS): %.hs: %.ag
	$(AGC) -dr -P$(EHC_BLD_VARIANT_PREFIX) -P$(GRINC_BLD_LIB_HS_VARIANT_PREFIX) -P$(INS_EHC_LIB_AG_PREFIX) $<

$(GRINC_AG_S_MAIN_DRV_HS): %.hs: %.ag
	$(AGC) -cfspr -P$(EHC_BLD_VARIANT_PREFIX) -P$(GRINC_BLD_LIB_HS_VARIANT_PREFIX) -P$(INS_EHC_LIB_AG_PREFIX) $<

$(GRINC_HS_MAIN_DRV_HS): $(EHC_BLD_VARIANT_PREFIX)%.hs: $(SRC_GRINC_PREFIX)%.chs $(SHUFFLE) $(GRINC_MKF)
	mkdir -p $(@D)
	$(SHUFFLE_HS) $(LIB_EHC_SHUFFLE_DEFS) $(LIB_GRINC_SHUFFLE_DEFS) --gen=$(EHC_VARIANT) --base=Main --order="$(EHC_SHUFFLE_ORDER)" $< > $@ && \
	touch $@

$(GRINC_HS_UTIL_DRV_HS): $(GRINC_BLD_LIB_HS_VARIANT_PREFIX)%.hs: $(SRC_GRINC_PREFIX)%.chs $(SHUFFLE)
	mkdir -p $(@D)
	$(SHUFFLE_HS) $(LIB_EHC_SHUFFLE_DEFS) $(LIB_GRINC_SHUFFLE_DEFS) --gen=$(EHC_VARIANT) --base=$(*F) --order="$(EHC_SHUFFLE_ORDER)" $< > $@ && \
	touch $@

$(GRINC_HS_UTILCPP_DRV_HS): $(GRINC_BLD_LIB_HS_VARIANT_PREFIX)%.hs: $(SRC_GRINC_PREFIX)%.chs $(SHUFFLE)
	mkdir -p $(@D)
	$(SHUFFLE_HS_PRE) $(LIB_EHC_SHUFFLE_DEFS) $(LIB_GRINC_SHUFFLE_DEFS) --gen=$(EHC_VARIANT) --base=$(*F) --order="$(EHC_SHUFFLE_ORDER)" $< > $@ && \
	touch $@

### END of library

### External files ###

# files from grini and ehc used by grinc

#GRINC_ALL_HS_SRC  := $(GRINC_MAIN_HS)
#GRINC_ALL_HS_SRC  := $(GRINC_MAIN_HS) $(EHC_BLD_VARIANT_PREFIX)GRIParser.hs \
#                     $(addprefix $(EHC_BLD_VARIANT_PREFIX),GrinCode.hs GrinCodePretty.hs) \

# Just to start the compilation depend on all 'real' files of ehc and grini
# comilation will abort after the first step if the files are not used by grinc
#GRINC_ALL_SRC += $(EHC_ALL_SRC) $(GRINI_ALL_SRC)

### Build rules ####

# top rules
$(patsubst %,grinc-variant-%,$(GRIN_VARIANTS)): grinc-variant-dflt

grinc-variant-dflt: $(GRINC_ALL_DPDS) $(LIB_EH_UTIL_INS_FLAG) $(LIB_EHC_INS_FLAG) $(LIB_GRINC_INS_FLAG)
	mkdir -p $(dir $(GRINC_BLD_EXEC))
	$(GHC) --make $(GHC_OPTS) -package $(LIB_EH_UTIL_PKG_NAME) -package $(LIB_EHC_PKG_NAME) -package $(LIB_GRINC_PKG_NAME) -i$(EHC_BLD_VARIANT_PREFIX) $(GRINC_MAIN_HS) -o $(GRINC_BLD_EXEC)

# variant dispatch rules
$(patsubst $(BIN_PREFIX)%$(EXEC_SUFFIX),%,$(GRINC_ALL_EXECS)): %: $(BIN_PREFIX)%$(EXEC_SUFFIX)

$(GRINC_ALL_EXECS): $(EHC_BIN_PREFIX)%/$(GRINC_EXEC_NAME)$(EXEC_SUFFIX): $(GRINC_ALL_SRC) $(EHC_ALL_SRC)
	$(MAKE) lib-eh-$*
	$(MAKE) EHC_VARIANT=$* grinc-variant-$*

