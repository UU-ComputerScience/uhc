# location of grin compiler src
SRC_GRINC_PREFIX						:= $(SRC_PREFIX)grinc/

# this file
GRINC_MKF								:= $(SRC_GRINC_PREFIX)files.mk

#support for AGC on files in different directories
AGCC = cd $(dir $1) && $(AGC) -P$(INS_EHC_LIB_AG_PREFIX) $2 $(notdir $1)

GRINC_HS          :=
GRINC_HS_FROM_CHS :=
GRINC_AG_FROM_CAG :=
GRINC_HS_FROM_AG  :=
GRINC_HS_DATA_FROM_AG :=

#collect all 'real' source code (not derived) - dependencies just to start compilation on the real files
#GRINC_ALL_SRC     :=

# end products, binary, executable, etc
GRINC_EXEC_NAME							:= grinc
GRINC_BLD_EXEC							:= $(EHC_BIN_VARIANT_PREFIX)$(GRINC_EXEC_NAME)$(EXEC_SUFFIX)
GRINC_ALL_PUB_EXECS						:= $(patsubst %,$(EHC_BIN_PREFIX)%/$(GRINC_EXEC_NAME)$(EXEC_SUFFIX),$(GRIN_PUB_VARIANTS))
GRINC_ALL_EXECS							:= $(patsubst %,$(EHC_BIN_PREFIX)%/$(GRINC_EXEC_NAME)$(EXEC_SUFFIX),$(GRIN_VARIANTS))

# main source
GRINC_MAIN_HS                                           := $(EHC_BLD_VARIANT_PREFIX)GRINC.hs
#GRINC_ALL_SRC                                           += $(SRC_GRINC_PREFIX)GRINC.chs


### BEGIN of library (will make part of remainder obsolete)
# library
# derived stuff
LIB_GRINC_CABAL_DRV						:= $(EHC_BLD_LIBGRINC_VARIANT_PREFIX)lib-eh$(EHC_VARIANT).cabal
LIB_GRINC_SETUP_HS_DRV					:= $(EHC_BLD_LIBGRINC_VARIANT_PREFIX)Setup.hs
LIB_GRINC_SETUP2						:= $(EHC_BLD_LIBGRINC_VARIANT_PREFIX)setup$(EXEC_SUFFIX)
LIB_GRINC_SETUP							:= ./setup$(EXEC_SUFFIX)

# main + sources + dpds, for .chs
GRINC_MAIN								:= GRINC
GRINC_HS_MAIN_SRC_CHS					:= $(patsubst %,$(SRC_GRINC_PREFIX)%.chs,$(GRINC_MAIN))
GRINC_HS_MAIN_DRV_HS					:= $(patsubst $(SRC_GRINC_PREFIX)%.chs,$(EHC_BLD_VARIANT_PREFIX)%.hs,$(GRINC_HS_MAIN_SRC_CHS))

GRINC_HS_UTIL_SRC_CHS					:= $(patsubst %,$(SRC_GRINC_PREFIX)%.chs,\
													GRINCCommon \
													HeapPointsToFixpoint Primitives \
													CmmCode/Building \
													CompilerDriver \
											)
GRINC_HS_UTIL_DRV_HS					:= $(patsubst $(SRC_GRINC_PREFIX)%.chs,$(GRINC_BLD_LIB_HS_VARIANT_PREFIX)%.hs,$(GRINC_HS_UTIL_SRC_CHS))

GRINC_HS_ALL_SRC_CHS					:= $(GRINC_HS_MAIN_SRC_CHS) $(GRINC_HS_UTIL_SRC_CHS)
GRINC_HS_ALL_DRV_HS						:= $(GRINC_HS_MAIN_DRV_HS) $(GRINC_HS_UTIL_DRV_HS)

# main + sources + dpds, for .cag
GRINC_AGCMMCODE_MAIN_SRC_CAG			:= $(patsubst %,$(SRC_GRINC_PREFIX)%.cag,CmmCode)
GRINC_AGCMMCODE_DPDS_SRC_CAG			:= $(patsubst %,$(SRC_GRINC_PREFIX)CmmCode/%.cag,AbsSyn)
$(patsubst $(SRC_GRINC_PREFIX)%.cag,$(GRINC_BLD_LIB_HS_VARIANT_PREFIX)%.hs,$(GRINC_AGCMMCODE_MAIN_SRC_CAG)) \
										: $(patsubst $(SRC_GRINC_PREFIX)%.cag,$(GRINC_BLD_LIB_HS_VARIANT_PREFIX)%.ag,$(GRINC_AGCMMCODE_DPDS_SRC_CAG))

GRINC_AGCMMCODE_PRETTY_MAIN_SRC_CAG		:= $(patsubst %,$(SRC_GRINC_PREFIX)CmmCode/%.cag,Pretty)
GRINC_AGCMMCODE_PRETTY_DPDS_SRC_CAG		:= $(patsubst %,$(SRC_GRINC_PREFIX)CmmCode/%.cag,AbsSyn)
$(patsubst $(SRC_GRINC_PREFIX)%.cag,$(GRINC_BLD_LIB_HS_VARIANT_PREFIX)%.hs,$(GRINC_AGCMMCODE_PRETTY_MAIN_SRC_CAG)) \
										: $(patsubst $(SRC_GRINC_PREFIX)%.cag,$(GRINC_BLD_LIB_HS_VARIANT_PREFIX)%.ag,$(GRINC_AGCMMCODE_PRETTY_DPDS_SRC_CAG))

GRINC_AGGRINCODE_CMMCODE_MAIN_SRC_CAG		:= $(patsubst %,$(SRC_GRINC_PREFIX)GrinCode/%.cag,CmmCode)
GRINC_AGGRINCODE_CMMCODE_DPDS_SRC_CAG		:= $(patsubst %,$(SRC_GRINC_PREFIX)GrinCode/%.cag,TagInfo ValueInfo ReturnSize Primitives ImportExport Globals ExceptionHandlers ToCmm TraceInfo LastExpr)
$(patsubst $(SRC_GRINC_PREFIX)%.cag,$(GRINC_BLD_LIB_HS_VARIANT_PREFIX)%.hs,$(GRINC_AGGRINCODE_CMMCODE_MAIN_SRC_CAG)) \
										: $(patsubst $(SRC_GRINC_PREFIX)%.cag,$(GRINC_BLD_LIB_HS_VARIANT_PREFIX)%.ag,$(GRINC_AGGRINCODE_CMMCODE_DPDS_SRC_CAG))

GRINC_AGGRINCODE_POINTSTO_MAIN_SRC_CAG		:= $(patsubst %,$(SRC_GRINC_PREFIX)GrinCode/%.cag,PointsToAnalysis)
GRINC_AGGRINCODE_POINTSTO_DPDS_SRC_CAG		:= $(patsubst %,$(SRC_GRINC_PREFIX)GrinCode/%.cag,)
$(patsubst $(SRC_GRINC_PREFIX)%.cag,$(GRINC_BLD_LIB_HS_VARIANT_PREFIX)%.hs,$(GRINC_AGGRINCODE_POINTSTO_MAIN_SRC_CAG)) \
										: $(patsubst $(SRC_GRINC_PREFIX)%.cag,$(GRINC_BLD_LIB_HS_VARIANT_PREFIX)%.ag,$(GRINC_AGGRINCODE_POINTSTO_DPDS_SRC_CAG))

GRINC_AGGRINCODE_ALLTRF_MAIN_SRC_CAG		:= $(patsubst %,$(SRC_GRINC_PREFIX)GrinCode/Trf/%.cag,DropUnusedExpr NameIdents SparseCase NormForHPT NumberIdents CaseElimination SplitFetch DropUnusedBindings DropUnusedTags GrInline RightSkew LowerGrin CleanupPass CopyPropagation BuildAppBindings ReturningCatch)
GRINC_AGGRINCODE_ALLTRF_DPDS_SRC_CAG		:= $(patsubst %,$(SRC_GRINC_PREFIX)GrinCode/%.cag,CAFNames)
$(patsubst $(SRC_GRINC_PREFIX)%.cag,$(GRINC_BLD_LIB_HS_VARIANT_PREFIX)%.hs,$(GRINC_AGGRINCODE_ALLTRF_MAIN_SRC_CAG)) \
										: $(patsubst $(SRC_GRINC_PREFIX)%.cag,$(GRINC_BLD_LIB_HS_VARIANT_PREFIX)%.ag,$(GRINC_AGGRINCODE_ALLTRF_DPDS_SRC_CAG))

GRINC_AG_D_MAIN_SRC_CAG					:= \
											$(GRINC_AGCMMCODE_MAIN_SRC_CAG)

GRINC_AG_S_MAIN_SRC_CAG					:= \
											$(GRINC_AGCMMCODE_PRETTY_MAIN_SRC_CAG) \
											$(GRINC_AGGRINCODE_CMMCODE_MAIN_SRC_CAG) \
											$(GRINC_AGGRINCODE_POINTSTO_MAIN_SRC_CAG) \
											$(GRINC_AGGRINCODE_ALLTRF_MAIN_SRC_CAG)

GRINC_AG_ALL_MAIN_SRC_CAG				:= $(GRINC_AG_D_MAIN_SRC_CAG) $(GRINC_AG_S_MAIN_SRC_CAG) $(GRINC_AG_DS_MAIN_SRC_CAG)

GRINC_AG_ALL_DPDS_SRC_CAG				:= $(sort \
											$(GRINC_AGCMMCODE_DPDS_SRC_CAG) \
											$(GRINC_AGCMMCODE_PRETTY_DPDS_SRC_CAG) \
											$(GRINC_AGGRINCODE_CMMCODE_DPDS_SRC_CAG) \
											$(GRINC_AGGRINCODE_POINTSTO_DPDS_SRC_CAG) \
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
		, AllowUndecidableInstances \
		, Part of GRINC$(EHC_VARIANT) compiler packaged as library \
		, $(subst $(PATH_SEP),.,$(patsubst $(GRINC_BLD_LIB_HS_VARIANT_PREFIX)%.hs,$(LIB_GRINC_QUAL_PREFIX)%,$(shell $(FILTER_NONEMP_FILES) $(GRINC_HS_UTIL_DRV_HS) $(GRINC_AG_ALL_MAIN_DRV_HS)))) \
	) > $@

$(LIB_GRINC_SETUP_HS_DRV): $(GRINC_MKF)
	mkdir -p $(@D)
	$(call GEN_CABAL_SETUP) > $@

$(LIB_GRINC_SETUP2): $(LIB_GRINC_SETUP_HS_DRV)
	$(call GHC_CABAL,$<,$@)

$(LIB_GRINC_INS_FLAG): $(LIB_GRINC_CABAL_DRV) $(LIB_GRINC_SETUP2) $(INS_GRINC_LIB_ALL_AG) $(GRINC_MKF)
	mkdir -p $(@D)
	cd $(EHC_BLD_LIBGRINC_VARIANT_PREFIX) && \
	$(LIB_GRINC_SETUP) configure --prefix=$(INS_PREFIX) --user && \
	$(LIB_GRINC_SETUP) build && \
	$(LIB_GRINC_SETUP) install --user && \
	echo $@ > $@

$(INS_GRINC_LIB_ALL_AG): $(INS_GRINC_LIB_AG_PREFIX)%: $(GRINC_BLD_LIB_HS_VARIANT_PREFIX)%
	mkdir -p $(@D)
	cp $< $@

# rules for ehc library sources+derived
$(GRINC_AG_ALL_MAIN_DRV_AG) $(GRINC_AG_ALL_DPDS_DRV_AG): $(GRINC_BLD_LIB_HS_VARIANT_PREFIX)%.ag: $(SRC_GRINC_PREFIX)%.cag $(SHUFFLE)
	mkdir -p $(@D)
	$(SHUFFLE) $(LIB_EHC_SHUFFLE_DEFS) $(LIB_GRINC_SHUFFLE_DEFS) --gen=$(EHC_VARIANT) --base=$(*F) --ag --preamble=no --lhs2tex=no --order="$(EHC_SHUFFLE_ORDER)" $< > $@

$(GRINC_AG_D_MAIN_DRV_HS): %.hs: %.ag
	$(AGC) -dr -P$(EHC_BLD_VARIANT_PREFIX) -P$(GRINC_BLD_LIB_HS_VARIANT_PREFIX) -P$(INS_EHC_LIB_AG_PREFIX) $<

$(GRINC_AG_S_MAIN_DRV_HS): %.hs: %.ag
	$(AGC) -cfspr -P$(EHC_BLD_VARIANT_PREFIX) -P$(GRINC_BLD_LIB_HS_VARIANT_PREFIX) -P$(INS_EHC_LIB_AG_PREFIX) $<

$(GRINC_HS_MAIN_DRV_HS): $(EHC_BLD_VARIANT_PREFIX)%.hs: $(SRC_GRINC_PREFIX)%.chs $(SHUFFLE) $(GRINC_MKF)
	mkdir -p $(@D)
	$(SHUFFLE) $(LIB_EHC_SHUFFLE_DEFS) $(LIB_GRINC_SHUFFLE_DEFS) --gen=$(EHC_VARIANT) --base=Main --hs --preamble=no --lhs2tex=no --order="$(EHC_SHUFFLE_ORDER)" $< > $@

$(GRINC_HS_UTIL_DRV_HS): $(GRINC_BLD_LIB_HS_VARIANT_PREFIX)%.hs: $(SRC_GRINC_PREFIX)%.chs $(SHUFFLE)
	mkdir -p $(@D)
	$(SHUFFLE) $(LIB_EHC_SHUFFLE_DEFS) $(LIB_GRINC_SHUFFLE_DEFS) --gen=$(EHC_VARIANT) --base=$(*F) --hs --preamble=no --lhs2tex=no --order="$(EHC_SHUFFLE_ORDER)" $< > $@

### END of library

### External files ###

# files from grini and ehc used by grinc

#GRINC_ALL_HS_SRC  := $(GRINC_MAIN_HS)
#GRINC_ALL_HS_SRC  := $(GRINC_MAIN_HS) $(EHC_BLD_VARIANT_PREFIX)GRIParser.hs \
#                     $(addprefix $(EHC_BLD_VARIANT_PREFIX),GrinCode.hs GrinCodePretty.hs) \

# Just to start the compilation depend on all 'real' files of ehc and grini
# comilation will abort after the first step if the files are not used by grinc
#GRINC_ALL_SRC += $(EHC_ALL_SRC) $(GRINI_ALL_SRC)


### Utils ###

# base names of all utils
GRINC_UTILS := CompilerDriver GRINCCommon Primitives

#GRINC_ALL_SRC      += $(patsubst %,$(SRC_GRINC_PREFIX)%.chs,$(GRINC_UTILS))
#GRINC_UTILS_SRC_HS := $(patsubst %,$(EHC_BLD_VARIANT_PREFIX)%.hs,$(GRINC_UTILS))

#GRINC_HS_FROM_CHS  += $(GRINC_UTILS_SRC_HS)

#deps
#$(EHC_BLD_VARIANT_PREFIX)Primitives.hs: $(patsubst %,$(EHC_BLD_VARIANT_PREFIX)%.hs,HeapPointsToFixpoint Cmm/CmmCode)

### GRIN Transformations ###

# base names of all transformations
TRFS := NumberIdents NormForHPT RightSkew NameIdents LowerGrin DropUnusedBindings DropUnusedExpr \
        GrInline CaseElimination SparseCase CopyPropagation BuildAppBindings DropUnusedTags SplitFetch \
        CleanupPass ReturningCatch

#sourcefiles of all transformations
GRINC_TRF_SRC_CAG := $(patsubst %,$(SRC_GRINC_PREFIX)Trf/%.cag,$(TRFS))
GRINC_TRF_SRC_AG  := $(patsubst %,$(EHC_BLD_VARIANT_PREFIX)Trf/%.ag,$(TRFS))
GRINC_TRF_SRC_HS  := $(patsubst %,$(EHC_BLD_VARIANT_PREFIX)Trf/%.hs,$(TRFS))

GRINC_TRF_UTILS_BASE := GrCAFNames GrLastExpr

GRINC_AG_FROM_CAG += $(GRINC_TRF_SRC_AG) $(patsubst %,$(EHC_BLD_VARIANT_PREFIX)%.ag, $(GRINC_TRF_UTILS_BASE))
GRINC_HS_FROM_AG  += $(GRINC_TRF_SRC_HS)
#GRINC_ALL_SRC     += $(GRINC_TRF_SRC_CAG) $(patsubst %,$(SRC_GRINC_PREFIX)%.cag,$(GRINC_TRF_UTILS_BASE))

#every transformation depends on GrinCodeAbsSyn.ag and GrinCode.hs
#$(GRINC_TRF_SRC_HS): $(EHC_BLD_VARIANT_PREFIX)GrinCodeAbsSyn.ag $(EHC_BLD_VARIANT_PREFIX)GrinCode.hs

#some transformations depend on some extra files
#$(EHC_BLD_VARIANT_PREFIX)Trf/CleanupPass.hs: $(EHC_BLD_VARIANT_PREFIX)GrCAFNames.ag
#$(EHC_BLD_VARIANT_PREFIX)Trf/DropUnusedTags.hs: $(EHC_BLD_VARIANT_PREFIX)GrCAFNames.ag

#$(EHC_BLD_VARIANT_PREFIX)Trf/NormForHPT.hs: $(EHC_BLD_VARIANT_PREFIX)GrLastExpr.ag
#$(EHC_BLD_VARIANT_PREFIX)Trf/SplitFetch.hs: $(EHC_BLD_VARIANT_PREFIX)GrLastExpr.ag
#$(EHC_BLD_VARIANT_PREFIX)Trf/DropUnusedBindings.hs: $(EHC_BLD_VARIANT_PREFIX)GrLastExpr.ag

### Analysis ###
#GRINC_ALL_SRC      += $(addprefix $(SRC_GRINC_PREFIX),HeapPointsToFixpoint.chs GrPointsToAnalysis.cag)

GRINC_AG_FROM_CAG  += $(EHC_BLD_VARIANT_PREFIX)GrPointsToAnalysis.ag
GRINC_HS_FROM_AG   += $(EHC_BLD_VARIANT_PREFIX)GrPointsToAnalysis.hs
GRINC_HS_FROM_CHS  += $(EHC_BLD_VARIANT_PREFIX)HeapPointsToFixpoint.hs

#points to analysis dependencies
$(EHC_BLD_VARIANT_PREFIX)GrPointsToAnalysis.hs: $(EHC_BLD_VARIANT_PREFIX)GrCAFNames.ag $(EHC_BLD_VARIANT_PREFIX)Primitives.hs

### C-- ###

GRINC_CMM_FROMGRIN_CAG := $(wildcard $(SRC_GRINC_PREFIX)Cmm/FromGrin/*)
GRINC_CMM_FROMGRIN_AG  := $(patsubst $(SRC_GRINC_PREFIX)%.cag,$(EHC_BLD_VARIANT_PREFIX)%.ag,$(GRINC_CMM_FROMGRIN_CAG))
GRINC_CMM_SRC_CAG      := $(patsubst %,$(SRC_GRINC_PREFIX)CmmCode/%.cag,FromGrin) $(GRINC_CMM_FROMGRIN_CAG)
GRINC_CMM_BUILDING_CHS := 

#GRINC_ALL_SRC          += $(GRINC_CMM_SRC_CAG) $(GRINC_CMM_BUILDING_CHS)
GRINC_AG_FROM_CAG      += $(patsubst $(SRC_GRINC_PREFIX)Cmm/%.cag,$(EHC_BLD_VARIANT_PREFIX)Cmm/%.ag,$(GRINC_CMM_SRC_CAG))
GRINC_HS_FROM_AG       += $(patsubst %,$(EHC_BLD_VARIANT_PREFIX)Cmm/%.hs,CmmCodePretty FromGrin)
GRINC_HS_FROM_CHS      += $(patsubst $(SRC_GRINC_PREFIX)%.chs,$(EHC_BLD_VARIANT_PREFIX)%.hs,$(GRINC_CMM_BUILDING_CHS))
GRINC_HS_DATA_FROM_AG  += $(EHC_BLD_VARIANT_PREFIX)Cmm/CmmCode.hs

# all C-- files will depend on GRINC_CMM_AST
GRINC_CMM_AST := $(EHC_BLD_VARIANT_PREFIX)Cmm/CmmCodeAbsSyn.ag

#deps
$(EHC_BLD_VARIANT_PREFIX)Cmm/FromGrin.hs: $(GRINC_CMM_FROMGRIN_AG) $(GRINC_CMM_AST) $(EHC_BLD_VARIANT_PREFIX)Primitives.hs
$(patsubst %,$(EHC_BLD_VARIANT_PREFIX)Cmm/%.hs,CmmCode CmmCodePretty): $(GRINC_CMM_AST)


### Build rules ####

#collect all hs source
#GRINC_ALL_HS_SRC += $(GRINC_HS_FROM_AG) $(GRINC_HS_DATA_FROM_AG) $(GRINC_HS_FROM_CHS) $(GRINC_CMM_FROMGRIN_CAG)

#$(GRINC_HS_DATA_FROM_AG): %.hs: %.ag
#	$(call AGCC,$<,-dr)

#$(GRINC_HS_FROM_AG): %.hs: %.ag
#	$(call AGCC,$<,-cfspr)

#$(GRINC_AG_FROM_CAG): $(EHC_BLD_VARIANT_PREFIX)%.ag: $(SRC_GRINC_PREFIX)%.cag $(SHUFFLE)
#	mkdir -p $(@D)
#	$(SHUFFLE) $(LIB_EHC_SHUFFLE_DEFS) --gen=$(EHC_VARIANT) --base=$(*F) --ag --preamble=no --lhs2tex=no --order="$(EHC_SHUFFLE_ORDER)" $< > $@

#$(GRINC_HS_FROM_CHS): $(EHC_BLD_VARIANT_PREFIX)%.hs: $(SRC_GRINC_PREFIX)%.chs $(SHUFFLE)
#	mkdir -p $(@D)
#	$(SHUFFLE) $(LIB_EHC_SHUFFLE_DEFS) --gen=$(EHC_VARIANT) --base=$(*F) --hs --preamble=no --lhs2tex=no --order="$(EHC_SHUFFLE_ORDER)" $< > $@

#$(GRINC_MAIN_HS): $(EHC_BLD_VARIANT_PREFIX)%.hs: $(SRC_GRINC_PREFIX)%.chs $(SHUFFLE)
#	mkdir -p $(@D)
#	$(SHUFFLE) $(LIB_EHC_SHUFFLE_DEFS) --gen=$(EHC_VARIANT) --base=Main --hs --preamble=no --lhs2tex=no --order="$(EHC_SHUFFLE_ORDER)" $< > $@


# top rules
$(patsubst %,grinc-variant-%,$(GRIN_VARIANTS)): grinc-variant-dflt

grinc-variant-dflt: $(GRINC_ALL_DPDS) $(LIB_EH_UTIL_INS_FLAG) $(LIB_EHC_INS_FLAG) $(LIB_GRINC_INS_FLAG)
	mkdir -p $(dir $(GRINC_BLD_EXEC))
	$(GHC) --make $(GHC_OPTS) -package $(LIB_EH_UTIL_PKG_NAME) -package $(LIB_EHC_PKG_NAME) -package $(LIB_GRINC_PKG_NAME) -i$(EHC_BLD_VARIANT_PREFIX) $(GRINC_MAIN_HS) -o $(GRINC_BLD_EXEC)

# variant dispatch rules
$(patsubst $(BIN_PREFIX)%$(EXEC_SUFFIX),%,$(GRINC_ALL_EXECS)): %: $(BIN_PREFIX)%$(EXEC_SUFFIX)

$(GRINC_ALL_EXECS): $(EHC_BIN_PREFIX)%/$(GRINC_EXEC_NAME)$(EXEC_SUFFIX): $(GRINC_ALL_SRC)
	$(MAKE) lib-eh-$*
	$(MAKE) EHC_VARIANT=$* grinc-variant-$*

