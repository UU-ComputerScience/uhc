# location of grin compiler src
SRC_GRINC_PREFIX						:= $(SRC_PREFIX)grinc/

# this file
GRINC_MKF								:= $(SRC_GRINC_PREFIX)files.mk

#support for AGC on files in different directories
AGCC = cd $(dir $1) && $(AGC) $2 $(notdir $1)

GRINC_HS          :=
GRINC_HS_FROM_CHS :=
GRINC_AG_FROM_CAG :=
GRINC_HS_FROM_AG  :=
GRINC_HS_DATA_FROM_AG :=

#collect all 'real' source code (not derived) - dependencies just to start compilation on the real files
GRINC_ALL_SRC     :=

# end products, binary, executable, etc
GRINC_EXEC_NAME							:= grinc
GRINC_BLD_EXEC							:= $(GRIN_BLD_BIN_VARIANT_PREFIX)$(GRINC_EXEC_NAME)
GRINC_ALL_PUB_EXECS						:= $(patsubst %,$(GRIN_BIN_PREFIX)%/grinc,$(GRIN_PUB_VARIANTS))
GRINC_ALL_EXECS							:= $(patsubst %,$(GRIN_BIN_PREFIX)%/grinc,$(GRIN_VARIANTS))

# main source
GRINC_MAIN_HS                                           := $(GRIN_BLD_VARIANT_PREFIX)GRINC.hs
GRINC_ALL_SRC                                           += $(SRC_GRINC_PREFIX)GRINC.chs


### External files ###

# files from grini and ehc used by grinc

GRINC_ALL_HS_SRC  := $(GRINC_MAIN_HS) $(GRIN_BLD_VARIANT_PREFIX)GRIParser.hs \
                     $(addprefix $(GRIN_BLD_VARIANT_PREFIX),GrinCode.hs GrinCodePretty.hs) \

# Just to start the compilation depend on all 'real' files of ehc and grini
# comilation will abort after the first step if the files are not used by grinc
GRINC_ALL_SRC += $(EHC_ALL_SRC) $(GRINI_ALL_SRC)


### Utils ###

# base names of all utils
GRINC_UTILS := CompilerDriver GRINCCommon Primitives

GRINC_ALL_SRC      += $(patsubst %,$(SRC_GRINC_PREFIX)%.chs,$(GRINC_UTILS))
GRINC_UTILS_SRC_HS := $(patsubst %,$(GRIN_BLD_VARIANT_PREFIX)%.hs,$(GRINC_UTILS))

GRINC_HS_FROM_CHS  += $(GRINC_UTILS_SRC_HS)

#deps
$(GRIN_BLD_VARIANT_PREFIX)Primitives.hs: $(patsubst %,$(GRIN_BLD_VARIANT_PREFIX)%.hs,HeapPointsToFixpoint GrinCode Cmm/CmmCode)

### GRIN Transformations ###

# base names of all transformations
TRFS := NumberIdents NormForHPT RightSkew NameIdents LowerGrin DropUnusedBindings DropUnusedExpr \
        GrInline CaseElimination SparseCase CopyPropagation BuildAppBindings DropUnusedTags SplitFetch \
        CleanupPass ReturningCatch

#sourcefiles of all transformations
GRINC_TRF_SRC_CAG := $(patsubst %,$(SRC_GRINC_PREFIX)Trf/%.cag,$(TRFS))
GRINC_TRF_SRC_AG  := $(patsubst %,$(GRIN_BLD_VARIANT_PREFIX)Trf/%.ag,$(TRFS))
GRINC_TRF_SRC_HS  := $(patsubst %,$(GRIN_BLD_VARIANT_PREFIX)Trf/%.hs,$(TRFS))

GRINC_TRF_UTILS_BASE := GrCAFNames GrLastExpr

GRINC_AG_FROM_CAG += $(GRINC_TRF_SRC_AG) $(patsubst %,$(GRIN_BLD_VARIANT_PREFIX)%.ag, $(GRINC_TRF_UTILS_BASE))
GRINC_HS_FROM_AG  += $(GRINC_TRF_SRC_HS)
GRINC_ALL_SRC     += $(GRINC_TRF_SRC_CAG) $(patsubst %,$(SRC_GRINC_PREFIX)%.cag,$(GRINC_TRF_UTILS_BASE))

#every transformation depends on GrinCodeAbsSyn.ag and GrinCode.hs
$(GRINC_TRF_SRC_HS): $(GRIN_BLD_VARIANT_PREFIX)GrinCodeAbsSyn.ag $(GRIN_BLD_VARIANT_PREFIX)GrinCode.hs

#some transformations depend on some extra files
$(GRIN_BLD_VARIANT_PREFIX)Trf/CleanupPass.hs: $(GRIN_BLD_VARIANT_PREFIX)GrCAFNames.ag
$(GRIN_BLD_VARIANT_PREFIX)Trf/DropUnusedTags.hs: $(GRIN_BLD_VARIANT_PREFIX)GrCAFNames.ag

$(GRIN_BLD_VARIANT_PREFIX)Trf/NormForHPT.hs: $(GRIN_BLD_VARIANT_PREFIX)GrLastExpr.ag
$(GRIN_BLD_VARIANT_PREFIX)Trf/SplitFetch.hs: $(GRIN_BLD_VARIANT_PREFIX)GrLastExpr.ag
$(GRIN_BLD_VARIANT_PREFIX)Trf/DropUnusedBindings.hs: $(GRIN_BLD_VARIANT_PREFIX)GrLastExpr.ag

### Analysis ###
GRINC_ALL_SRC      += $(addprefix $(SRC_GRINC_PREFIX),HeapPointsToFixpoint.chs GrPointsToAnalysis.cag)

GRINC_AG_FROM_CAG  += $(GRIN_BLD_VARIANT_PREFIX)GrPointsToAnalysis.ag
GRINC_HS_FROM_AG   += $(GRIN_BLD_VARIANT_PREFIX)GrPointsToAnalysis.hs
GRINC_HS_FROM_CHS  += $(GRIN_BLD_VARIANT_PREFIX)HeapPointsToFixpoint.hs

#points to analysis dependencies
$(GRIN_BLD_VARIANT_PREFIX)GrPointsToAnalysis.hs: $(GRIN_BLD_VARIANT_PREFIX)GrCAFNames.ag $(GRIN_BLD_VARIANT_PREFIX)Primitives.hs

### C-- ###

GRINC_CMM_FROMGRIN_CAG := $(wildcard $(SRC_GRINC_PREFIX)Cmm/FromGrin/*)
GRINC_CMM_FROMGRIN_AG  := $(patsubst $(SRC_GRINC_PREFIX)%.cag,$(GRIN_BLD_VARIANT_PREFIX)%.ag,$(GRINC_CMM_FROMGRIN_CAG))
GRINC_CMM_SRC_CAG      := $(patsubst %,$(SRC_GRINC_PREFIX)Cmm/%.cag,CmmCode CmmCodePretty FromGrin CmmCodeAbsSyn) $(GRINC_CMM_FROMGRIN_CAG)
GRINC_CMM_BUILDING_CHS := $(SRC_GRINC_PREFIX)Cmm/CmmBuilding.chs

GRINC_ALL_SRC          += $(GRINC_CMM_SRC_CAG) $(GRINC_CMM_BUILDING_CHS)
GRINC_AG_FROM_CAG      += $(patsubst $(SRC_GRINC_PREFIX)Cmm/%.cag,$(GRIN_BLD_VARIANT_PREFIX)Cmm/%.ag,$(GRINC_CMM_SRC_CAG))
GRINC_HS_FROM_AG       += $(patsubst %,$(GRIN_BLD_VARIANT_PREFIX)Cmm/%.hs,CmmCodePretty FromGrin)
GRINC_HS_FROM_CHS      += $(patsubst $(SRC_GRINC_PREFIX)%.chs,$(GRIN_BLD_VARIANT_PREFIX)%.hs,$(GRINC_CMM_BUILDING_CHS))
GRINC_HS_DATA_FROM_AG  += $(GRIN_BLD_VARIANT_PREFIX)Cmm/CmmCode.hs

# all C-- files will depend on GRINC_CMM_AST
GRINC_CMM_AST := $(GRIN_BLD_VARIANT_PREFIX)Cmm/CmmCodeAbsSyn.ag

#deps
$(GRIN_BLD_VARIANT_PREFIX)Cmm/FromGrin.hs: $(GRINC_CMM_FROMGRIN_AG) $(GRINC_CMM_AST) $(GRIN_BLD_VARIANT_PREFIX)Primitives.hs
$(patsubst %,$(GRIN_BLD_VARIANT_PREFIX)Cmm/%.hs,CmmCode CmmCodePretty): $(GRINC_CMM_AST)


### Build rules ####

#collect all hs source
GRINC_ALL_HS_SRC += $(GRINC_HS_FROM_AG) $(GRINC_HS_DATA_FROM_AG) $(GRINC_HS_FROM_CHS) $(GRINC_CMM_FROMGRIN_CAG)

$(GRINC_HS_DATA_FROM_AG): %.hs: %.ag
	$(call AGCC,$<,-dr)

$(GRINC_HS_FROM_AG): %.hs: %.ag
	$(call AGCC,$<,-cfspr)

$(GRINC_AG_FROM_CAG): $(GRIN_BLD_VARIANT_PREFIX)%.ag: $(SRC_GRINC_PREFIX)%.cag $(SHUFFLE)
	mkdir -p $(@D)
	$(SHUFFLE) $(LIB_EHC_SHUFFLE_DEFS) --gen=$(GRIN_VARIANT) --base=$(*F) --ag --preamble=no --lhs2tex=no --order="$(EHC_SHUFFLE_ORDER)" $< > $@

$(GRINC_HS_FROM_CHS): $(GRIN_BLD_VARIANT_PREFIX)%.hs: $(SRC_GRINC_PREFIX)%.chs $(SHUFFLE)
	mkdir -p $(@D)
	$(SHUFFLE) $(LIB_EHC_SHUFFLE_DEFS) --gen=$(GRIN_VARIANT) --base=$(*F) --hs --preamble=no --lhs2tex=no --order="$(EHC_SHUFFLE_ORDER)" $< > $@

$(GRINC_MAIN_HS): $(GRIN_BLD_VARIANT_PREFIX)%.hs: $(SRC_GRINC_PREFIX)%.chs $(SHUFFLE)
	mkdir -p $(@D)
	$(SHUFFLE) $(LIB_EHC_SHUFFLE_DEFS) --gen=$(GRIN_VARIANT) --base=Main --hs --preamble=no --lhs2tex=no --order="$(EHC_SHUFFLE_ORDER)" $< > $@


# top rules
$(patsubst %,grinc-variant-%,$(GRIN_VARIANTS)): grinc-variant-dflt

grinc-variant-dflt: $(GRINC_ALL_HS_SRC) $(LIB_EH_UTIL_INS_FLAG) $(LIB_EHC_INS_FLAG)
	mkdir -p $(dir $(GRINC_BLD_EXEC))
	$(GHC) --make $(GHC_OPTS) -package $(LIB_EH_UTIL_PKG_NAME) -package $(LIB_EHC_PKG_NAME) -i$(GRIN_BLD_VARIANT_PREFIX) $(GRINC_MAIN_HS) -o $(GRINC_BLD_EXEC)

# variant dispatch rules
$(patsubst $(BIN_PREFIX)%$(EXEC_SUFFIX),%,$(GRINC_ALL_EXECS)): %: $(BIN_PREFIX)%$(EXEC_SUFFIX)

$(GRINC_ALL_EXECS): %: $(GRINC_ALL_SRC) $(GRIN_ALL_SRC)
	$(MAKE) EHC_VARIANT=$(notdir $(*D)) GRIN_VARIANT=$(notdir $(*D)) grinc-variant-$(notdir $(*D))

