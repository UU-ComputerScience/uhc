###########################################################################################
# definitions
###########################################################################################

# location of RULER2 src
SRC_RULER2_PREFIX	:= $(SRC_PREFIX)ruler2/
RULER2_DEMO_PREFIX	:= $(SRC_RULER2_PREFIX)demo/

# location of ruler build
RULER2_BLD_PREFIX	:= $(BLD_PREFIX)ruler2/

# this file
RULER2_MKF			:= $(SRC_RULER2_PREFIX)files.mk

# sources + dpds, for .rul
RULER2_RULES_SRC_RL2					:= $(SRC_RULER2_PREFIX)RulerRules.rul

# main + sources + dpds
RULER2_MAIN			:= Ruler

RULER2_HS_MAIN_SRC_HS					:= $(addprefix $(SRC_RULER2_PREFIX),$(RULER2_MAIN).chs)
RULER2_HS_MAIN_DRV_HS					:= $(patsubst $(SRC_RULER2_PREFIX)%.chs,$(RULER2_BLD_PREFIX)%.hs,$(RULER2_HS_MAIN_SRC_HS))
RULER2_HS_DPDS_SRC_HS					:= $(patsubst %,$(SRC_RULER2_PREFIX)%.chs,\
											Version Common Err Opts AttrProps \
											NmParser ViewSel/Parser SelParser KeywParser Parser \
											ARule/Utils Expr/Utils Ty/Utils LaTeXFmtUtils Utils ViewSel/Utils \
											Gam FmGam ECnstrGam RwExprGam WrKindGam JdShpGam \
											Admin MkAdmin \
											Expr/ToAEqn \
											Scanner ScannerMachine \
											)
RULER2_HS_DPDS_DRV_HS					:= $(patsubst $(SRC_RULER2_PREFIX)%.chs,$(RULER2_BLD_PREFIX)%.hs,$(RULER2_HS_DPDS_SRC_HS))

RULER2_CHS_UTIL_SRC_CHS					:= $(patsubst %,$(SRC_RULER2_PREFIX)%.chs,Config)
RULER2_CHS_UTIL_DRV_HS					:= $(patsubst $(SRC_RULER2_PREFIX)%.chs,$(RULER2_BLD_PREFIX)%.hs,$(RULER2_CHS_UTIL_SRC_CHS))


# conditional turned off
ifeq ($(INCLUDE_DERIVED_MK),yes)
-include $(RULER2_BLD_PREFIX)files-ag-d-dep.mk
-include $(RULER2_BLD_PREFIX)files-ag-s-dep.mk
endif


RULER2_AG_DS_MAIN_SRC_AG				:=

RULER2_AG_ALL_DPDS_SRC_AG				:= $(pathsubst $(SRC_RULER2_PREFIX)%.cag,$(RULER2_BLD_PREFIX)%.ag,$(sort $(RULER2_AG_D_DPDS_SRC_AG) $(RULER2_AG_S_DPDS_SRC_AG)))
RULER2_AG_ALL_MAIN_SRC_AG				:= $(RULER2_AG_D_MAIN_SRC_AG) $(RULER2_AG_S_MAIN_SRC_AG) $(RULER2_AG_DS_MAIN_SRC_AG)
RULER2_AG_ALL_ODPDS_SRC_AG                              := $(sort $(RULER2_AG_D_ODPDS_SRC_AG) $(RULER2_AG_S_ODPDS_SRC_AG))


# Regenerate derived makefile
$(RULER2_BLD_PREFIX)files-ag-s-dep.mk : $(SRC_PREFIX)ruler2/files-ag-s.dep $(SHUFFLE) $(RULER2_AG_ALL_ODPDS_SRC_AG) $(RULER2_AG_ALL_MAIN_SRC_AG)
	mkdir -p $(RULER2_BLD_PREFIX)
	$(SHUFFLE) $(SRC_RULER2_PREFIX)files-ag-s.dep --dep --depnameprefix=RULER2_ --depsrcvar=SRC_RULER2_PREFIX --depdstvar=RULER2_BLD_PREFIX --depmainvar=RULER2_AG_S_MAIN_SRC_AG --depdpdsvar=RULER2_AG_S_DPDS_SRC_AG --deporigdpdsvar=RULER2_AG_S_ODPDS_SRC_AG --depbase=$(SRC_RULER2_PREFIX) > $(RULER2_BLD_PREFIX)files-ag-s-dep.mk

$(RULER2_BLD_PREFIX)files-ag-d-dep.mk : $(SRC_PREFIX)ruler2/files-ag-d.dep $(SHUFFLE) $(RULER2_AG_ALL_ODPDS_SRC_AG) $(RULER2_AG_ALL_MAIN_SRC_AG)
	mkdir -p $(RULER2_BLD_PREFIX)
	$(SHUFFLE) $(SRC_RULER2_PREFIX)files-ag-d.dep --dep --depnameprefix=RULER2_ --depsrcvar=SRC_RULER2_PREFIX --depdstvar=RULER2_BLD_PREFIX --depmainvar=RULER2_AG_D_MAIN_SRC_AG --depdpdsvar=RULER2_AG_D_DPDS_SRC_AG --deporigdpdsvar=RULER2_AG_D_ODPDS_SRC_AG --depbase=$(SRC_RULER2_PREFIX) > $(RULER2_BLD_PREFIX)files-ag-d-dep.mk

# all src
RULER2_ALL_SRC							:= $(RULER2_AG_ALL_MAIN_SRC_AG) $(RULER2_AG_ALL_DPDS_SRC_AG) $(RULER2_HS_MAIN_SRC_HS) \
											$(RULER2_CHS_UTIL_SRC_CHS) $(RULER2_HS_DPDS_SRC_HS) $(RULER2_MKF) $(RULER2_RULES_SRC_RL2)


# derived
RULER2_AG_D_MAIN_DRV_HS					:= $(patsubst $(SRC_RULER2_PREFIX)%.cag,$(RULER2_BLD_PREFIX)%.hs,$(RULER2_AG_D_MAIN_SRC_AG))
RULER2_AG_S_MAIN_DRV_HS					:= $(patsubst $(SRC_RULER2_PREFIX)%.cag,$(RULER2_BLD_PREFIX)%.hs,$(RULER2_AG_S_MAIN_SRC_AG))
RULER2_AG_DS_MAIN_DRV_HS				:= $(patsubst $(SRC_RULER2_PREFIX)%.cag,$(RULER2_BLD_PREFIX)%.hs,$(RULER2_AG_DS_MAIN_SRC_AG))
RULER2_AG_ALL_MAIN_DRV_HS				:= $(RULER2_AG_D_MAIN_DRV_HS) $(RULER2_AG_S_MAIN_DRV_HS) $(RULER2_AG_DS_MAIN_DRV_HS)

RULER2_HS_ALL_DRV_HS					:= $(RULER2_HS_MAIN_DRV_HS) $(RULER2_HS_DPDS_DRV_HS)

# binary/executable
RULER2_NAME								:= ruler
RULER2_BLD_EXEC							:= $(BIN_PREFIX)$(RULER2_NAME)$(EXEC_SUFFIX)
RULER2									:= $(RULER2_BLD_EXEC)

###########################################################################################
# targets
###########################################################################################

$(RULER2_NAME): $(RULER2_BLD_EXEC)

ruler-clean:
	rm -rf $(RULER2_BLD_EXEC) $(RULER2_BLD_PREFIX)

###########################################################################################
# rules
###########################################################################################

$(RULER2_BLD_EXEC): $(RULER2_AG_ALL_MAIN_DRV_HS) $(RULER2_HS_ALL_DRV_HS) $(RULER2_CHS_UTIL_DRV_HS) $(LIB_EH_UTIL_INS_FLAG)
	$(GHC) --make $(GHC_OPTS) $(GHC_OPTS_WHEN_EHC) -package $(LIB_EH_UTIL_PKG_NAME) -i$(RULER2_BLD_PREFIX) $(RULER2_BLD_PREFIX)$(RULER2_MAIN).hs -o $@
	$(STRIP) $@

$(RULER2_BLD_PREFIX)%.ag: $(SRC_RULER2_PREFIX)%.cag $(SHUFFLE)
	mkdir -p $(@D); \
	$(SHUFFLE) --gen-reqm=1 --base=$(*F) --ag --preamble=no --lhs2tex=no --variant-order="1" $< > $@

$(RULER2_AG_D_MAIN_DRV_HS): $(RULER2_BLD_PREFIX)%.hs: $(RULER2_BLD_PREFIX)%.ag
	mkdir -p $(@D) ; \
	$(AGC) -dr $(UUAGC_OPTS_WHEN_EHC) -P$(RULER2_BLD_PREFIX) -o $@ $<

$(RULER2_AG_S_MAIN_DRV_HS): $(RULER2_BLD_PREFIX)%.hs: $(RULER2_BLD_PREFIX)%.ag
	mkdir -p $(@D) ; \
	$(AGC) -cfspr $(UUAGC_OPTS_WHEN_EHC) -P$(RULER2_BLD_PREFIX) -o $@ $<

$(RULER2_AG_DS_MAIN_DRV_HS): $(RULER2_BLD_PREFIX)%.hs: $(RULER2_BLD_PREFIX)%.ag
	mkdir -p $(@D) ; \
	$(AGC) --module=$(*F) -dcfspr $(UUAGC_OPTS_WHEN_EHC) -P$(RULER2_BLD_PREFIX) -o $@ $<

$(RULER2_HS_ALL_DRV_HS): $(RULER2_BLD_PREFIX)%.hs: $(SRC_RULER2_PREFIX)%.chs
	mkdir -p $(@D) ; \
	$(SHUFFLE) --gen-reqm=1 --base=$(*F) --hs --preamble=no --lhs2tex=no --variant-order="1" $< > $@

$(RULER2_CHS_UTIL_DRV_HS): $(RULER2_BLD_PREFIX)%.hs: $(SRC_RULER2_PREFIX)%.chs $(SHUFFLE)
	mkdir -p $(@D) ; \
	$(SHUFFLE) --gen-reqm=1 --base=$(*F) --hs --preamble=no --lhs2tex=no --variant-order="1" $< > $@

###########################################################################################
# demo
###########################################################################################

### demo stuff
RULER2_DEMO_AG_MAIN				:= RulerDemoMain
RULER2_DEMO_SRC_CAG_MAIN		:= $(RULER2_DEMO_PREFIX)$(RULER2_DEMO_AG_MAIN).cag
RULER2_DEMO_DRV_AG_MAIN			:= $(RULER2_DEMO_SRC_CAG_MAIN:.cag=.ag)
RULER2_DEMO_DRV_AG_MAIN_TEX		:= $(RULER2_DEMO_SRC_CAG_MAIN:.cag=.tex)
RULER2_DEMO_DRV_HS_MAIN			:= $(RULER2_DEMO_DRV_AG_MAIN:.ag=.hs)

RULER2_DEMO_SRC_CHS_UTILS		:= $(RULER2_DEMO_PREFIX)RulerDemoUtils.chs
RULER2_DEMO_DRV_HS_UTILS		:= $(RULER2_DEMO_SRC_CHS_UTILS:.chs=.hs)
RULER2_DEMO_DRV_HS_UTILS_TEX	:= $(RULER2_DEMO_SRC_CHS_UTILS:.chs=.tex)

RULER2_DEMO_EXEC				:= $(BLD_BIN_PREFIX)rulerdemo$(EXEC_SUFFIX)

RULER2_DEMO_RUL_BASE		:= rulerDemoRL
RULER2_DEMO_AG_BASE			:= rulerDemoAG
RULER2_DEMO_AGWCOPY_BASE	:= rulerDemoAGWithCopy
RULER2_DEMO_SRC_CRL			:= $(RULER2_DEMO_PREFIX)$(RULER2_DEMO_RUL_BASE).crl2
RULER2_DEMO_DRV_CAG			:= $(RULER2_DEMO_PREFIX)$(RULER2_DEMO_AG_BASE).cag
RULER2_DEMO_DRV_WCOPY_CAG	:= $(RULER2_DEMO_PREFIX)$(RULER2_DEMO_AGWCOPY_BASE).cag
RULER2_DEMO_DRV_LCTEX		:= $(RULER2_DEMO_PREFIX)demo.lctex
RULER2_DEMO_DRV_CTEX		:= $(RULER2_DEMO_DRV_LCTEX:.lctex=.ctex)
RULER2_DEMO_DRV_RL2			:= $(RULER2_DEMO_DRV_LCTEX:.lctex=.rl2)
RULER2_DEMO_DRV_LRTEX		:= $(RULER2_DEMO_DRV_LCTEX:.lctex=.lrtex)
RULER2_DEMO_DRV_RTEX		:= $(RULER2_DEMO_DRV_LCTEX:.lctex=.rtex)
RULER2_DEMO_DRV_LATEX		:= $(RULER2_DEMO_DRV_LCTEX:.lctex=.latex)
RULER2_DEMO_DRV_ATEX		:= $(RULER2_DEMO_DRV_LCTEX:.lctex=.atex)
RULER2_DEMO_DRV_AG			:= $(RULER2_DEMO_DRV_LCTEX:.lctex=.ag)

RULER2_DEMO_ALL_DRV_TEX		:= $(RULER2_DEMO_DRV_HS_UTILS_TEX) $(RULER2_DEMO_DRV_AG_MAIN_TEX)

RULER2_DEMO_ALL_SRC			:= $(RULER2_DEMO_SRC_CRL) $(RULER2_DEMO_SRC_CAG_MAIN) $(RULER2_DEMO_SRC_CHS_UTILS) 

# chunks for inclusion in main text by shuffle
RULER2_ALL_CHUNK_SRC		:= $(RULER2_DEMO_SRC_CRL) $(RULER2_DEMO_DRV_CAG) $(RULER2_DEMO_DRV_WCOPY_CAG) $(RULER2_DEMO_SRC_CAG_MAIN) $(RULER2_DEMO_SRC_CHS_UTILS) \
								$(RULER2_CHS_UTIL_SRC_CHS)

# chunk view order for demo src
RULER2_DEMO_RULER2_ORDER	:= 1 < 2 < 3
RULER2_DEMO_RULER2_FINAL	:= 3

# configuration of ruler, to be done on top level
RULER2_DEMO_MARK_CHANGES_CFG	:= --markchanges="E - AG"

# distribution
RULER2_DIST_FILES			:= $(RULER2_ALL_SRC) $(RULER2_DEMO_ALL_SRC) \
								$(addprefix $(SRC_RULER2_PREFIX),Makefile README) \
								$(wildcard $(RULER2_DEMO_PREFIX)tst*)

# make rules
$(RULER2_DEMO_DRV_LCTEX): $(RULER2_DEMO_SRC_CRL) $(SHUFFLE)
	$(SHUFFLE) --gen-reqm=all --latex --variant-order="$(RULER2_DEMO_RULER2_ORDER)" --base=$(RULER2_DEMO_RUL_BASE) --lhs2tex=yes $< > $@

$(RULER2_DEMO_DRV_CTEX): $(RULER2_DEMO_DRV_LCTEX)
	$(LHS2TEX_CMD) $(LHS2TEX_OPTS_POLY) $< > $@

$(RULER2_DEMO_DRV_RL2): $(RULER2_DEMO_SRC_CRL) $(SHUFFLE)
	$(SHUFFLE) --gen-reqm=$(RULER2_DEMO_RULER2_FINAL) --plain --variant-order="$(RULER2_DEMO_RULER2_ORDER)"  --lhs2tex=no $< > $@

$(RULER2_DEMO_DRV_LRTEX): $(RULER2_DEMO_DRV_RL2) $(RULER2)
	$(RULER2) $(RULER2_OPTS) --lhs2tex --selrule="(E - *).(*).(*)" $(RULER2_DEMO_MARK_CHANGES_CFG) --base=rulerDemo $< > $@

$(RULER2_DEMO_DRV_RTEX): $(RULER2_DEMO_DRV_LRTEX)
	$(LHS2TEX_CMD) $(LHS2TEX_OPTS_POLY) $< > $@

$(RULER2_DEMO_DRV_CAG): $(RULER2_DEMO_DRV_RL2) $(RULER2)
	$(RULER2) $(RULER2_OPTS) --ag --ATTR --DATA --selrule="(3).(*).(*)" --wrapshuffle  --base=$(RULER2_DEMO_AG_BASE) $< > $@

$(RULER2_DEMO_DRV_WCOPY_CAG): $(RULER2_DEMO_DRV_RL2) $(RULER2)
	$(RULER2) $(RULER2_OPTS) --ag --ATTR --DATA --selrule="(3).(*).(*)" --wrapshuffle --copyelim=no --base=$(RULER2_DEMO_AGWCOPY_BASE) $< > $@

$(RULER2_DEMO_DRV_AG): $(RULER2_DEMO_DRV_CAG) $(SHUFFLE)
	$(SHUFFLE) --gen-reqm=$(RULER2_DEMO_RULER2_FINAL) --plain --variant-order="$(RULER2_DEMO_RULER2_ORDER)"  --lhs2tex=no $< > $@

$(RULER2_DEMO_DRV_LATEX): $(RULER2_DEMO_DRV_CAG) $(SHUFFLE)
	$(SHUFFLE) --gen-reqm=$(RULER2_DEMO_RULER2_FINAL) --latex --variant-order="$(RULER2_DEMO_RULER2_ORDER)" --base=$(RULER2_DEMO_AG_BASE) --lhs2tex=yes $< > $@

$(RULER2_DEMO_DRV_ATEX): $(RULER2_DEMO_DRV_LATEX)
	$(LHS2TEX_CMD) $(LHS2TEX_OPTS_POLY) $< > $@

$(RULER2_DEMO_DRV_HS_UTILS): $(RULER2_DEMO_SRC_CHS_UTILS) $(SHUFFLE)
	$(SHUFFLE) --gen-reqm=$(RULER2_DEMO_RULER2_FINAL) --hs --variant-order="$(RULER2_DEMO_RULER2_ORDER)" --preamble=no --lhs2tex=no $< > $@

$(RULER2_DEMO_DRV_HS_UTILS_TEX): $(RULER2_DEMO_SRC_CHS_UTILS) $(SHUFFLE)
	$(SHUFFLE) --gen-reqm=$(RULER2_DEMO_RULER2_FINAL) --latex --variant-order="$(RULER2_DEMO_RULER2_ORDER)" --base=rulerDemoUtils --lhs2tex=yes $< | $(LHS2TEX_CMD) $(LHS2TEX_OPTS_POLY) > $@

$(RULER2_DEMO_DRV_AG_MAIN): $(RULER2_DEMO_SRC_CAG_MAIN) $(SHUFFLE)
	$(SHUFFLE) --gen-reqm=$(RULER2_DEMO_RULER2_FINAL) --ag --variant-order="$(RULER2_DEMO_RULER2_ORDER)" --base=Main --preamble=no --lhs2tex=no $< > $@

$(RULER2_DEMO_DRV_AG_MAIN_TEX): $(RULER2_DEMO_SRC_CAG_MAIN) $(SHUFFLE)
	$(SHUFFLE) --gen-reqm=$(RULER2_DEMO_RULER2_FINAL) --latex --variant-order="$(RULER2_DEMO_RULER2_ORDER)" --base=rulerDemoMain --lhs2tex=yes $< | $(LHS2TEX_CMD) $(LHS2TEX_OPTS_POLY) > $@

$(RULER2_DEMO_DRV_HS_MAIN): $(RULER2_DEMO_DRV_AG_MAIN) $(RULER2_DEMO_DRV_AG)
	$(AGC) -csdfr $(UUAGC_OPTS_WHEN_EHC) -P$(RULER2_DEMO_PREFIX) $<

$(RULER2_DEMO_EXEC): $(RULER2_DEMO_DRV_HS_MAIN) $(RULER2_DEMO_DRV_HS_UTILS) $(LIB_EH_UTIL_INS_FLAG)
	mkdir -p $(@D)
	$(GHC) --make $(GHC_OPTS) -i$(RULER2_DEMO_PREFIX) $(RULER2_DEMO_DRV_HS_MAIN) -o $@

