# variant, to be configured on top level
# see variant.mk

# location of ehc src:
# see shared.mk

# this file + other mk files
EHC_MKF									:= $(patsubst %,$(SRC_EHC_PREFIX)%.mk,files1 files2 shared)

# end products, binary, executable, etc
EHC_EXEC_NAME							:= ehc
EHC_HADDOCK_NAME						:= hdoc
EHC_BLD_EXEC							:= $(EHC_BIN_VARIANT_PREFIX)$(EHC_EXEC_NAME)$(EXEC_SUFFIX)
EHC_ALL_PUB_EXECS						:= $(patsubst %,$(EHC_BIN_PREFIX)%/$(EHC_EXEC_NAME)$(EXEC_SUFFIX),$(EHC_PUB_VARIANTS))
EHC_ALL_EXECS							:= $(patsubst %,$(EHC_BIN_PREFIX)%/$(EHC_EXEC_NAME)$(EXEC_SUFFIX),$(EHC_VARIANTS))
EHC_ALL_HADDOCKS						:= $(patsubst %,$(EHC_HDOC_PREFIX)%/$(EHC_HADDOCK_NAME),$(EHC_VARIANTS))

# sources + dpds, for .rul
EHC_RULES_1_SRC_RUL						:= $(SRC_EHC_PREFIX)rules.rul
EHC_RULES_2_SRC_RUL						:= $(SRC_EHC_PREFIX)rules2.rul
EHC_RULES_3_SRC_RL2						:= $(SRC_EHC_RULES_PREFIX)EhcRulesOrig.rul

EHC_RULER_RULES							:= EHRulerRules
EHC_RULES_3_DRV_CAG						:= $(EHC_BLD_VARIANT_PREFIX)$(EHC_RULER_RULES).cag
EHC_RULES_3_DRV_AG						:= $(EHC_RULES_3_DRV_CAG:.cag=.ag)

EHC_RULES_4_MAIN_SRC_RUL				:= $(patsubst %,$(SRC_EHC_RULES_PREFIX)%.rul,EhcRulesExpr2 EhcRulesTyMatch EhcRulesTyElimAlt)
EHC_RULES_4_DPDS_SRC_RUL				:= $(patsubst %,$(SRC_EHC_RULES_PREFIX)%.rul, \
													EhcRulesShared EhcRulesShared2 \
													EhcRulesAST EhcRulesCommon \
													EhcRulesRelations EhcRulesCommonSchemes EhcRulesSchemes EhcRulesSchemes2 \
											)
EHC_RULES_ALL_SRC						:= $(EHC_RULES_1_SRC_RUL) $(EHC_RULES_2_SRC_RUL) $(EHC_RULES_3_SRC_RL2) $(EHC_RULES_4_MAIN_SRC_RUL) $(EHC_RULES_4_DPDS_SRC_RUL)

# library
# derived stuff
LIB_EHC_CABAL_DRV						:= $(EHC_BLD_LIBEHC_VARIANT_PREFIX)lib-$(LIB_EHC_BASE)$(EHC_VARIANT).cabal
LIB_EHC_SETUP_HS_DRV					:= $(EHC_BLD_LIBEHC_VARIANT_PREFIX)Setup.hs
LIB_EHC_SETUP2							:= $(EHC_BLD_LIBEHC_VARIANT_PREFIX)setup$(EXEC_SUFFIX)
LIB_EHC_SETUP							:= ./setup$(EXEC_SUFFIX)

# special files
# file with signature of code
EHC_HS_SIG_MAIN							:= SourceCodeSig
EHC_HS_SIG_DRV_HS						:= $(EHC_BLD_LIB_HS_VARIANT_PREFIX)$(EHC_HS_SIG_MAIN).hs

# main + sources + dpds, for .chs
EHC_MAIN								:= EHC
EHC_HS_MAIN_SRC_CHS						:= $(patsubst %,$(SRC_EHC_PREFIX)%.chs,$(EHC_MAIN))
EHC_HS_MAIN_DRV_HS						:= $(patsubst $(SRC_EHC_PREFIX)%.chs,$(EHC_BLD_VARIANT_PREFIX)%.hs,$(EHC_HS_MAIN_SRC_CHS))

EHC_HS_UTIL_SRC_CHS						:= $(patsubst %,$(SRC_EHC_PREFIX)%.chs,\
													Substitutable Gam VarMp Deriving Module Config BuiltinPrims \
													CHR CHR/Key CHR/Constraint CHR/Solve \
													Pred Pred/ToCHR Pred/CHR Pred/Evidence Pred/EvidenceToCore Pred/Heuristics Pred/CommonCHR Pred/RedGraph \
													Base/Opts Base/Common Base/Builtin Base/HsName Base/Debug Base/Trie Base/CfgPP Base/ForceEval \
													NameAspect \
													Scanner/Common Scanner/Machine Scanner/Scanner Scanner/Token Scanner/TokenParser \
													Base/Parser Ty/Parser EH/Parser HS/Parser HI/Parser Core/Parser GrinCode/Parser \
													Ty/FitsInCommon Ty/FitsIn \
													Core/Utils \
													Gam/Utils \
													Annotations/StateMachine Annotations/Constraints Annotations/ConstraintSolver Annotations/BelownessSolver Annotations/VarianceSolver Annotations/UniquenessSolver \
													Base/HtmlCommon \
													Debug/HighWaterMark \
											)
EHC_HS_UTIL_DRV_HS						:= $(patsubst $(SRC_EHC_PREFIX)%.chs,$(EHC_BLD_LIB_HS_VARIANT_PREFIX)%.hs,$(EHC_HS_UTIL_SRC_CHS))

EHC_HS_UTILCPP_SRC_CHS					:= $(patsubst %,$(SRC_EHC_PREFIX)%.chs,\
													ConfigDefines \
											)
EHC_HS_UTILCPP_DRV_HS					:= $(patsubst $(SRC_EHC_PREFIX)%.chs,$(EHC_BLD_LIB_HS_VARIANT_PREFIX)%.hs,$(EHC_HS_UTILCPP_SRC_CHS))

EHC_HS_UTIL_SRC_CC						:= $(patsubst %,$(SRC_EHC_PREFIX)%.cc,\
													Debug/mblocks \
											)
EHC_HS_UTIL_DRV_C						:= $(patsubst $(SRC_EHC_PREFIX)%.cc,$(EHC_BLD_LIB_HS_VARIANT_PREFIX)%.c,$(EHC_HS_UTIL_SRC_CC))

EHC_HS_ALL_SRC_CHS						:= $(EHC_HS_MAIN_SRC_CHS) $(EHC_HS_UTIL_SRC_CHS) $(EHC_HS_UTILCPP_SRC_CHS)
EHC_HS_ALL_DRV_HS_NO_MAIN				:= $(EHC_HS_UTIL_DRV_HS) $(EHC_HS_UTILCPP_DRV_HS)
EHC_HS_ALL_DRV_HS						:= $(EHC_HS_MAIN_DRV_HS) $(EHC_HS_ALL_DRV_HS_NO_MAIN)


# main + sources + dpds, for .cag
include $(EHC_BLD_LIB_HS_VARIANT_PREFIX)files-ag-s-dep.mk
include $(EHC_BLD_LIB_HS_VARIANT_PREFIX)files-ag-d-dep.mk

EHC_AG_DS_MAIN_SRC_CAG					:= 
EHC_AG_ALL_MAIN_SRC_CAG					:= $(EHC_AG_D_MAIN_SRC_CAG) $(EHC_AG_S_MAIN_SRC_CAG) $(EHC_AG_DS_MAIN_SRC_CAG)
EHC_AG_ALL_DPDS_SRC_CAG					:= $(sort $(EHC_AG_D_DPDS_SRC_CAG) $(EHC_AG_S_DPDS_SRC_CAG))

$(patsubst $(SRC_EHC_PREFIX)%.cag,$(EHC_BLD_LIB_HS_VARIANT_PREFIX)%.hs,$(EHC_EH_MAINAG_MAIN_SRC_AG)) \
										: $(EHC_RULES_3_DRV_AG)


# Regenerate derived makefiles
$(EHC_BLD_LIB_HS_VARIANT_PREFIX)files-ag-s-dep.mk : $(SRC_EHC_PREFIX)/files-ag-s.dep $(SHUFFLE) $(EHC_AG_S_ODPDS_SRC_CAG) $(EHC_AG_S_MAIN_SRC_CAG)
	mkdir -p $(EHC_BLD_LIB_HS_VARIANT_PREFIX)
	$(SHUFFLE) $(SRC_EHC_PREFIX)files-ag-s.dep --dep \
	  --depnameprefix=EHC_ \
	  --depsrcvar=SRC_EHC_PREFIX \
	  --depdstvar=EHC_BLD_LIB_HS_VARIANT_PREFIX \
	  --depmainvar=EHC_AG_S_MAIN_SRC_CAG \
	  --depdpdsvar=EHC_AG_S_DPDS_SRC_CAG \
	  --deporigdpdsvar=EHC_AG_S_ODPDS_SRC_CAG \
	  --depbase=$(SRC_EHC_PREFIX) \
	  --depterm="EHRulerRules>" \
	  --depign="EHRulerRules EHRulerRules.cag" \
	    > $(EHC_BLD_LIB_HS_VARIANT_PREFIX)files-ag-s-dep.mk

$(EHC_BLD_LIB_HS_VARIANT_PREFIX)files-ag-d-dep.mk : $(SRC_EHC_PREFIX)/files-ag-d.dep $(SHUFFLE) $(EHC_AG_D_ODPDS_SRC_CAG) $(EHC_AG_D_MAIN_SRC_CAG)
	mkdir -p $(EHC_BLD_LIB_HS_VARIANT_PREFIX)
	$(SHUFFLE) $(SRC_EHC_PREFIX)files-ag-d.dep --dep \
	  --depnameprefix=EHC_ \
	  --depsrcvar=SRC_EHC_PREFIX \
	  --depdstvar=EHC_BLD_LIB_HS_VARIANT_PREFIX \
	  --depmainvar=EHC_AG_D_MAIN_SRC_CAG \
	  --depdpdsvar=EHC_AG_D_DPDS_SRC_CAG \
	  --deporigdpdsvar=EHC_AG_D_ODPDS_SRC_CAG \
	  --depbase=$(SRC_EHC_PREFIX) \
	  --depterm="EHRulerRules>" \
	  --depign="EHRulerRules EHRulerRules.cag" \
	    > $(EHC_BLD_LIB_HS_VARIANT_PREFIX)files-ag-d-dep.mk

# all src
EHC_ALL_CHUNK_SRC						:= $(EHC_AG_ALL_MAIN_SRC_CAG) $(EHC_AG_ALL_DPDS_SRC_CAG) $(EHC_HS_ALL_SRC_CHS)
EHC_ALL_SRC								:= $(EHC_ALL_CHUNK_SRC) $(EHC_RULES_ALL_SRC) $(EHC_MKF)

# distribution
EHC_DIST_FILES							:= $(EHC_ALL_SRC)

# configuration of tools dependend on variant
EHC_UUAGC_OPTS_WHEN_UHC_AST_DATA_99		:= $(UUAGC_OPTS_WHEN_UHC_AST_DATA)
EHC_UUAGC_OPTS_WHEN_UHC_AST_DATA_100	:= $(EHC_UUAGC_OPTS_WHEN_UHC_AST_DATA_99)
EHC_UUAGC_OPTS_WHEN_UHC_AST_DATA_101	:= $(EHC_UUAGC_OPTS_WHEN_UHC_AST_DATA_100)

EHC_UUAGC_OPTS_WHEN_UHC_AST_SEM_99		:= $(UUAGC_OPTS_WHEN_UHC_AST_SEM)
EHC_UUAGC_OPTS_WHEN_UHC_AST_SEM_100		:= $(EHC_UUAGC_OPTS_WHEN_UHC_AST_SEM_99)
EHC_UUAGC_OPTS_WHEN_UHC_AST_SEM_101		:= $(EHC_UUAGC_OPTS_WHEN_UHC_AST_SEM_100)

# what is based on which Ruler view
EHC_ON_RULES_VIEW_1						:= K
EHC_ON_RULES_VIEW_2						:= C
EHC_ON_RULES_VIEW_3						:= HM
EHC_ON_RULES_VIEW_4						:= EX
EHC_ON_RULES_VIEW_4_2					:= I2
EHC_ON_RULES_VIEW_5						:= DT
EHC_ON_RULES_VIEW_6						:= DT
EHC_ON_RULES_VIEW_7						:= DT
EHC_ON_RULES_VIEW_7_2					:= ANN
EHC_ON_RULES_VIEW_8						:= CG
EHC_ON_RULES_VIEW_9						:= P
EHC_ON_RULES_VIEW_10					:= P
EHC_ON_RULES_VIEW_11					:= TS
EHC_ON_RULES_VIEW_12					:= EP
EHC_ON_RULES_VIEW_13					:= EP
EHC_ON_RULES_VIEW_14					:= EP
EHC_ON_RULES_VIEW_15					:= EP
EHC_ON_RULES_VIEW_16					:= EP
EHC_ON_RULES_VIEW_20					:= MD
EHC_ON_RULES_VIEW_95					:= MD
EHC_ON_RULES_VIEW_96					:= MD
EHC_ON_RULES_VIEW_97					:= NUM
EHC_ON_RULES_VIEW_98					:= NUM
EHC_ON_RULES_VIEW_99					:= HS
EHC_ON_RULES_VIEW_100					:= HS
EHC_ON_RULES_VIEW_101					:= HS

# what is generated by Ruler
EHC_BY_RULER_GROUPS_BASE				:= expr.base patexpr.base tyexpr.base decl.base
EHC_BY_RULER_RULES_BASE3				:= e.int e.char e.var e.con
EHC_BY_RULER_RULES_BASE2				:= $(EHC_BY_RULER_RULES_BASE3) \
											e.apptop e.app e.app.f e.lam e.let e.ann \
											t.con t.app t.wild t.quant t.var t.var.w \
											d.tysig d.val
EHC_BY_RULER_RULES_BASE1				:= $(EHC_BY_RULER_RULES_BASE2) \
											p.int p.char p.con p.apptop p.app p.var p.varas p.ann
EHC_BY_RULER_RULES_1					:= $(EHC_BY_RULER_RULES_BASE1)
EHC_BY_RULER_RULES_2					:= $(EHC_BY_RULER_RULES_1)
EHC_BY_RULER_RULES_3					:= $(EHC_BY_RULER_RULES_2) 
EHC_BY_RULER_RULES_4					:= $(EHC_BY_RULER_RULES_BASE1)
EHC_BY_RULER_RULES_4_2					:= $(EHC_BY_RULER_RULES_4)
EHC_BY_RULER_RULES_5					:= $(EHC_BY_RULER_RULES_BASE3) e.str p.str
EHC_BY_RULER_RULES_6					:= $(EHC_BY_RULER_RULES_5)
EHC_BY_RULER_RULES_7					:= $(EHC_BY_RULER_RULES_6)
EHC_BY_RULER_RULES_7_2					:= $(EHC_BY_RULER_RULES_6)
#EHC_BY_RULER_RULES_8					:= $(EHC_BY_RULER_RULES_7) e.float p.float
EHC_BY_RULER_RULES_8					:= $(EHC_BY_RULER_RULES_7)
EHC_BY_RULER_RULES_9					:= $(EHC_BY_RULER_RULES_8)
EHC_BY_RULER_RULES_10					:= $(EHC_BY_RULER_RULES_9)
EHC_BY_RULER_RULES_11					:= $(EHC_BY_RULER_RULES_10)
EHC_BY_RULER_RULES_12					:= $(EHC_BY_RULER_RULES_11)
EHC_BY_RULER_RULES_13					:= $(EHC_BY_RULER_RULES_12)
EHC_BY_RULER_RULES_14					:= $(EHC_BY_RULER_RULES_13)
EHC_BY_RULER_RULES_15					:= $(EHC_BY_RULER_RULES_14)
EHC_BY_RULER_RULES_16					:= $(EHC_BY_RULER_RULES_15)
EHC_BY_RULER_RULES_20					:= $(EHC_BY_RULER_RULES_16)
EHC_BY_RULER_RULES_95					:= $(EHC_BY_RULER_RULES_20)
EHC_BY_RULER_RULES_96					:= $(EHC_BY_RULER_RULES_95)
EHC_BY_RULER_RULES_97					:= $(EHC_BY_RULER_RULES_96) e.iint
EHC_BY_RULER_RULES_98					:= $(EHC_BY_RULER_RULES_97)
EHC_BY_RULER_RULES_99					:= $(EHC_BY_RULER_RULES_98)
EHC_BY_RULER_RULES_100					:= $(EHC_BY_RULER_RULES_99)
EHC_BY_RULER_RULES_101					:= $(EHC_BY_RULER_RULES_100)

# derived
EHC_AG_D_MAIN_DRV_AG					:= $(patsubst $(SRC_EHC_PREFIX)%.cag,$(EHC_BLD_LIB_HS_VARIANT_PREFIX)%.ag,$(EHC_AG_D_MAIN_SRC_CAG))
EHC_AG_S_MAIN_DRV_AG					:= $(patsubst $(SRC_EHC_PREFIX)%.cag,$(EHC_BLD_LIB_HS_VARIANT_PREFIX)%.ag,$(EHC_AG_S_MAIN_SRC_CAG))
EHC_AG_DS_MAIN_DRV_AG					:= $(patsubst $(SRC_EHC_PREFIX)%.cag,$(EHC_BLD_LIB_HS_VARIANT_PREFIX)%.ag,$(EHC_AG_DS_MAIN_SRC_CAG))
EHC_AG_ALL_MAIN_DRV_AG					:= $(EHC_AG_D_MAIN_DRV_AG) $(EHC_AG_S_MAIN_DRV_AG) $(EHC_AG_DS_MAIN_DRV_AG)

EHC_AG_D_MAIN_DRV_HS					:= $(EHC_AG_D_MAIN_DRV_AG:.ag=.hs)
EHC_AG_S_MAIN_DRV_HS					:= $(EHC_AG_S_MAIN_DRV_AG:.ag=.hs)
EHC_AG_DS_MAIN_DRV_HS					:= $(EHC_AG_DS_MAIN_DRV_AG:.ag=.hs)
EHC_AG_ALL_MAIN_DRV_HS					:= $(EHC_AG_D_MAIN_DRV_HS) $(EHC_AG_S_MAIN_DRV_HS) $(EHC_AG_DS_MAIN_DRV_HS)

EHC_AG_ALL_DPDS_DRV_AG					:= $(patsubst $(SRC_EHC_PREFIX)%.cag,$(EHC_BLD_LIB_HS_VARIANT_PREFIX)%.ag,$(EHC_AG_ALL_DPDS_SRC_CAG))

# lib installed ag
INS_EHC_LIB_ALL_AG_NAMES				:= HS/AbsSyn EH/AbsSyn Ty/AbsSyn GrinCode/AbsSyn
INS_EHC_LIB_ALL_AG						:= $(patsubst %,$(INS_EHC_LIB_AG_PREFIX)%.ag,$(INS_EHC_LIB_ALL_AG_NAMES))
INSABS_EHC_LIB_ALL_AG					:= $(patsubst %,$(INSABS_EHC_LIB_AG_PREFIX)%.ag,$(INS_EHC_LIB_ALL_AG_NAMES))

# all dependents for a variant to kick of building
EHC_ALL_DPDS_NO_MAIN					:= $(EHC_HS_ALL_DRV_HS_NO_MAIN) $(EHC_AG_ALL_MAIN_DRV_HS) $(EHC_HS_SIG_DRV_HS) $(EHC_HS_UTIL_DRV_C)
EHC_ALL_DPDS							:= $(EHC_HS_ALL_DRV_HS) $(EHC_AG_ALL_MAIN_DRV_HS) $(EHC_HS_SIG_DRV_HS)

EHC_ALL_DPDS_NOPREPROC					:= $(subst $(EHC_BLD_LIB_HS_VARIANT_PREFIX)ConfigDefines.hs, ,$(EHC_ALL_DPDS))


# variant dispatch rules
$(patsubst %,echo-gen-by-ruler-%,$(EHC_VARIANTS)):
	@v=`echo $@ | sed -e 's/.*ruler-\([0-9_]*\)/\1/'` ; \
	$(MAKE) EHC_VARIANT=$$v echo-gen-by-ruler

# rules for meta info: which rules are gen by ruler
echo-gen-by-ruler:
	@echo "\verb|$(EHC_VARIANT)| & \textit{$(EHC_ON_RULES_VIEW_$(EHC_VARIANT))} & " ; \
	for r in $(sort $(EHC_BY_RULER_RULES_$(EHC_VARIANT))) ; \
	do \
	  echo -n "\textRL{$$r}\hspace{.5em} " ; \
	done ; \
	echo "\\\\"

# rules for ehc library construction
$(LIB_EHC_CABAL_DRV): $(EHC_ALL_DPDS_NO_MAIN) $(EHC_MKF)
	mkdir -p $(@D)
	$(call GEN_CABAL \
		, $(LIB_EHC_PKG_NAME) \
		, $(EH_VERSION) \
		, $(LIB_EH_UTIL_PKG_NAME) \
		, $(CABAL_OPT_ALLOW_UNDECIDABLE_INSTANCES) \
		, Part of EH$(EHC_VARIANT) compiler packaged as library \
		, $(subst $(PATH_SEP),.,$(patsubst $(EHC_BLD_LIB_HS_VARIANT_PREFIX)%.hs,$(LIB_EHC_QUAL_PREFIX)%,$(shell $(FILTER_NONEMP_FILES) $(EHC_HS_UTIL_DRV_HS) $(EHC_HS_UTILCPP_DRV_HS) $(EHC_AG_ALL_MAIN_DRV_HS) $(EHC_HS_SIG_DRV_HS)))) \
		, $(patsubst $(EHC_BLD_LIBEHC_VARIANT_PREFIX)%,%,$(EHC_HS_UTIL_DRV_C)) \
	) > $@

$(LIB_EHC_SETUP_HS_DRV): $(EHC_MKF)
	mkdir -p $(@D)
	$(call GEN_CABAL_SETUP) > $@

$(LIB_EHC_SETUP2): $(LIB_EHC_SETUP_HS_DRV)
	$(call GHC_CABAL,$<,$@)

$(LIB_EHC_INS_FLAG): $(LIB_EHC_CABAL_DRV) $(LIB_EHC_SETUP2) $(INSABS_EHC_LIB_ALL_AG) $(EHC_MKF)
	mkdir -p $(@D)
	cd $(EHC_BLD_LIBEHC_VARIANT_PREFIX) && \
	$(LIB_EHC_SETUP) configure $(CABAL_SETUP_OPTS) --prefix=$(INSABS_PREFIX) --user && \
	$(LIB_EHC_SETUP) build && \
	$(LIB_EHC_SETUP) install --user && \
	echo $@ > $@

$(INSABS_EHC_LIB_ALL_AG): $(INSABS_EHC_LIB_AG_PREFIX)%: $(EHC_BLD_LIB_HS_VARIANT_PREFIX)%
	mkdir -p $(@D)
	cp $< $@

# rules for ehc library sources+derived
$(EHC_AG_ALL_MAIN_DRV_AG) $(EHC_AG_ALL_DPDS_DRV_AG): $(EHC_BLD_LIB_HS_VARIANT_PREFIX)%.ag: $(SRC_EHC_PREFIX)%.cag $(SHUFFLE)
	mkdir -p $(@D)
	$(SHUFFLE_AG) $(LIB_EHC_SHUFFLE_DEFS) --gen=$(EHC_VARIANT) --base=$(*F)  --order="$(EHC_SHUFFLE_ORDER)" $< > $@&& \
	touch $@

$(EHC_RULES_3_DRV_AG): $(EHC_BLD_VARIANT_PREFIX)%.ag: $(EHC_BLD_VARIANT_PREFIX)%.cag $(SHUFFLE)
	$(SHUFFLE_AG) $(LIB_EHC_SHUFFLE_DEFS) --gen=$(EHC_VARIANT) --base=$(*F)  --order="$(EHC_SHUFFLE_ORDER)" $< > $@&& \
	touch $@

$(EHC_AG_D_MAIN_DRV_HS) $(LIB_EHC_AG_D_MAIN_DRV_HS): %.hs: %.ag
	$(AGC) -dr $(UUAGC_OPTS_WHEN_EHC) $(UUAGC_OPTS_WHEN_EHC_AST_DATA) $(EHC_UUAGC_OPTS_WHEN_UHC_AST_DATA_$(EHC_VARIANT)) -P$(EHC_BLD_VARIANT_PREFIX) -P$(EHC_BLD_LIB_HS_VARIANT_PREFIX) $<

$(EHC_AG_S_MAIN_DRV_HS) $(LIB_EHC_AG_S_MAIN_DRV_HS): %.hs: %.ag
	$(AGC) -cfspr $(UUAGC_OPTS_WHEN_EHC) $(UUAGC_OPTS_WHEN_EHC_AST_SEM) $(EHC_UUAGC_OPTS_WHEN_UHC_AST_SEM_$(EHC_VARIANT)) -P$(EHC_BLD_VARIANT_PREFIX) -P$(EHC_BLD_LIB_HS_VARIANT_PREFIX) $<

$(EHC_AG_DS_MAIN_DRV_HS) $(LIB_EHC_AG_DS_MAIN_DRV_HS): %.hs: %.ag
	$(AGC) -dcfspr $(UUAGC_OPTS_WHEN_EHC) $(UUAGC_OPTS_WHEN_EHC_AST_SEM) $(EHC_UUAGC_OPTS_WHEN_UHC_AST_SEM_$(EHC_VARIANT)) $(UUAGC_OPTS_WHEN_EHC_AST_DATA) $(EHC_UUAGC_OPTS_WHEN_UHC_AST_DATA_$(EHC_VARIANT)) -P$(EHC_BLD_VARIANT_PREFIX) -P$(EHC_BLD_LIB_HS_VARIANT_PREFIX) $<

$(EHC_HS_SIG_DRV_HS): $(EHC_ALL_CHUNK_SRC) $(EHC_RULES_ALL_SRC) $(EHC_MKF)
	@(echo "module $(LIB_EHC_PKG_NAME).$(EHC_HS_SIG_MAIN) where" ; \
	  echo "sig = \"`cat $^ | md5`\"" ; \
	  echo "timestamp = \"`date`\"" \
	) > $@

$(EHC_HS_MAIN_DRV_HS): $(EHC_BLD_VARIANT_PREFIX)%.hs: $(SRC_EHC_PREFIX)%.chs $(SHUFFLE) $(LIB_EHC_INS_FLAG) $(LIB_GRINC_INS_FLAG)
	mkdir -p $(@D)
	$(SHUFFLE_HS) $(LIB_EHC_SHUFFLE_DEFS) $(LIB_GRINC_SHUFFLE_DEFS) --gen=$(EHC_VARIANT) --base=Main --order="$(EHC_SHUFFLE_ORDER)" $< > $@ && \
	touch $@

$(EHC_HS_UTIL_DRV_HS): $(EHC_BLD_LIB_HS_VARIANT_PREFIX)%.hs: $(SRC_EHC_PREFIX)%.chs $(SHUFFLE)
	mkdir -p $(@D)
	$(SHUFFLE_HS) $(LIB_EHC_SHUFFLE_DEFS) --gen=$(EHC_VARIANT) --base=$(*F) --order="$(EHC_SHUFFLE_ORDER)" $< > $@&& \
	touch $@

$(EHC_HS_UTIL_DRV_C): $(EHC_BLD_LIB_HS_VARIANT_PREFIX)%.c: $(SRC_EHC_PREFIX)%.cc $(SHUFFLE)
	mkdir -p $(@D)
	$(SHUFFLE_C) $(LIB_EHC_SHUFFLE_DEFS) --gen=$(EHC_VARIANT) --base=$(*F) --order="$(EHC_SHUFFLE_ORDER)" $< > $@&& \
	touch $@

$(EHC_RULES_3_DRV_CAG): $(EHC_RULES_3_SRC_RL2) $(RULER2) $(EHC_MKF)
	mkdir -p $(@D)
	$(RULER2) $(RULER2_OPTS) --ag --wrapshuffle --preamble=no --selrule="$(EHC_VARIANT_RULER_SEL)" --base=$(*F) $< > $@

$(EHC_HS_UTILCPP_DRV_HS): $(EHC_BLD_LIB_HS_VARIANT_PREFIX)%.hs: $(SRC_EHC_PREFIX)%.chs $(SHUFFLE)
	mkdir -p $(@D)
	$(SHUFFLE_HS_PRE) $(LIB_EHC_SHUFFLE_DEFS) --gen=$(EHC_VARIANT) --base=$(*F) --order="$(EHC_SHUFFLE_ORDER)" $< > $@ && \
	touch $@

