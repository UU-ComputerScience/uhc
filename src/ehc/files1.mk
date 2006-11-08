# variant, to be configured on top level
# see variant.mk

# location of ehc src:
# see shared.mk

# this file + other mk files
EHC_MKF									:= $(patsubst %,$(SRC_EHC_PREFIX)%.mk,files1 files2 shared)

# end products, binary, executable, etc
EHC_EXEC_NAME							:= ehc
EHC_BLD_EXEC							:= $(EHC_BIN_VARIANT_PREFIX)$(EHC_EXEC_NAME)$(EXEC_SUFFIX)
EHC_ALL_PUB_EXECS						:= $(patsubst %,$(EHC_BIN_PREFIX)%/$(EHC_EXEC_NAME)$(EXEC_SUFFIX),$(EHC_PUB_VARIANTS))
EHC_ALL_EXECS							:= $(patsubst %,$(EHC_BIN_PREFIX)%/$(EHC_EXEC_NAME)$(EXEC_SUFFIX),$(EHC_VARIANTS))

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
													Substitutable Gam Cnstr Pred Module Config \
													Base/Opts Base/Common Base/Builtin Base/HsName Base/Debug \
													NameAspect \
													Scanner/Common Scanner/Machine Scanner/Scanner Scanner/Token Scanner/TokenParser \
													Base/Parser Ty/Parser EH/Parser HS/Parser HI/Parser Core/Parser GrinCode/Parser \
													Ty/FitsInCommon Ty/FitsIn \
													Core/Utils \
													Gam/Utils \
											)
EHC_HS_UTIL_DRV_HS						:= $(patsubst $(SRC_EHC_PREFIX)%.chs,$(EHC_BLD_LIB_HS_VARIANT_PREFIX)%.hs,$(EHC_HS_UTIL_SRC_CHS))

EHC_HS_ALL_SRC_CHS						:= $(EHC_HS_MAIN_SRC_CHS) $(EHC_HS_UTIL_SRC_CHS)
EHC_HS_ALL_DRV_HS						:= $(EHC_HS_MAIN_DRV_HS) $(EHC_HS_UTIL_DRV_HS)

# main + sources + dpds, for .cag
EHC_AGEHAST_MAIN_SRC_CAG				:= $(patsubst %,$(SRC_EHC_PREFIX)%.cag,EH)
EHC_AGEHAST_DPDS_SRC_CAG				:= $(patsubst %,$(SRC_EHC_PREFIX)EH/%.cag,AbsSyn)
$(patsubst $(SRC_EHC_PREFIX)%.cag,$(EHC_BLD_LIB_HS_VARIANT_PREFIX)%.hs,$(EHC_AGEHAST_MAIN_SRC_CAG)) \
										: $(patsubst $(SRC_EHC_PREFIX)%.cag,$(EHC_BLD_LIB_HS_VARIANT_PREFIX)%.ag,$(EHC_AGEHAST_DPDS_SRC_CAG))

EHC_AGEHMAIN_MAIN_SRC_CAG				:= $(patsubst %,$(SRC_EHC_PREFIX)EH/%.cag,MainAG)
EHC_AGEHMAIN_DPDS_SRC_CAG				:= $(patsubst %,$(SRC_EHC_PREFIX)EH/%.cag,AbsSyn \
													Infer InferExpr \
													InferPatExpr InferTyExpr InferKiExpr InferData \
													InferCaseExpr Pretty PrettyAST \
													Uniq ExtraChecks GatherError \
													ToCore ResolvePred InferClass \
											)
$(patsubst $(SRC_EHC_PREFIX)%.cag,$(EHC_BLD_LIB_HS_VARIANT_PREFIX)%.hs,$(EHC_AGEHMAIN_MAIN_SRC_CAG)) \
										: $(patsubst $(SRC_EHC_PREFIX)%.cag,$(EHC_BLD_LIB_HS_VARIANT_PREFIX)%.ag,$(EHC_AGEHMAIN_DPDS_SRC_CAG)) \
											$(EHC_RULES_3_DRV_AG)

EHC_AGHSAST_MAIN_SRC_CAG				:= $(patsubst %,$(SRC_EHC_PREFIX)%.cag,HS)
EHC_AGHSAST_DPDS_SRC_CAG				:= $(patsubst %,$(SRC_EHC_PREFIX)HS/%.cag,AbsSyn)
$(patsubst $(SRC_EHC_PREFIX)%.cag,$(EHC_BLD_LIB_HS_VARIANT_PREFIX)%.hs,$(EHC_AGHSAST_MAIN_SRC_CAG)) \
										: $(patsubst $(SRC_EHC_PREFIX)%.cag,$(EHC_BLD_LIB_HS_VARIANT_PREFIX)%.ag,$(EHC_AGHSAST_DPDS_SRC_CAG))

EHC_AGHSMAIN_MAIN_SRC_CAG				:= $(patsubst %,$(SRC_EHC_PREFIX)HS/%.cag,MainAG)
EHC_AGHSMAIN_DPDS_SRC_CAG				:= $(patsubst %,$(SRC_EHC_PREFIX)HS/%.cag,AbsSyn \
													ToEH Fixity Pretty Uniq \
													NameAnalysis NameDef NameLevel \
													ExtraChecks GatherError \
											)
$(patsubst $(SRC_EHC_PREFIX)%.cag,$(EHC_BLD_LIB_HS_VARIANT_PREFIX)%.hs,$(EHC_AGHSMAIN_MAIN_SRC_CAG)) \
										: $(patsubst $(SRC_EHC_PREFIX)%.cag,$(EHC_BLD_LIB_HS_VARIANT_PREFIX)%.ag,$(EHC_AGHSMAIN_DPDS_SRC_CAG))

EHC_AGHSIMPEXP_MAIN_SRC_CAG				:= $(patsubst %,$(SRC_EHC_PREFIX)HS/%.cag,ModImpExp)
EHC_AGHSIMPEXP_DPDS_SRC_CAG				:= $(patsubst %,$(SRC_EHC_PREFIX)HS/%.cag,AbsSyn \
													NameDef NameLevel \
													Module Uniq \
											)
$(patsubst $(SRC_EHC_PREFIX)%.cag,$(EHC_BLD_LIB_HS_VARIANT_PREFIX)%.hs,$(EHC_AGHSIMPEXP_MAIN_SRC_CAG)) \
										: $(patsubst $(SRC_EHC_PREFIX)%.cag,$(EHC_BLD_LIB_HS_VARIANT_PREFIX)%.ag,$(EHC_AGHSIMPEXP_DPDS_SRC_CAG))

EHC_AGHIAST_MAIN_SRC_CAG				:= $(patsubst %,$(SRC_EHC_PREFIX)%.cag,HI)
EHC_AGHIAST_DPDS_SRC_CAG				:= $(patsubst %,$(SRC_EHC_PREFIX)HI/%.cag,AbsSyn)
$(patsubst $(SRC_EHC_PREFIX)%.cag,$(EHC_BLD_LIB_HS_VARIANT_PREFIX)%.hs,$(EHC_AGHIAST_MAIN_SRC_CAG)) \
										: $(patsubst $(SRC_EHC_PREFIX)%.cag,$(EHC_BLD_LIB_HS_VARIANT_PREFIX)%.ag,$(EHC_AGHIAST_DPDS_SRC_CAG))

EHC_AGHIMAIN_MAIN_SRC_CAG				:= $(patsubst %,$(SRC_EHC_PREFIX)HI/%.cag,MainAG)
EHC_AGHIMAIN_DPDS_SRC_CAG				:= $(patsubst %,$(SRC_EHC_PREFIX)HI/%.cag,AbsSyn \
													Pretty Uniq \
											)
$(patsubst $(SRC_EHC_PREFIX)%.cag,$(EHC_BLD_LIB_HS_VARIANT_PREFIX)%.hs,$(EHC_AGHIMAIN_MAIN_SRC_CAG)) \
										: $(patsubst $(SRC_EHC_PREFIX)%.cag,$(EHC_BLD_LIB_HS_VARIANT_PREFIX)%.ag,$(EHC_AGHIMAIN_DPDS_SRC_CAG))

EHC_AGTY_MAIN_SRC_CAG					:= $(patsubst %,$(SRC_EHC_PREFIX)%.cag,Ty)
EHC_AGTY_DPDS_SRC_CAG					:= $(patsubst %,$(SRC_EHC_PREFIX)Ty/%.cag,AbsSyn)
$(patsubst $(SRC_EHC_PREFIX)%.cag,$(EHC_BLD_LIB_HS_VARIANT_PREFIX)%.hs,$(EHC_AGTY_MAIN_SRC_CAG)) \
										: $(patsubst $(SRC_EHC_PREFIX)%.cag,$(EHC_BLD_LIB_HS_VARIANT_PREFIX)%.ag,$(EHC_AGTY_DPDS_SRC_CAG))

EHC_AGTY_FTV_MAIN_SRC_CAG				:= $(patsubst %,$(SRC_EHC_PREFIX)Ty/%.cag,Ftv)
EHC_AGTY_FTV_DPDS_SRC_CAG				:= $(patsubst %,$(SRC_EHC_PREFIX)Ty/%.cag,AbsSyn)
$(patsubst $(SRC_EHC_PREFIX)%.cag,$(EHC_BLD_LIB_HS_VARIANT_PREFIX)%.hs,$(EHC_AGTY_FTV_MAIN_SRC_CAG)) \
										: $(patsubst $(SRC_EHC_PREFIX)%.cag,$(EHC_BLD_LIB_HS_VARIANT_PREFIX)%.ag,$(EHC_AGTY_FTV_DPDS_SRC_CAG))

EHC_AGTY_INST_MAIN_SRC_CAG				:= $(patsubst %,$(SRC_EHC_PREFIX)Ty/%.cag,Instantiate)
EHC_AGTY_INST_DPDS_SRC_CAG				:= $(patsubst %,$(SRC_EHC_PREFIX)Ty/%.cag,CommonAG AbsSyn)
$(patsubst $(SRC_EHC_PREFIX)%.cag,$(EHC_BLD_LIB_HS_VARIANT_PREFIX)%.hs,$(EHC_AGTY_INST_MAIN_SRC_CAG)) \
										: $(patsubst $(SRC_EHC_PREFIX)%.cag,$(EHC_BLD_LIB_HS_VARIANT_PREFIX)%.ag,$(EHC_AGTY_INST_DPDS_SRC_CAG))

EHC_AGTY_PRETTY_MAIN_SRC_CAG			:= $(patsubst %,$(SRC_EHC_PREFIX)Ty/%.cag,Pretty)
EHC_AGTY_PRETTY_DPDS_SRC_CAG			:= $(patsubst %,$(SRC_EHC_PREFIX)Ty/%.cag,CommonAG AbsSyn)
$(patsubst $(SRC_EHC_PREFIX)%.cag,$(EHC_BLD_LIB_HS_VARIANT_PREFIX)%.hs,$(EHC_AGTY_PRETTY_MAIN_SRC_CAG)) \
										: $(patsubst $(SRC_EHC_PREFIX)%.cag,$(EHC_BLD_LIB_HS_VARIANT_PREFIX)%.ag,$(EHC_AGTY_PRETTY_DPDS_SRC_CAG))

EHC_AGTY_QU_MAIN_SRC_CAG				:= $(patsubst %,$(SRC_EHC_PREFIX)Ty/%.cag,Quantify)
EHC_AGTY_QU_DPDS_SRC_CAG				:= $(patsubst %,$(SRC_EHC_PREFIX)Ty/%.cag,CommonAG AbsSyn)
$(patsubst $(SRC_EHC_PREFIX)%.cag,$(EHC_BLD_LIB_HS_VARIANT_PREFIX)%.hs,$(EHC_AGTY_QU_MAIN_SRC_CAG)) \
										: $(patsubst $(SRC_EHC_PREFIX)%.cag,$(EHC_BLD_LIB_HS_VARIANT_PREFIX)%.ag,$(EHC_AGTY_QU_DPDS_SRC_CAG))

EHC_AGTY_SUBST_MAIN_SRC_CAG				:= $(patsubst %,$(SRC_EHC_PREFIX)Ty/%.cag,Subst)
EHC_AGTY_SUBST_DPDS_SRC_CAG				:= $(patsubst %,$(SRC_EHC_PREFIX)Ty/%.cag,AbsSyn)
$(patsubst $(SRC_EHC_PREFIX)%.cag,$(EHC_BLD_LIB_HS_VARIANT_PREFIX)%.hs,$(EHC_AGTY_SUBST_MAIN_SRC_CAG)) \
										: $(patsubst $(SRC_EHC_PREFIX)%.cag,$(EHC_BLD_LIB_HS_VARIANT_PREFIX)%.ag,$(EHC_AGTY_SUBST_DPDS_SRC_CAG))

EHC_AGTY_ELIMALT_MAIN_SRC_CAG			:= $(patsubst %,$(SRC_EHC_PREFIX)Ty/%.cag,ElimAlts)
EHC_AGTY_ELIMALT_DPDS_SRC_CAG			:= $(patsubst %,$(SRC_EHC_PREFIX)Ty/%.cag,AbsSyn)
$(patsubst $(SRC_EHC_PREFIX)%.cag,$(EHC_BLD_LIB_HS_VARIANT_PREFIX)%.hs,$(EHC_AGTY_ELIMALT_MAIN_SRC_CAG)) \
										: $(patsubst $(SRC_EHC_PREFIX)%.cag,$(EHC_BLD_LIB_HS_VARIANT_PREFIX)%.ag,$(EHC_AGTY_ELIMALT_DPDS_SRC_CAG))

EHC_AGTY_ELIMBT_MAIN_SRC_CAG			:= $(patsubst %,$(SRC_EHC_PREFIX)Ty/%.cag,ElimBoth)
EHC_AGTY_ELIMBT_DPDS_SRC_CAG			:= $(patsubst %,$(SRC_EHC_PREFIX)Ty/%.cag,AbsSyn)
$(patsubst $(SRC_EHC_PREFIX)%.cag,$(EHC_BLD_LIB_HS_VARIANT_PREFIX)%.hs,$(EHC_AGTY_ELIMBT_MAIN_SRC_CAG)) \
										: $(patsubst $(SRC_EHC_PREFIX)%.cag,$(EHC_BLD_LIB_HS_VARIANT_PREFIX)%.ag,$(EHC_AGTY_ELIMBT_DPDS_SRC_CAG))

EHC_AGTY_ELIMEQ_MAIN_SRC_CAG			:= $(patsubst %,$(SRC_EHC_PREFIX)Ty/%.cag,ElimEqual)
EHC_AGTY_ELIMEQ_DPDS_SRC_CAG			:= $(patsubst %,$(SRC_EHC_PREFIX)Ty/%.cag,AbsSyn)
$(patsubst $(SRC_EHC_PREFIX)%.cag,$(EHC_BLD_LIB_HS_VARIANT_PREFIX)%.hs,$(EHC_AGTY_ELIMEQ_MAIN_SRC_CAG)) \
										: $(patsubst $(SRC_EHC_PREFIX)%.cag,$(EHC_BLD_LIB_HS_VARIANT_PREFIX)%.ag,$(EHC_AGTY_ELIMEQ_DPDS_SRC_CAG))

EHC_AGTY_FRESH_MAIN_SRC_CAG				:= $(patsubst %,$(SRC_EHC_PREFIX)Ty/%.cag,FreshVar)
EHC_AGTY_FRESH_DPDS_SRC_CAG				:= $(patsubst %,$(SRC_EHC_PREFIX)Ty/%.cag,AbsSyn)
$(patsubst $(SRC_EHC_PREFIX)%.cag,$(EHC_BLD_LIB_HS_VARIANT_PREFIX)%.hs,$(EHC_AGTY_FRESH_MAIN_SRC_CAG)) \
										: $(patsubst $(SRC_EHC_PREFIX)%.cag,$(EHC_BLD_LIB_HS_VARIANT_PREFIX)%.ag,$(EHC_AGTY_FRESH_DPDS_SRC_CAG))

EHC_AGTY_MAPNAME_MAIN_SRC_CAG			:= $(patsubst %,$(SRC_EHC_PREFIX)Ty/%.cag,MapName)
EHC_AGTY_MAPNAME_DPDS_SRC_CAG			:= $(patsubst %,$(SRC_EHC_PREFIX)Ty/%.cag,AbsSyn)
$(patsubst $(SRC_EHC_PREFIX)%.cag,$(EHC_BLD_LIB_HS_VARIANT_PREFIX)%.hs,$(EHC_AGTY_MAPNAME_MAIN_SRC_CAG)) \
										: $(patsubst $(SRC_EHC_PREFIX)%.cag,$(EHC_BLD_LIB_HS_VARIANT_PREFIX)%.ag,$(EHC_AGTY_MAPNAME_DPDS_SRC_CAG))

EHC_AGERR_MAIN_SRC_CAG					:= $(patsubst %,$(SRC_EHC_PREFIX)%.cag,Error)
EHC_AGERR_DPDS_SRC_CAG					:= $(patsubst %,$(SRC_EHC_PREFIX)Error/%.cag,AbsSyn)
$(patsubst $(SRC_EHC_PREFIX)%.cag,$(EHC_BLD_LIB_HS_VARIANT_PREFIX)%.hs,$(EHC_AGERR_MAIN_SRC_CAG)) \
										: $(patsubst $(SRC_EHC_PREFIX)%.cag,$(EHC_BLD_LIB_HS_VARIANT_PREFIX)%.ag,$(EHC_AGERR_DPDS_SRC_CAG))

EHC_AGERR_PRETTY_MAIN_SRC_CAG			:= $(patsubst %,$(SRC_EHC_PREFIX)Error/%.cag,Pretty)
EHC_AGERR_PRETTY_DPDS_SRC_CAG			:= $(patsubst %,$(SRC_EHC_PREFIX)Error/%.cag,AbsSyn)
$(patsubst $(SRC_EHC_PREFIX)%.cag,$(EHC_BLD_LIB_HS_VARIANT_PREFIX)%.hs,$(EHC_AGERR_PRETTY_MAIN_SRC_CAG)) \
										: $(patsubst $(SRC_EHC_PREFIX)%.cag,$(EHC_BLD_LIB_HS_VARIANT_PREFIX)%.ag,$(EHC_AGERR_PRETTY_DPDS_SRC_CAG))

EHC_AGCORE_MAIN_SRC_CAG					:= $(patsubst %,$(SRC_EHC_PREFIX)%.cag,Core)
EHC_AGCORE_DPDS_SRC_CAG					:= $(patsubst %,$(SRC_EHC_PREFIX)Core/%.cag,AbsSyn)
$(patsubst $(SRC_EHC_PREFIX)%.cag,$(EHC_BLD_LIB_HS_VARIANT_PREFIX)%.hs,$(EHC_AGCORE_MAIN_SRC_CAG)) \
										: $(patsubst $(SRC_EHC_PREFIX)%.cag,$(EHC_BLD_LIB_HS_VARIANT_PREFIX)%.ag,$(EHC_AGCORE_DPDS_SRC_CAG))

EHC_AGCORE_GRIN_MAIN_SRC_CAG			:= $(patsubst %,$(SRC_EHC_PREFIX)Core/%.cag,ToGrin)
EHC_AGCORE_GRIN_DPDS_SRC_CAG			:= $(patsubst %,$(SRC_EHC_PREFIX)Core/%.cag,CommonLev AbsSyn)
$(patsubst $(SRC_EHC_PREFIX)%.cag,$(EHC_BLD_LIB_HS_VARIANT_PREFIX)%.hs,$(EHC_AGCORE_GRIN_MAIN_SRC_CAG)) \
										: $(patsubst $(SRC_EHC_PREFIX)%.cag,$(EHC_BLD_LIB_HS_VARIANT_PREFIX)%.ag,$(EHC_AGCORE_GRIN_DPDS_SRC_CAG))

EHC_AGCORE_JAVA_MAIN_SRC_CAG			:= $(patsubst %,$(SRC_EHC_PREFIX)Core/%.cag,ToJava)
EHC_AGCORE_JAVA_DPDS_SRC_CAG			:= $(patsubst %,$(SRC_EHC_PREFIX)Core/%.cag,CommonLev AbsSyn)
$(patsubst $(SRC_EHC_PREFIX)%.cag,$(EHC_BLD_LIB_HS_VARIANT_PREFIX)%.hs,$(EHC_AGCORE_JAVA_MAIN_SRC_CAG)) \
										: $(patsubst $(SRC_EHC_PREFIX)%.cag,$(EHC_BLD_LIB_HS_VARIANT_PREFIX)%.ag,$(EHC_AGCORE_JAVA_DPDS_SRC_CAG))

EHC_AGCORE_PRETTY_MAIN_SRC_CAG			:= $(patsubst %,$(SRC_EHC_PREFIX)Core/%.cag,Pretty)
EHC_AGCORE_PRETTY_DPDS_SRC_CAG			:= $(patsubst %,$(SRC_EHC_PREFIX)Core/%.cag,AbsSyn)
$(patsubst $(SRC_EHC_PREFIX)%.cag,$(EHC_BLD_LIB_HS_VARIANT_PREFIX)%.hs,$(EHC_AGCORE_PRETTY_MAIN_SRC_CAG)) \
										: $(patsubst $(SRC_EHC_PREFIX)%.cag,$(EHC_BLD_LIB_HS_VARIANT_PREFIX)%.ag,$(EHC_AGCORE_PRETTY_DPDS_SRC_CAG))

EHC_AGCORE_SUBST_MAIN_SRC_CAG			:= $(patsubst %,$(SRC_EHC_PREFIX)Core/%.cag,Subst)
EHC_AGCORE_SUBST_DPDS_SRC_CAG			:= $(patsubst %,$(SRC_EHC_PREFIX)Core/%.cag,AbsSyn)
$(patsubst $(SRC_EHC_PREFIX)%.cag,$(EHC_BLD_LIB_HS_VARIANT_PREFIX)%.hs,$(EHC_AGCORE_SUBST_MAIN_SRC_CAG)) \
										: $(patsubst $(SRC_EHC_PREFIX)%.cag,$(EHC_BLD_LIB_HS_VARIANT_PREFIX)%.ag,$(EHC_AGCORE_SUBST_DPDS_SRC_CAG))

EHC_AGCORE_SUBSTCALTFAIL_MAIN_SRC_CAG	:= $(patsubst %,$(SRC_EHC_PREFIX)Core/%.cag,SubstCaseAltFail)
EHC_AGCORE_SUBSTCALTFAIL_DPDS_SRC_CAG	:= $(patsubst %,$(SRC_EHC_PREFIX)Core/%.cag,AbsSyn)
$(patsubst $(SRC_EHC_PREFIX)%.cag,$(EHC_BLD_LIB_HS_VARIANT_PREFIX)%.hs,$(EHC_AGCORE_SUBSTCALTFAIL_MAIN_SRC_CAG)) \
										: $(patsubst $(SRC_EHC_PREFIX)%.cag,$(EHC_BLD_LIB_HS_VARIANT_PREFIX)%.ag,$(EHC_AGCORE_SUBSTCALTFAIL_DPDS_SRC_CAG))

EHC_AGCORE_FVS_MAIN_SRC_CAG				:= $(patsubst %,$(SRC_EHC_PREFIX)Core/%.cag,FvS)
EHC_AGCORE_FVS_DPDS_SRC_CAG				:= $(patsubst %,$(SRC_EHC_PREFIX)Core/%.cag,AbsSyn Trf/CommonFv)
$(patsubst $(SRC_EHC_PREFIX)%.cag,$(EHC_BLD_LIB_HS_VARIANT_PREFIX)%.hs,$(EHC_AGCORE_FVS_MAIN_SRC_CAG)) \
										: $(patsubst $(SRC_EHC_PREFIX)%.cag,$(EHC_BLD_LIB_HS_VARIANT_PREFIX)%.ag,$(EHC_AGCORE_FVS_DPDS_SRC_CAG))

EHC_AGCORE_TRF_CONSTPROP_MAIN_SRC_CAG	:= $(patsubst %,$(SRC_EHC_PREFIX)Core/Trf/%.cag,ConstProp)
EHC_AGCORE_TRF_CONSTPROP_DPDS_SRC_CAG	:= $(patsubst %,$(SRC_EHC_PREFIX)Core/%.cag,CommonLev AbsSyn)
$(patsubst $(SRC_EHC_PREFIX)%.cag,$(EHC_BLD_LIB_HS_VARIANT_PREFIX)%.hs,$(EHC_AGCORE_TRF_CONSTPROP_MAIN_SRC_CAG)) \
										: $(patsubst $(SRC_EHC_PREFIX)%.cag,$(EHC_BLD_LIB_HS_VARIANT_PREFIX)%.ag,$(EHC_AGCORE_TRF_CONSTPROP_DPDS_SRC_CAG))

EHC_AGCORE_TRF_ETARED_MAIN_SRC_CAG		:= $(patsubst %,$(SRC_EHC_PREFIX)Core/Trf/%.cag,EtaRed)
EHC_AGCORE_TRF_ETARED_DPDS_SRC_CAG		:= $(patsubst %,$(SRC_EHC_PREFIX)Core/%.cag,CommonLev Trf/CommonFv AbsSyn)
$(patsubst $(SRC_EHC_PREFIX)%.cag,$(EHC_BLD_LIB_HS_VARIANT_PREFIX)%.hs,$(EHC_AGCORE_TRF_ETARED_MAIN_SRC_CAG)) \
										: $(patsubst $(SRC_EHC_PREFIX)%.cag,$(EHC_BLD_LIB_HS_VARIANT_PREFIX)%.ag,$(EHC_AGCORE_TRF_ETARED_DPDS_SRC_CAG))

EHC_AGCORE_TRF_FULLAZY_MAIN_SRC_CAG		:= $(patsubst %,$(SRC_EHC_PREFIX)Core/Trf/%.cag,FullLazy)
EHC_AGCORE_TRF_FULLAZY_DPDS_SRC_CAG		:= $(patsubst %,$(SRC_EHC_PREFIX)Core/%.cag,Trf/CommonFv Trf/CommonLev CommonLev AbsSyn)
$(patsubst $(SRC_EHC_PREFIX)%.cag,$(EHC_BLD_LIB_HS_VARIANT_PREFIX)%.hs,$(EHC_AGCORE_TRF_FULLAZY_MAIN_SRC_CAG)) \
										: $(patsubst $(SRC_EHC_PREFIX)%.cag,$(EHC_BLD_LIB_HS_VARIANT_PREFIX)%.ag,$(EHC_AGCORE_TRF_FULLAZY_DPDS_SRC_CAG))

EHC_AGCORE_TRF_INLLETALI_MAIN_SRC_CAG	:= $(patsubst %,$(SRC_EHC_PREFIX)Core/Trf/%.cag,InlineLetAlias)
EHC_AGCORE_TRF_INLLETALI_DPDS_SRC_CAG	:= $(patsubst %,$(SRC_EHC_PREFIX)Core/%.cag,AbsSyn CommonLev)
$(patsubst $(SRC_EHC_PREFIX)%.cag,$(EHC_BLD_LIB_HS_VARIANT_PREFIX)%.hs,$(EHC_AGCORE_TRF_INLLETALI_MAIN_SRC_CAG)) \
										: $(patsubst $(SRC_EHC_PREFIX)%.cag,$(EHC_BLD_LIB_HS_VARIANT_PREFIX)%.ag,$(EHC_AGCORE_TRF_INLLETALI_DPDS_SRC_CAG))

EHC_AGCORE_TRF_LAMLIFT_MAIN_SRC_CAG		:= $(patsubst %,$(SRC_EHC_PREFIX)Core/Trf/%.cag,LamLift)
EHC_AGCORE_TRF_LAMLIFT_DPDS_SRC_CAG		:= $(patsubst %,$(SRC_EHC_PREFIX)Core/%.cag,Trf/CommonFv Trf/CommonLev CommonLev AbsSyn)
$(patsubst $(SRC_EHC_PREFIX)%.cag,$(EHC_BLD_LIB_HS_VARIANT_PREFIX)%.hs,$(EHC_AGCORE_TRF_LAMLIFT_MAIN_SRC_CAG)) \
										: $(patsubst $(SRC_EHC_PREFIX)%.cag,$(EHC_BLD_LIB_HS_VARIANT_PREFIX)%.ag,$(EHC_AGCORE_TRF_LAMLIFT_DPDS_SRC_CAG))

EHC_AGCORE_TRF_LETUNREC_MAIN_SRC_CAG	:= $(patsubst %,$(SRC_EHC_PREFIX)Core/Trf/%.cag,LetUnrec)
EHC_AGCORE_TRF_LETUNREC_DPDS_SRC_CAG	:= $(patsubst %,$(SRC_EHC_PREFIX)Core/%.cag,AbsSyn)
$(patsubst $(SRC_EHC_PREFIX)%.cag,$(EHC_BLD_LIB_HS_VARIANT_PREFIX)%.hs,$(EHC_AGCORE_TRF_LETUNREC_MAIN_SRC_CAG)) \
										: $(patsubst $(SRC_EHC_PREFIX)%.cag,$(EHC_BLD_LIB_HS_VARIANT_PREFIX)%.ag,$(EHC_AGCORE_TRF_LETUNREC_DPDS_SRC_CAG))

EHC_AGCORE_TRF_RENUNQ_MAIN_SRC_CAG		:= $(patsubst %,$(SRC_EHC_PREFIX)Core/Trf/%.cag,RenUniq)
EHC_AGCORE_TRF_RENUNQ_DPDS_SRC_CAG		:= $(patsubst %,$(SRC_EHC_PREFIX)Core/%.cag,AbsSyn)
$(patsubst $(SRC_EHC_PREFIX)%.cag,$(EHC_BLD_LIB_HS_VARIANT_PREFIX)%.hs,$(EHC_AGCORE_TRF_RENUNQ_MAIN_SRC_CAG)) \
										: $(patsubst $(SRC_EHC_PREFIX)%.cag,$(EHC_BLD_LIB_HS_VARIANT_PREFIX)%.ag,$(EHC_AGCORE_TRF_RENUNQ_DPDS_SRC_CAG))

EHC_AGGRINCODE_MAIN_SRC_CAG				:= $(patsubst %,$(SRC_EHC_PREFIX)%.cag,GrinCode)
EHC_AGGRINCODE_DPDS_SRC_CAG				:= $(patsubst %,$(SRC_EHC_PREFIX)GrinCode/%.cag,AbsSyn)
$(patsubst $(SRC_EHC_PREFIX)%.cag,$(EHC_BLD_LIB_HS_VARIANT_PREFIX)%.hs,$(EHC_AGGRINCODE_MAIN_SRC_CAG)) \
										: $(patsubst $(SRC_EHC_PREFIX)%.cag,$(EHC_BLD_LIB_HS_VARIANT_PREFIX)%.ag,$(EHC_AGGRINCODE_DPDS_SRC_CAG))

EHC_AGGRINCODE_PRETTY_MAIN_SRC_CAG		:= $(patsubst %,$(SRC_EHC_PREFIX)GrinCode/%.cag,Pretty)
EHC_AGGRINCODE_PRETTY_DPDS_SRC_CAG		:= $(patsubst %,$(SRC_EHC_PREFIX)GrinCode/%.cag,AbsSyn)
$(patsubst $(SRC_EHC_PREFIX)%.cag,$(EHC_BLD_LIB_HS_VARIANT_PREFIX)%.hs,$(EHC_AGGRINCODE_PRETTY_MAIN_SRC_CAG)) \
										: $(patsubst $(SRC_EHC_PREFIX)%.cag,$(EHC_BLD_LIB_HS_VARIANT_PREFIX)%.ag,$(EHC_AGGRINCODE_PRETTY_DPDS_SRC_CAG))

EHC_AG_D_MAIN_SRC_CAG					:= \
											$(EHC_AGEHAST_MAIN_SRC_CAG) \
											$(EHC_AGHSAST_MAIN_SRC_CAG) \
											$(EHC_AGHIAST_MAIN_SRC_CAG) \
											$(EHC_AGTY_MAIN_SRC_CAG) \
											$(EHC_AGCORE_MAIN_SRC_CAG) \
											$(EHC_AGGRINCODE_MAIN_SRC_CAG) \
											$(EHC_AGERR_MAIN_SRC_CAG)

EHC_AG_S_MAIN_SRC_CAG					:= \
											$(EHC_AGEHMAIN_MAIN_SRC_CAG) \
											$(EHC_AGHSMAIN_MAIN_SRC_CAG) \
											$(EHC_AGHSIMPEXP_MAIN_SRC_CAG) \
											$(EHC_AGHIMAIN_MAIN_SRC_CAG) \
											$(EHC_AGERR_PRETTY_MAIN_SRC_CAG) \
											$(EHC_AGCORE_JAVA_MAIN_SRC_CAG) \
											$(EHC_AGCORE_GRIN_MAIN_SRC_CAG) \
											$(EHC_AGCORE_PRETTY_MAIN_SRC_CAG) \
											$(EHC_AGCORE_SUBST_MAIN_SRC_CAG) \
											$(EHC_AGCORE_SUBSTCALTFAIL_MAIN_SRC_CAG) \
											$(EHC_AGCORE_FVS_MAIN_SRC_CAG) \
											$(EHC_AGCORE_TRF_CONSTPROP_MAIN_SRC_CAG) \
											$(EHC_AGCORE_TRF_ETARED_MAIN_SRC_CAG) \
											$(EHC_AGCORE_TRF_FULLAZY_MAIN_SRC_CAG) \
											$(EHC_AGCORE_TRF_INLLETALI_MAIN_SRC_CAG) \
											$(EHC_AGCORE_TRF_LAMLIFT_MAIN_SRC_CAG) \
											$(EHC_AGCORE_TRF_LETUNREC_MAIN_SRC_CAG) \
											$(EHC_AGCORE_TRF_RENUNQ_MAIN_SRC_CAG) \
											$(EHC_AGTY_FTV_MAIN_SRC_CAG) \
											$(EHC_AGTY_INST_MAIN_SRC_CAG) \
											$(EHC_AGTY_QU_MAIN_SRC_CAG) \
											$(EHC_AGTY_SUBST_MAIN_SRC_CAG) \
											$(EHC_AGTY_ELIMALT_MAIN_SRC_CAG) \
											$(EHC_AGTY_ELIMBT_MAIN_SRC_CAG) \
											$(EHC_AGTY_ELIMEQ_MAIN_SRC_CAG) \
											$(EHC_AGTY_FRESH_MAIN_SRC_CAG) \
											$(EHC_AGTY_MAPNAME_MAIN_SRC_CAG) \
											$(EHC_AGTY_PRETTY_MAIN_SRC_CAG) \
											$(EHC_AGGRINCODE_PRETTY_MAIN_SRC_CAG)

EHC_AG_DS_MAIN_SRC_CAG					:= 

EHC_AG_ALL_MAIN_SRC_CAG					:= $(EHC_AG_D_MAIN_SRC_CAG) $(EHC_AG_S_MAIN_SRC_CAG) $(EHC_AG_DS_MAIN_SRC_CAG)

EHC_AG_ALL_DPDS_SRC_CAG					:= $(sort \
											$(EHC_AGEHAST_DPDS_SRC_CAG) \
											$(EHC_AGEHMAIN_DPDS_SRC_CAG) \
											$(EHC_AGHSAST_DPDS_SRC_CAG) \
											$(EHC_AGHSMAIN_DPDS_SRC_CAG) \
											$(EHC_AGHSIMPEXP_DPDS_SRC_CAG) \
											$(EHC_AGHIAST_DPDS_SRC_CAG) \
											$(EHC_AGHIMAIN_DPDS_SRC_CAG) \
											$(EHC_AGCORE_DPDS_SRC_CAG) \
											$(EHC_AGCORE_JAVA_DPDS_SRC_CAG) \
											$(EHC_AGCORE_GRIN_DPDS_SRC_CAG) \
											$(EHC_AGCORE_PRETTY_DPDS_SRC_CAG) \
											$(EHC_AGCORE_SUBST_DPDS_SRC_CAG) \
											$(EHC_AGCORE_SUBSTCALTFAIL_DPDS_SRC_CAG) \
											$(EHC_AGCORE_FVS_DPDS_SRC_CAG) \
											$(EHC_AGCORE_TRF_CONSTPROP_DPDS_SRC_CAG) \
											$(EHC_AGCORE_TRF_ETARED_DPDS_SRC_CAG) \
											$(EHC_AGCORE_TRF_FULLAZY_DPDS_SRC_CAG) \
											$(EHC_AGCORE_TRF_INLLETALI_DPDS_SRC_CAG) \
											$(EHC_AGCORE_TRF_LAMLIFT_DPDS_SRC_CAG) \
											$(EHC_AGCORE_TRF_LETUNREC_DPDS_SRC_CAG) \
											$(EHC_AGCORE_TRF_RENUNQ_DPDS_SRC_CAG) \
											$(EHC_AGTY_DPDS_SRC_CAG) \
											$(EHC_AGTY_FTV_DPDS_SRC_CAG) \
											$(EHC_AGTY_INST_DPDS_SRC_CAG) \
											$(EHC_AGTY_QU_DPDS_SRC_CAG) \
											$(EHC_AGTY_SUBST_DPDS_SRC_CAG) \
											$(EHC_AGTY_ELIMALT_DPDS_SRC_CAG) \
											$(EHC_AGTY_ELIMBT_DPDS_SRC_CAG) \
											$(EHC_AGTY_ELIMEQ_DPDS_SRC_CAG) \
											$(EHC_AGTY_MAPNAME_DPDS_SRC_CAG) \
											$(EHC_AGTY_PRETTY_DPDS_SRC_CAG) \
											$(EHC_AGGRINCODE_DPDS_SRC_CAG) \
											$(EHC_AGGRINCODE_PRETTY_DPDS_SRC_CAG) \
											$(EHC_AGERR_DPDS_SRC_CAG) \
											$(EHC_AGERR_PRETTY_DPDS_SRC_CAG) \
											)

# all src
EHC_ALL_CHUNK_SRC						:= $(EHC_AG_ALL_MAIN_SRC_CAG) $(EHC_AG_ALL_DPDS_SRC_CAG) $(EHC_HS_ALL_SRC_CHS)
EHC_ALL_SRC								:= $(EHC_ALL_CHUNK_SRC) $(EHC_RULES_ALL_SRC) $(EHC_MKF)

# distribution
EHC_DIST_FILES							:= $(EHC_ALL_SRC)

# what is based on which Ruler view
EHC_ON_RULES_VIEW_1						:= K
EHC_ON_RULES_VIEW_2						:= C
EHC_ON_RULES_VIEW_3						:= HM
EHC_ON_RULES_VIEW_4						:= EX
EHC_ON_RULES_VIEW_4_2					:= I2
EHC_ON_RULES_VIEW_5						:= DT
EHC_ON_RULES_VIEW_6						:= DT
EHC_ON_RULES_VIEW_7						:= DT
EHC_ON_RULES_VIEW_8						:= CG
EHC_ON_RULES_VIEW_9						:= P
EHC_ON_RULES_VIEW_10					:= P
EHC_ON_RULES_VIEW_11					:= TS
EHC_ON_RULES_VIEW_12					:= MD
EHC_ON_RULES_VIEW_99					:= HS

# what is generated by Ruler
EHC_BY_RULER_GROUPS_BASE				:= expr.base patexpr.base tyexpr.base decl.base
EHC_BY_RULER_RULES_BASE2				:= e.int e.char e.var e.con
EHC_BY_RULER_RULES_BASE1				:= $(EHC_BY_RULER_RULES_BASE2) \
											p.int p.char p.con p.apptop p.app p.var p.varas p.ann \
											e.apptop e.app e.app.f e.lam e.let e.ann \
											t.con t.app t.wild t.quant t.var t.var.w \
											d.tysig d.val
EHC_BY_RULER_RULES_1					:= $(EHC_BY_RULER_RULES_BASE1)
EHC_BY_RULER_RULES_2					:= $(EHC_BY_RULER_RULES_1)
EHC_BY_RULER_RULES_3					:= $(EHC_BY_RULER_RULES_2) 
EHC_BY_RULER_RULES_4					:= $(EHC_BY_RULER_RULES_3)
EHC_BY_RULER_RULES_4_2					:= $(EHC_BY_RULER_RULES_4)
EHC_BY_RULER_RULES_5					:= $(EHC_BY_RULER_RULES_BASE2) e.str p.str
EHC_BY_RULER_RULES_6					:= $(EHC_BY_RULER_RULES_5)
EHC_BY_RULER_RULES_7					:= $(EHC_BY_RULER_RULES_6)
EHC_BY_RULER_RULES_8					:= $(EHC_BY_RULER_RULES_7) e.float p.float
EHC_BY_RULER_RULES_9					:= $(EHC_BY_RULER_RULES_8)
EHC_BY_RULER_RULES_10					:= $(EHC_BY_RULER_RULES_9)
EHC_BY_RULER_RULES_11					:= $(EHC_BY_RULER_RULES_10)
EHC_BY_RULER_RULES_12					:= $(EHC_BY_RULER_RULES_11)
EHC_BY_RULER_RULES_99					:= $(EHC_BY_RULER_RULES_12) e.iint e.double

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
EHC_ALL_DPDS							:= $(EHC_HS_ALL_DRV_HS) $(EHC_AG_ALL_MAIN_DRV_HS) $(EHC_HS_SIG_DRV_HS)

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
$(LIB_EHC_CABAL_DRV): $(EHC_ALL_DPDS) $(EHC_MKF)
	mkdir -p $(@D)
	$(call GEN_CABAL \
		, $(LIB_EHC_PKG_NAME) \
		, $(EH_VERSION) \
		, $(LIB_EH_UTIL_PKG_NAME) \
		, $(CABAL_OPT_ALLOW_UNDECIDABLE_INSTANCES) \
		, Part of EH$(EHC_VARIANT) compiler packaged as library \
		, $(subst $(PATH_SEP),.,$(patsubst $(EHC_BLD_LIB_HS_VARIANT_PREFIX)%.hs,$(LIB_EHC_QUAL_PREFIX)%,$(shell $(FILTER_NONEMP_FILES) $(EHC_HS_UTIL_DRV_HS) $(EHC_AG_ALL_MAIN_DRV_HS) $(EHC_HS_SIG_DRV_HS)))) \
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
	$(AGC) -dr -P$(EHC_BLD_VARIANT_PREFIX) -P$(EHC_BLD_LIB_HS_VARIANT_PREFIX) $<

$(EHC_AG_S_MAIN_DRV_HS) $(LIB_EHC_AG_S_MAIN_DRV_HS): %.hs: %.ag
	$(AGC) -cfspr -P$(EHC_BLD_VARIANT_PREFIX) -P$(EHC_BLD_LIB_HS_VARIANT_PREFIX) $<

$(EHC_AG_DS_MAIN_DRV_HS) $(LIB_EHC_AG_DS_MAIN_DRV_HS): %.hs: %.ag
	$(AGC) -dcfspr -P$(EHC_BLD_VARIANT_PREFIX) -P$(EHC_BLD_LIB_HS_VARIANT_PREFIX) $<

$(EHC_HS_SIG_DRV_HS): $(EHC_ALL_CHUNK_SRC) $(EHC_RULES_ALL_SRC) $(EHC_MKF)
	@(echo "module $(LIB_EHC_PKG_NAME).$(EHC_HS_SIG_MAIN) where" ; \
	  echo "sig = \"`cat $^ | md5`\"" ; \
	  echo "timestamp = \"`date`\"" \
	) > $@

$(EHC_HS_MAIN_DRV_HS): $(EHC_BLD_VARIANT_PREFIX)%.hs: $(SRC_EHC_PREFIX)%.chs $(SHUFFLE)
	mkdir -p $(@D)
	$(SHUFFLE_HS) $(LIB_EHC_SHUFFLE_DEFS) $(LIB_GRINC_SHUFFLE_DEFS) --gen=$(EHC_VARIANT) --base=Main --order="$(EHC_SHUFFLE_ORDER)" $< > $@ && \
	touch $@

$(EHC_HS_UTIL_DRV_HS): $(EHC_BLD_LIB_HS_VARIANT_PREFIX)%.hs: $(SRC_EHC_PREFIX)%.chs $(SHUFFLE)
	mkdir -p $(@D)
	$(SHUFFLE_HS) $(LIB_EHC_SHUFFLE_DEFS) --gen=$(EHC_VARIANT) --base=$(*F) --order="$(EHC_SHUFFLE_ORDER)" $< > $@&& \
	touch $@

$(EHC_RULES_3_DRV_CAG): $(EHC_RULES_3_SRC_RL2) $(RULER2) $(EHC_MKF)
	mkdir -p $(@D)
	$(RULER2) $(RULER2_OPTS) --ag --wrapshuffle --preamble=no --selrule="$(EHC_VARIANT_RULER_SEL)" --base=$(*F) $< > $@

