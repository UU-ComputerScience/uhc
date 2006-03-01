# variant, to be configured on top level
# see variant.mk

# all variants
EHC_PUB_VARIANTS						:= 1 2 3 4 5 6 7 8 9 10
EHC_VARIANTS							:= $(EHC_PUB_VARIANTS) 11 4_2 6_4

# location of ehc src
EHC_SRC_PREFIX							:= $(TOP_PREFIX)ehc/

# this file
EHC_MKF									:= $(EHC_SRC_PREFIX)files.mk

# end products, binary, executable, etc
EHC_EXEC_NAME							:= ehc
EHC_BLD_EXEC							:= $(EHC_BLD_BIN_VARIANT_PREFIX)$(EHC_EXEC_NAME)$(EXEC_SUFFIX)
EHC_ALL_PUB_EXECS						:= $(patsubst %,$(EHC_BIN_PREFIX)%/$(EHC_EXEC_NAME)$(EXEC_SUFFIX),$(EHC_PUB_VARIANTS))
EHC_ALL_EXECS							:= $(patsubst %,$(EHC_BIN_PREFIX)%/$(EHC_EXEC_NAME)$(EXEC_SUFFIX),$(EHC_VARIANTS))

# sources + dpds, for .rul
EHC_RULES_1_SRC_RUL						:= $(EHC_SRC_PREFIX)rules.rul
EHC_RULES_2_SRC_RUL						:= $(EHC_SRC_PREFIX)rules2.rul
EHC_RULES_3_SRC_RL2						:= $(EHC_SRC_PREFIX)rules3.rul

EHC_RULES_ALL_SRC						:= $(EHC_RULES_1_SRC_RUL) $(EHC_RULES_2_SRC_RUL) $(EHC_RULES_3_SRC_RL2)

EHC_RULER_RULES							:= EHRulerRules
EHC_RULES_3_DRV_CAG						:= $(EHC_BLD_VARIANT_PREFIX)$(EHC_RULER_RULES).cag
EHC_RULES_3_DRV_AG						:= $(EHC_RULES_3_DRV_CAG:.cag=.ag)

# main + sources + dpds, for .chs
EHC_MAIN								:= EHC
EHC_HS_MAIN_SRC_CHS						:= $(patsubst %,$(EHC_SRC_PREFIX)%.chs,$(EHC_MAIN))
EHC_HS_MAIN_DRV_HS						:= $(patsubst $(EHC_SRC_PREFIX)%.chs,$(EHC_BLD_VARIANT_PREFIX)%.hs,$(EHC_HS_MAIN_SRC_CHS))

EHC_HS_UTIL_SRC_CHS						:= $(patsubst %,$(EHC_SRC_PREFIX)%.chs,\
													EHCommon EHOpts EHCnstr EHSubstitutable EHTyFitsIn EHTyFitsInCommon \
													EHGam EHGamUtils EHPred EHParser EHScannerCommon EHScanner EHScannerMachine EHCoreUtils EHDebug \
											)
EHC_HS_UTIL_DRV_HS						:= $(patsubst $(EHC_SRC_PREFIX)%.chs,$(EHC_BLD_VARIANT_PREFIX)%.hs,$(EHC_HS_UTIL_SRC_CHS))

EHC_HS_ALL_SRC_CHS						:= $(EHC_HS_MAIN_SRC_CHS) $(EHC_HS_UTIL_SRC_CHS)
EHC_HS_ALL_DRV_HS						:= $(EHC_HS_MAIN_DRV_HS) $(EHC_HS_UTIL_DRV_HS)

# main + sources + dpds, for .cag
EHC_AGMAIN_MAIN_SRC_CAG					:= $(patsubst %,$(EHC_SRC_PREFIX)%.cag,EHMainAG)
EHC_AGMAIN_DPDS_SRC_CAG					:= $(patsubst %,$(EHC_SRC_PREFIX)%.cag,EHInfer EHInferExpr \
													EHInferPatExpr EHInferTyExpr EHInferKiExpr EHInferData \
													EHInferCaseExpr EHPretty EHPrettyAST EHAbsSynAG \
													EHUniq EHExtraChecks EHGatherError \
													EHGenCore EHResolvePred EHInferClass \
											)
$(patsubst $(EHC_SRC_PREFIX)%.cag,$(EHC_BLD_VARIANT_PREFIX)%.hs,$(EHC_AGMAIN_MAIN_SRC_CAG)) \
										: $(patsubst $(EHC_SRC_PREFIX)%.cag,$(EHC_BLD_VARIANT_PREFIX)%.ag,$(EHC_AGMAIN_DPDS_SRC_CAG)) \
											$(EHC_RULES_3_DRV_AG)

EHC_AGEHAST_MAIN_SRC_CAG				:= $(patsubst %,$(EHC_SRC_PREFIX)%.cag,EHAbsSyn)
EHC_AGEHAST_DPDS_SRC_CAG				:= $(patsubst %,$(EHC_SRC_PREFIX)%.cag,EHAbsSynAG)
$(patsubst $(EHC_SRC_PREFIX)%.cag,$(EHC_BLD_VARIANT_PREFIX)%.hs,$(EHC_AGEHAST_MAIN_SRC_CAG)) \
										: $(patsubst $(EHC_SRC_PREFIX)%.cag,$(EHC_BLD_VARIANT_PREFIX)%.ag,$(EHC_AGEHAST_DPDS_SRC_CAG))

EHC_AGTY_MAIN_SRC_CAG					:= $(patsubst %,$(EHC_SRC_PREFIX)%.cag,EHTy)
EHC_AGTY_DPDS_SRC_CAG					:= $(patsubst %,$(EHC_SRC_PREFIX)%.cag,EHTyAbsSyn)
$(patsubst $(EHC_SRC_PREFIX)%.cag,$(EHC_BLD_VARIANT_PREFIX)%.hs,$(EHC_AGTY_MAIN_SRC_CAG)) \
										: $(patsubst $(EHC_SRC_PREFIX)%.cag,$(EHC_BLD_VARIANT_PREFIX)%.ag,$(EHC_AGTY_DPDS_SRC_CAG))

EHC_AGTY_FTV_MAIN_SRC_CAG				:= $(patsubst %,$(EHC_SRC_PREFIX)%.cag,EHTyFtv)
EHC_AGTY_FTV_DPDS_SRC_CAG				:= $(patsubst %,$(EHC_SRC_PREFIX)%.cag,EHTyAbsSyn)
$(patsubst $(EHC_SRC_PREFIX)%.cag,$(EHC_BLD_VARIANT_PREFIX)%.hs,$(EHC_AGTY_FTV_MAIN_SRC_CAG)) \
										: $(patsubst $(EHC_SRC_PREFIX)%.cag,$(EHC_BLD_VARIANT_PREFIX)%.ag,$(EHC_AGTY_FTV_DPDS_SRC_CAG))

EHC_AGTY_INST_MAIN_SRC_CAG				:= $(patsubst %,$(EHC_SRC_PREFIX)%.cag,EHTyInstantiate)
EHC_AGTY_INST_DPDS_SRC_CAG				:= $(patsubst %,$(EHC_SRC_PREFIX)%.cag,EHTyCommonAG EHTyAbsSyn)
$(patsubst $(EHC_SRC_PREFIX)%.cag,$(EHC_BLD_VARIANT_PREFIX)%.hs,$(EHC_AGTY_INST_MAIN_SRC_CAG)) \
										: $(patsubst $(EHC_SRC_PREFIX)%.cag,$(EHC_BLD_VARIANT_PREFIX)%.ag,$(EHC_AGTY_INST_DPDS_SRC_CAG))

EHC_AGTY_PRETTY_MAIN_SRC_CAG			:= $(patsubst %,$(EHC_SRC_PREFIX)%.cag,EHTyPretty)
EHC_AGTY_PRETTY_DPDS_SRC_CAG			:= $(patsubst %,$(EHC_SRC_PREFIX)%.cag,EHTyCommonAG EHTyAbsSyn)
$(patsubst $(EHC_SRC_PREFIX)%.cag,$(EHC_BLD_VARIANT_PREFIX)%.hs,$(EHC_AGTY_PRETTY_MAIN_SRC_CAG)) \
										: $(patsubst $(EHC_SRC_PREFIX)%.cag,$(EHC_BLD_VARIANT_PREFIX)%.ag,$(EHC_AGTY_PRETTY_DPDS_SRC_CAG))

EHC_AGTY_QU_MAIN_SRC_CAG				:= $(patsubst %,$(EHC_SRC_PREFIX)%.cag,EHTyQuantify)
EHC_AGTY_QU_DPDS_SRC_CAG				:= $(patsubst %,$(EHC_SRC_PREFIX)%.cag,EHTyCommonAG EHTyAbsSyn)
$(patsubst $(EHC_SRC_PREFIX)%.cag,$(EHC_BLD_VARIANT_PREFIX)%.hs,$(EHC_AGTY_QU_MAIN_SRC_CAG)) \
										: $(patsubst $(EHC_SRC_PREFIX)%.cag,$(EHC_BLD_VARIANT_PREFIX)%.ag,$(EHC_AGTY_QU_DPDS_SRC_CAG))

EHC_AGTY_SUBST_MAIN_SRC_CAG				:= $(patsubst %,$(EHC_SRC_PREFIX)%.cag,EHTySubst)
EHC_AGTY_SUBST_DPDS_SRC_CAG				:= $(patsubst %,$(EHC_SRC_PREFIX)%.cag,EHTyAbsSyn)
$(patsubst $(EHC_SRC_PREFIX)%.cag,$(EHC_BLD_VARIANT_PREFIX)%.hs,$(EHC_AGTY_SUBST_MAIN_SRC_CAG)) \
										: $(patsubst $(EHC_SRC_PREFIX)%.cag,$(EHC_BLD_VARIANT_PREFIX)%.ag,$(EHC_AGTY_SUBST_DPDS_SRC_CAG))

EHC_AGTY_ELIMALT_MAIN_SRC_CAG			:= $(patsubst %,$(EHC_SRC_PREFIX)%.cag,EHTyElimAlts)
EHC_AGTY_ELIMALT_DPDS_SRC_CAG			:= $(patsubst %,$(EHC_SRC_PREFIX)%.cag,EHTyAbsSyn)
$(patsubst $(EHC_SRC_PREFIX)%.cag,$(EHC_BLD_VARIANT_PREFIX)%.hs,$(EHC_AGTY_ELIMALT_MAIN_SRC_CAG)) \
										: $(patsubst $(EHC_SRC_PREFIX)%.cag,$(EHC_BLD_VARIANT_PREFIX)%.ag,$(EHC_AGTY_ELIMALT_DPDS_SRC_CAG))

EHC_AGTY_ELIMBT_MAIN_SRC_CAG			:= $(patsubst %,$(EHC_SRC_PREFIX)%.cag,EHTyElimBoth)
EHC_AGTY_ELIMBT_DPDS_SRC_CAG			:= $(patsubst %,$(EHC_SRC_PREFIX)%.cag,EHTyAbsSyn)
$(patsubst $(EHC_SRC_PREFIX)%.cag,$(EHC_BLD_VARIANT_PREFIX)%.hs,$(EHC_AGTY_ELIMBT_MAIN_SRC_CAG)) \
										: $(patsubst $(EHC_SRC_PREFIX)%.cag,$(EHC_BLD_VARIANT_PREFIX)%.ag,$(EHC_AGTY_ELIMBT_DPDS_SRC_CAG))

EHC_AGTY_ELIMEQ_MAIN_SRC_CAG			:= $(patsubst %,$(EHC_SRC_PREFIX)%.cag,EHTyElimEqual)
EHC_AGTY_ELIMEQ_DPDS_SRC_CAG			:= $(patsubst %,$(EHC_SRC_PREFIX)%.cag,EHTyAbsSyn)
$(patsubst $(EHC_SRC_PREFIX)%.cag,$(EHC_BLD_VARIANT_PREFIX)%.hs,$(EHC_AGTY_ELIMEQ_MAIN_SRC_CAG)) \
										: $(patsubst $(EHC_SRC_PREFIX)%.cag,$(EHC_BLD_VARIANT_PREFIX)%.ag,$(EHC_AGTY_ELIMEQ_DPDS_SRC_CAG))

EHC_AGTY_FRESH_MAIN_SRC_CAG				:= $(patsubst %,$(EHC_SRC_PREFIX)%.cag,EHTyFreshVar)
EHC_AGTY_FRESH_DPDS_SRC_CAG				:= $(patsubst %,$(EHC_SRC_PREFIX)%.cag,EHTyAbsSyn)
$(patsubst $(EHC_SRC_PREFIX)%.cag,$(EHC_BLD_VARIANT_PREFIX)%.hs,$(EHC_AGTY_FRESH_MAIN_SRC_CAG)) \
										: $(patsubst $(EHC_SRC_PREFIX)%.cag,$(EHC_BLD_VARIANT_PREFIX)%.ag,$(EHC_AGTY_FRESH_DPDS_SRC_CAG))

EHC_AGERR_MAIN_SRC_CAG					:= $(patsubst %,$(EHC_SRC_PREFIX)%.cag,EHError)
EHC_AGERR_DPDS_SRC_CAG					:= $(patsubst %,$(EHC_SRC_PREFIX)%.cag,EHErrorAbsSyn)
$(patsubst $(EHC_SRC_PREFIX)%.cag,$(EHC_BLD_VARIANT_PREFIX)%.hs,$(EHC_AGERR_MAIN_SRC_CAG)) \
										: $(patsubst $(EHC_SRC_PREFIX)%.cag,$(EHC_BLD_VARIANT_PREFIX)%.ag,$(EHC_AGERR_DPDS_SRC_CAG))

EHC_AGERR_PRETTY_MAIN_SRC_CAG			:= $(patsubst %,$(EHC_SRC_PREFIX)%.cag,EHErrorPretty)
EHC_AGERR_PRETTY_DPDS_SRC_CAG			:= $(patsubst %,$(EHC_SRC_PREFIX)%.cag,EHErrorAbsSyn)
$(patsubst $(EHC_SRC_PREFIX)%.cag,$(EHC_BLD_VARIANT_PREFIX)%.hs,$(EHC_AGERR_PRETTY_MAIN_SRC_CAG)) \
										: $(patsubst $(EHC_SRC_PREFIX)%.cag,$(EHC_BLD_VARIANT_PREFIX)%.ag,$(EHC_AGERR_PRETTY_DPDS_SRC_CAG))

EHC_AGCORE_MAIN_SRC_CAG					:= $(patsubst %,$(EHC_SRC_PREFIX)%.cag,EHCore)
EHC_AGCORE_DPDS_SRC_CAG					:= $(patsubst %,$(EHC_SRC_PREFIX)%.cag,EHCoreAbsSyn)
$(patsubst $(EHC_SRC_PREFIX)%.cag,$(EHC_BLD_VARIANT_PREFIX)%.hs,$(EHC_AGCORE_MAIN_SRC_CAG)) \
										: $(patsubst $(EHC_SRC_PREFIX)%.cag,$(EHC_BLD_VARIANT_PREFIX)%.ag,$(EHC_AGCORE_DPDS_SRC_CAG))

EHC_AGCORE_GRIN_MAIN_SRC_CAG			:= $(patsubst %,$(EHC_SRC_PREFIX)%.cag,EHCoreGrin)
EHC_AGCORE_GRIN_DPDS_SRC_CAG			:= $(patsubst %,$(EHC_SRC_PREFIX)%.cag,EHCoreCommonLev EHCoreAbsSyn)
$(patsubst $(EHC_SRC_PREFIX)%.cag,$(EHC_BLD_VARIANT_PREFIX)%.hs,$(EHC_AGCORE_GRIN_MAIN_SRC_CAG)) \
										: $(patsubst $(EHC_SRC_PREFIX)%.cag,$(EHC_BLD_VARIANT_PREFIX)%.ag,$(EHC_AGCORE_GRIN_DPDS_SRC_CAG))

EHC_AGCORE_JAVA_MAIN_SRC_CAG			:= $(patsubst %,$(EHC_SRC_PREFIX)%.cag,EHCoreJava)
EHC_AGCORE_JAVA_DPDS_SRC_CAG			:= $(patsubst %,$(EHC_SRC_PREFIX)%.cag,EHCoreCommonLev EHCoreAbsSyn)
$(patsubst $(EHC_SRC_PREFIX)%.cag,$(EHC_BLD_VARIANT_PREFIX)%.hs,$(EHC_AGCORE_JAVA_MAIN_SRC_CAG)) \
										: $(patsubst $(EHC_SRC_PREFIX)%.cag,$(EHC_BLD_VARIANT_PREFIX)%.ag,$(EHC_AGCORE_JAVA_DPDS_SRC_CAG))

EHC_AGCORE_PRETTY_MAIN_SRC_CAG			:= $(patsubst %,$(EHC_SRC_PREFIX)%.cag,EHCorePretty)
EHC_AGCORE_PRETTY_DPDS_SRC_CAG			:= $(patsubst %,$(EHC_SRC_PREFIX)%.cag,EHCoreAbsSyn)
$(patsubst $(EHC_SRC_PREFIX)%.cag,$(EHC_BLD_VARIANT_PREFIX)%.hs,$(EHC_AGCORE_PRETTY_MAIN_SRC_CAG)) \
										: $(patsubst $(EHC_SRC_PREFIX)%.cag,$(EHC_BLD_VARIANT_PREFIX)%.ag,$(EHC_AGCORE_PRETTY_DPDS_SRC_CAG))

EHC_AGCORE_SUBST_MAIN_SRC_CAG			:= $(patsubst %,$(EHC_SRC_PREFIX)%.cag,EHCoreSubst)
EHC_AGCORE_SUBST_DPDS_SRC_CAG			:= $(patsubst %,$(EHC_SRC_PREFIX)%.cag,EHCoreAbsSyn)
$(patsubst $(EHC_SRC_PREFIX)%.cag,$(EHC_BLD_VARIANT_PREFIX)%.hs,$(EHC_AGCORE_SUBST_MAIN_SRC_CAG)) \
										: $(patsubst $(EHC_SRC_PREFIX)%.cag,$(EHC_BLD_VARIANT_PREFIX)%.ag,$(EHC_AGCORE_SUBST_DPDS_SRC_CAG))

EHC_AGCORE_TRF_CONSTPROP_MAIN_SRC_CAG	:= $(patsubst %,$(EHC_SRC_PREFIX)%.cag,EHCoreTrfConstProp)
EHC_AGCORE_TRF_CONSTPROP_DPDS_SRC_CAG	:= $(patsubst %,$(EHC_SRC_PREFIX)%.cag,EHCoreCommonLev EHCoreAbsSyn)
$(patsubst $(EHC_SRC_PREFIX)%.cag,$(EHC_BLD_VARIANT_PREFIX)%.hs,$(EHC_AGCORE_TRF_CONSTPROP_MAIN_SRC_CAG)) \
										: $(patsubst $(EHC_SRC_PREFIX)%.cag,$(EHC_BLD_VARIANT_PREFIX)%.ag,$(EHC_AGCORE_TRF_CONSTPROP_DPDS_SRC_CAG))

EHC_AGCORE_TRF_FULLAZY_MAIN_SRC_CAG		:= $(patsubst %,$(EHC_SRC_PREFIX)%.cag,EHCoreTrfFullLazy)
EHC_AGCORE_TRF_FULLAZY_DPDS_SRC_CAG		:= $(patsubst %,$(EHC_SRC_PREFIX)%.cag,EHCoreTrfCommonFv EHCoreTrfCommonLev EHCoreCommonLev EHCoreAbsSyn)
$(patsubst $(EHC_SRC_PREFIX)%.cag,$(EHC_BLD_VARIANT_PREFIX)%.hs,$(EHC_AGCORE_TRF_FULLAZY_MAIN_SRC_CAG)) \
										: $(patsubst $(EHC_SRC_PREFIX)%.cag,$(EHC_BLD_VARIANT_PREFIX)%.ag,$(EHC_AGCORE_TRF_FULLAZY_DPDS_SRC_CAG))

EHC_AGCORE_TRF_INLLETALI_MAIN_SRC_CAG	:= $(patsubst %,$(EHC_SRC_PREFIX)%.cag,EHCoreTrfInlineLetAlias)
EHC_AGCORE_TRF_INLLETALI_DPDS_SRC_CAG	:= $(patsubst %,$(EHC_SRC_PREFIX)%.cag,EHCoreAbsSyn)
$(patsubst $(EHC_SRC_PREFIX)%.cag,$(EHC_BLD_VARIANT_PREFIX)%.hs,$(EHC_AGCORE_TRF_INLLETALI_MAIN_SRC_CAG)) \
										: $(patsubst $(EHC_SRC_PREFIX)%.cag,$(EHC_BLD_VARIANT_PREFIX)%.ag,$(EHC_AGCORE_TRF_INLLETALI_DPDS_SRC_CAG))

EHC_AGCORE_TRF_LAMLIFT_MAIN_SRC_CAG		:= $(patsubst %,$(EHC_SRC_PREFIX)%.cag,EHCoreTrfLamLift)
EHC_AGCORE_TRF_LAMLIFT_DPDS_SRC_CAG		:= $(patsubst %,$(EHC_SRC_PREFIX)%.cag,EHCoreTrfCommonFv EHCoreTrfCommonLev EHCoreCommonLev EHCoreAbsSyn)
$(patsubst $(EHC_SRC_PREFIX)%.cag,$(EHC_BLD_VARIANT_PREFIX)%.hs,$(EHC_AGCORE_TRF_LAMLIFT_MAIN_SRC_CAG)) \
										: $(patsubst $(EHC_SRC_PREFIX)%.cag,$(EHC_BLD_VARIANT_PREFIX)%.ag,$(EHC_AGCORE_TRF_LAMLIFT_DPDS_SRC_CAG))

EHC_AGCORE_TRF_LETUNREC_MAIN_SRC_CAG	:= $(patsubst %,$(EHC_SRC_PREFIX)%.cag,EHCoreTrfLetUnrec)
EHC_AGCORE_TRF_LETUNREC_DPDS_SRC_CAG	:= $(patsubst %,$(EHC_SRC_PREFIX)%.cag,EHCoreAbsSyn)
$(patsubst $(EHC_SRC_PREFIX)%.cag,$(EHC_BLD_VARIANT_PREFIX)%.hs,$(EHC_AGCORE_TRF_LETUNREC_MAIN_SRC_CAG)) \
										: $(patsubst $(EHC_SRC_PREFIX)%.cag,$(EHC_BLD_VARIANT_PREFIX)%.ag,$(EHC_AGCORE_TRF_LETUNREC_DPDS_SRC_CAG))

EHC_AGCORE_TRF_RENUNQ_MAIN_SRC_CAG		:= $(patsubst %,$(EHC_SRC_PREFIX)%.cag,EHCoreTrfRenUniq)
EHC_AGCORE_TRF_RENUNQ_DPDS_SRC_CAG		:= $(patsubst %,$(EHC_SRC_PREFIX)%.cag,EHCoreAbsSyn)
$(patsubst $(EHC_SRC_PREFIX)%.cag,$(EHC_BLD_VARIANT_PREFIX)%.hs,$(EHC_AGCORE_TRF_RENUNQ_MAIN_SRC_CAG)) \
										: $(patsubst $(EHC_SRC_PREFIX)%.cag,$(EHC_BLD_VARIANT_PREFIX)%.ag,$(EHC_AGCORE_TRF_RENUNQ_DPDS_SRC_CAG))

EHC_AG_D_MAIN_SRC_CAG					:= $(EHC_AGTY_MAIN_SRC_CAG) \
											$(EHC_AGERR_MAIN_SRC_CAG) \
											$(EHC_AGCORE_MAIN_SRC_CAG) \
											$(EHC_AGEHAST_MAIN_SRC_CAG)
EHC_AG_S_MAIN_SRC_CAG					:= $(EHC_AGTY_FTV_MAIN_SRC_CAG) \
											$(EHC_AGTY_INST_MAIN_SRC_CAG) \
											$(EHC_AGTY_PRETTY_MAIN_SRC_CAG) \
											$(EHC_AGTY_QU_MAIN_SRC_CAG) \
											$(EHC_AGTY_SUBST_MAIN_SRC_CAG) \
											$(EHC_AGTY_ELIMALT_MAIN_SRC_CAG) \
											$(EHC_AGTY_ELIMBT_MAIN_SRC_CAG) \
											$(EHC_AGTY_ELIMEQ_MAIN_SRC_CAG) \
											$(EHC_AGTY_FRESH_MAIN_SRC_CAG) \
											$(EHC_AGERR_PRETTY_MAIN_SRC_CAG) \
											$(EHC_AGCORE_GRIN_MAIN_SRC_CAG) \
											$(EHC_AGCORE_JAVA_MAIN_SRC_CAG) \
											$(EHC_AGCORE_PRETTY_MAIN_SRC_CAG) \
											$(EHC_AGCORE_SUBST_MAIN_SRC_CAG) \
											$(EHC_AGCORE_TRF_CONSTPROP_MAIN_SRC_CAG) \
											$(EHC_AGCORE_TRF_FULLAZY_MAIN_SRC_CAG) \
											$(EHC_AGCORE_TRF_INLLETALI_MAIN_SRC_CAG) \
											$(EHC_AGCORE_TRF_LAMLIFT_MAIN_SRC_CAG) \
											$(EHC_AGCORE_TRF_LETUNREC_MAIN_SRC_CAG) \
											$(EHC_AGCORE_TRF_RENUNQ_MAIN_SRC_CAG) \
											$(EHC_AGMAIN_MAIN_SRC_CAG)
EHC_AG_DS_MAIN_SRC_CAG					:= 
EHC_AG_ALL_MAIN_SRC_CAG					:= $(EHC_AG_D_MAIN_SRC_CAG) $(EHC_AG_S_MAIN_SRC_CAG) $(EHC_AG_DS_MAIN_SRC_CAG)

EHC_AG_ALL_DPDS_SRC_CAG					:= $(sort \
											$(EHC_AGMAIN_DPDS_SRC_CAG) \
											$(EHC_AGEHAST_DPDS_SRC_CAG) \
											$(EHC_AGTY_DPDS_SRC_CAG) \
											$(EHC_AGTY_FTV_DPDS_SRC_CAG) \
											$(EHC_AGTY_INST_DPDS_SRC_CAG) \
											$(EHC_AGTY_PRETTY_DPDS_SRC_CAG) \
											$(EHC_AGTY_QU_DPDS_SRC_CAG) \
											$(EHC_AGTY_SUBST_DPDS_SRC_CAG) \
											$(EHC_AGTY_ELIMALT_DPDS_SRC_CAG) \
											$(EHC_AGTY_ELIMBT_DPDS_SRC_CAG) \
											$(EHC_AGTY_ELIMEQ_DPDS_SRC_CAG) \
											$(EHC_AGTY_FRESH_DPDS_SRC_CAG) \
											$(EHC_AGERR_DPDS_SRC_CAG) \
											$(EHC_AGERR_PRETTY_DPDS_SRC_CAG) \
											$(EHC_AGCORE_DPDS_SRC_CAG) \
											$(EHC_AGCORE_GRIN_DPDS_SRC_CAG) \
											$(EHC_AGCORE_JAVA_DPDS_SRC_CAG) \
											$(EHC_AGCORE_PRETTY_DPDS_SRC_CAG) \
											$(EHC_AGCORE_SUBST_DPDS_SRC_CAG) \
											$(EHC_AGCORE_TRF_CONSTPROP_DPDS_SRC_CAG) \
											$(EHC_AGCORE_TRF_FULLAZY_DPDS_SRC_CAG) \
											$(EHC_AGCORE_TRF_INLLETALI_DPDS_SRC_CAG) \
											$(EHC_AGCORE_TRF_LAMLIFT_DPDS_SRC_CAG) \
											$(EHC_AGCORE_TRF_LETUNREC_DPDS_SRC_CAG) \
											$(EHC_AGCORE_TRF_RENUNQ_DPDS_SRC_CAG) \
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
EHC_ON_RULES_VIEW_11					:= P

# what is generated by Ruler
EHC_BY_RULER_GROUPS_BASE				:= expr.base patexpr.base tyexpr.base decl.base
EHC_BY_RULER_RULES_BASE					:= e.int e.char e.var e.con
EHC_BY_RULER_RULES_10					:= $(EHC_BY_RULER_RULES_BASE)
EHC_BY_RULER_RULES_9					:= $(EHC_BY_RULER_RULES_10)
EHC_BY_RULER_RULES_8					:= $(EHC_BY_RULER_RULES_9)
EHC_BY_RULER_RULES_7					:= $(EHC_BY_RULER_RULES_8)
EHC_BY_RULER_RULES_6					:= $(EHC_BY_RULER_RULES_7)
EHC_BY_RULER_RULES_5					:= $(EHC_BY_RULER_RULES_6)
EHC_BY_RULER_RULES_4_2					:= $(EHC_BY_RULER_RULES_5) \
											p.int p.char p.con p.apptop p.app p.var p.varas p.ann \
											e.apptop e.app e.app.f e.lam e.let e.ann \
											t.con t.app t.wild t.quant t.var t.var.w \
											d.tysig d.val
EHC_BY_RULER_RULES_4					:= $(EHC_BY_RULER_RULES_4_2)
EHC_BY_RULER_RULES_3					:= $(EHC_BY_RULER_RULES_4) 
EHC_BY_RULER_RULES_2					:= $(EHC_BY_RULER_RULES_3)
EHC_BY_RULER_RULES_1					:= $(EHC_BY_RULER_RULES_2)

# derived
EHC_AG_D_MAIN_DRV_AG					:= $(patsubst $(EHC_SRC_PREFIX)%.cag,$(EHC_BLD_VARIANT_PREFIX)%.ag,$(EHC_AG_D_MAIN_SRC_CAG))
EHC_AG_S_MAIN_DRV_AG					:= $(patsubst $(EHC_SRC_PREFIX)%.cag,$(EHC_BLD_VARIANT_PREFIX)%.ag,$(EHC_AG_S_MAIN_SRC_CAG))
EHC_AG_DS_MAIN_DRV_AG					:= $(patsubst $(EHC_SRC_PREFIX)%.cag,$(EHC_BLD_VARIANT_PREFIX)%.ag,$(EHC_AG_DS_MAIN_SRC_CAG))
EHC_AG_ALL_MAIN_DRV_AG					:= $(EHC_AG_D_MAIN_DRV_AG) $(EHC_AG_S_MAIN_DRV_AG) $(EHC_AG_DS_MAIN_DRV_AG)

EHC_AG_D_MAIN_DRV_HS					:= $(EHC_AG_D_MAIN_DRV_AG:.ag=.hs)
EHC_AG_S_MAIN_DRV_HS					:= $(EHC_AG_S_MAIN_DRV_AG:.ag=.hs)
EHC_AG_DS_MAIN_DRV_HS					:= $(EHC_AG_DS_MAIN_DRV_AG:.ag=.hs)
EHC_AG_ALL_MAIN_DRV_HS					:= $(EHC_AG_D_MAIN_DRV_HS) $(EHC_AG_S_MAIN_DRV_HS) $(EHC_AG_DS_MAIN_DRV_HS)

EHC_AG_ALL_DPDS_DRV_AG					:= $(patsubst $(EHC_SRC_PREFIX)%.cag,$(EHC_BLD_VARIANT_PREFIX)%.ag,$(EHC_AG_ALL_DPDS_SRC_CAG))

# all dependents for a variant to kick of building
EHC_ALL_DPDS							:= $(EHC_HS_ALL_DRV_HS) $(EHC_AG_ALL_MAIN_DRV_HS) $(UHC_HS_ALL_DRV_HS) $(UHC_AG_ALL_MAIN_DRV_HS) $(GRIN_AG_ALL_MAIN_DRV_HS)

# variant dispatch rules
$(EHC_ALL_EXECS): %: $(EHC_ALL_SRC) $(GRIN_ALL_SRC) $(UHC_ALL_SRC) $(EHC_MKF)
	$(MAKE) EHC_VARIANT=$(notdir $(*D)) GRIN_VARIANT=$(notdir $(*D)) ehc-variant

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

# rules for ehc compiler
ehc-variant: 
	$(MAKE) EHC_VARIANT_RULER_SEL="(($(EHC_VARIANT)=$(EHC_ON_RULES_VIEW_$(EHC_VARIANT)))).($(EHC_BY_RULER_GROUPS_BASE)).($(EHC_BY_RULER_RULES_$(EHC_VARIANT)))" \
	  ehc-variant-dflt

ehc-variant-dflt: $(EHC_ALL_DPDS) $(EHC_RULES_3_DRV_CAG)
	mkdir -p $(dir $(EHC_BLD_EXEC))
	$(GHC) --make $(GHC_OPTS) -i$(EHC_BLD_VARIANT_PREFIX) -i$(LIB_SRC_PREFIX) $(EHC_BLD_VARIANT_PREFIX)$(EHC_MAIN).hs -o $(EHC_BLD_EXEC)

#ehc-variant-selrule: 
#	$(MAKE) EHC_VARIANT_RULER_SEL="($(EHC_VARIANT)).(expr.base).(e.int e.char)" ehc-variant-dflt

$(EHC_AG_ALL_MAIN_DRV_AG) $(EHC_AG_ALL_DPDS_DRV_AG): $(EHC_BLD_VARIANT_PREFIX)%.ag: $(EHC_SRC_PREFIX)%.cag $(SHUFFLE)
	mkdir -p $(@D)
	$(SHUFFLE) --gen=$(EHC_VARIANT) --base=$(*F) --ag --preamble=no --lhs2tex=no --order="$(EHC_SHUFFLE_ORDER)" $< > $@

$(EHC_RULES_3_DRV_AG): $(EHC_BLD_VARIANT_PREFIX)%.ag: $(EHC_BLD_VARIANT_PREFIX)%.cag $(SHUFFLE)
	$(SHUFFLE) --gen=$(EHC_VARIANT) --base=$(*F) --ag --preamble=no --lhs2tex=no --order="$(EHC_SHUFFLE_ORDER)" $< > $@

$(EHC_AG_D_MAIN_DRV_HS): %.hs: %.ag
	$(AGC) -dr -P$(EHC_BLD_VARIANT_PREFIX) $<

$(EHC_AG_S_MAIN_DRV_HS): %.hs: %.ag
	$(AGC) -cfspr -P$(EHC_BLD_VARIANT_PREFIX) $<

$(EHC_AG_DS_MAIN_DRV_HS): %.hs: %.ag
	$(AGC) -dcfspr -P$(EHC_BLD_VARIANT_PREFIX) $<

$(EHC_HS_MAIN_DRV_HS): $(EHC_BLD_VARIANT_PREFIX)%.hs: $(EHC_SRC_PREFIX)%.chs $(SHUFFLE)
	mkdir -p $(@D)
	$(SHUFFLE) --gen=$(EHC_VARIANT) --base=Main --hs --preamble=no --lhs2tex=no --order="$(EHC_SHUFFLE_ORDER)" $< > $@

$(EHC_HS_UTIL_DRV_HS): $(EHC_BLD_VARIANT_PREFIX)%.hs: $(EHC_SRC_PREFIX)%.chs $(SHUFFLE)
	mkdir -p $(@D)
	$(SHUFFLE) --gen=$(EHC_VARIANT) --base=$(*F) --hs --preamble=no --lhs2tex=no --order="$(EHC_SHUFFLE_ORDER)" $< > $@

$(EHC_RULES_3_DRV_CAG): $(EHC_RULES_3_SRC_RL2) $(RULER2) $(EHC_MKF)
	mkdir -p $(@D)
	$(RULER2) $(RULER2_OPTS) --ag --wrapshuffle --preamble=no --selrule="$(EHC_VARIANT_RULER_SEL)" --base=$(*F) $< > $@

