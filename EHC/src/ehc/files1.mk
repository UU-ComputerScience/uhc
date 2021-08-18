###########################################################################################
# variant, to be configured on top level
###########################################################################################

# see variant.mk

###########################################################################################
# location of ehc src:
###########################################################################################

# see shared.mk

# this file + other mk files
EHC_MKF									:= $(patsubst %,$(SRC_EHC_PREFIX)%.mk,files1 files2 shared variant)

# end products, binary, executable, etc
#EHC_EXEC_NAME							:= ehc
EHC_HADDOCK_NAME						:= hdoc
EHC_ALL_HADDOCKS						:= $(patsubst %,$(EHC_HDOC_PREFIX)%/$(EHC_HADDOCK_NAME),$(EHC_VARIANTS))

#UHC_EXEC_NAME							:= uhc
#UHC_BLD_EXEC							:= $(EHC_BIN_PREFIX)$(UHC_EXEC_NAME)$(EXEC_SUFFIX)
UHC_INSTALL_EXEC						:= $(INSTALL_UHC_BIN_PREFIX)$(UHC_EXEC_NAME)$(EXEC_SUFFIX)
UHC_INSTALL_SHELL						:= $(INSTALL_UHC_BIN_PREFIX)$(UHC_EXEC_NAME)
# ehc
EHC_FOR_UHC_BLD_EXEC					:= $(call FUN_EHC_INSTALL_VARIANT_ASPECTS_EXEC,$(EHC_UHC_INSTALL_VARIANT))
EHC_FOR_UHCLIGHT_BLD_EXEC				:= $(call FUN_EHC_INSTALL_VARIANT_ASPECTS_EXEC,$(EHC_UHCLIGHT_CABAL_VARIANT))
# ehcr
EHC_FOR_UHCRUN_BLD_EXEC					:= $(call FUN_EHCRUN_INSTALL_VARIANT_ASPECTS_EXEC,$(EHC_UHC_INSTALL_VARIANT))

## sources + dpds, for .rul
#EHC_RULES_1_SRC_RUL						:= $(SRC_EHC_PREFIX)rules.rul
#EHC_RULES_2_SRC_RUL						:= $(SRC_EHC_PREFIX)rules2.rul
#EHC_RULES_3_SRC_RL2						:= $(SRC_EHC_RULES_PREFIX)EhcRulesOrig.rul
#
#EHC_RULER_RULES							:= EHRulerRules
#EHC_RULES_3_DRV_CAG						:= $(EHC_BLD_VARIANT_ASPECTS_PREFIX)$(EHC_RULER_RULES).cag
#EHC_RULES_3_DRV_AG						:= $(EHC_RULES_3_DRV_CAG:.cag=.ag)
#
#EHC_RULES_4_MAIN_SRC_RUL				:= $(patsubst %,$(SRC_EHC_RULES_PREFIX)%.rul,EhcRulesExpr2 EhcRulesTyMatch EhcRulesTyElimAlt)
#EHC_RULES_4_DPDS_SRC_RUL				:= $(patsubst %,$(SRC_EHC_RULES_PREFIX)%.rul, \
#													EhcRulesShared EhcRulesShared2 \
#													EhcRulesAST EhcRulesCommon \
#													EhcRulesRelations EhcRulesCommonSchemes EhcRulesSchemes EhcRulesSchemes2 \
#											)
#EHC_RULES_ALL_SRC						:= $(EHC_RULES_1_SRC_RUL) $(EHC_RULES_2_SRC_RUL) $(EHC_RULES_3_SRC_RL2) $(EHC_RULES_4_MAIN_SRC_RUL) $(EHC_RULES_4_DPDS_SRC_RUL)
#

# library
# derived stuff
LIB_EHC_CABAL_DRV						:= $(EHC_BLD_LIBEHC_VARIANT_PREFIX)lib-$(GHC_PKG_NAME_PREFIX)$(LIB_EHC_BASE)$(EHC_VARIANT_ASPECTS).cabal
LIB_EHC_SETUP_HS_DRV					:= $(EHC_BLD_LIBEHC_VARIANT_PREFIX)Setup.hs
LIB_EHC_SETUP2							:= $(EHC_BLD_LIBEHC_VARIANT_PREFIX)setup$(EXEC_SUFFIX)
LIB_EHC_SETUP							:= ./setup$(EXEC_SUFFIX)

# main + sources + dpds, for .chs
EHC_MAIN								:= EHC
EHC_HS_MAIN_SRC_CHS						:= $(patsubst %,$(SRC_EHC_PREFIX)%.chs,$(EHC_MAIN))
EHC_HS_MAIN_DRV_HS						:= $(patsubst $(SRC_EHC_PREFIX)%.chs,$(EHC_BLD_VARIANT_ASPECTS_PREFIX)%.hs,$(EHC_HS_MAIN_SRC_CHS))

EHCRUN_MAIN								:= EHCRun
EHCRUN_HS_MAIN_SRC_CHS					:= $(patsubst %,$(SRC_EHC_PREFIX)%.chs,$(EHCRUN_MAIN))
EHCRUN_HS_MAIN_DRV_HS					:= $(patsubst $(SRC_EHC_PREFIX)%.chs,$(EHC_BLD_VARIANT_ASPECTS_PREFIX)%.hs,$(EHCRUN_HS_MAIN_SRC_CHS))

EHC_HS_UTIL_SRC_CHS_DFLT				:= $(patsubst %,$(SRC_EHC_PREFIX)%.chs,\
													FinalEnv Substitutable Opts Gam VarMp VarLookup Deriving Generics NameAspect DerivationTree Pred HI LamInfo AbstractCore \
													Config ConfigInternalVersions ConfigCabal Paths_uhc_light \
													$(addprefix EH/,Main) \
													$(addprefix CHR/,\
														$(addprefix CtxtRedOnly/,Key Constraint Guard Solve Instances) \
														$(addprefix TySys/,Types) \
													) \
													$(addprefix AbstractCore/,Utils) \
													$(addprefix AnaDomain/,Utils) \
													$(addprefix VarMp/,Utils) \
													$(addprefix Opts/,Base CommandLine) \
													$(addprefix Pred/CtxtRedOnly/,ToCHR Evidence EvidenceToCore Heuristics RedGraph) \
													$(addprefix Base/,UnderDev Trace Range TermLike UID Parser Parser2 Pragma Strictness Target Fld Common HsName Debug CfgPP LaTeX HtmlCommon FileSearchLocation PackageDatabase ParseUtils Optimize) \
													$(addprefix Base/HsName/,Builtin) \
													$(addprefix Scanner/,Common Machine Scanner Token TokenParser) \
													$(addsuffix /Parser,Ty EH HS Core CoreRun Foreign GrinCode) \
													$(addsuffix /Trf,Core JavaScript Cmm) \
													$(addprefix Ty/,FIEnv FIEnv2 FitsInCommon FitsInCommon2 FitsIn Utils1 Utils2 AppSpineGam Trf/BetaReduce) \
													$(addprefix Gam/,Base Utils Instantiate Quantify Full ClGam AppSpineGam FixityGam TyGam KiGam DataGam PolGam TyKiGam ValGam ClassDefaultGam) \
													$(addprefix CodeGen/,CEnv BuiltinPrims BasicAnnot Bits BuiltinSizeInfo GenC Tag CVar ValAccess Const RefGenerator GenJavaLike ModuleImportExportImpl ImportUsedModules TrfUtils) \
													$(addprefix Module/,Merge ImportExport) \
													$(addprefix Foreign/,Boxing) \
													$(addprefix CoreRun/,Run Prim) \
													$(addprefix CoreRun/Run/,Val) \
													$(addprefix CoreRun/Run/Val/,RunImplStk RunExplStk Prim) \
													$(addprefix Core/,Utils Merge FFI Coercion) \
													$(addprefix Core/SysF/,AsTy) \
													$(addprefix EHC/,Main Common FileSuffMp Environment CompileUnit CompileGroup CompileRun InitialSetup ASTHandler ASTPipeline ASTTypes BuildFunction \
														$(addprefix Main/,Utils Compile) \
														$(addprefix BuildFunction/,Run) \
														$(addprefix ASTHandler/,Instances) \
														$(addprefix CompileRun/,Base) \
														$(addprefix CompilePhase/,Run Parsers Output Translations Transformations Common \
															FlowBetweenPhase TransformGrin Semantics \
															CompileLLVM CompileC CompileJVM CompileJavaScript Link \
															Cleanup Module TopLevelPhases \
													)	) \
													Debug/HighWaterMark \
											)

EHC_HS_UTIL_SRC_CHS_ASIS				:= $(patsubst %,$(SRC_EHC_PREFIX)%.chs,\
													API \
													$(addsuffix /API,Core CoreRun Base) \
													$(addsuffix /API/Internal,CoreRun) \
											)

EHC_HS_UTIL_SRC_CHS						:= $(EHC_HS_UTIL_SRC_CHS_DFLT) $(EHC_HS_UTIL_SRC_CHS_ASIS)

EHC_HS_UTIL_DRV_HS_DFLT					:= $(patsubst $(SRC_EHC_PREFIX)%.chs,$(EHC_BLD_LIB_HS_VARIANT_PREFIX)%.hs,$(EHC_HS_UTIL_SRC_CHS_DFLT))
EHC_HS_UTIL_DRV_HS_ASIS					:= $(patsubst $(SRC_EHC_PREFIX)%.chs,$(EHC_BLD_LIB_HS_VARIANT_PREFIX)%.hs,$(EHC_HS_UTIL_SRC_CHS_ASIS))
EHC_HS_UTIL_DRV_HS						:= $(EHC_HS_UTIL_DRV_HS_DFLT) $(EHC_HS_UTIL_DRV_HS_ASIS)

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
EHC_HS_ALL_DRV_HS						:= $(EHCRUN_HS_MAIN_DRV_HS) $(EHC_HS_MAIN_DRV_HS) $(EHC_HS_ALL_DRV_HS_NO_MAIN)


# main + sources + dpds, for .cag
EHC_MK_AG_S_DEP_MK						:= $(EHC_BLD_LIB_HS_VARIANT_PREFIX)files-ag-s-dep.mk
EHC_MK_AG_D_DEP_MK						:= $(EHC_BLD_LIB_HS_VARIANT_PREFIX)files-ag-d-dep.mk

# conditional turned off, because text building depends on default rules:
ifeq ($(INCLUDE_DERIVED_MK),yes)
-include $(EHC_MK_AG_S_DEP_MK)
-include $(EHC_MK_AG_D_DEP_MK)
endif

EHC_AG_DS_MAIN_SRC_CAG					:= 
EHC_AG_ALL_MAIN_SRC_CAG					:= $(EHC_AG_D_MAIN_SRC_CAG) $(EHC_AG_S_MAIN_SRC_CAG) $(EHC_AG_DS_MAIN_SRC_CAG)
EHC_AG_ALL_DPDS_SRC_CAG					:= $(sort $(EHC_AG_D_DPDS_SRC_CAG) $(EHC_AG_S_DPDS_SRC_CAG))

$(patsubst $(SRC_EHC_PREFIX)%.cag,$(EHC_BLD_LIB_HS_VARIANT_PREFIX)%.hs,$(EHC_EH_MAINAG_MAIN_SRC_AG)) \
										: $(if $(EHC_CFG_USE_RULER),$(EHC_RULES_3_DRV_AG))


# all src
EHC_ALL_CHUNK_SRC						:= $(EHC_AG_ALL_MAIN_SRC_CAG) $(EHC_AG_ALL_DPDS_SRC_CAG) $(EHC_HS_ALL_SRC_CHS)
EHC_ALL_SRC								:= $(EHC_ALL_CHUNK_SRC) \
											$(if $(EHC_CFG_USE_RULER),$(EHC_RULES_ALL_SRC)) \
											$(EHC_MKF)
EHC_ALL_SRC_FIND						:= $(shell find $(call FUN_PREFIX2DIR,$(SRC_EHC_PREFIX)) \( -name '*.chs' -or -name '*.cag' \))
EHC_ALL_SRC_GEN							:= $(if $(EHC_CFG_USE_RULER),$(EHC_RULES_3_DRV_CAG),)

# distribution
EHC_DIST_FILES							:= $(EHC_ALL_SRC)

###########################################################################################
# generated files
###########################################################################################

# file with signature of code
EHC_HS_SIG_MAIN							:= SourceCodeSig
EHC_HS_SIG_DRV_HS						:= $(EHC_BLD_LIB_HS_VARIANT_PREFIX)$(EHC_HS_SIG_MAIN).hs

# file with info about installation configuration
EHC_HS_CFGINSTALL_MAIN					:= ConfigInstall
EHC_HS_CFGINSTALL_DRV_HS				:= $(EHC_BLD_LIB_HS_VARIANT_PREFIX)$(EHC_HS_CFGINSTALL_MAIN).hs

###########################################################################################
# uhc-light files in cabal buildable distr
###########################################################################################

CABALDIST_UHCLIGHT_SRC_ALL_DRV_NO_MAIN_PREFIX	:= $(CABALDIST_UHCLIGHT_SRC_PREFIX)$(LIB_EHC_HS_PREFIX)

#EHC_CABALDIST_ALL_DRV_HS_NO_MAIN		:= $(patsubst $(EHC_BLD_LIB_HS_VARIANT_PREFIX)%.hs,$(CABALDIST_SRC_ALL_DRV_NO_MAIN_PREFIX)%.hs,$(EHC_HS_ALL_DRV_HS_NO_MAIN))
#EHC_CABALDIST_MAIN_DRV_HS				:= $(patsubst $(EHC_BLD_VARIANT_ASPECTS_PREFIX)%.hs,$(CABALDIST_SRC_PREFIX)%.hs,$(EHC_HS_MAIN_DRV_HS))

CABALDIST_UHCLIGHT_VARIANT_LIB_PREFIX			:= $(call FUN_DIR_VARIANT_LIB_PREFIX,$(CABALDIST_UHCLIGHT_PREFIX),$(EHC_VARIANT))

###########################################################################################
# (re)generate derived makefiles
###########################################################################################

$(EHC_MK_AG_S_DEP_MK) : $(SRC_EHC_PREFIX)files-ag-s.dep $(SHUFFLE) $(EHC_ALL_SRC_FIND)
	mkdir -p $(EHC_BLD_LIB_HS_VARIANT_PREFIX)
	$(SHUFFLE) $(SRC_EHC_PREFIX)files-ag-s.dep --dep \
	  --depnameprefix=EHC_ \
	  --depsrcvar=SRC_EHC_PREFIX \
	  --depdstvar=EHC_BLD_LIB_HS_VARIANT_PREFIX \
	  --depmainvar=EHC_AG_S_MAIN_SRC_CAG \
	  --depdpdsvar=EHC_AG_S_DPDS_SRC_CAG \
	  --deporigdpdsvar=EHC_AG_S_ODPDS_SRC_CAG \
	  --depderivdpdsvar=EHC_AG_S_DDPDS_DERIV_AG \
	  --depbase=$(SRC_EHC_PREFIX) \
	  --depterm="EHRulerRules>" \
	  --depign="EHRulerRules EHRulerRules.cag" \
	    > $@

$(EHC_MK_AG_D_DEP_MK) : $(SRC_EHC_PREFIX)files-ag-d.dep $(SHUFFLE) $(EHC_ALL_SRC_FIND)
	mkdir -p $(EHC_BLD_LIB_HS_VARIANT_PREFIX)
	$(SHUFFLE) $(SRC_EHC_PREFIX)files-ag-d.dep --dep \
	  --depnameprefix=EHC_ \
	  --depsrcvar=SRC_EHC_PREFIX \
	  --depdstvar=EHC_BLD_LIB_HS_VARIANT_PREFIX \
	  --depmainvar=EHC_AG_D_MAIN_SRC_CAG \
	  --depdpdsvar=EHC_AG_D_DPDS_SRC_CAG \
	  --deporigdpdsvar=EHC_AG_D_ODPDS_SRC_CAG \
	  --depderivdpdsvar=EHC_AG_D_DDPDS_DERIV_AG \
	  --depbase=$(SRC_EHC_PREFIX) \
	  --depterm="EHRulerRules>" \
	  --depign="EHRulerRules EHRulerRules.cag" \
	    > $@

###########################################################################################
# configuration of tools dependend on variant
###########################################################################################

EHC_UUAGC_OPTS_WHEN_UHC_AST_DATA_99		:= $(UUAGC_OPTS_WHEN_UHC_AST_DATA)
EHC_UUAGC_OPTS_WHEN_UHC_AST_DATA_100	:= $(EHC_UUAGC_OPTS_WHEN_UHC_AST_DATA_99)
EHC_UUAGC_OPTS_WHEN_UHC_AST_DATA_101	:= $(EHC_UUAGC_OPTS_WHEN_UHC_AST_DATA_100)
EHC_UUAGC_OPTS_WHEN_UHC_AST_DATA_102	:= $(EHC_UUAGC_OPTS_WHEN_UHC_AST_DATA_100)
EHC_UUAGC_OPTS_WHEN_UHC_AST_DATA_103	:= $(EHC_UUAGC_OPTS_WHEN_UHC_AST_DATA_100)

EHC_UUAGC_OPTS_WHEN_UHC_AST_SEM_99		:= $(UUAGC_OPTS_WHEN_UHC_AST_SEM)
EHC_UUAGC_OPTS_WHEN_UHC_AST_SEM_100		:= $(EHC_UUAGC_OPTS_WHEN_UHC_AST_SEM_99)
EHC_UUAGC_OPTS_WHEN_UHC_AST_SEM_101		:= $(EHC_UUAGC_OPTS_WHEN_UHC_AST_SEM_100)
EHC_UUAGC_OPTS_WHEN_UHC_AST_SEM_102		:= $(EHC_UUAGC_OPTS_WHEN_UHC_AST_SEM_100)
EHC_UUAGC_OPTS_WHEN_UHC_AST_SEM_103		:= $(EHC_UUAGC_OPTS_WHEN_UHC_AST_SEM_100)

EHC_SHUFFLE_OPTS_WHEN_UHC_99			:= $(SHUFFLE_OPTS_WHEN_UHC)
EHC_SHUFFLE_OPTS_WHEN_UHC_100			:= $(EHC_SHUFFLE_OPTS_WHEN_UHC_99)
EHC_SHUFFLE_OPTS_WHEN_UHC_101			:= $(EHC_SHUFFLE_OPTS_WHEN_UHC_100)
EHC_SHUFFLE_OPTS_WHEN_UHC_102			:= $(EHC_SHUFFLE_OPTS_WHEN_UHC_100)

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
EHC_ON_RULES_VIEW_17					:= EP
EHC_ON_RULES_VIEW_18					:= EP
EHC_ON_RULES_VIEW_20					:= MD
EHC_ON_RULES_VIEW_94					:= MD
EHC_ON_RULES_VIEW_95					:= MD
EHC_ON_RULES_VIEW_96					:= MD
EHC_ON_RULES_VIEW_97					:= NUM
EHC_ON_RULES_VIEW_98					:= NUM
EHC_ON_RULES_VIEW_99					:= HS
EHC_ON_RULES_VIEW_100					:= HS
EHC_ON_RULES_VIEW_101					:= HS
EHC_ON_RULES_VIEW_102					:= HS
EHC_ON_RULES_VIEW_103					:= HS

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
EHC_BY_RULER_RULES_8					:= $(EHC_BY_RULER_RULES_7)
EHC_BY_RULER_RULES_9					:= $(EHC_BY_RULER_RULES_8)
EHC_BY_RULER_RULES_10					:= $(EHC_BY_RULER_RULES_9)
EHC_BY_RULER_RULES_11					:= $(EHC_BY_RULER_RULES_10)
EHC_BY_RULER_RULES_12					:= $(EHC_BY_RULER_RULES_11)
EHC_BY_RULER_RULES_13					:= $(EHC_BY_RULER_RULES_12)
EHC_BY_RULER_RULES_14					:= $(EHC_BY_RULER_RULES_13)
EHC_BY_RULER_RULES_15					:= $(EHC_BY_RULER_RULES_14)
EHC_BY_RULER_RULES_16					:= $(EHC_BY_RULER_RULES_15)
EHC_BY_RULER_RULES_17					:= $(EHC_BY_RULER_RULES_16)
EHC_BY_RULER_RULES_18					:= $(EHC_BY_RULER_RULES_17)
EHC_BY_RULER_RULES_20					:= $(EHC_BY_RULER_RULES_18)
EHC_BY_RULER_RULES_94					:= $(EHC_BY_RULER_RULES_20)
EHC_BY_RULER_RULES_95					:= $(EHC_BY_RULER_RULES_94)
EHC_BY_RULER_RULES_96					:= $(EHC_BY_RULER_RULES_95)
EHC_BY_RULER_RULES_97					:= $(EHC_BY_RULER_RULES_96)
EHC_BY_RULER_RULES_98					:= $(EHC_BY_RULER_RULES_97)
EHC_BY_RULER_RULES_99					:= $(EHC_BY_RULER_RULES_98)
EHC_BY_RULER_RULES_100					:= $(EHC_BY_RULER_RULES_99)
EHC_BY_RULER_RULES_101					:= $(EHC_BY_RULER_RULES_100)
EHC_BY_RULER_RULES_102					:= $(EHC_BY_RULER_RULES_100)
EHC_BY_RULER_RULES_103					:= $(EHC_BY_RULER_RULES_100)

###########################################################################################
# derived
###########################################################################################

EHC_AG_D_MAIN_DRV_AG					:= $(patsubst $(SRC_EHC_PREFIX)%.cag,$(EHC_BLD_LIB_HS_VARIANT_PREFIX)%.ag,$(EHC_AG_D_MAIN_SRC_CAG))
EHC_AG_S_MAIN_DRV_AG					:= $(patsubst $(SRC_EHC_PREFIX)%.cag,$(EHC_BLD_LIB_HS_VARIANT_PREFIX)%.ag,$(EHC_AG_S_MAIN_SRC_CAG))
EHC_AG_DS_MAIN_DRV_AG					:= $(patsubst $(SRC_EHC_PREFIX)%.cag,$(EHC_BLD_LIB_HS_VARIANT_PREFIX)%.ag,$(EHC_AG_DS_MAIN_SRC_CAG))
EHC_AG_ALL_MAIN_DRV_AG					:= $(EHC_AG_D_MAIN_DRV_AG) $(EHC_AG_S_MAIN_DRV_AG) $(EHC_AG_DS_MAIN_DRV_AG)

EHC_AG_D_MAIN_DRV_HS					:= $(EHC_AG_D_MAIN_DRV_AG:.ag=.hs)
EHC_AG_S_MAIN_DRV_HS					:= $(EHC_AG_S_MAIN_DRV_AG:.ag=.hs)
EHC_AG_DS_MAIN_DRV_HS					:= $(EHC_AG_DS_MAIN_DRV_AG:.ag=.hs)
EHC_AG_ALL_MAIN_DRV_HS					:= $(EHC_AG_D_MAIN_DRV_HS) $(EHC_AG_S_MAIN_DRV_HS) $(EHC_AG_DS_MAIN_DRV_HS)

EHC_AG_ALL_DPDS_DRV_AG					:= $(patsubst $(SRC_EHC_PREFIX)%.cag,$(EHC_BLD_LIB_HS_VARIANT_PREFIX)%.ag,$(EHC_AG_ALL_DPDS_SRC_CAG))

# all files participating in .hs library construction, not generated from AG
EHC_ALL_LIB_FROMHS_HS					:= $(EHC_HS_ALL_DRV_HS_NO_MAIN) $(EHC_HS_SIG_DRV_HS) $(EHC_HS_CFGINSTALL_DRV_HS)
# all files participating in .hs library construction, generated from AG
EHC_ALL_LIB_FROMAG_HS					:= $(EHC_AG_ALL_MAIN_DRV_HS)

# lib installed ag
INS_EHC_LIB_ALL_AG_NAMES				:= HS/AbsSyn EH/AbsSyn Ty/AbsSyn GrinCode/AbsSyn GrinByteCode/AbsSyn LLVM/AbsSyn
INS_EHC_LIB_ALL_AG						:= $(patsubst $(EHC_BLD_LIB_HS_VARIANT_PREFIX)%,$(INSTALL_VARIANT_LIB_AG_PREFIX)%,$(EHC_AG_D_DDPDS_DERIV_AG))
INSABS_EHC_LIB_ALL_AG					:= $(patsubst $(EHC_BLD_LIB_HS_VARIANT_PREFIX)%,$(INSTALLABS_VARIANT_LIB_AG_PREFIX)%,$(EHC_AG_D_DDPDS_DERIV_AG))
#INS_EHC_LIB_ALL_AG						:= $(patsubst %,$(INSTALL_VARIANT_LIB_AG_PREFIX)%.ag,$(INS_EHC_LIB_ALL_AG_NAMES))
#INSABS_EHC_LIB_ALL_AG					:= $(patsubst %,$(INSTALLABS_VARIANT_LIB_AG_PREFIX)%.ag,$(INS_EHC_LIB_ALL_AG_NAMES))

# all dependents for a variant to kick of building
EHC_ALL_DPDS_NO_MAIN					:= $(EHC_ALL_LIB_FROMHS_HS) $(EHC_ALL_LIB_FROMAG_HS) $(EHC_HS_UTIL_DRV_C)
EHC_ALL_DPDS							:= $(EHC_HS_ALL_DRV_HS) $(EHC_ALL_LIB_FROMAG_HS) $(EHC_HS_SIG_DRV_HS) $(EHC_HS_CFGINSTALL_DRV_HS)

EHC_ALL_DPDS_NOPREPROC					:= $(subst $(EHC_BLD_LIB_HS_VARIANT_PREFIX)ConfigDefines.hs, ,$(EHC_ALL_DPDS))


###########################################################################################
# cabal library dependencies and extensions
###########################################################################################

CABAL_EHCLIB_DEPENDS_ON					:= binary syb bytestring uulib>=0.9.12 old-locale base fgl syb uulib network binary hashable uhc-util mtl containers directory array chr-data utf8-string process

CABAL_EHCLIB_EXTENSIONS					:= UndecidableInstances DeriveDataTypeable LiberalTypeSynonyms StandaloneDeriving DeriveGeneric FlexibleContexts FlexibleInstances TypeSynonymInstances ScopedTypeVariables TypeFamilies

###########################################################################################
# variant dispatch rules
###########################################################################################

$(patsubst %,echo-gen-by-ruler-%,$(EHC_VARIANTS)):
	@v=`echo $@ | sed -e 's/.*ruler-\([0-9_]*\)/\1/'` ; \
	$(MAKE) EHC_VARIANT=$$v echo-gen-by-ruler

###########################################################################################
# rules for top level stuff: library, etc
###########################################################################################

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
	$(call FUN_GEN_CABAL_LIB \
		, $(LIB_EHC_PKG_NAME) \
		, $(EH_VERSION_SHORT) \
		, $(CABAL_EHCLIB_DEPENDS_ON) \
		, $(CABAL_EHCLIB_EXTENSIONS) \
		, Part of EH$(EHC_VARIANT_ASPECTS) compiler packaged as library \
		, $(subst $(PATH_SEP),.,$(patsubst $(EHC_BLD_LIB_HS_VARIANT_PREFIX)%.hs,$(LIB_EHC_QUAL_PREFIX)%,\
			$(shell echo $(EHC_ALL_LIB_FROMHS_HS) $(EHC_ALL_LIB_FROMAG_HS) \
				 | sed -e 's/\([^ ]*\)\.hs\s*/ls \1\*\.hs ;/g' | sh | sed -e 's/\s+/ /g' | sort | uniq | xargs $(SHELL_FILTER_NONEMP_FILES) ))) \
		, $(patsubst $(EHC_BLD_LIBEHC_VARIANT_PREFIX)%,%,$(EHC_HS_UTIL_DRV_C)) \
	) > $@

$(LIB_EHC_SETUP_HS_DRV): $(EHC_MKF)
	mkdir -p $(@D)
	$(call GEN_CABAL_SETUP) > $@

$(LIB_EHC_SETUP2): $(LIB_EHC_SETUP_HS_DRV)
	$(call GHC_CABAL,$<,$@)

ifeq ($(ENABLE_V2_COMMANDS),yes)
# cabal v2 version
$(LIB_EHC_INS_FLAG): $(LIB_EHC_CABAL_DRV) $(INSABS_EHC_LIB_ALL_AG) $(EHC_MKF)
	mkdir -p $(@D)
	cd $(EHC_BLD_LIBEHC_VARIANT_PREFIX) && \
	$(CABAL) v2-configure $(CABAL_SETUP_OPTS) --write-ghc-environment-files=always && \
	$(CABAL) v2-build && \
	echo $@ > $@
else ifeq ($(ENABLE_SANDBOX),yes)
# cabal sandbox version
# Note/TBD: configure should be done before installing dpds, in particular choice of compiler cannot be done correctly now
$(LIB_EHC_INS_FLAG): $(LIB_EHC_CABAL_DRV) $(INSABS_EHC_LIB_ALL_AG) $(EHC_MKF)
	mkdir -p $(@D)
	cd $(EHC_BLD_LIBEHC_VARIANT_PREFIX) && \
	echo "documentation: False" > cabal.config && \
	$(CABAL) sandbox init && \
	$(CABAL) install --only-dependencies && \
	$(CABAL) configure $(CABAL_SETUP_OPTS) && \
	$(CABAL) build && \
	echo $@ > $@
else
# The cabal user install version of the above, i.e. not using cabal sandbox (available with cabal 1.18)
$(LIB_EHC_INS_FLAG): $(LIB_EHC_CABAL_DRV) $(LIB_EHC_SETUP2) $(INSABS_EHC_LIB_ALL_AG) $(EHC_MKF)
	mkdir -p $(@D)
	cd $(EHC_BLD_LIBEHC_VARIANT_PREFIX) && \
	$(LIB_EHC_SETUP) configure $(CABAL_SETUP_OPTS) --prefix=$(INSTALLFORBLDABS_PREFIX) $(CABAL_OPT_INSTALL_LOC) && \
	$(LIB_EHC_SETUP) build && \
	$(LIB_EHC_SETUP) install && \
	echo $@ > $@
endif

$(INSABS_EHC_LIB_ALL_AG): $(INSTALLABS_VARIANT_LIB_AG_PREFIX)%: $(EHC_BLD_LIB_HS_VARIANT_PREFIX)%
	mkdir -p $(@D)
	cp $< $@

###########################################################################################
# rules for internal stuff: building derived sources, etc
###########################################################################################

# rules for ehc library sources+derived
$(EHC_AG_ALL_MAIN_DRV_AG) $(EHC_AG_ALL_DPDS_DRV_AG): $(EHC_BLD_LIB_HS_VARIANT_PREFIX)%.ag: $(SRC_EHC_PREFIX)%.cag $(SHUFFLE)
	mkdir -p $(@D)
	$(SHUFFLE_AG) $(LIB_EHC_SHUFFLE_DEFS) $(SHUFFLE_OPTS_WHEN_EHC) $(EHC_SHUFFLE_OPTS_WHEN_UHC_$(EHC_VARIANT)) --gen-reqm="($(EHC_VARIANT) $(EHC_ASPECTS))" --base=$(*F)  --variant-order="$(EHC_SHUFFLE_ORDER)" $< > $@&& \
	touch $@

#ifeq($(EHC_CFG_USE_RULER),yes)
#$(EHC_RULES_3_DRV_AG): $(EHC_BLD_VARIANT_ASPECTS_PREFIX)%.ag: $(EHC_BLD_VARIANT_ASPECTS_PREFIX)%.cag $(SHUFFLE)
#	$(SHUFFLE_AG) $(LIB_EHC_SHUFFLE_DEFS) $(SHUFFLE_OPTS_WHEN_EHC) $(EHC_SHUFFLE_OPTS_WHEN_UHC_$(EHC_VARIANT)) --gen-reqm="($(EHC_VARIANT) $(EHC_ASPECTS))" --base=$(*F)  --variant-order="$(EHC_SHUFFLE_ORDER)" $< > $@&& \
#	touch $@
#endif

$(EHC_AG_D_MAIN_DRV_HS) $(LIB_EHC_AG_D_MAIN_DRV_HS): %.hs: %.ag
	$(AGC) -dr $(UUAGC_OPTS_WHEN_EHC) $(UUAGC_OPTS_WHEN_EHC_AST_DATA) $(EHC_UUAGC_OPTS_WHEN_UHC_AST_DATA_$(EHC_VARIANT)) -P$(EHC_BLD_VARIANT_ASPECTS_PREFIX) -P$(EHC_BLD_LIB_HS_VARIANT_PREFIX) $<

$(EHC_AG_S_MAIN_DRV_HS) $(LIB_EHC_AG_S_MAIN_DRV_HS): %.hs: %.ag $(if $(EHC_CFG_USE_RULER),$(EHC_RULES_3_DRV_AG),)
	$(AGC) -cfspr $(UUAGC_OPTS_WHEN_EHC) $(UUAGC_OPTS_WHEN_EHC_AST_SEM) $(EHC_UUAGC_OPTS_WHEN_UHC_AST_SEM_$(EHC_VARIANT)) -P$(EHC_BLD_VARIANT_ASPECTS_PREFIX) -P$(EHC_BLD_LIB_HS_VARIANT_PREFIX) $<
	touch $@

$(EHC_AG_DS_MAIN_DRV_HS) $(LIB_EHC_AG_DS_MAIN_DRV_HS): %.hs: %.ag
	$(AGC) -dcfspr $(UUAGC_OPTS_WHEN_EHC) $(UUAGC_OPTS_WHEN_EHC_AST_SEM) $(EHC_UUAGC_OPTS_WHEN_UHC_AST_SEM_$(EHC_VARIANT)) $(UUAGC_OPTS_WHEN_EHC_AST_DATA) $(EHC_UUAGC_OPTS_WHEN_UHC_AST_DATA_$(EHC_VARIANT)) -P$(EHC_BLD_VARIANT_ASPECTS_PREFIX) -P$(EHC_BLD_LIB_HS_VARIANT_PREFIX) $<

$(EHC_HS_MAIN_DRV_HS) $(EHCRUN_HS_MAIN_DRV_HS): $(EHC_BLD_VARIANT_ASPECTS_PREFIX)%.hs: $(SRC_EHC_PREFIX)%.chs $(SHUFFLE) $(LIB_EHC_INS_FLAG)
	mkdir -p $(@D)
	$(SHUFFLE_HS) $(LIB_EHC_SHUFFLE_DEFS) --gen-reqm="($(EHC_VARIANT) $(EHC_ASPECTS))" --base=Main --variant-order="$(EHC_SHUFFLE_ORDER)" $< > $@ && \
	touch $@

$(EHC_HS_UTIL_DRV_HS_DFLT): $(EHC_BLD_LIB_HS_VARIANT_PREFIX)%.hs: $(SRC_EHC_PREFIX)%.chs $(SHUFFLE) # $(MK_CONFIG_MKF)
	mkdir -p $(@D)
	$(SHUFFLE_HS_DFLT) $(LIB_EHC_SHUFFLE_DEFS) --gen-reqm="($(EHC_VARIANT) $(EHC_ASPECTS))" --base=$(*F) --variant-order="$(EHC_SHUFFLE_ORDER)" $< > $@ && \
	touch $@

$(EHC_HS_UTIL_DRV_HS_ASIS): $(EHC_BLD_LIB_HS_VARIANT_PREFIX)%.hs: $(SRC_EHC_PREFIX)%.chs $(SHUFFLE) # $(MK_CONFIG_MKF)
	mkdir -p $(@D)
	$(SHUFFLE_HS_ASIS) $(LIB_EHC_SHUFFLE_DEFS) --gen-reqm="($(EHC_VARIANT) $(EHC_ASPECTS))" --variant-order="$(EHC_SHUFFLE_ORDER)" $< > $@ && \
	touch $@

$(EHC_HS_UTIL_DRV_C): $(EHC_BLD_LIB_HS_VARIANT_PREFIX)%.c: $(SRC_EHC_PREFIX)%.cc $(SHUFFLE)
	mkdir -p $(@D)
	$(SHUFFLE_C) $(LIB_EHC_SHUFFLE_DEFS) --gen-reqm="($(EHC_VARIANT) $(EHC_ASPECTS))" --base=$(*F) --variant-order="$(EHC_SHUFFLE_ORDER)" $< > $@ && \
	touch $@

#ifeq($(RULER_EXISTS),yes)
$(EHC_RULES_3_DRV_CAG): $(EHC_RULES_3_SRC_RL2) $(RULER2) $(EHC_MKF)
	mkdir -p $(@D)
	$(RULER2) $(RULER2_OPTS) --ag --wrapshuffle --preamble=no --selrule="$(EHC_VARIANT_RULER_SEL)" --base=$(*F) $< > $@
#endif

$(EHC_HS_UTILCPP_DRV_HS): $(EHC_BLD_LIB_HS_VARIANT_PREFIX)%.hs: $(SRC_EHC_PREFIX)%.chs $(SHUFFLE)
	mkdir -p $(@D)
	$(SHUFFLE_HS_PRE) $(LIB_EHC_SHUFFLE_DEFS) --gen-reqm="($(EHC_VARIANT) $(EHC_ASPECTS))" --base=$(*F) --variant-order="$(EHC_SHUFFLE_ORDER)" $< > $@ && \
	touch $@

# signature of source code
$(EHC_HS_SIG_DRV_HS): \
			$(EHC_ALL_CHUNK_SRC) \
			$(if $(EHC_CFG_USE_RULER),$(EHC_RULES_ALL_SRC)) \
			$(EHC_MKF)
	@(echo "module $(LIB_EHC_QUAL_PREFIX)$(EHC_HS_SIG_MAIN) where" ; \
	  echo "sig = \"`$(call FUN_MD5,$^)`\"" ; \
	  echo "timestamp = \"`date '+%G%m%d %z %H%M%S'`\"" \
	) > $@

# installation configuration
$(EHC_HS_CFGINSTALL_DRV_HS): $(EHC_MKF) $(UHC_MK_SHARED_MKF)
	@(echo "-- AUTO GENERATED MODULE - see files1.mk" ; \
	  echo "module $(LIB_EHC_QUAL_PREFIX)$(EHC_HS_CFGINSTALL_MAIN) where" ; \
	  echo "import $(LIB_EHC_QUAL_PREFIX)Opts.CommandLine" ; \
	  echo "import Data.List" ; \
	  echo "" ; \
	  echo "ehcDefaultVariant = \"$(EHC_VARIANT_ASPECTS)\"" ; \
	  echo "" ; \
	  echo "gccOpts  = fst $$ parseCmdLineOpts Cmd_CPP \"$(GCC_OPTS_WHEN_EHC)\"" ; \
	  echo "-- gccOpts  = showCmdLineOpts gccOpts'" ; \
	  echo "" ; \
	  echo "cppOpts  = fst $$ parseCmdLineOpts Cmd_CPP \"$(CPP_OPTS_WHEN_EHC)\"" ; \
	  echo "-- cppOpts  = showCmdLineOpts cppOpts'" ; \
	  echo "" ; \
	  if test x$(GIT_VERSION_EXISTS) = xyes ; \
	  then \
	    svnRevision=`$(GIT_VERSION_CMD)` ; \
	  else \
	    svnRevision=`$(GIT_REVISION)` ; \
	  fi ; \
	  echo "ehcSvnRevision = \"$$svnRevision\"" ; \
	  echo "" ; \
	  echo "ehcDefaultInplaceInstallDir = \"$(INSTALLABS_DIR)\"" ; \
	  echo "" ; \
	  echo "ehcPkgConfigfileName = \"$(UHC_PKG_CONFIGFILE_NAME)\"" ; \
	  echo "" ; \
	  echo "data WhatInstallFile = USER_PKG | INST_BIN | INST_LIB | INST_LIB_SHARED | INST_INCLUDE | INST_INCLUDE_SHARED | INST_LIB_PKG2 {-- | INST_LIB_PKG | INST_LIB_PKG_INCLUDE -} " ; \
	  echo "" ; \
	  echo "mkCLibFilename dirprefix pkg = \"$(call FUN_MK_CLIB_FILENAME,\" ++ dirprefix ++ \",\" ++ pkg ++ \")\"" ; \
	  echo "" ; \
	  echo "mkJarFilename dirprefix pkg = \"$(call FUN_MK_JAVALIB_FILENAME,\" ++ dirprefix ++ \",\" ++ pkg ++ \")\"" ; \
	  echo "" ; \
	  if test x$(ENABLE_JS) = xyes ; \
	  then \
	    echo "mkJavaScriptLibFilename dirprefix pkg = \"$(call FUN_MK_JSLIB_FILENAME,\" ++ dirprefix ++ \",\" ++ pkg ++ \")\"" ; \
	    echo "" ; \
	  fi ; \
	  echo "mkInternalPkgFileBase pkg variant target tvariant = \"$(call FUN_PKG_VARIANT_TARGET_TVARIANT,\" ++ pkg ++ \",\" ++ variant ++ \",\" ++ target ++ \",\" ++ tvariant ++ \")\"" ; \
	  echo "" ; \
	  echo "mkPkgIncludeDir libdirprefix = \"$(call FUN_MK_PKG_INC_DIR,\" ++ libdirprefix ++ \")\"" ; \
	  echo "" ; \
	  if test x$(ENABLE_JAVA) = xyes ; \
	  then \
	    echo "mkJavaLibFilename dirprefix pkg = \"$(call FUN_MK_JAVALIB_FILENAME,\" ++ dirprefix ++ \",\" ++ pkg ++ \")\"" ; \
	    echo "" ; \
	  fi ; \
	  echo "mkDirbasedInstallPrefix dir what variant target pkg = case what of" ; \
	  echo "  USER_PKG              -> dir ++ \"/\" ++ target" ; \
	  echo "  INST_LIB              -> \"$(call FUN_DIR_VARIANT_LIB_TARGET_PREFIX,\" ++ dir ++ \",\" ++ variant ++ \",\" ++ target ++ \")\"" ; \
	  echo "  INST_BIN              -> \"$(call FUN_DIR_VARIANT_BIN_PREFIX,\" ++ dir ++ \",\" ++ variant ++ \")\"" ; \
	  echo "  INST_INCLUDE          -> \"$(call FUN_DIR_VARIANT_INC_TARGET_PREFIX,\" ++ dir ++ \",\" ++ variant ++ \",\" ++ target ++ \")\"" ; \
	  echo "  INST_LIB_SHARED       -> \"$(call FUN_DIR_VARIANT_LIB_SHARED_PREFIX,\" ++ dir ++ \",\" ++ variant ++ \")\"" ; \
	  echo "  INST_INCLUDE_SHARED   -> \"$(call FUN_DIR_VARIANT_INC_SHARED_PREFIX,\" ++ dir ++ \",\" ++ variant ++ \")\"" ; \
	  echo "  INST_LIB_PKG2         -> \"$(call FUN_DIR_VARIANT_LIB_PKG_PREFIX,\" ++ dir ++ \",\" ++ variant ++ \")\"" ; \
	) > $@

#	  echo "ehcAssumedPackages = words \"$(EHC_PACKAGES_ASSUMED)\"" ; \
#	  echo "" ; \
#	  echo "mkDirbasedLibVariantTargetPkgPrefix dir variant target pkg = \"$(call FUN_DIR_VARIANT_LIB_TARGET_PKG_PREFIX,\" ++ dir ++ \",\" ++ variant ++ \",\" ++ target ++ \",\" ++ pkg ++ \")\"" ; \
#	  echo "" ; \
#	  echo "mkDirbasedTargetVariantPkgPrefix dir variant target pkg = \"$(call FUN_DIR_VARIANT_LIB_TARGET_PKG_PREFIX,\" ++ dir ++ \",\" ++ variant ++ \",\" ++ target ++ \",\" ++ pkg ++ \")\"" ; \
#	  echo "" ; \
#	  echo "  INST_LIB_PKG_INCLUDE  -> \"$(call FUN_MK_PKG_INC_DIR,$(call FUN_DIR_VARIANT_LIB_TARGET_PKG_PREFIX,\" ++ dir ++ \",\" ++ variant ++ \",\" ++ target ++ \",\" ++ pkg ++ \"))\"" ; \
#	  echo "  INST_LIB_PKG          -> \"$(call FUN_DIR_VARIANT_LIB_TARGET_PKG_PREFIX,\" ++ dir ++ \",\" ++ variant ++ \",\" ++ target ++ \",\" ++ pkg ++ \")\"" ; \
