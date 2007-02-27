# location of library src
SRC_HELIUM_PREFIX         := $(SRC_PREFIX)helium/
BLD_HELIUM_PREFIX         := $(BLD_PREFIX)lib-helium/
BLD_LIBHELIUM_PREFIX      := $(BLD_HELIUM_PREFIX)Helium/

# this file + other mk files
HELIUM_MKF             := $(patsubst %,$(SRC_HELIUM_PREFIX)%.mk,files)

# Helium sources

LIB_HELIUM_AG_DAT_SRC = \
    $(SRC_HELIUM_PREFIX)syntax/UHA_Syntax.ag \
    $(SRC_HELIUM_PREFIX)staticanalysis/directives/TS_Syntax.ag \
    $(SRC_HELIUM_PREFIX)staticanalysis/directives/TS_CoreSyntax.ag \

LIB_HELIUM_AG_SEM_SRC = \
    $(SRC_HELIUM_PREFIX)syntax/UHA_Pretty.ag \
    $(SRC_HELIUM_PREFIX)syntax/UHA_OneLine.ag \
    $(SRC_HELIUM_PREFIX)parser/ResolveOperators.ag \
    $(SRC_HELIUM_PREFIX)modulesystem/ExtractImportDecls.ag \
    $(SRC_HELIUM_PREFIX)staticanalysis/staticchecks/StaticChecks.ag \
    $(SRC_HELIUM_PREFIX)staticanalysis/inferencers/TypeInferencing.ag \
    $(SRC_HELIUM_PREFIX)staticanalysis/inferencers/KindInferencing.ag \
    $(SRC_HELIUM_PREFIX)staticanalysis/directives/TS_Apply.ag \
    $(SRC_HELIUM_PREFIX)staticanalysis/directives/TS_Analyse.ag \
    $(SRC_HELIUM_PREFIX)staticanalysis/directives/TS_ToCore.ag \
    $(SRC_HELIUM_PREFIX)codegeneration/CodeGeneration.ag

LIB_HELIUM_AG_DEP_SRC = \
    $(SRC_HELIUM_PREFIX)staticanalysis/staticchecks/Collect.ag \
    $(SRC_HELIUM_PREFIX)staticanalysis/staticchecks/ExportErrors.ag \
    $(SRC_HELIUM_PREFIX)staticanalysis/staticchecks/HeliumPartialSyntax.ag \
    $(SRC_HELIUM_PREFIX)staticanalysis/staticchecks/KindChecking.ag \
    $(SRC_HELIUM_PREFIX)staticanalysis/staticchecks/MiscErrors.ag \
    $(SRC_HELIUM_PREFIX)staticanalysis/staticchecks/Scope.ag \
    $(SRC_HELIUM_PREFIX)staticanalysis/staticchecks/ScopeErrors.ag \
    $(SRC_HELIUM_PREFIX)staticanalysis/staticchecks/TopLevelErrors.ag \
    $(SRC_HELIUM_PREFIX)staticanalysis/staticchecks/Warnings.ag \
    $(SRC_HELIUM_PREFIX)staticanalysis/inferencers/GlobalInfo.ag \
    $(SRC_HELIUM_PREFIX)staticanalysis/inferencers/LocalInfo.ag \
    $(SRC_HELIUM_PREFIX)staticanalysis/inferencers/PatternMatchWarnings.ag \
    $(SRC_HELIUM_PREFIX)staticanalysis/inferencers/TypeInferenceCollect.ag \
    $(SRC_HELIUM_PREFIX)staticanalysis/inferencers/TypeInferenceInfo.ag \
    $(SRC_HELIUM_PREFIX)staticanalysis/inferencers/TypeInferenceOverloading.ag \
    $(SRC_HELIUM_PREFIX)staticanalysis/inferencers/TypeInferenceRules.ag \
    $(SRC_HELIUM_PREFIX)staticanalysis/directives/TS_Collect.ag \
    $(SRC_HELIUM_PREFIX)staticanalysis/directives/TS_PatternMatching.ag \
    $(SRC_HELIUM_PREFIX)codegeneration/ToCoreDecl.ag \
    $(SRC_HELIUM_PREFIX)codegeneration/ToCoreExpr.ag \
    $(SRC_HELIUM_PREFIX)codegeneration/ToCoreModule.ag \
    $(SRC_HELIUM_PREFIX)codegeneration/ToCoreName.ag \
    $(SRC_HELIUM_PREFIX)codegeneration/ToCorePat.ag


LIB_HELIUM_HS_SRC = \
    $(SRC_HELIUM_PREFIX)parser/CollectFunctionBindings.hs \
    $(SRC_HELIUM_PREFIX)parser/OperatorTable.hs \
    $(SRC_HELIUM_PREFIX)parser/Parser.hs \
    $(SRC_HELIUM_PREFIX)parser/Lexer.hs \
    $(SRC_HELIUM_PREFIX)parser/ParseLibrary.hs \
    $(SRC_HELIUM_PREFIX)parser/ParseMessage.hs \
    $(SRC_HELIUM_PREFIX)parser/LexerMonad.hs \
    $(SRC_HELIUM_PREFIX)parser/LexerMessage.hs \
    $(SRC_HELIUM_PREFIX)parser/LayoutRule.hs \
    $(SRC_HELIUM_PREFIX)parser/LexerToken.hs \
    $(SRC_HELIUM_PREFIX)modulesystem/ImportEnvironment.hs \
    $(SRC_HELIUM_PREFIX)modulesystem/DictionaryEnvironment.hs \
    $(SRC_HELIUM_PREFIX)modulesystem/CoreToImportEnv.hs \
    $(SRC_HELIUM_PREFIX)staticanalysis/messages/Messages.hs \
    $(SRC_HELIUM_PREFIX)staticanalysis/messages/HeliumMessages.hs \
    $(SRC_HELIUM_PREFIX)staticanalysis/messages/StaticErrors.hs \
    $(SRC_HELIUM_PREFIX)staticanalysis/messages/TypeErrors.hs \
    $(SRC_HELIUM_PREFIX)staticanalysis/messages/KindErrors.hs \
    $(SRC_HELIUM_PREFIX)staticanalysis/messages/Warnings.hs \
    $(SRC_HELIUM_PREFIX)staticanalysis/messages/Information.hs \
    $(SRC_HELIUM_PREFIX)staticanalysis/heuristics/HeuristicsInfo.hs \
    $(SRC_HELIUM_PREFIX)staticanalysis/heuristics/ListOfHeuristics.hs \
    $(SRC_HELIUM_PREFIX)staticanalysis/heuristics/OnlyResultHeuristics.hs \
    $(SRC_HELIUM_PREFIX)staticanalysis/heuristics/RepairHeuristics.hs \
    $(SRC_HELIUM_PREFIX)staticanalysis/heuristics/TieBreakerHeuristics.hs \
    $(SRC_HELIUM_PREFIX)staticanalysis/heuristics/UnifierHeuristics.hs \
    $(SRC_HELIUM_PREFIX)staticanalysis/inferencers/ExpressionTypeInferencer.hs \
    $(SRC_HELIUM_PREFIX)staticanalysis/inferencers/SelectConstraintSolver.hs \
    $(SRC_HELIUM_PREFIX)staticanalysis/inferencers/BindingGroupAnalysis.hs \
    $(SRC_HELIUM_PREFIX)staticanalysis/directives/TS_Parser.hs \
    $(SRC_HELIUM_PREFIX)staticanalysis/directives/Matchers.hs \
    $(SRC_HELIUM_PREFIX)staticanalysis/directives/TS_Messages.hs \
    $(SRC_HELIUM_PREFIX)staticanalysis/directives/TS_Compile.hs \
    $(SRC_HELIUM_PREFIX)staticanalysis/directives/TS_Attributes.hs \
    $(SRC_HELIUM_PREFIX)staticanalysis/miscellaneous/ConstraintInfo.hs \
    $(SRC_HELIUM_PREFIX)staticanalysis/miscellaneous/DoublyLinkedTree.hs \
    $(SRC_HELIUM_PREFIX)staticanalysis/miscellaneous/TypeConstraints.hs \
    $(SRC_HELIUM_PREFIX)staticanalysis/miscellaneous/TypesToAlignedDocs.hs \
    $(SRC_HELIUM_PREFIX)staticanalysis/miscellaneous/TypeConversion.hs \
    $(SRC_HELIUM_PREFIX)staticanalysis/miscellaneous/UHA_Source.hs \
    $(SRC_HELIUM_PREFIX)syntax/UHA_Utils.hs \
    $(SRC_HELIUM_PREFIX)syntax/UHA_Range.hs \
    $(SRC_HELIUM_PREFIX)codegeneration/CoreToLvm.hs \
    $(SRC_HELIUM_PREFIX)codegeneration/PatternMatch.hs \
    $(SRC_HELIUM_PREFIX)codegeneration/DerivingShow.hs \
    $(SRC_HELIUM_PREFIX)codegeneration/DerivingEq.hs \
    $(SRC_HELIUM_PREFIX)codegeneration/CoreUtils.hs \
    $(SRC_HELIUM_PREFIX)utils/Utils.hs \
    $(SRC_HELIUM_PREFIX)utils/Logger.hs \
    $(SRC_HELIUM_PREFIX)utils/OSSpecific.hs \
    $(SRC_HELIUM_PREFIX)utils/OneLiner.hs \
    $(SRC_HELIUM_PREFIX)utils/Similarity.hs \
    $(SRC_HELIUM_PREFIX)utils/Texts.hs \
    $(SRC_HELIUM_PREFIX)main/Main.hs \
    $(SRC_HELIUM_PREFIX)main/Version.hs \
    $(SRC_HELIUM_PREFIX)main/Args.hs \
    $(SRC_HELIUM_PREFIX)main/Compile.hs \
    $(SRC_HELIUM_PREFIX)main/PhaseLexer.hs \
    $(SRC_HELIUM_PREFIX)main/PhaseParser.hs \
    $(SRC_HELIUM_PREFIX)main/PhaseImport.hs \
    $(SRC_HELIUM_PREFIX)main/PhaseResolveOperators.hs \
    $(SRC_HELIUM_PREFIX)main/PhaseStaticChecks.hs \
    $(SRC_HELIUM_PREFIX)main/PhaseTypingStrategies.hs \
    $(SRC_HELIUM_PREFIX)main/PhaseTypeInferencer.hs \
    $(SRC_HELIUM_PREFIX)main/PhaseDesugarer.hs \
    $(SRC_HELIUM_PREFIX)main/PhaseCodeGenerator.hs \
    $(SRC_HELIUM_PREFIX)main/CompileUtils.hs \
    $(SRC_HELIUM_PREFIX)main/PhaseKindInferencer.hs


# all sources have a counterpart in the build directory
LIB_HELIUM_HS_DRV_HS	    := $(patsubst $(SRC_HELIUM_PREFIX)%,$(BLD_LIBHELIUM_PREFIX)%,$(LIB_HELIUM_HS_SRC)    )
LIB_HELIUM_AG_DAT_DRV_AG	:= $(patsubst $(SRC_HELIUM_PREFIX)%,$(BLD_LIBHELIUM_PREFIX)%,$(LIB_HELIUM_AG_DAT_SRC))
LIB_HELIUM_AG_SEM_DRV_AG	:= $(patsubst $(SRC_HELIUM_PREFIX)%,$(BLD_LIBHELIUM_PREFIX)%,$(LIB_HELIUM_AG_SEM_SRC))
LIB_HELIUM_AG_DEP_DRV_AG	:= $(patsubst $(SRC_HELIUM_PREFIX)%,$(BLD_LIBHELIUM_PREFIX)%,$(LIB_HELIUM_AG_DEP_SRC))
LIB_HELIUM_ALL_DRV_COPIED   := $(LIB_HELIUM_HS_DRV_HS) $(LIB_HELIUM_AG_DAT_DRV_AG) $(LIB_HELIUM_AG_SEM_DRV_AG) $(LIB_HELIUM_AG_DEP_DRV_AG)

# all AG-units generate a corresponding Haskell file
LIB_HELIUM_AG_DAT_DRV_HS	:= $(patsubst $(SRC_HELIUM_PREFIX)%,$(BLD_LIBHELIUM_PREFIX)%,$(LIB_HELIUM_AG_DAT_SRC))
LIB_HELIUM_AG_SEM_DRV_HS	:= $(patsubst $(SRC_HELIUM_PREFIX)%,$(BLD_LIBHELIUM_PREFIX)%,$(LIB_HELIUM_AG_SEM_SRC))

# all Haskell files that will be part of the library: copied from HS sources and generated from (copied) AG sources
LIB_HELIUM_ALL_DRV_HS       := $(LIB_HELIUM_HS_DRV_HS) $(LIB_HELIUM_AG_DAT_DRV_HS) $(LIB_HELIUM_AG_SEM_DRV_HS)







# lib/cabal config
LIB_HELIUM_QUAL			:= Helium
LIB_HELIUM_QUAL_PREFIX	:= $(LIB_HELIUM_QUAL).
LIB_HELIUM_PKG_NAME		:= $(subst .,-,$(LIB_HELIUM_QUAL))
LIB_HELIUM_HS_PREFIX	:= $(SRC_HELIUM_PREFIX)$(subst .,$(PATH_SEP),$(LIB_HELIUM_QUAL_PREFIX))
LIB_HELIUM_INS_FLAG		:= $(INSABS_FLAG_PREFIX)$(LIB_HELIUM_PKG_NAME)

# derived stuff
LIB_HELIUM_CABAL_DRV	:= $(BLD_HELIUM_PREFIX)lib-Helium.cabal
LIB_HELIUM_SETUP_HS_DRV	:= $(BLD_HELIUM_PREFIX)Setup.hs
LIB_HELIUM_SETUP2		:= $(BLD_HELIUM_PREFIX)setup$(EXEC_SUFFIX)
LIB_HELIUM_SETUP		:= ./setup$(EXEC_SUFFIX)

# distribution
HELIUM_DIST_FILES		:= $(HELIUM_MKF) $(LIB_HELIUM_HS_SRC)

# target
helium: $(LIB_HELIUM_INS_FLAG)

# rules
$(LIB_HELIUM_CABAL_DRV): $(HELIUM_MKF) $(LIB_HELIUM_ALL_DRV_HS)
	mkdir -p $(@D)
	$(call GEN_CABAL \
	, $(LIB_HELIUM_PKG_NAME) \
	, $(EH_VERSION) \
	, mtl \
	, $(CABAL_OPT_ALLOW_UNDECIDABLE_INSTANCES) OverlappingInstances \
	, Helium library \
	, $(subst $(PATH_SEP),.,$(patsubst $(DRV_HELIUM_PREFIX)%.hs,$(LIB_HELIUM_QUAL_PREFIX)%, $(LIB_HELIUM_ALL_DRV_HS) )) \
	) > $@


# Rule how to copy a source file to the build directory
$(LIB_HELIUM_ALL_DRV_COPIED): $(BLD_LIBHELIUM_PREFIX)%: $(SRC_HELIUM_PREFIX)%
	mkdir -p $(@D)
	cp $< $@
	

# Rule how to compile an AG data unit to Haskell
$(LIB_HELIUM_AG_DAT_DRV_HS): %.hs: %.ag
	$(AGC) -dr -P$(BLD_LIBHELIUM_PREFIX) $<

# Rule how to compile an AG semantics unit to Haskell
$(GRINC_AG_S_MAIN_DRV_HS): %.hs: %.ag
	$(AGC) -cfspr -P$(BLD_LIBHELIUM_PREFIX) $<

	

$(LIB_HELIUM_SETUP_HS_DRV): $(HELIUM_MKF)
	mkdir -p $(@D)
	$(call GEN_CABAL_SETUP) > $@

$(LIB_HELIUM_SETUP2): $(LIB_HELIUM_SETUP_HS_DRV)
	$(call GHC_CABAL,$<,$@)

$(LIB_HELIUM_INS_FLAG): $(LIB_HELIUM_HS_DRV) $(LIB_HELIUM_CABAL_DRV) $(LIB_HELIUM_SETUP2) $(HELIUM_MKF)
	mkdir -p $(@D)
	cd $(BLD_HELIUM_PREFIX) && \
	$(LIB_HELIUM_SETUP) configure $(CABAL_SETUP_OPTS) --prefix=$(INSABS_PREFIX) --user && \
	$(LIB_HELIUM_SETUP) build && \
	$(LIB_HELIUM_SETUP) install --user && \
	echo $@ > $@

