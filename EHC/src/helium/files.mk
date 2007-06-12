# location of library src
SRC_HELIUM_PREFIX         := $(SRC_PREFIX)helium/
BLD_HELIUM_PREFIX         := $(BLD_PREFIX)lib-helium/
BLD_LIBHELIUM_PREFIX      := $(BLD_HELIUM_PREFIX)Helium/

# this file + other mk files
HELIUM_MKF             := $(patsubst %,$(SRC_HELIUM_PREFIX)%.mk,files)

# Helium sources


LIB_HELIUM_AG_DAT_SRC = \
    $(SRC_HELIUM_PREFIX)Syntax/UHA.ag \
    $(SRC_HELIUM_PREFIX)StaticAnalysis/Directives/TS.ag \
    $(SRC_HELIUM_PREFIX)StaticAnalysis/Directives/TS_Core.ag

LIB_HELIUM_AG_SEM_SRC = \
    $(SRC_HELIUM_PREFIX)Syntax/UHA_Pretty.ag \
    $(SRC_HELIUM_PREFIX)Syntax/UHA_OneLine.ag \
    $(SRC_HELIUM_PREFIX)Parser/ResolveOperators.ag \
    $(SRC_HELIUM_PREFIX)ModuleSystem/ExtractImportDecls.ag \
    $(SRC_HELIUM_PREFIX)StaticAnalysis/StaticChecks/StaticChecks.ag \
    $(SRC_HELIUM_PREFIX)StaticAnalysis/Inferencers/TypeInferencing.ag \
    $(SRC_HELIUM_PREFIX)StaticAnalysis/Inferencers/KindInferencing.ag \
    $(SRC_HELIUM_PREFIX)StaticAnalysis/Directives/TS_Apply.ag \
    $(SRC_HELIUM_PREFIX)StaticAnalysis/Directives/TS_Analyse.ag \
    $(SRC_HELIUM_PREFIX)StaticAnalysis/Directives/TS_ToCore.ag \
    $(SRC_HELIUM_PREFIX)CodeGeneration/CodeGeneration.ag


LIB_HELIUM_AG_DEP_SRC = \
    $(SRC_HELIUM_PREFIX)Syntax/UHA_Syntax.ag \
    $(SRC_HELIUM_PREFIX)StaticAnalysis/Directives/TS_Syntax.ag \
    $(SRC_HELIUM_PREFIX)StaticAnalysis/Directives/TS_CoreSyntax.ag \
    $(SRC_HELIUM_PREFIX)StaticAnalysis/StaticChecks/Collect.ag \
    $(SRC_HELIUM_PREFIX)StaticAnalysis/StaticChecks/ExportErrors.ag \
    $(SRC_HELIUM_PREFIX)StaticAnalysis/StaticChecks/HeliumPartialSyntax.ag \
    $(SRC_HELIUM_PREFIX)StaticAnalysis/StaticChecks/KindChecking.ag \
    $(SRC_HELIUM_PREFIX)StaticAnalysis/StaticChecks/MiscErrors.ag \
    $(SRC_HELIUM_PREFIX)StaticAnalysis/StaticChecks/Scope.ag \
    $(SRC_HELIUM_PREFIX)StaticAnalysis/StaticChecks/ScopeErrors.ag \
    $(SRC_HELIUM_PREFIX)StaticAnalysis/StaticChecks/TopLevelErrors.ag \
    $(SRC_HELIUM_PREFIX)StaticAnalysis/StaticChecks/Warnings.ag \
    $(SRC_HELIUM_PREFIX)StaticAnalysis/Inferencers/GlobalInfo.ag \
    $(SRC_HELIUM_PREFIX)StaticAnalysis/Inferencers/LocalInfo.ag \
    $(SRC_HELIUM_PREFIX)StaticAnalysis/Inferencers/PatternMatchWarnings.ag \
    $(SRC_HELIUM_PREFIX)StaticAnalysis/Inferencers/TypeInferenceCollect.ag \
    $(SRC_HELIUM_PREFIX)StaticAnalysis/Inferencers/TypeInferenceInfo.ag \
    $(SRC_HELIUM_PREFIX)StaticAnalysis/Inferencers/TypeInferenceOverloading.ag \
    $(SRC_HELIUM_PREFIX)StaticAnalysis/Inferencers/TypeInferenceRules.ag \
    $(SRC_HELIUM_PREFIX)StaticAnalysis/Directives/TS_Collect.ag \
    $(SRC_HELIUM_PREFIX)StaticAnalysis/Directives/TS_PatternMatching.ag \
    $(SRC_HELIUM_PREFIX)CodeGeneration/ToCoreDecl.ag \
    $(SRC_HELIUM_PREFIX)CodeGeneration/ToCoreExpr.ag \
    $(SRC_HELIUM_PREFIX)CodeGeneration/ToCoreModule.ag \
    $(SRC_HELIUM_PREFIX)CodeGeneration/ToCoreName.ag \
    $(SRC_HELIUM_PREFIX)CodeGeneration/ToCorePat.ag



LIB_HELIUM_HS_SRC = \
    $(SRC_HELIUM_PREFIX)Parser/CollectFunctionBindings.hs \
    $(SRC_HELIUM_PREFIX)Parser/OperatorTable.hs \
    $(SRC_HELIUM_PREFIX)Parser/Parser.hs \
    $(SRC_HELIUM_PREFIX)Parser/Lexer.hs \
    $(SRC_HELIUM_PREFIX)Parser/ParseLibrary.hs \
    $(SRC_HELIUM_PREFIX)Parser/ParseMessage.hs \
    $(SRC_HELIUM_PREFIX)Parser/LexerMonad.hs \
    $(SRC_HELIUM_PREFIX)Parser/LexerMessage.hs \
    $(SRC_HELIUM_PREFIX)Parser/LayoutRule.hs \
    $(SRC_HELIUM_PREFIX)Parser/LexerToken.hs \
    $(SRC_HELIUM_PREFIX)ModuleSystem/ImportEnvironment.hs \
    $(SRC_HELIUM_PREFIX)ModuleSystem/DictionaryEnvironment.hs \
    $(SRC_HELIUM_PREFIX)ModuleSystem/CoreToImportEnv.hs \
    $(SRC_HELIUM_PREFIX)StaticAnalysis/Messages/Messages.hs \
    $(SRC_HELIUM_PREFIX)StaticAnalysis/Messages/HeliumMessages.hs \
    $(SRC_HELIUM_PREFIX)StaticAnalysis/Messages/StaticErrors.hs \
    $(SRC_HELIUM_PREFIX)StaticAnalysis/Messages/TypeErrors.hs \
    $(SRC_HELIUM_PREFIX)StaticAnalysis/Messages/KindErrors.hs \
    $(SRC_HELIUM_PREFIX)StaticAnalysis/Messages/Warnings.hs \
    $(SRC_HELIUM_PREFIX)StaticAnalysis/Messages/Information.hs \
    $(SRC_HELIUM_PREFIX)StaticAnalysis/Heuristics/HeuristicsInfo.hs \
    $(SRC_HELIUM_PREFIX)StaticAnalysis/Heuristics/ListOfHeuristics.hs \
    $(SRC_HELIUM_PREFIX)StaticAnalysis/Heuristics/OnlyResultHeuristics.hs \
    $(SRC_HELIUM_PREFIX)StaticAnalysis/Heuristics/RepairHeuristics.hs \
    $(SRC_HELIUM_PREFIX)StaticAnalysis/Heuristics/TieBreakerHeuristics.hs \
    $(SRC_HELIUM_PREFIX)StaticAnalysis/Heuristics/UnifierHeuristics.hs \
    $(SRC_HELIUM_PREFIX)StaticAnalysis/Inferencers/ExpressionTypeInferencer.hs \
    $(SRC_HELIUM_PREFIX)StaticAnalysis/Inferencers/SelectConstraintSolver.hs \
    $(SRC_HELIUM_PREFIX)StaticAnalysis/Inferencers/BindingGroupAnalysis.hs \
    $(SRC_HELIUM_PREFIX)StaticAnalysis/Directives/TS_Parser.hs \
    $(SRC_HELIUM_PREFIX)StaticAnalysis/Directives/Matchers.hs \
    $(SRC_HELIUM_PREFIX)StaticAnalysis/Directives/TS_Messages.hs \
    $(SRC_HELIUM_PREFIX)StaticAnalysis/Directives/TS_Compile.hs \
    $(SRC_HELIUM_PREFIX)StaticAnalysis/Directives/TS_Attributes.hs \
    $(SRC_HELIUM_PREFIX)StaticAnalysis/Miscellaneous/ConstraintInfo.hs \
    $(SRC_HELIUM_PREFIX)StaticAnalysis/Miscellaneous/DoublyLinkedTree.hs \
    $(SRC_HELIUM_PREFIX)StaticAnalysis/Miscellaneous/TypeConstraints.hs \
    $(SRC_HELIUM_PREFIX)StaticAnalysis/Miscellaneous/TypesToAlignedDocs.hs \
    $(SRC_HELIUM_PREFIX)StaticAnalysis/Miscellaneous/TypeConversion.hs \
    $(SRC_HELIUM_PREFIX)StaticAnalysis/Miscellaneous/UHA_Source.hs \
    $(SRC_HELIUM_PREFIX)Syntax/UHA_Utils.hs \
    $(SRC_HELIUM_PREFIX)Syntax/UHA_Range.hs \
    $(SRC_HELIUM_PREFIX)CodeGeneration/CoreToLvm.hs \
    $(SRC_HELIUM_PREFIX)CodeGeneration/PatternMatch.hs \
    $(SRC_HELIUM_PREFIX)CodeGeneration/DerivingShow.hs \
    $(SRC_HELIUM_PREFIX)CodeGeneration/DerivingEq.hs \
    $(SRC_HELIUM_PREFIX)CodeGeneration/CoreUtils.hs \
    $(SRC_HELIUM_PREFIX)Utils/Utils.hs \
    $(SRC_HELIUM_PREFIX)Utils/Logger.hs \
    $(SRC_HELIUM_PREFIX)Utils/OSSpecific.hs \
    $(SRC_HELIUM_PREFIX)Utils/OneLiner.hs \
    $(SRC_HELIUM_PREFIX)Utils/Similarity.hs \
    $(SRC_HELIUM_PREFIX)Utils/Texts.hs \
    $(SRC_HELIUM_PREFIX)Compiler/Main.hs \
    $(SRC_HELIUM_PREFIX)Compiler/Version.hs \
    $(SRC_HELIUM_PREFIX)Compiler/Args.hs \
    $(SRC_HELIUM_PREFIX)Compiler/Compile.hs \
    $(SRC_HELIUM_PREFIX)Compiler/PhaseLexer.hs \
    $(SRC_HELIUM_PREFIX)Compiler/PhaseParser.hs \
    $(SRC_HELIUM_PREFIX)Compiler/PhaseImport.hs \
    $(SRC_HELIUM_PREFIX)Compiler/PhaseResolveOperators.hs \
    $(SRC_HELIUM_PREFIX)Compiler/PhaseStaticChecks.hs \
    $(SRC_HELIUM_PREFIX)Compiler/PhaseTypingStrategies.hs \
    $(SRC_HELIUM_PREFIX)Compiler/PhaseTypeInferencer.hs \
    $(SRC_HELIUM_PREFIX)Compiler/PhaseDesugarer.hs \
    $(SRC_HELIUM_PREFIX)Compiler/PhaseCodeGenerator.hs \
    $(SRC_HELIUM_PREFIX)Compiler/CompileUtils.hs \
    $(SRC_HELIUM_PREFIX)Compiler/PhaseKindInferencer.hs



# all sources have a counterpart in the build directory
LIB_HELIUM_HS_DRV_HS	    := $(patsubst $(SRC_HELIUM_PREFIX)%,$(BLD_LIBHELIUM_PREFIX)%,$(LIB_HELIUM_HS_SRC)    )
LIB_HELIUM_AG_DAT_DRV_AG	:= $(patsubst $(SRC_HELIUM_PREFIX)%,$(BLD_LIBHELIUM_PREFIX)%,$(LIB_HELIUM_AG_DAT_SRC))
LIB_HELIUM_AG_SEM_DRV_AG	:= $(patsubst $(SRC_HELIUM_PREFIX)%,$(BLD_LIBHELIUM_PREFIX)%,$(LIB_HELIUM_AG_SEM_SRC))
LIB_HELIUM_AG_DEP_DRV_AG	:= $(patsubst $(SRC_HELIUM_PREFIX)%,$(BLD_LIBHELIUM_PREFIX)%,$(LIB_HELIUM_AG_DEP_SRC))
LIB_HELIUM_ALL_DRV_COPIED   := $(LIB_HELIUM_HS_DRV_HS) $(LIB_HELIUM_AG_DAT_DRV_AG) $(LIB_HELIUM_AG_SEM_DRV_AG) $(LIB_HELIUM_AG_DEP_DRV_AG)

# all AG-units generate a corresponding Haskell file
LIB_HELIUM_AG_DAT_DRV_HS	:= $(patsubst $(SRC_HELIUM_PREFIX)%.ag,$(BLD_LIBHELIUM_PREFIX)%.hs,$(LIB_HELIUM_AG_DAT_SRC))
LIB_HELIUM_AG_SEM_DRV_HS	:= $(patsubst $(SRC_HELIUM_PREFIX)%.ag,$(BLD_LIBHELIUM_PREFIX)%.hs,$(LIB_HELIUM_AG_SEM_SRC))

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
	, mtl parsec Top Lvm\
	, $(CABAL_OPT_ALLOW_UNDECIDABLE_INSTANCES) OverlappingInstances \
	, Helium library \
	, $(subst $(PATH_SEP),.,$(patsubst $(BLD_LIBHELIUM_PREFIX)%.hs,$(LIB_HELIUM_QUAL_PREFIX)%, $(LIB_HELIUM_ALL_DRV_HS) )) \
	, \
	) > $@


# Rule how to copy a source file to the build directory
$(LIB_HELIUM_ALL_DRV_COPIED): $(BLD_LIBHELIUM_PREFIX)%: $(SRC_HELIUM_PREFIX)%
	mkdir -p $(@D)
	cp $< $@
	

# Rule how to compile an AG data unit to Haskell
$(LIB_HELIUM_AG_DAT_DRV_HS): %.hs: %.ag $(LIB_HELIUM_AG_DEP_DRV_AG)
	$(AGC) -dr $(UUAGC_OPTS_WHEN_EHC) -P$(@D) $<

# Rule how to compile an AG semantics unit to Haskell
$(LIB_HELIUM_AG_SEM_DRV_HS): %.hs: %.ag $(LIB_HELIUM_AG_DEP_DRV_AG)
	$(AGC) -cfspr $(UUAGC_OPTS_WHEN_EHC) -P$(@D) $<

$(LIB_HELIUM_SETUP_HS_DRV): $(HELIUM_MKF)
	mkdir -p $(@D)
	$(call GEN_CABAL_SETUP) > $@

$(LIB_HELIUM_SETUP2): $(LIB_HELIUM_SETUP_HS_DRV)
	$(call GHC_CABAL,$<,$@)

$(LIB_HELIUM_INS_FLAG): $(LIB_HELIUM_ALL_DRV_HS) $(LIB_HELIUM_CABAL_DRV) $(LIB_HELIUM_SETUP2) $(HELIUM_MKF)
	mkdir -p $(@D)
	cd $(BLD_HELIUM_PREFIX) && \
	$(LIB_HELIUM_SETUP) configure $(CABAL_SETUP_OPTS) --prefix=$(INSABS_PREFIX) --user && \
	$(LIB_HELIUM_SETUP) build && \
	$(LIB_HELIUM_SETUP) install --user && \
	echo $@ > $@

