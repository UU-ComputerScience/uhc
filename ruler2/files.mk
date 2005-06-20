# location of ruler2 src
RULER2_SRC_PREFIX	:= $(TOP_PREFIX)ruler2/

RULER2_MAIN			:= Ruler

RULER2_AG_MAIN_SRC	:= $(addprefix $(RULER2_SRC_PREFIX),RulerPretty.ag RulerAST.ag RulerGen.ag RulerParser.ag RulerExprMatchSubst.ag RulerWrap.ag \
						RulerViewDpd.ag RulerMisc.ag RulerARule.ag \
						RulerARuleOptim.ag RulerARuleOptim2.ag RulerARuleOptim3.ag \
						RulerRlSel.ag RulerPatternUniq.ag \
						$(RULER2_MAIN).ag \
						)

RULER2_HS_SRC		:= $(addprefix $(RULER2_SRC_PREFIX),RulerUtils.hs RulerAdmin.hs RulerMkAdmin.hs)
RULER2_HS_DRV		:= $(addprefix $(RULER2_SRC_PREFIX),$(RULER2_MAIN).hs)

RULER2_BLD_EXEC		:= $(BIN_PREFIX)ruler2

$(RULER2_BLD_EXEC): $(RULER2_HS_DRV) $(RULER2_HS_SRC) $(LIB_SRC_HS)
	$(GHC) --make $(GHC_OPTS) -i$(LIB_SRC_PREFIX) -i$(RULER2_SRC_PREFIX) $(RULER2_SRC_PREFIX)$(RULER2_MAIN).hs -o $@

$(RULER2_SRC_PREFIX)$(RULER2_MAIN).hs: $(RULER2_AG_MAIN_SRC)
	$(AGC) -csdfr --module=Main $(RULER2_SRC_PREFIX)$(RULER2_MAIN).ag

