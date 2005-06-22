# location of ruler2 src
RULER2_SRC_PREFIX	:= $(TOP_PREFIX)ruler2/
RULER2_DEMO_PREFIX	:= $(RULER2_SRC_PREFIX)demo/

# main + sources
RULER2_MAIN			:= Ruler

RULER2_AG_MAIN_SRC	:= $(addprefix $(RULER2_SRC_PREFIX),RulerPretty.ag RulerAST.ag RulerGen.ag RulerParser.ag RulerExprMatchSubst.ag RulerWrap.ag \
						RulerViewDpd.ag RulerMisc.ag RulerARule.ag \
						RulerARuleOptim.ag RulerARuleOptim2.ag RulerARuleOptim3.ag \
						RulerRlSel.ag RulerPatternUniq.ag \
						$(RULER2_MAIN).ag \
						)

RULER2_HS_SRC		:= $(addprefix $(RULER2_SRC_PREFIX),RulerUtils.hs RulerAdmin.hs RulerMkAdmin.hs)
RULER2_HS_DRV		:= $(addprefix $(RULER2_SRC_PREFIX),$(RULER2_MAIN).hs)

# binary/executable
RULER2_BLD_EXEC		:= $(BIN_PREFIX)ruler2
RULER2				:= $(RULER2_BLD_EXEC)

# make rules
$(RULER2_BLD_EXEC): $(RULER2_HS_DRV) $(RULER2_HS_SRC) $(LIB_SRC_HS)
	$(GHC) --make $(GHC_OPTS) -i$(LIB_SRC_PREFIX) -i$(RULER2_SRC_PREFIX) $(RULER2_SRC_PREFIX)$(RULER2_MAIN).hs -o $@

$(RULER2_SRC_PREFIX)$(RULER2_MAIN).hs: $(RULER2_AG_MAIN_SRC)
	$(AGC) -csdfr --module=Main -P$(RULER2_SRC_PREFIX) $(RULER2_SRC_PREFIX)$(RULER2_MAIN).ag

### demo stuff
RULER2_DEMO_AG_MAIN			:= DemoMain
RULER2_DEMO_AG_MAIN_SRC		:= $(RULER2_DEMO_PREFIX)$(RULER2_DEMO_AG_MAIN).ag
RULER2_DEMO_HS_SRC			:= $(addprefix $(RULER2_DEMO_PREFIX),DemoUtils.hs)
RULER2_DEMO_HS_DRV			:= $(addprefix $(RULER2_DEMO_PREFIX),$(RULER2_DEMO_AG_MAIN).hs)
RULER2_DEMO_EXEC			:= $(RULER2_DEMO_PREFIX)demo

RULER2_DEMO_CRL_SRC			:= $(RULER2_DEMO_PREFIX)demo.crl2
RULER2_DEMO_DRV_LCTEX		:= $(RULER2_DEMO_CRL_SRC:.crl2=.lctex)
RULER2_DEMO_DRV_CTEX		:= $(RULER2_DEMO_CRL_SRC:.crl2=.ctex)
RULER2_DEMO_DRV_RL2			:= $(RULER2_DEMO_CRL_SRC:.crl2=.rl2)
RULER2_DEMO_DRV_LRTEX		:= $(RULER2_DEMO_CRL_SRC:.crl2=.lrtex)
RULER2_DEMO_DRV_RTEX		:= $(RULER2_DEMO_CRL_SRC:.crl2=.rtex)
RULER2_DEMO_DRV_CAG			:= $(RULER2_DEMO_CRL_SRC:.crl2=.cag)
RULER2_DEMO_DRV_LATEX		:= $(RULER2_DEMO_CRL_SRC:.crl2=.latex)
RULER2_DEMO_DRV_ATEX		:= $(RULER2_DEMO_CRL_SRC:.crl2=.atex)
RULER2_DEMO_DRV_AG			:= $(RULER2_DEMO_CRL_SRC:.crl2=.ag)

# chunk view order for demo src
RULER2_DEMO_SHUFFLE_ORDER	:= 1 < 2 < 3
RULER2_DEMO_SHUFFLE_FINAL	:= 3

# make rules
$(RULER2_DEMO_DRV_LCTEX): $(RULER2_DEMO_CRL_SRC) $(SHUFFLE)
	$(SHUFFLE) --gen=all --latex --order="$(RULER2_DEMO_SHUFFLE_ORDER)" --base=rulerDemoRL --lhs2tex=yes $< > $@

$(RULER2_DEMO_DRV_CTEX): $(RULER2_DEMO_DRV_LCTEX)
	$(LHS2TEX) $(LHS2TEX_OPTS_POLY) $< > $@

$(RULER2_DEMO_DRV_RL2): $(RULER2_DEMO_CRL_SRC) $(SHUFFLE)
	$(SHUFFLE) --gen=$(RULER2_DEMO_SHUFFLE_FINAL) --plain --order="$(RULER2_DEMO_SHUFFLE_ORDER)"  --lhs2tex=no $< > $@

$(RULER2_DEMO_DRV_LRTEX): $(RULER2_DEMO_DRV_RL2) $(RULER2)
	$(RULER2) --lhs2tex --selrule="(E - AG).(*).(*)" --markchanges="E - AG" --base=rulerDemo $< > $@

$(RULER2_DEMO_DRV_RTEX): $(RULER2_DEMO_DRV_LRTEX)
	$(LHS2TEX) $(LHS2TEX_OPTS_POLY) $< > $@

$(RULER2_DEMO_DRV_CAG): $(RULER2_DEMO_DRV_RL2) $(RULER2)
	$(RULER2) --ag --ATTR --selrule="(3).(*).(*)" --wrapfrag  --base=rulerDemoAG $< > $@

$(RULER2_DEMO_DRV_AG): $(RULER2_DEMO_DRV_CAG) $(SHUFFLE)
	$(SHUFFLE) --gen=$(RULER2_DEMO_SHUFFLE_FINAL) --plain --order="$(RULER2_DEMO_SHUFFLE_ORDER)"  --lhs2tex=no $< > $@

$(RULER2_DEMO_DRV_LATEX): $(RULER2_DEMO_DRV_CAG) $(SHUFFLE)
	$(SHUFFLE) --gen=$(RULER2_DEMO_SHUFFLE_FINAL) --latex --order="$(RULER2_DEMO_SHUFFLE_ORDER)" --base=rulerDemoAG --lhs2tex=yes $< > $@

$(RULER2_DEMO_DRV_ATEX): $(RULER2_DEMO_DRV_LATEX)
	$(LHS2TEX) $(LHS2TEX_OPTS_POLY) $< > $@

$(RULER2_DEMO_HS_DRV): $(RULER2_DEMO_AG_MAIN_SRC) $(RULER2_DEMO_DRV_AG)
	$(AGC) -csdfr --module=Main -P$(RULER2_DEMO_PREFIX) $<

$(RULER2_DEMO_EXEC): $(RULER2_DEMO_HS_DRV) $(RULER2_DEMO_HS_SRC) $(LIB_SRC_HS)
	$(GHC) --make $(GHC_OPTS) -i$(LIB_SRC_PREFIX) -i$(RULER2_DEMO_PREFIX) $(RULER2_DEMO_HS_DRV) -o $@

