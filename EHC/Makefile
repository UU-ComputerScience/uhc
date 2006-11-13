#!/usr/bin/make -f
TOP_PREFIX				:= 

default: explanation

# files, dependencies, rules
# do not change the order of these includes
-include latex/files.mk
-include lhs2TeX/files.mk

include mk/config.mk

### BEGIN of Ruler1
# Ruler1, will be obsolete until all type rules are specified with Ruler2 (currently not in Explicit/implicit story)
RULER1				:= bin/ruler1$(EXEC_SUFFIX)
RULER1_DIR			:= ruler1
RULER1_MAIN			:= Ruler
RULER1_AG			:= $(RULER1_MAIN).ag
RULER1_HS			:= $(RULER1_AG:.ag=.hs)
RULER1_DERIV		:= $(RULER1_DIR)/$(RULER1_HS)

RULER1_SRC			:= $(RULER1_DIR)/$(RULER1_AG)

ruler1: $(RULER1)

$(RULER1): $(RULER1_DIR)/$(RULER1_AG) $(LIB_EH_UTIL_INS_FLAG)
	cd $(RULER1_DIR) && \
	$(AGC) -csdfr --module=Main `basename $<` && \
	$(GHC) --make $(GHC_OPTS) -package $(LIB_EH_UTIL_PKG_NAME) $(RULER1_HS) -o ../$@ && \
	$(STRIP) ../$@
### END of Ruler1

include src/files.mk
include $(SRC_PREFIX)ehc/shared.mk
include mk/shared.mk

include $(SRC_PREFIX)libutil/files.mk

include $(SRC_PREFIX)shuffle/files.mk
include $(SRC_PREFIX)ruler2/files.mk
include $(SRC_PREFIX)ehc/variant.mk
include $(SRC_PREFIX)grinc/variant.mk
include $(SRC_PREFIX)ehc/files1.mk
include $(SRC_PREFIX)grini/files.mk
include $(SRC_PREFIX)grinc/files.mk

include extlibs/bgc/files.mk
include $(SRC_PREFIX)rts/files.mk
include $(SRC_PREFIX)ehc/files2.mk
include $(SRC_PREFIX)agprimer/files.mk
-include $(SRC_PREFIX)infer2pass/variant.mk
-include $(SRC_PREFIX)infer2pass/files.mk
-include figs/files.mk
-include text/files1.mk
-include $(wildcard text/files1-*.mk)
-include text/files2.mk
-include $(wildcard text/files2-*.mk)
-include www/files.mk
include test/files.mk

-include mk/dist.mk

# all versions (as used by testing)
VERSIONS			:= $(EHC_PUB_VARIANTS)

# distributed/published stuff for WWW
WWW_SRC_TGZ					:= www/current-ehc-src.tgz
WWW_DOC_PDF					:= www/current-ehc-doc.pdf

explanation:
	@echo "make <n>/ehc             : make compiler version <n> (in bin/, where <n> in {$(EHC_PUB_VARIANTS)})" ; \
	echo  "make <n>/grini           : make grin interpreter version <n> (bin/, where <n> in {$(GRIN_PUB_VARIANTS)})" ; \
	echo  "make <n>/grinc           : make grin compiler version <n> (where <n> in {$(GRIN_PUB_VARIANTS)})" ; \
	echo  "make $(RULER2_NAME)               : make ruler tool" ; \
	echo  "make $(SHUFFLE_NAME)              : make shuffle tool" ; \
	echo  "" ; \
	echo  "make doc/<d>.pdf         : make (public) documentation <d> (where <d> in {$(TEXT_PUB_VARIANTS)})," ; \
	echo  "                           or (non-public): <d> in {$(TEXT_PRIV_VARIANTS)}" ; \
	echo  "                           only if text src available, otherwise already generated" ; \
	echo  "" ; \
	echo  "make ehcs                : make all compiler ($(EHC_EXEC_NAME)) versions" ; \
	echo  "make grinis              : make all grin interpreter ($(GRINI_EXEC_NAME)) versions" ; \
	echo  "make grincs              : make all grin compiler ($(GRINC_EXEC_NAME)) versions" ; \
	echo  "make test-regress        : run regression test," ; \
	echo  "                           restrict to versions <v> by specifying 'VERSIONS=<v>'," ; \
	echo  "                           requires corresponding $(EHC_EXEC_NAME)/$(GRINI_EXEC_NAME) already built" ; \
	echo  "make test-expect         : make expected output (for later comparison with test-regress), see test-regress for remarks" ; \
	echo  "" ; \
	echo  "make <n>/infer2pass      : make infer2pass demo version <n> (in bin/, where <n> in {$(INF2PS_VARIANTS)})" ; \

all: afp-full ehcs doc grinis
	$(MAKE) initial-test-expect

.PHONY: ehcs dist www www-sync install lib src build ruler1

rules2.tex: rules2.rul
	$(RULER1) -l --base=rules $< | $(LHS2TEX) $(LHS2TEX_OPTS_POLY) > $@

ehcs: $(EHC_ALL_PUB_EXECS)

grinis: $(GRINI_ALL_PUB_EXECS)

grincs: $(GRINC_ALL_PUB_EXECS)

grinllvms: $(GRINLLVM_ALL_PUB_EXECS)

docs: $(TEXT_DIST_DOC_FILES)

edit-s:
	$(OPEN_FOR_EDIT) \
	$(SHUFFLE_ALL_SRC) \
	Makefile

edit-r:
	$(OPEN_FOR_EDIT) \
	$(RULER2_ALL_SRC) \
	$(EHC_RULES_3_SRC_RL2) \
	Makefile

edit-t:
	$(OPEN_FOR_EDIT) \
	$(TEXT_EDIT_SRC) \
	$(EHC_RULES_3_SRC_RL2) $(RULER2_RULES_SRC_RL2) \
	Makefile

edit-e:
	$(OPEN_FOR_EDIT) \
	$(EHC_ALL_SRC) $(UHC_ALL_SRC) $(GRIN_ALL_SRC) $(GRINI_ALL_SRC) \
	Makefile

edit: edit-r edit-e edit-t edit-s

A_EH_TEST			:= $(word 1,$(wildcard test/*.eh))
A_EH_TEST_EXP		:= $(addsuffix .exp$(VERSION_FIRST),$(A_EH_TEST))

tst:
	@echo $(if $(filter $(EHC_VARIANT),$(EHC_CODE_VARIANTS)),$(RTS_INS_FLAG),)

tstv:
	$(MAKE) EHC_VARIANT=8 tst

initial-test-expect: $(A_EH_TEST_EXP)

$(A_EH_TEST_EXP): $(A_EH_TEST)
	$(MAKE) test-expect

WWW_EXAMPLES_TMPL			:=	www/ehc-examples-templ.html
WWW_EXAMPLES_HTML			:=	www/ehc-examples.html

www-ex: $(WWW_EXAMPLES_HTML)

www: $(WWW_SRC_TGZ) www-ex $(WWW_DOC_FILES)

www/DoneSyncStamp: www-ex
	(date; echo -n ", " ; svn up) > www/DoneSyncStamp ; \
	rsync --progress -azv -e ssh www/* atze@modena.cs.uu.nl:/users/www/groups/ST/Projects/ehc

www-sync: www/DoneSyncStamp

$(WWW_EXAMPLES_HTML): $(WWW_EXAMPLES_TMPL)
	$(call PERL_SUBST_EHC,$(WWW_EXAMPLES_TMPL),$(WWW_EXAMPLES_HTML))

$(WWW_SRC_TGZ): $(DIST_TGZ)
	cp $^ $@

