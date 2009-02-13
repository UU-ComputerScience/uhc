#!/usr/bin/make -f

.PHONY: ehcs dist www www-sync install lib src build ruler1

###########################################################################################
# Location from which make is invoked
###########################################################################################

TOP_PREFIX				:= 

###########################################################################################
# First (default) target just explains what can be done
###########################################################################################

default: explanation

###########################################################################################
# Definitions, dependencies, rules, etc: spread over subdirectories for subproducts
###########################################################################################

# do not change the order of these includes
-include latex/files.mk
-include lhs2TeX/files.mk

include mk/config.mk

### BEGIN of Ruler1
# This definitely should not remain here....!!!!
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
include $(MK_PREFIX)shared.mk

include $(SRC_PREFIX)libutil/files.mk
include $(SRC_PREFIX)top/files.mk
include $(SRC_PREFIX)helium/files.mk
include $(SRC_PREFIX)lvm/files.mk
include $(SRC_PREFIX)text2text/files.mk
include $(SRC_PREFIX)shuffle/files.mk
include $(SRC_PREFIX)ruler2/files.mk
include $(SRC_PREFIX)ehc/variant.mk
include $(SRC_PREFIX)gen/files.mk
include $(SRC_PREFIX)ehc/files1.mk
include $(SRC_PREFIX)grini/files.mk

-include $(SRC_PREFIX)experiments/files.mk
-include $(SRC_EXPERIMENTS_PREFIX)subst/files.mk

include extlibs/bgc/files.mk
include extlibs/gmp/files.mk
include $(SRC_PREFIX)rts/files.mk
include $(SRC_PREFIX)ehc/files2.mk
include $(SRC_PREFIX)agprimer/files.mk
-include $(SRC_PREFIX)infer2pass/variant.mk
-include $(SRC_PREFIX)infer2pass/files.mk
-include figs/files.mk
-include text/files1.mk
-include text/files-variants.mk
-include $(wildcard text/files1-*.mk)
-include text/files2.mk
-include text/files-targets.mk
-include $(wildcard text/files2-*.mk)
-include www/files.mk
include ehclib/files.mk
include test/files.mk

-include $(MK_PREFIX)dist.mk

###########################################################################################
# all versions (as used by testing)
###########################################################################################

VERSIONS			:= $(EHC_PUB_VARIANTS)

# distributed/published stuff for WWW
#WWW_SRC_TGZ					:= www/current-ehc-src.tgz
#WWW_DOC_PDF					:= www/current-ehc-doc.pdf

###########################################################################################
# Target: explain what can be done
###########################################################################################

explanation:
	@echo "make <n>/ehc             : make compiler variant <n> (in bin/, where <n> in {$(EHC_PUB_VARIANTS)})" ; \
	echo  "make <n>/ehclib          : make ehc library (i.e. used to compile with ehc) variant <n> (in bin/, where <n> in {$(EHC_PREL_VARIANTS)})" ; \
	echo  "make <n>/grini           : make grin interpreter variant <n> (in bin/, where <n> in {$(GRIN_PUB_VARIANTS)})" ; \
	echo  "make <n>/hdoc            : make Haddock documentation for variant <n> (in hdoc/)" ; \
	echo  "make <n>/bare            : make bare source dir for variant <n> (in bare/)," ; \
	echo  "                           then 'cd' to there and 'make'" ; \
	echo  "make $(RULER2_NAME)               : make ruler tool" ; \
	echo  "make $(SHUFFLE_NAME)              : make shuffle tool" ; \
	echo  "" ; \
	echo  "make doc/<d>.pdf         : make (public) documentation <d> (where <d> in {$(TEXT_PUB_VARIANTS)})," ; \
	echo  "                           or (non-public): <d> in {$(TEXT_PRIV_VARIANTS)}" ; \
	echo  "                           or (doc): <d> in {$(TEXT_DOCLTX_VARIANTS)}" ; \
	echo  "                           only if text src available, otherwise already generated" ; \
	echo  "" ; \
	echo  "make ehcs                : make all compiler ($(EHC_EXEC_NAME)) versions" ; \
	echo  "make grinis              : make all grin interpreter ($(GRINI_EXEC_NAME)) versions" ; \
	echo  "make top                 : make Typing Our Programs library" ; \
	echo  "make lvm                 : make Lazy Virtual Machine library" ; \
	echo  "make helium              : make Helium library" ; \
	echo  "make heliumdoc           : make Haddock documentation for Helium, Top and Lvm (in hdoc/)" ; \
	echo  "make test-regress        : run regression test," ; \
	echo  "                           restrict to versions <v> by specifying 'TEST_VARIANTS=<v>' (default '${TEST_VARIANTS}')," ; \
	echo  "                           requires corresponding $(EHC_EXEC_NAME)/$(GRINI_EXEC_NAME)/$(EHCLIB_EHCLIB) already built" ; \
	echo  "make test-expect         : make expected output (for later comparison with test-regress), see test-regress for remarks" ; \
	echo  "" ; \
	echo  "make <n>/infer2pass      : make infer2pass demo version <n> (in bin/, where <n> in {$(INF2PS_VARIANTS)})" ; \
	echo  "" ; \
	echo  "make <n>/clean           : cleanup for variant <n>" ; \
	echo  "" ; \
	echo  "make install             : make 100/ehc and library, install globally" ; \

###########################################################################################
# Target: make every variant of something
###########################################################################################

ehcs: $(EHC_ALL_PUB_EXECS)

grinis: $(GRINI_ALL_PUB_EXECS)

grinllvms: $(GRINLLVM_ALL_PUB_EXECS)

docs: $(TEXT_DIST_DOC_FILES)

cleans: $(patsubst %,%/clean,$(EHC_VARIANTS))

###########################################################################################
# Target: www stuff + sync to www
###########################################################################################

# www: $(WWW_SRC_TGZ) www-ex $(WWW_DOC_FILES)
www: $(WWW_DOC_FILES)

# www/DoneSyncStamp: www-ex
www/DoneSyncStamp: www
	(date "+%G%m%d %H:%M") > www/DoneSyncStamp ; \
	chmod 664 www/* ; \
	rsync --progress -azv -e ssh www/* `whoami`@shell.cs.uu.nl:/users/www/groups/ST/Projects/ehc

www-sync: www/DoneSyncStamp

###########################################################################################
# Target: helium doc
###########################################################################################

heliumdoc: $(LIB_HELIUM_ALL_DRV_HS) $(LIB_TOP_HS_DRV) $(LIB_LVM_HS_DRV)
	mkdir -p hdoc/helium
	haddock --html --odir=hdoc/helium $(LIB_HELIUM_ALL_DRV_HS) $(LIB_TOP_HS_DRV) $(LIB_LVM_HS_DRV)

###########################################################################################
# Target: installation
###########################################################################################

install-test:
	#$(MAKE) EHC_BLD_VARIANT_PREFIX=$(INSTALL_UHC_PREFIX) EHC_BLD_EXEC=$(UHC_INSTALL_EXEC) $(EHC_UHC_INSTALL_VARIANT)/ehclib

install: uhc-install
#	rm -f $(EHC_FOR_UHC_BLD_EXEC)
#	$(MAKE) INSABS_RTS_LIB_PREFIX=$(INSTALL_UHC_LIB_PREFIX) INSABS_RTS_INC_PREFIX=$(INSTALL_UHC_INC_PREFIX) INSABS_PREFIX=$(INSTALL_UHC_PREFIX) INS_PREFIX=$(INSTALL_UHC_PREFIX) \
#		CABAL_OPT_INSTALL_LOC="--global" \
#		GHC_PKG_NAME_PREFIX= \
#		$(EHC_FOR_UHC_BLD_EXEC)
#	mkdir -p $(dir $(UHC_INSTALL_EXEC))
#	install $(EHC_FOR_UHC_BLD_EXEC) $(UHC_INSTALL_EXEC)
#	$(MAKE) EHC_BLD_VARIANT_PREFIX=$(INSTALL_UHC_PREFIX) EHC_BLD_EXEC=$(UHC_INSTALL_EXEC) $(EHC_UHC_INSTALL_VARIANT)/ehclib

###########################################################################################
# Target: uhc + libs
###########################################################################################

uhc: $(EHC_FOR_UHC_BLD_EXEC) $(EHC_UHC_INSTALL_VARIANT)/ehclibs

UHC_INSTALL_VARIANT_PREFIX 	:= $(call FUN_INSTALLABS_VARIANT_PREFIX,$(EHC_UHC_INSTALL_VARIANT))
UHC_INSTALL_PREFIX			:= $(call FUN_DIR_VARIANT_PREFIX,$(INSTALL_UHC_ROOT),$(UHC_EXEC_NAME))

uhc-install: uhc
	mkdir $(UHC_INSTALL_PREFIX)
	$(call COPY_FILES_BY_TAR,$(UHC_INSTALL_VARIANT_PREFIX),$(UHC_INSTALL_PREFIX),*) ; \
	for target in `$(EHC_FOR_UHC_BLD_EXEC) --meta-targets` ; \
	do \
	  $(MAKE) uhc-install-postprocess-$${target} EHC_VARIANT_TARGET=$${target} ; \
	done ; \
	rm -f $(INSTALL_UHC_BIN_PREFIX)$(UHC_EXEC_NAME) ; \
	ln -s $(UHC_INSTALL_PREFIX)bin/$(EHC_EXEC_NAME) $(INSTALL_UHC_BIN_PREFIX)$(UHC_EXEC_NAME)
	$(UHC_INSTALL_PREFIX)bin/$(EHC_EXEC_NAME) --meta-export-env=$(INSTALL_UHC_ROOT),$(UHC_EXEC_NAME)

uhc-install-postprocess-bc:
	cd $(call FUN_DIR_VARIANT_LIB_TARGET_PREFIX,$(INSTALL_UHC_ROOT),$(UHC_EXEC_NAME),$(EHC_VARIANT_TARGET)) ; \
	for pkg in $(EHCLIB_SYNC_ALL_PKG) ; \
	do \
	  rm -f $${pkg}/*.{hs,hs-cpp,c} $${pkg}/*/*.{hs,hs-cpp,c} $${pkg}/$(EHCLIB_MAIN)* ; \
	done

uhc-install-postprocess-C:
	cd $(call FUN_DIR_VARIANT_LIB_TARGET_PREFIX,$(INSTALL_UHC_ROOT),$(UHC_EXEC_NAME),$(EHC_VARIANT_TARGET)) ; \
	for pkg in $(EHCLIB_SYNC_ALL_PKG) ; \
	do \
	  rm -f $${pkg}/*.{hs,hs-cpp} $${pkg}/*/*.{hs,hs-cpp} $${pkg}/$(EHCLIB_MAIN)* ; \
	done

uhc-install-postprocess-core:
	
# still to do: uhc --meta-export-env=$(INSTALL_UHC_LIB_PREFIX),$(UHC_EXEC_NAME)

###########################################################################################
# Target: clean build stuff
###########################################################################################

clean: cleans

###########################################################################################
# Target: try outs and debugging of make variable definitions
###########################################################################################

FUN_PREFIX2DIR			= $(patsubst %/,%,$(1))

tst:
	@echo $(INSTALLABS_EXTLIBS_BGC_LIB_PREFIX)
	@echo $(call FUN_PREFIX2DIR,$(INSTALLABS_EXTLIBS_BGC_LIB_PREFIX))

tstv:
	$(MAKE) EHC_VARIANT=100 tst

###########################################################################################
# Target: obsolete or to become so
###########################################################################################

#: afp-full ehcs doc grinis
#	$(MAKE) initial-test-expect

rules2.tex: rules2.rul
	$(RULER1) -l --base=rules $< | $(LHS2TEX) $(LHS2TEX_OPTS_POLY) > $@

A_EH_TEST			:= $(word 1,$(wildcard test/*.eh))
A_EH_TEST_EXP		:= $(addsuffix .exp$(VERSION_FIRST),$(A_EH_TEST))

$(A_EH_TEST_EXP): $(A_EH_TEST)
	$(MAKE) test-expect

initial-test-expect: $(A_EH_TEST_EXP)

WWW_EXAMPLES_TMPL			:=	www/ehc-examples-templ.html
WWW_EXAMPLES_HTML			:=	www/ehc-examples.html

www-ex: $(WWW_EXAMPLES_HTML)

$(WWW_EXAMPLES_HTML): $(WWW_EXAMPLES_TMPL)
	$(call PERL_SUBST_EHC,$(WWW_EXAMPLES_TMPL),$(WWW_EXAMPLES_HTML))

$(WWW_SRC_TGZ): $(DIST_TGZ)
	cp $^ $@

