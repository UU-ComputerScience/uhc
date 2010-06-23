###########################################################################################
# location of these srcs
###########################################################################################

SRC_JAZY_PREFIX				:= $(SRC_PREFIX)jazy/

###########################################################################################
# this file
###########################################################################################

JAZY_MKF						:= $(patsubst %,$(SRC_JAZY_PREFIX)%.mk,files)

###########################################################################################
# build location
###########################################################################################

JAZY_BLD_JAZY_PREFIX			:= $(EHC_BLD_VARIANT_ASPECTS_PREFIX)jazy/

###########################################################################################
# install location, config
###########################################################################################

# java package
JAZY_PKG_NAME						:= jazy
JAZY_PKG_CORE_NAME					:= uu.jazy.core
JAZY_PKG_EHC_NAME					:= uu.jazy.ehc
JAZY_PKG_GUI_NAME					:= uu.jazy.gui
JAZY_PKG_CORE_DIRNAME				:= $(subst .,/,$(JAZY_PKG_CORE_NAME))
JAZY_PKG_EHC_DIRNAME				:= $(subst .,/,$(JAZY_PKG_EHC_NAME))
JAZY_PKG_GUI_DIRNAME				:= $(subst .,/,$(JAZY_PKG_GUI_NAME))

# install location
INSTALLABS_JAZY_LIB_PREFIX			:= $(call FUN_INSTALLABS_VARIANT_LIB_TARGET_PREFIX,$(EHC_VARIANT_ASPECTS),$(EHC_VARIANT_TARGET))

# install
INSTALL_LIB_JAZY					:= $(call FUN_MK_JAVALIB_FILENAME,$(INSTALLABS_JAZY_LIB_PREFIX),$(JAZY_PKG_NAME))

###########################################################################################
# names of sources + deriveds
###########################################################################################

# sources
JAZY_JAVA_JAZY_SRC_JAVA			:= $(wildcard $(SRC_JAZY_PREFIX)$(JAZY_PKG_CORE_DIRNAME)/*.java) \
									$(wildcard $(SRC_JAZY_PREFIX)$(JAZY_PKG_GUI_DIRNAME)/*.java)
JAZY_JAVA_JAZY_SRC_CJAVA		:= $(wildcard $(SRC_JAZY_PREFIX)$(JAZY_PKG_CORE_DIRNAME)/*.cjava) \
									$(wildcard $(SRC_JAZY_PREFIX)$(JAZY_PKG_GUI_DIRNAME)/*.cjava) \
									$(wildcard $(SRC_JAZY_PREFIX)$(JAZY_PKG_EHC_DIRNAME)/*.cjava)

# derived
JAZY_JAVA_JAZY_DRV_JAVA			:= $(patsubst $(SRC_JAZY_PREFIX)%.cjava,$(JAZY_BLD_JAZY_PREFIX)%.java,$(JAZY_JAVA_JAZY_SRC_CJAVA))

# all src files
JAZY_ALL_SRC					:= $(JAZY_JAVA_JAZY_SRC_JAVA) $(JAZY_JAVA_JAZY_SRC_CJAVA)

# all java files to be compiled, src of derived
JAZY_ALL_SRCDRV_JAVA			:= $(JAZY_JAVA_JAZY_SRC_JAVA) $(JAZY_JAVA_JAZY_DRV_JAVA)

###########################################################################################
# top level build
###########################################################################################

# library install
$(INSTALL_LIB_JAZY): $(JAZY_ALL_SRCDRV_JAVA) $(JAZY_MKF)
	mkdir -p $(@D) $(@D)/classes
	$(JAVAC) -d $(@D)/classes $(JAZY_ALL_SRCDRV_JAVA)
	cd $(@D)/classes && $(JAR) cf $@ .
	touch $@

###########################################################################################
# build rules for subparts
###########################################################################################

$(JAZY_JAVA_JAZY_DRV_JAVA): $(JAZY_BLD_JAZY_PREFIX)%.java: $(SRC_JAZY_PREFIX)%.cjava
	mkdir -p $(@D)
	$(SHUFFLE_JAVA) $(LIB_EHC_SHUFFLE_DEFS) --gen-reqm="($(EHC_VARIANT) $(EHC_ASPECTS))" --base=$(*F) --variant-order="$(EHC_SHUFFLE_ORDER)" $< > $@ && \
	touch $@


