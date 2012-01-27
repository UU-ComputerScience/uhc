###########################################################################################
# location of these srcs
###########################################################################################

SRC_JSCRIPT_PREFIX					:= $(SRC_PREFIX)javascript/

###########################################################################################
# this file
###########################################################################################

JSCRIPT_MKF							:= $(patsubst %,$(SRC_JSCRIPT_PREFIX)%.mk,files)

###########################################################################################
# build location
###########################################################################################

JSCRIPT_BLD_JSCRIPT_PREFIX			:= $(EHC_BLD_VARIANT_ASPECTS_PREFIX)js/

###########################################################################################
# install location, config
###########################################################################################

# js package
JSCRIPT_PKG_NAME					:= $(RTS_PKG_NAME)
JSCRIPT_PKG_CORE_NAME				:= core
JSCRIPT_PKG_RTS_NAME				:= rts
JSCRIPT_PKG_BIGINT_NAME				:= jsbn
JSCRIPT_PKG_EHC_NAME				:= uhc
JSCRIPT_PKG_CORE_DIRNAME			:= $(JSCRIPT_PKG_CORE_NAME)
JSCRIPT_PKG_UHC_DIRNAME				:= $(JSCRIPT_PKG_UHC_NAME)

# install location
INSTALLABS_JSCRIPT_LIB_PREFIX		:= $(call FUN_INSTALLABS_VARIANT_LIB_TARGET_PREFIX,$(EHC_VARIANT_ASPECTS),$(EHC_VARIANT_TARGET))

# install
INSTALL_LIB_JSCRIPT					:= $(call FUN_MK_JSLIB_FILENAME,$(INSTALLABS_JSCRIPT_LIB_PREFIX),$(JSCRIPT_PKG_NAME))
INSTALL_LIB_JS						:= $(INSTALL_LIB_JSCRIPT)

###########################################################################################
# names of sources + deriveds
###########################################################################################

# sources
JSCRIPT_JS_JSCRIPT_SRC_JS			:= $(wildcard $(SRC_JSCRIPT_PREFIX)$(JSCRIPT_PKG_CORE_DIRNAME)/*.js) \
										$(wildcard $(SRC_JSCRIPT_PREFIX)$(JSCRIPT_PKG_BIGINT_NAME)/*.js)
JSCRIPT_JS_JSCRIPT_SRC_CJS			= $(wildcard $(SRC_JSCRIPT_PREFIX)$(JSCRIPT_PKG_CORE_DIRNAME)/*.cjs) \
										$(wildcard $(SRC_JSCRIPT_PREFIX)$(JSCRIPT_PKG_RTS_NAME)/*.cjs) \
										$(wildcard $(SRC_JSCRIPT_PREFIX)$(JSCRIPT_PKG_UHC_DIRNAME)/*.cjs)

# derived
JSCRIPT_JS_JSCRIPT_DRV_JS			:= $(patsubst $(SRC_JSCRIPT_PREFIX)%.cjs,$(JSCRIPT_BLD_JSCRIPT_PREFIX)%.js,$(JSCRIPT_JS_JSCRIPT_SRC_CJS))

# all src files
JSCRIPT_ALL_SRC						:= $(JSCRIPT_JS_JSCRIPT_SRC_JS) $(JSCRIPT_JS_JSCRIPT_SRC_CJS)

# all js files to be compiled, src of derived
JSCRIPT_ALL_SRCDRV_JS				:= $(JSCRIPT_JS_JSCRIPT_SRC_JS) $(JSCRIPT_JS_JSCRIPT_DRV_JS)

###########################################################################################
# top level build
###########################################################################################

# library install
$(INSTALL_LIB_JSCRIPT): $(JSCRIPT_ALL_SRCDRV_JS) $(JSCRIPT_MKF)
	mkdir -p $(@D)
	cat $(JSCRIPT_ALL_SRCDRV_JS) > $@
	touch $@

###########################################################################################
# build rules for subparts
###########################################################################################

$(JSCRIPT_JS_JSCRIPT_DRV_JS): $(JSCRIPT_BLD_JSCRIPT_PREFIX)%.js: $(SRC_JSCRIPT_PREFIX)%.cjs
	mkdir -p $(@D)
	$(SHUFFLE_JS) $(LIB_EHC_SHUFFLE_DEFS) --gen-reqm="($(EHC_VARIANT) $(EHC_ASPECTS))" --base=$(*F) --variant-order="$(EHC_SHUFFLE_ORDER)" $< > $@ && \
	touch $@


