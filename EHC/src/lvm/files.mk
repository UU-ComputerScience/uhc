# location of library src
SRC_LVM_PREFIX				:= $(SRC_PREFIX)lvm/
BLD_LVM_PREFIX				:= $(BLD_PREFIX)lib-lvm/

# this file + other mk files
LVM_MKF						:= $(patsubst %,$(SRC_LVM_PREFIX)%.mk,files)

# LVM sources



LIB_LVM_HS_SRC = \
	  $(SRC_LVM_PREFIX)Common/Id.hs \
	  $(SRC_LVM_PREFIX)Common/IdMap.hs \
	  $(SRC_LVM_PREFIX)Common/IdSet.hs \
	  $(SRC_LVM_PREFIX)Common/IntMap.hs \
	  $(SRC_LVM_PREFIX)Common/Byte.hs \
	  $(SRC_LVM_PREFIX)Common/Standard.hs \
	  $(SRC_LVM_PREFIX)Common/TopSort.hs \
	  $(SRC_LVM_PREFIX)Common/Set.hs \
	  $(SRC_LVM_PREFIX)Common/Special.hs \
	  $(SRC_LVM_PREFIX)Common/PPrint.hs \
      $(SRC_LVM_PREFIX)Lvm/Lvm.hs \
      $(SRC_LVM_PREFIX)Lvm/LvmPretty.hs \
      $(SRC_LVM_PREFIX)Lvm/LvmWrite.hs \
      $(SRC_LVM_PREFIX)Lvm/LvmRead.hs \
	  $(SRC_LVM_PREFIX)Lvm/LvmImport.hs \
	  $(SRC_LVM_PREFIX)Lvm/Module.hs \
	  $(SRC_LVM_PREFIX)Lvm/ModulePretty.hs \
	  $(SRC_LVM_PREFIX)Lvm/Instr.hs \
	  $(SRC_LVM_PREFIX)Lvm/InstrPretty.hs \
	  $(SRC_LVM_PREFIX)Lvm/InstrResolve.hs \
	  $(SRC_LVM_PREFIX)Lvm/InstrRewrite.hs \
      $(SRC_LVM_PREFIX)Asm/AsmOptimize.hs \
      $(SRC_LVM_PREFIX)Asm/AsmInline.hs \
      $(SRC_LVM_PREFIX)Asm/AsmOccur.hs \
	  $(SRC_LVM_PREFIX)Asm/Asm.hs \
	  $(SRC_LVM_PREFIX)Asm/AsmPretty.hs \
	  $(SRC_LVM_PREFIX)Asm/AsmToLvm.hs \
      $(SRC_LVM_PREFIX)Core/Core.hs \
      $(SRC_LVM_PREFIX)Core/CorePretty.hs \
      $(SRC_LVM_PREFIX)Core/CoreToAsm.hs \
	  $(SRC_LVM_PREFIX)Core/CoreParse.hs \
	  $(SRC_LVM_PREFIX)Core/CoreLexer.hs \
	  $(SRC_LVM_PREFIX)Core/CoreNormalize.hs \
	  $(SRC_LVM_PREFIX)Core/CoreRemoveDead.hs \
	  $(SRC_LVM_PREFIX)Core/CoreNoShadow.hs \
	  $(SRC_LVM_PREFIX)Core/CoreFreeVar.hs \
	  $(SRC_LVM_PREFIX)Core/CoreLetSort.hs \
	  $(SRC_LVM_PREFIX)Core/CoreLift.hs \
	  $(SRC_LVM_PREFIX)Core/CoreSaturate.hs



# lib/cabal config
LIB_LVM_QUAL				:= Lvm
LIB_LVM_QUAL_PREFIX			:= $(LIB_LVM_QUAL).
LIB_LVM_PKG_NAME			:= $(subst .,-,$(LIB_LVM_QUAL))
LIB_LVM_HS_PREFIX			:= $(SRC_LVM_PREFIX)$(subst .,$(PATH_SEP),$(LIB_LVM_QUAL_PREFIX))
LIB_LVM_INS_FLAG			:= $(INSABS_FLAG_PREFIX)$(LIB_LVM_PKG_NAME)

# derived stuff
LIB_LVM_CABAL_DRV			:= $(BLD_LVM_PREFIX)lib-Lvm.cabal
LIB_LVM_HS_DRV				:= $(patsubst $(SRC_LVM_PREFIX)%,$(BLD_LVM_PREFIX)Lvm/%,$(LIB_LVM_HS_SRC))
LIB_LVM_SETUP_HS_DRV		:= $(BLD_LVM_PREFIX)Setup.hs
LIB_LVM_SETUP2				:= $(BLD_LVM_PREFIX)setup$(EXEC_SUFFIX)
LIB_LVM_SETUP				:= ./setup$(EXEC_SUFFIX)

# distribution
LVM_DIST_FILES				:= $(LVM_MKF) $(LIB_LVM_HS_SRC)

# target
lvm: $(LIB_LVM_INS_FLAG)

# rules
$(LIB_LVM_CABAL_DRV): $(LVM_MKF) $(LIB_LVM_HS_SRC)
	mkdir -p $(@D)
	$(call GEN_CABAL \
		, $(LIB_LVM_PKG_NAME) \
		, $(EH_VERSION) \
		, mtl parsec\
		, $(CABAL_OPT_ALLOW_UNDECIDABLE_INSTANCES) OverlappingInstances \
		, Lazy Virtual Machine library \
		, $(subst $(PATH_SEP),.,$(patsubst $(SRC_LVM_PREFIX)%.hs,$(LIB_LVM_QUAL_PREFIX)%, $(LIB_LVM_HS_SRC) )) \
		, \
	) > $@

$(LIB_LVM_HS_DRV): $(BLD_LVM_PREFIX)Lvm/%.hs: $(SRC_LVM_PREFIX)%.hs
	mkdir -p $(@D)
	cp $< $@

$(LIB_LVM_SETUP_HS_DRV): $(LVM_MKF)
	mkdir -p $(@D)
	$(call GEN_CABAL_SETUP) > $@

$(LIB_LVM_SETUP2): $(LIB_LVM_SETUP_HS_DRV)
	$(call GHC_CABAL,$<,$@)

$(LIB_LVM_INS_FLAG): $(LIB_LVM_HS_DRV) $(LIB_LVM_CABAL_DRV) $(LIB_LVM_SETUP2) $(LVM_MKF)
	mkdir -p $(@D)
	cd $(BLD_LVM_PREFIX) && \
	$(LIB_LVM_SETUP) configure $(CABAL_SETUP_OPTS) --prefix=$(INSABS_PREFIX) --user && \
	$(LIB_LVM_SETUP) build && \
	$(LIB_LVM_SETUP) install --user && \
	echo $@ > $@

