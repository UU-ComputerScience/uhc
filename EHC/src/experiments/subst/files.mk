###
### Run experiment with:
###  make experiment-subst-run DO_TIMING=yes
###

# location of substitution experiment src
SRC_EXPERIMENTS_SUBST_PREFIX				:= $(SRC_EXPERIMENTS_PREFIX)subst/

# various prefixes
BIN_EXPERIMENTS_SUBST_PREFIX				:= $(BIN_EXPERIMENTS_PREFIX)subst/
BINABS_EXPERIMENTS_SUBST_PREFIX				:= $(BINABS_EXPERIMENTS_PREFIX)subst/
BIN_EXPERIMENTS_SUBST_VARIANT_PREFIX		:= $(BIN_EXPERIMENTS_SUBST_PREFIX)$(EXPERIMENTS_VARIANT_PREFIX)
BINABS_EXPERIMENTS_SUBST_VARIANT_PREFIX		:= $(BINABS_EXPERIMENTS_SUBST_PREFIX)$(EXPERIMENTS_VARIANT_PREFIX)

# location of this experiment build
BLD_EXPERIMENTS_SUBST_PREFIX				:= $(BLD_EXPERIMENTS_PREFIX)subst/
BLD_EXPERIMENTS_SUBST_VARIANT_PREFIX		:= $(BLD_EXPERIMENTS_SUBST_PREFIX)$(EXPERIMENTS_VARIANT_PREFIX)
RUN_EXPERIMENTS_SUBST_PREFIX				:= $(BLD_EXPERIMENTS_SUBST_PREFIX)runs/

# run parameters
DO_TIMING									:= no

# tex run output
RUN_EXPERIMENTS_SUBST_TEX					:= $(RUN_EXPERIMENTS_SUBST_PREFIX)timings.tex

# runs
RUN_EXPERIMENTS_SUBST_RUNS					:= a/500 a/1100 a/1600 b/20 b/25 b/28 
#RUN_EXPERIMENTS_SUBST_RUNS					:= b/15 b/20 b/23 b/25
#RUN_EXPERIMENTS_SUBST_RUNS					:= a/1600
#RUN_EXPERIMENTS_SUBST_RUNS					:= a/500

# variants
EXPERIMENTS_SUBST_VARIANTS					:= 1 3 2 22 221

# flags to tools
EXPERIMENTS_SUBST_SHUFFLE_DEFS				:= 
EXPERIMENTS_SUBST_SHUFFLE_ORDER				:= 1 < 2 < 3, 2 < 21, 2 < 22 < 221, 22 < 222, 3 < 31
GHC_PLAIN_OPTS_WHEN_EXPERIMENTS_SUBST		:= 
GHC_PROF_OPTS_WHEN_EXPERIMENTS_SUBST		:= -prof -auto-all -caf-all
RTS_OPTS_WHEN_EXPERIMENTS_SUBST1			:= +RTS -hc -i0.02 -RTS
RTS_OPTS_WHEN_EXPERIMENTS_SUBST2			:= +RTS -p -i0.02 -RTS
HP2PS_OPTS_WHEN_EXPERIMENTS_SUBST			:= -c

# this file + other mk files
EXPERIMENTS_SUBST_MKF						:= $(patsubst %,$(SRC_EXPERIMENTS_SUBST_PREFIX)%.mk,files)

# main + sources + dpds
EXPERIMENTS_SUBST_MAIN						:= Main

EXPERIMENTS_SUBST_HS_MAIN_SRC_CHS			:= $(addprefix $(SRC_EXPERIMENTS_SUBST_PREFIX),$(EXPERIMENTS_SUBST_MAIN).chs)
EXPERIMENTS_SUBST_HS_MAIN_DRV_HS			:= $(patsubst $(SRC_EXPERIMENTS_SUBST_PREFIX)%.chs,$(BLD_EXPERIMENTS_SUBST_VARIANT_PREFIX)%.hs,$(EXPERIMENTS_SUBST_HS_MAIN_SRC_CHS))

EXPERIMENTS_SUBST_HS_ALL_SRC_CHS			:= $(EXPERIMENTS_SUBST_HS_MAIN_SRC_CHS)

# all dependents for a variant to kick of building
EXPERIMENTS_SUBST_ALL_DPDS					:= $(EXPERIMENTS_SUBST_HS_MAIN_DRV_HS)

# end products, binary, executable, etc
EXPERIMENTS_SUBST_EXEC_NAME					:= subst
EXPERIMENTS_SUBST_EXEC_PROF_NAME			:= subst-prof
EXPERIMENTS_SUBST_BLD_EXEC					:= $(BIN_EXPERIMENTS_SUBST_VARIANT_PREFIX)$(EXPERIMENTS_SUBST_EXEC_NAME)$(EXEC_SUFFIX)
EXPERIMENTS_SUBST_BLD_EXEC_PROF				:= $(BIN_EXPERIMENTS_SUBST_VARIANT_PREFIX)$(EXPERIMENTS_SUBST_EXEC_PROF_NAME)$(EXEC_SUFFIX)
EXPERIMENTS_SUBST_ALL_EXECS					:= $(patsubst %,$(BIN_EXPERIMENTS_SUBST_PREFIX)%/$(EXPERIMENTS_SUBST_EXEC_NAME)$(EXEC_SUFFIX),$(EXPERIMENTS_SUBST_VARIANTS))

# ruler src, used for paper only, not for executables
EXPERIMENTS_SUBST_SRC_RUL					:= $(addprefix $(SRC_EXPERIMENTS_SUBST_PREFIX),RulerSubst.rul)
EXPERIMENTS_SUBST_RUL_DRV_LTEX				:= $(patsubst $(SRC_EXPERIMENTS_SUBST_PREFIX)%.rul,$(TEXT_TMP_VARIANT_PREFIX)%.ltex,$(EXPERIMENTS_SUBST_SRC_RUL))
EXPERIMENTS_SUBST_RUL_DRV_TEX				:= $(EXPERIMENTS_SUBST_RUL_DRV_LTEX:.ltex=.tex)

# all src
EXPERIMENTS_SUBST_ALL_CHUNK_SRC				:= $(EXPERIMENTS_SUBST_HS_ALL_SRC_CHS)
EXPERIMENTS_SUBST_ALL_RULES_SRC				:= $(EXPERIMENTS_SUBST_SRC_RUL)
EXPERIMENTS_SUBST_ALL_SRC					:= $(EXPERIMENTS_SUBST_ALL_CHUNK_SRC) $(EXPERIMENTS_SUBST_ALL_RULES_SRC) $(EXPERIMENTS_SUBST_MKF)

# distribution
EXPERIMENTS_SUBST_DIST_FILES				:= $(EXPERIMENTS_SUBST_ALL_SRC)

# rules for src to build
$(EXPERIMENTS_SUBST_HS_MAIN_DRV_HS): $(BLD_EXPERIMENTS_SUBST_VARIANT_PREFIX)%.hs: $(SRC_EXPERIMENTS_SUBST_PREFIX)%.chs $(SHUFFLE)
	mkdir -p $(@D)
	$(SHUFFLE_HS) $(EXPERIMENTS_SUBST_SHUFFLE_DEFS) --gen-reqm=$(EXPERIMENTS_VARIANT) --base=Main --variant-order="$(EXPERIMENTS_SUBST_SHUFFLE_ORDER)" $< > $@ && \
	touch $@

# rules for executables, dispatching on variant
$(patsubst $(BIN_EXPERIMENTS_SUBST_PREFIX)%$(EXEC_SUFFIX),%,$(EXPERIMENTS_SUBST_ALL_EXECS)): %: $(BIN_EXPERIMENTS_SUBST_PREFIX)%$(EXEC_SUFFIX)

$(EXPERIMENTS_SUBST_ALL_EXECS): %: $(EXPERIMENTS_SUBST_ALL_SRC) $(EXPERIMENTS_SUBST_MKF)
	$(MAKE) EXPERIMENTS_VARIANT=$(notdir $(*D)) experiment-subst-variant

experiment-subst-variant: 
	$(MAKE)  \
	  experiment-subst-variant-dflt

experiment-subst-variant-dflt: $(EXPERIMENTS_SUBST_ALL_DPDS)
	mkdir -p $(dir $(EXPERIMENTS_SUBST_BLD_EXEC)) && \
	$(GHC) --make $(GHC_OPTS) $(GHC_PLAIN_OPTS_WHEN_EXPERIMENTS_SUBST) -package $(LIB_EH_UTIL_PKG_NAME) -i$(BLD_EXPERIMENTS_SUBST_VARIANT_PREFIX) $(BLD_EXPERIMENTS_SUBST_VARIANT_PREFIX)$(EXPERIMENTS_SUBST_MAIN).hs -o $(EXPERIMENTS_SUBST_BLD_EXEC)
#	$(GHC) --make $(GHC_OPTS) $(GHC_PROF_OPTS_WHEN_EXPERIMENTS_SUBST) -package $(LIB_EH_UTIL_PKG_NAME) -i$(BLD_EXPERIMENTS_SUBST_VARIANT_PREFIX) $(BLD_EXPERIMENTS_SUBST_VARIANT_PREFIX)$(EXPERIMENTS_SUBST_MAIN).hs -o $(EXPERIMENTS_SUBST_BLD_EXEC_PROF)

experiment-subst-run: $(EXPERIMENTS_SUBST_ALL_EXECS)
	@if test $(DO_TIMING) = "yes" ; \
	then \
	  rm -f $(RUN_EXPERIMENTS_SUBST_TEX) ; \
	fi ; \
	for v in $(EXPERIMENTS_SUBST_VARIANTS) ; \
	do \
	  echo "== version $${v} ==" ; \
	  expexec="$(BINABS_EXPERIMENTS_SUBST_PREFIX)$${v}/$(EXPERIMENTS_SUBST_EXEC_NAME)$(EXEC_SUFFIX)" ; \
	  expprof="$(BINABS_EXPERIMENTS_SUBST_PREFIX)$${v}/$(EXPERIMENTS_SUBST_EXEC_PROF_NAME)$(EXEC_SUFFIX)" ; \
	  echo "== exec $${expexec} ==" ; \
	  for run in $(RUN_EXPERIMENTS_SUBST_RUNS) ; \
	  do \
	    vrun="$${v}/$${run}" ; \
	    rundir="$(RUN_EXPERIMENTS_SUBST_PREFIX)$${vrun}" ; \
	    echo "== rundir $${rundir} ==" ; \
	    mkdir -p $${rundir} ; \
	    runoutput="$${rundir}/out"  ; \
	    runtime="$${rundir}/time"  ; \
	    runtimesraw="$${rundir}/times-raw"  ; \
	    runtimes="$${rundir}/times"  ; \
	    if test $(DO_TIMING) = "yes" ; \
	    then \
	      rm -f $${runtimesraw} ; \
	      for t in 1 ; \
	      do \
	        (cd $${rundir} ; /usr/bin/time -l $${expexec} $${run} q $${v} > /dev/null) 2>> $${runtimesraw} ; \
	        sed -n -e 's/ *\([0-9.]*\) real.*/secs:\1/p' -e 's/ *\([0-9]*\)  maximum resident set size.*/bytes:\1/p' < $${runtimesraw} > $${runtimes} ; \
	        runreal=`sed -n -e 's/ *\([0-9.]*\) real.*/\1/p' < $${runtimesraw}` ; \
	        runmem=`sed -n -e 's/ *\([0-9]*\)  maximum resident set size.*/\1/p' < $${runtimesraw}` ; \
	        echo "\\\\rulerCmdDef{exp-subst-run-time-$${v}-$${run}}{$${runreal}}" >> $(RUN_EXPERIMENTS_SUBST_TEX) ; \
	        echo "\\\\rulerCmdDef{exp-subst-run-mem-$${v}-$${run}}{$$(($${runmem} / 1048576)).$$((($${runmem} % 1048576) / 104857))}" >> $(RUN_EXPERIMENTS_SUBST_TEX) ; \
	      done \
	    else \
          time ((cd $${rundir} ; $${expexec} $${run} q $${v}) > $${runoutput}) 2> $${runtime} ; \
	    fi \
	  done \
	done


          #time ((cd $${rundir} ; $${expprof} $(RTS_OPTS_WHEN_EXPERIMENTS_SUBST1) $${run} q $${v}) >> $${runoutput}) 2>> $${runtime} ; \
          #(cd $${rundir} && hp2ps $(HP2PS_OPTS_WHEN_EXPERIMENTS_SUBST) $(EXPERIMENTS_SUBST_EXEC_PROF_NAME).hp && ps2pdf $(EXPERIMENTS_SUBST_EXEC_PROF_NAME).ps ) ; \
          #time ((cd $${rundir} ; $${expprof} $(RTS_OPTS_WHEN_EXPERIMENTS_SUBST2) $${run} q $${v}) >> $${runoutput}) 2>> $${runtime} ; \

