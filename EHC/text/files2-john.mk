LLVM_CODE_IMG_PREFIX       := $(TOP_PREFIX)text/llvm/img/
LLVM_CODE_SRC_PREFIX       := $(TOP_PREFIX)text/llvm/code/
LLVM_CODE_DATA_PREFIX      := $(TOP_PREFIX)text/llvm/data/

LLVM_THESIS_EXAMPLES       := $(TEXT_TMP_VARIANT_PREFIX)Fib.lhs \
                              $(TEXT_TMP_VARIANT_PREFIX)FibExe.core \
                              $(TEXT_TMP_VARIANT_PREFIX)FibExe.grin \
                              $(TEXT_TMP_VARIANT_PREFIX)FibExe.sil \
                              $(TEXT_TMP_VARIANT_PREFIX)FibExe-opt.grin \
                              $(TEXT_TMP_VARIANT_PREFIX)GRIN_Fib_Tree.tex \
                              $(TEXT_TMP_VARIANT_PREFIX)GRIN_Fib_Tree_Opt.tex \
                              $(TEXT_TMP_VARIANT_PREFIX)EHC_Variants.tex \
                              $(TEXT_TMP_VARIANT_PREFIX)Silly_type_example.tex \
                              $(TEXT_TMP_VARIANT_PREFIX)LLVMExample.c \
                              $(TEXT_TMP_VARIANT_PREFIX)GetElementPtrExample.ll \
                              $(TEXT_TMP_VARIANT_PREFIX)PhiExample.ll \
                              $(TEXT_TMP_VARIANT_PREFIX)running-example.ll \
                              $(TEXT_TMP_VARIANT_PREFIX)MultipleReturnValuesMRV.ll \
                              $(TEXT_TMP_VARIANT_PREFIX)MultipleReturnValuesRP.ll \
                              $(TEXT_TMP_VARIANT_PREFIX)PrimitiveInline-No.ll \
                              $(TEXT_TMP_VARIANT_PREFIX)LLVMExample.ll \
                              $(TEXT_TMP_VARIANT_PREFIX)compiletime.gc.tex \
                              $(TEXT_TMP_VARIANT_PREFIX)compiletime.nogc.tex \
                              $(TEXT_TMP_VARIANT_PREFIX)runtime.gc.tex \
                              $(TEXT_TMP_VARIANT_PREFIX)runtime.nogc.tex

LLVM_GRIN_FILES            := $(LLVM_CODE_SRC_PREFIX)FibExe.grin
LLVM_GRIN_FILES_DEP        := $(LLVM_CODE_SRC_PREFIX)FibExe-013-renameuniform.grin \
                              $(LLVM_CODE_SRC_PREFIX)Eval.grin

LLVM_CODE_FILES            := $(LLVM_CODE_SRC_PREFIX)FibExe.hs 
LLVM_CODE_FILES_DEP        := $(LLVM_CODE_SRC_PREFIX)Fib.hs \
                              $(LLVM_CODE_SRC_PREFIX)PreludeEHC8.hs

LLVM_SILLY_FILES           := $(LLVM_CODE_SRC_PREFIX)FibExe.sil 
LLVM_SILLY_FILES_DEP       := $(LLVM_CODE_SRC_PREFIX)FibExe-205.sil

text-variant-llvm: $(LLVM_THESIS_EXAMPLES)
	$(MAKE) TEXT_CFG_FIGS_INCLUDES_DOT_SRC=yes \
	  LHS2TEX_OPTS_VARIANT_CONFIG="--unset=yesBeamer --set=blockstyle --set=inclTOC --set=useHyperref --set=refToPDF" \
	  TEXT_SHUFFLE_VARIANT=35 \
	  text-variant-dflt-bib

$(TEXT_TMP_VARIANT_PREFIX)%.lhs: $(TEXT_TMP_VARIANT_PREFIX)%.hs  
	echo '\\begin{code}' > $@
	cat $< >> $@
	echo '\end{code}' >> $@

$(TEXT_TMP_VARIANT_PREFIX)%: $(LLVM_CODE_SRC_PREFIX)%
	mkdir -p $(dir $@)
	cp $< $@

$(TEXT_TMP_VARIANT_PREFIX)%: $(LLVM_CODE_IMG_PREFIX)%
	mkdir -p $(dir $@)
	cp $< $@

$(TEXT_TMP_VARIANT_PREFIX)%: $(LLVM_CODE_DATA_PREFIX)%
	mkdir -p $(dir $@)
	cp $< $@

$(LLVM_CODE_SRC_PREFIX)FibExe.core $(LLVM_CODE_SRC_PREFIX)FibExe-012-aliaselim.grin: $(LLVM_CODE_FILES)
	install/8_2/bin/ehc --code=lexe --gen-cmt=0 --dump-grin-stages=1 --optimise=1 --priv=1 -p- $< && \
	sed -i '56,59d' $(LLVM_CODE_SRC_PREFIX)FibExe.core

$(LLVM_CODE_SRC_PREFIX)FibExe-opt.grin: $(LLVM_CODE_SRC_PREFIX)FibExe-179-final.grin
	sed -i 's/fun_x_[0-9]\+_//g' $<    ; \
	sed -i 's/x_[0-9]\+_//g' $<        ; \
	sed '17!d' $<                 > $@ ; \
	echo "        ..."           >> $@ ; \
	sed '51,57!d' $<             >> $@ ; \
	echo "..."           >> $@

$(LLVM_GRIN_FILES): $(LLVM_GRIN_FILES_DEP)
	cat $^ > $@

$(LLVM_CODE_FILES): $(LLVM_CODE_FILES_DEP)
	cat $^ > $@

$(LLVM_SILLY_FILES): $(LLVM_SILLY_FILES_DEP)
	sed '32,34!d' $<        > $@ ; \
	echo "        ..."     >> $@ ; \
	sed '60,74!d' $<       >> $@ ; \
	echo "        ..."     >> $@ ; \
	echo "}"               >> $@ ; \
	sed -i 's/fun_//' $@         ; \
	sed -i 's/, )/)/' $@         ; \
	sed -i 's/ )/)/' $@          ; \
	sed -i 's/ ;/;/' $@          ; \

