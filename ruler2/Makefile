.SUFFIXES:

TOP_PREFIX				:= ../

explanation: 
	@echo "to make ruler       :  make ruler" ; \
	echo  "to make ruler's demo:  make demo" ;

include $(TOP_PREFIX)/mk/shared.mk
include $(TOP_PREFIX)/shuffle/files.mk

include files.mk

ruler: $(RULER2_BLD_EXEC)

demo: $(RULER2_DEMO_EXEC)

clean:
	rm -rf $(RULER2_HS_DRV) $(wildcard $(addprefix $(RULER2_SRC_PREFIX),*.o *.hi))