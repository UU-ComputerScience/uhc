.SUFFIXES:

TOP_PREFIX				:= ../

include $(TOP_PREFIX)/mk/shared.mk

include files.mk

clean:
	rm -rf $(RULER2_HS_DRV) $(wildcard $(addprefix $(RULER2_SRC_PREFIX),*.o *.hi))