.SUFFIXES:

TOP_PREFIX				:= ../../

explanation: 
	@echo "to make ruler       :  make ruler (i.e.: $(RULER3_BLD_EXEC))" ; \
	echo  "to make ruler's demo:  make demo  (i.e.: $(RULER3_DEMO_EXEC))" ;

include $(TOP_PREFIX)/mk/shared.mk
include $(TOP_PREFIX)/src/files.mk
include $(SRC_PREFIX)/libutil/files.mk
include $(SRC_PREFIX)/shuffle/files.mk

include files.mk

ruler: $(RULER3_BLD_EXEC)

demo: $(RULER3_DEMO_EXEC)

