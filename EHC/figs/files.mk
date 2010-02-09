# location of figs src
FIGS_SRC_PREFIX				:= $(TOP_PREFIX)figs/
FIGS_SRC_SUBDIRS_PREFIX		:= $(patsubst %,$(FIGS_SRC_PREFIX)%/,hs09-uhcarch)

# this file
FIGS_MKF					:= $(FIGS_SRC_PREFIX)files.mk

# figures
ifeq ($(TEXT_CFG_FIGS_INCLUDES_XFIG_SRC),yes)
FIGS_XFIG_SRC_FIG			:= $(wildcard $(addsuffix *.fig,$(FIGS_SRC_PREFIX) $(FIGS_SRC_SUBDIRS_PREFIX)))
else
FIGS_XFIG_SRC_FIG			:= 
endif
FIGS_EPS_SRC_EPS			:= $(wildcard $(FIGS_SRC_PREFIX)*.eps)
ifeq ($(TEXT_CFG_FIGS_INCLUDES_DOT_SRC),yes)
FIGS_DOT_SRC_DOT			:= $(wildcard $(addsuffix *.dot,$(FIGS_SRC_PREFIX) $(FIGS_SRC_SUBDIRS_PREFIX)))
else
FIGS_DOT_SRC_DOT			:= 
endif
FIGS_ASIS_SRC_PDF			:= $(wildcard $(addsuffix *.pdf,$(FIGS_SRC_PREFIX) $(FIGS_SRC_SUBDIRS_PREFIX)))
FIGS_ASIS_SRC_JPG			:= $(wildcard $(addsuffix *.jpg,$(FIGS_SRC_PREFIX) $(FIGS_SRC_SUBDIRS_PREFIX)))
FIGS_ASIS_SRC_PNG			:= $(wildcard $(addsuffix *.png,$(FIGS_SRC_PREFIX) $(FIGS_SRC_SUBDIRS_PREFIX)))
FIGS_ASIS_SRC_EPS			:= # $(wildcard $(addsuffix *.eps,$(FIGS_SRC_PREFIX) $(FIGS_SRC_SUBDIRS_PREFIX)))

# exclude from pdf generation (because containing special chars)
FIGS_XFIG_SRC_FIG_NOPDF		:= $(filter-out $(patsubst %,$(FIGS_SRC_PREFIX)%.fig,type-lattice let-I2-flow roadmap),$(FIGS_XFIG_SRC_FIG))

# all src
FIGS_ASIS_SRC				:= $(FIGS_ASIS_SRC_PDF) $(FIGS_ASIS_SRC_JPG) $(FIGS_ASIS_SRC_EPS) $(FIGS_ASIS_SRC_PNG)
FIGS_ALL_SRC				:= $(FIGS_XFIG_SRC_FIG) $(FIGS_EPS_SRC_EPS) $(FIGS_ASIS_SRC) $(FIGS_DOT_SRC_DOT)

# distribution
FIGS_DIST_FILES				:= $(FIGS_ALL_SRC) $(FIGS_MKF)
