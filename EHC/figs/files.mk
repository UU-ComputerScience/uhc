# location of figs src
FIGS_SRC_PREFIX				:= $(TOP_PREFIX)figs/

# this file
FIGS_MKF					:= $(FIGS_SRC_PREFIX)files.mk

# figures
ifeq ($(TEXT_CFG_FIGS_INCLUDES_XFIG_SRC),yes)
FIGS_XFIG_SRC_FIG			:= $(wildcard $(FIGS_SRC_PREFIX)*.fig)
else
FIGS_XFIG_SRC_FIG			:= 
endif
FIGS_EPS_SRC_EPS			:= $(wildcard $(FIGS_SRC_PREFIX)*.eps)
ifeq ($(TEXT_CFG_FIGS_INCLUDES_DOT_SRC),yes)
FIGS_DOT_SRC_DOT			:= $(wildcard $(FIGS_SRC_PREFIX)*.dot)
else
FIGS_DOT_SRC_DOT			:= 
endif
FIGS_ASIS_SRC_PDF			:= $(wildcard $(FIGS_SRC_PREFIX)*.pdf)
FIGS_ASIS_SRC_JPG			:= $(wildcard $(FIGS_SRC_PREFIX)*.jpg)
FIGS_ASIS_SRC_EPS			:= # $(wildcard $(FIGS_SRC_PREFIX)*.eps)

# exclude from pdf generation (because containing special chars)
FIGS_XFIG_SRC_FIG_NOPDF		:= $(filter-out $(patsubst %,$(FIGS_SRC_PREFIX)%.fig,type-lattice let-I2-flow roadmap),$(FIGS_XFIG_SRC_FIG))

# all src
FIGS_ASIS_SRC				:= $(FIGS_ASIS_SRC_PDF) $(FIGS_ASIS_SRC_JPG) $(FIGS_ASIS_SRC_EPS)
FIGS_ALL_SRC				:= $(FIGS_XFIG_SRC_FIG) $(FIGS_EPS_SRC_EPS) $(FIGS_ASIS_SRC) $(FIGS_DOT_SRC_DOT)

# distribution
FIGS_DIST_FILES				:= $(FIGS_ALL_SRC) $(FIGS_MKF)
