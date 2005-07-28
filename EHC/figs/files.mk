# location of figs src
FIGS_SRC_PREFIX		:= $(TOP_PREFIX)figs/

# this file
FIGS_MKF			:= $(FIGS_SRC_PREFIX)files.mk

# figures
FIGS_XFIG_SRC_FIG	:= $(wildcard $(FIGS_SRC_PREFIX)*.fig)
FIGS_ASIS_SRC_PDF	:= $(wildcard $(FIGS_SRC_PREFIX)*.pdf)

# all src
FIGS_ALL_SRC		:= $(FIGS_XFIG_SRC_FIG) $(FIGS_ASIS_SRC_PDF)

# distribution
FIGS_DIST_FILES		:= $(FIGS_ALL_SRC) $(FIGS_MKF)
