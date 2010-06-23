# location of ehc src (from files.mk, but required globally by mk/shared.mk)
SRC_EHC_PREFIX							:= $(SRC_PREFIX)ehc/
SRC_EHC_RULES_PREFIX					:= $(SRC_EHC_PREFIX)rules/
EHC_SUBDIRS								:= $(patsubst $(SRC_EHC_PREFIX)/%/,%,$(SRC_EHC_RULES_PREFIX))
LATEX_EHC_SUBDIRS						:= $(patsubst $(SRC_EHC_PREFIX)%,%$(PATHS_SEP_COL),$(EHC_SUBDIRS))
