# location of ehc src (from files.mk, but required globally by mk/shared.mk)
EHC_SRC_PREFIX							:= $(TOP_PREFIX)ehc/
EHC_RULES_SRC_PREFIX					:= $(EHC_SRC_PREFIX)rules/
EHC_SUBDIRS								:= $(patsubst $(EHC_SRC_PREFIX)/%/,%,$(EHC_RULES_SRC_PREFIX))
LATEX_EHC_SUBDIRS						:= $(patsubst $(EHC_SRC_PREFIX)%,%:,$(EHC_SUBDIRS))
