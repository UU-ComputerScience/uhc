###########################################################################################
# Copy files
###########################################################################################

# copy files using tar
# $1: src dir
# $2: dest dir
# $3: src files relative to src dir
FUN_COPY_FILES_BY_TAR			= ((cd $(1) && $(TAR) --ignore-failed-read -cf - $(3)) | (mkdir -p $(2) && cd $(2) && $(TAR) xf -))

# copy files recursively while renaming the variant subdirectory
# $1: src dir
# $2: dest dir
# $3: original variant name
# $4: target variant name
FUN_COPY_FILES_AND_RENAME		= (cd $(1) && for i in `find . -type f`; do tgt=$(2)/`echo $${i} | sed s+$(3)/+$(4)/+g`; mkdir -p  `dirname $${tgt}` ; install $${i} $${tgt}; done)

###########################################################################################
# Shell script generation: shell alias to executable with config options
###########################################################################################

# $1: executable
# $2: shell script
# $3: config options
FUN_GEN_SHELL_ALIAS	= \
					( \
					  echo "\#!/bin/sh" ; \
					  echo $(1) '$(3) $$*' ; \
					) > $(2) ; \
					chmod +x $(2)

###########################################################################################
# Generate uhc wrapper shell script
###########################################################################################

# $1: name of binary
# $2: dst install shell script name
# $3: prefix of location of binary
# $4: abs installation root dir
# $5: variant name
FUN_INSTALLUHC_WRAPPER	= \
	echo "Generating uhc wrapper in $(2) ..." && \
	mkdir -p $(3) && \
	$(call FUN_GEN_SHELL_ALIAS,$(1),$(2),--cfg-install-root=$(4) --cfg-install-variant=$(5))

###########################################################################################
# installation locations for ehc running time, as functions still depending on variant + target, see shared.mk for more
###########################################################################################

FUN_DIR_VARIANT_PREFIX						= $(1)/$(2)/
