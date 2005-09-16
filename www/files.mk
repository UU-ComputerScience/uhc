# location of shuffle src
WWW_SRC_PREFIX			:= $(TOP_PREFIX)www/

# this file
WWW_MKF					:= $(WWW_SRC_PREFIX)files.mk

# main + sources
WWW_HTML_SRC			:= $(addprefix $(WWW_SRC_PREFIX),ehc.html)

# all src
WWW_ALL_SRC				:= $(WWW_HTML_SRC)

# www accessible files
WWW_DOC_FILES			:= $(patsubst $(DOC_PREFIX)%,$(WWW_SRC_PREFIX)%,$(TEXT_DIST_DOC_FILES))

# distribution
WWW_DIST_FILES			:= $(WWW_ALL_SRC)

# make rules
$(WWW_DOC_FILES): $(WWW_SRC_PREFIX)%: $(DOC_PREFIX)%
	cp $< $@

