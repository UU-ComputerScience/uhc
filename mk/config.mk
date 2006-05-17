### the overall version of EH
EH_VERSION_MAJOR	:= 0
EH_VERSION_MINOR	:= 1
EH_VERSION_STABLE	:= alpha
EH_VERSION			:= $(EH_VERSION_MAJOR).$(EH_VERSION_MINOR)

### platform config
# EXEC_SUFFIX	: suffix for executables
# PATH_SEP		: separator in file paths
# PATHS_SEP		: separator in sequences of file paths, as in environments
# PATHS_SEP_COL	: separator in sequences of file paths, as in environments which do not use absolute paths
# STRIP_CMD		: strip cmd
# TOPABS_PREFIX	: absolute prefix/pathname to top dir

### Unix
EXEC_SUFFIX			:=
PATH_SEP			:= /
PATHS_SEP			:= :
PATHS_SEP_COL		:= :
STRIP_CMD			:= strip
TOPABS_PREFIX		:= $(shell pwd)/

### WinXX
#EXEC_SUFFIX			:= .exe
#PATH_SEP			:= \
#PATHS_SEP			:= ;
#PATHS_SEP_COL		:= :
#STRIP_CMD				:= echo
#TOPABS_PREFIX		:= c:/whatever

### remaining config
# which version (usually v1 = current, v2 == under development)
#RULER2_OPTS_VERSION	:= --v1
#RULER2_OPTS_VERSION	:= --v2
RULER2_OPTS_VERSION	:=
