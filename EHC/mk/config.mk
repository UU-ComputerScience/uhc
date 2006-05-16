### platform config
# EXEC_SUFFIX	: suffix for executables
# PATH_SEP		: separator in file paths
# PATHS_SEP		: separator in sequences of file paths, as in environments
# STRIP_CMD		: strip cmd

### Unix
EXEC_SUFFIX			:=
PATH_SEP			:= /
PATHS_SEP			:= :
STRIP_CMD			:= strip

### WinXX
#EXEC_SUFFIX			:= .exe
#PATH_SEP			:= \
#PATHS_SEP			:= ;
#STRIP_CMD				:= echo

### remaining config
# which version (usually v1 = current, v2 == under development)
#RULER2_OPTS_VERSION	:= --v1
#RULER2_OPTS_VERSION	:= --v2
RULER2_OPTS_VERSION	:=
