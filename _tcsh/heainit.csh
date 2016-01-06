#!/bin/csh
#
# HEASOFT settings
#

source $HEADAS/headas-init.csh
# set HEA_LDLIB="`echo $LD_LIBRARY_PATH | tr ':' '\n' | grep 'heasoft' | head -n 1`"

if ( $?LD_LIBRARY_PATH_BAK ) then
    setenv LD_LIBRARY_PATH "${LD_LIBRARY_PATH_BAK}"
endif

exit 0

