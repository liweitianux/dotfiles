#!/bin/csh
#
# HEASOFT settings
#
unset printexitvalue

set HEA_STATE=`echo $PATH | tr ':' '\n' | grep 'heasoft'`
if ( "x${HEA_STATE}" == "x" ) then
    source $HEADAS/headas-init.csh
endif
unset HEA_STATE
# set HEA_LDLIB="`echo $LD_LIBRARY_PATH | tr ':' '\n' | grep 'heasoft' | head -n 1`"

exit 0

