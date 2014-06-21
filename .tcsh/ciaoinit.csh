#!/bin/csh -
#
# CIAO settings
#
unset printexitvalue

set HEA_STATE="`echo $PATH | tr ':' '\n' | grep 'heasoft'`"
set CIAO_STATE="`echo $PATH | tr ':' '\n' | grep 'ciao'`"
if ( "x${HEA_STATE}" == "x" ) then
    heainit
endif
if ( "x${CIAO_STATE}" == "x" ) then
    source $CIAO_PATH/bin/ciao.csh
else
    source $CIAO_PATH/bin/ciao.csh -o
endif
unset HEA_STATE
unset CIAO_STATE

exit 0

