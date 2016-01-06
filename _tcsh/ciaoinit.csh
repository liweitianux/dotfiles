#!/bin/csh -
#
# CIAO settings
#

set CIAO_STATE="`echo $PATH | tr ':' '\n' | grep 'ciao'`"
if ( "x${CIAO_STATE}" == "x" ) then
    source $CIAO_PATH/bin/ciao.csh
else
    source $CIAO_PATH/bin/ciao.csh -o
endif
unset CIAO_STATE

setenv CIAO_LD_LIBRARY_PATH "${ASCDS_INSTALL}/lib:${ASCDS_INSTALL}/ots/lib:${ASCDS_INSTALL}/ots/lib/vtk-5.10"
setenv ASCDS_IMAGER_PATH "${ASCDS_INSTALL}/ots/saord"

exit 0

