#!/bin/csh -v
#
# SAS settings
#
unset printexitvalue

set HEA_STATE="`echo $PATH | tr ':' '\n' | grep 'heasoft'`"
set SAS_STATE="`echo $PATH | tr ':' '\n' | grep 'xmmsas'`"
if ( "x${HEA_STATE}" == "x" ) then
    heainit
endif
if ( "x${SAS_STATE}" == "x" ) then
    ## heasoft, due to conflicts
    set HEASOFT_BIN="${SAS_DIR}/../heasoft_bin"
    setenv PATH ${HEASOFT_BIN}:${PATH}
    ## init SAS
    source ${SAS_DIR}/setsas.csh
    setenv SAS_CCFPATH "${SAS_DIR}/../ccf"
    setenv SAS_ESAS_CALDB "${SAS_DIR}/../esas_caldb"
    setenv SAS_VERBOSITY 2
    setenv SAS_SUPPRESS_WARNING 10
    setenv SAS_IMAGEVIEWER ds9
    # heasoft
    # setenv LD_LIBRARY_PATH $HEA_LDLIB:$LD_LIBRARY_PATH
endif
unset HEA_STATE
unset SAS_STATE

exit 0

