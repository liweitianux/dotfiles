#!/bin/csh
#
# $Id: complete.sample 52 2006-01-01 06:26:59Z koma2 $

if ($?PORTSDIR == 0) then
    setenv PORTSDIR /usr/ports
endif

if ($?PKG_DBDIR == 0) then
    setenv PKG_DBDIR /var/db/pkg
endif

complete pkg_create "n@-b@D:$PKG_DBDIR@@"
complete pkg_deinstall "p@*@D:$PKG_DBDIR@@"
complete pkg_fetch 'p@*@`ls $PKG_DBDIR | sed "s,\(.*\)-.*,\1," `@@'
complete pkg_which "c@*@D:$PKG_DBDIR@@" "o@*@D:$PKG_DBDIR@@"
complete pkgdb "c@*@D:$PKG_DBDIR@@" "o@*@D:$PKG_DBDIR@@"
complete portinstall "p@*@D:$PORTSDIR@"
complete ports_glob "p@*@D:$PORTSDIR@"
complete portupgrade "n@-o@D:$PORTSDIR@" "p@*@D:$PKG_DBDIR@@"
complete portversion "p@*@D:$PKG_DBDIR@@"

