#!/bin/sh
#
# Get the certificate of a server
#
# Reference:
# [1] ArchWiki - Isync
#     https://wiki.archlinux.org/index.php/Isync
#
# Aaron LI
# Created: 2016-01-30
#

if [ $# -ne 1 ] && [ $# -ne 2 ]; then
    echo "Usage:"
    echo "    `basename $0` <host>:<port> [ output.pem ]"
    exit 1
fi

SERVER="$1"
HOST=`echo "${SERVER}" | cut -d':' -f1`
PORT=`echo "${SERVER}" | cut -d':' -f2`
PEM="$2"
[ -z "${PEM}" ] && PEM="${HOST}.pem"

openssl s_client -connect ${HOST}:${PORT} -showcerts 2>&1 < /dev/null | \
    sed -ne '/-BEGIN CERTIFICATE-/,/-END CERTIFICATE-/p' | \
    sed -ne '1,/-END CERTIFICATE-/p' > ${PEM}

FINGERPRINT=`openssl x509 -noout -in ${PEM} -fingerprint -sha1 | cut -d'=' -f2`
FINGERPRINT2=`echo "${FINGERPRINT}" | tr -d ':' | tr '[[:upper:]]' '[[:lower:]]'`

NOT_BEFORE=`openssl x509 -noout -in ${PEM} -dates | grep 'notBefore' | cut -d'=' -f2`
NOT_AFTER=`openssl x509 -noout -in ${PEM} -dates | grep 'notAfter' | cut -d'=' -f2`

echo "Fingerprint_SHA1: ${FINGERPRINT} / ${FINGERPRINT2}"
echo "Validity: ${NOT_BEFORE} - ${NOT_AFTER}"
echo "Certificate save to file: ${PEM}"
echo "Place the certificate to a cert directory, and rehash with 'c_rehash'"

