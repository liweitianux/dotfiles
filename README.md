dotfiles
========

Personal & collected dotfiles ~~~

* ``linux``: Linux-specific configurations
* ``freebsd``: FreeBSD-specific configurations


# Configuration Notes

## Gmail IMAP
* 'Auto-Expunge' => off: for better performance

## msmtp: TLS
* ``tls_fingerprint``:
    ``$ msmtp --serverinfo --tls --tls-certcheck=off --host=<host>``

## offlineimap: fingerprint
Use the little script ``_bin/get_cert.sh``

# Explicit home or other path
Following files contains (at least currently) the *explicit*
home or other paths:
* ``_notmuch-config``: database.path
* ``_config/alot/config``: maildir


# License
Distributed under the MIT License.


# Author
Aaron LI

GnuPG key fingerprint: ``AC34 64FA DAAE 6321 8609  9CA6 240E 2A63 5D72 729A``

