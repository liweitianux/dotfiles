dotfiles
========

Personal & collected dotfiles ~~~

* ``freebsd``: FreeBSD-specific configurations

Usage
-----
To enable a dotfile and/or dot directory:
* ``$ ./_bin/enable.sh _zshrc _zsh``

To disable:
* ``$ ./_bin/disable.sh _zshrc _zsh``


Configuration Notes
-------------------
## Gmail IMAP
* "Auto-Expunge" => off: for better performance

## msmtp: TLS
* ``tls_fingerprint``:
    ``$ msmtp --serverinfo --tls --tls-certcheck=off --host=<host>``

## offlineimap: fingerprint
Use the little script ``_bin/get_cert.sh``

## Explicit home or other path
Following files contains (at least currently) the *explicit*
home or other paths:
* ``_notmuch-config``: ``database.path``
* ``_config/alot/config``: ``maildir``
* ``_rtorrent.rc``: ``cfg.basedir``


License
-------
Distributed under the MIT License.
