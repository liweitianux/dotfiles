#!/usr/bin/env python
#
# Add support of encrypting password with gpg2 for OfflineIMAP.
# Provide function 'mailpasswd' to decrypt the password.
#
# Configurations:
#   [general]
#   pythonfile = ~/.offlineimap/offlineimap.py
#   ...
#   [Repository <reponame>]
#   remotepasseval = mailpasswd("<accountname>")
#   ...
#
# Create encrypted password file:
# > echo "password" | gpg2 -e -r foo@bar.com > account.gpg
#
# Reference:
# [1] Encrypt OfflineIMAP Password
#     http://unix.stackexchange.com/a/48355
#
#
# Aaron LI
# Created: 2015-02-02
# Updated: 2016-09-09
#


import os
import sys
import subprocess


def mailpasswd(account):
    account = os.path.basename(account)
    passfile = os.path.expanduser('~/.private/{0}.gpg'.format(account))
    args = ['gpg2', '--for-your-eyes-only', '--no-tty',
            '--quiet', '--batch', '--decrypt', passfile]
    try:
        return subprocess.check_output(args).strip()
    except subprocess.CalledProcessError:
        return ''


# If you have several accounts that get checked simultaneously, and you
# use 'gpg-agent', then it will ask for you passphrase for each account.
# I prime the agent by creating a file, and priming the gpg-agent by
# decrypting this file on launch of offlineimap.
def prime_gpg_agent():
    # echo "prime" | gpg -e -r <recipient> > ~/.offlineimap/prime.gpg
    ret = False
    i = 1
    while not ret:
        ret = (mailpasswd("prime") == "prime")
        if i > 2:
            from offlineimap.ui import getglobalui
            sys.stderr.write("Error reading in passwords. Terminating.\n")
            getglobalui().terminate()
        i += 1
    return ret


if __name__ == "__main__":
    prime_gpg_agent()
