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
# Updated: 2016-01-11
#


import os
import subprocess


def mailpasswd(account):
    account = os.path.basename(account)
    path = '{0}/.offlineimap/{1}.gpg'.format(os.environ['HOME'], account)
    args = ['gpg2', '--for-your-eyes-only', '--no-tty',
            '--quiet', '--batch', '--decrypt', path]
    try:
        return subprocess.check_output(args).strip()
    except subprocess.CalledProcessError:
        return ''

# subprocess.check_output() only introduced in python 2.7
# this version of 'mailpasswd' works with older version of python
#def mailpasswd(account):
#    account = os.path.basename(account)
#    path = '{0}/.offlineimap/{1}.gpg'.format(os.environ['HOME'], account)
#    args = ['gpg2', '--for-your-eyes-only', '--no-tty',
#            '--quiet', '--batch', '--decrypt', path]
#    proc = subprocess.Popen(args, stdout=subprocess.PIPE)
#    output = proc.communicate()[0].strip()
#    retcode = proc.wait()
#    if retcode == 0:
#        return output
#    else:
#        return ''


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

prime_gpg_agent()

