#!/bin/sh
#
# ppt2text - convert MS PowerPoint files to ASCII text
#
# Written by Gary Johnson <garyjohn@spk.agilent.com>.
#
# Modification History:
#   2003-02-19
#       Changed the name pptHtml to ppthtml to match the xlhtml-0.5
#       release.

prog=${0##*/}

if [ $# -ne 1 ]
then
    echo "Usage: $prog file" >&2
    exit 2
fi

ppthtml $1 |
w3m -dump -T text/html |
perl -pe 's/\xa0/ /g'			# Change A0 spaces to ASCII
					# spaces.
