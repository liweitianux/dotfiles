#!/bin/sh
#
# excel2text - convert MS Excel files to ASCII text
#
# Written by Gary Johnson <garyjohn@spk.agilent.com>.
#
# Modification History:
#   2003-02-19
#       Changed the name xlHtml to xlhtml to match the xlhtml-0.5
#       release.

prog=${0##*/}

if [ $# -ne 1 ]
then
    echo "Usage: $prog file" >&2
    exit 2
fi

xlhtml -te "$1" |
perl -pe 's/>-{21,}</><HR></g' |	# This replaces long lines of
					# hyphens with HTML <HR> tags so
					# that these lines adapt to the
					# width of the cell.  "21" is a
					# value that works for the
					# tables I'm sent.
w3m -dump -T text/html |
perl -pe '
	s/\xa0/ /gs;			# Change A0 spaces to ASCII
					# spaces.
'
