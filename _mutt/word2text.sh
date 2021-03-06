#!/bin/sh
#
# word2text - convert MS Word files to ASCII text
#
# SYNOPSIS
#	word2text file
#
# DESCRIPTION
#	Word2text uses wvHtml, w3m and some perl glue to convert the MS
#	Word file specified by the argument to ASCII text on stdout.
#
#	wvHtml converts MS Word files to HTML, but is intended to be
#	used with a graphical browser such as Netscape Navigator, so it
#	converts certain graphical elements to image files and
#	corresponding <img> tags that the browser can render.  Since
#	this script uses a text based browser, it uses perl to eliminate
#	these <img> tags.
#
#	Compared to Quick View Plus (qvpview), the rendering of MS Word
#	documents done by word2text is usually more accurate.  Qvpview
#	doesn't render unrecognized characters well, if at all.  It also
#	renders numbered lists as bullet lists.
#
#	Compared to the plain text translations that some people include
#	in their e-mail along with the original MS Word attachments, the
#	rendering done by word2text is usually more readable:  vertical
#	spacing between paragraphs and list items is better and the
#	adjustment of text within paragraphs is better.
#
# BUGS
#	wvHtml occasionally dumps core.
#	With the wv-0.7.4 release, some unnumbered lists are rendered as
#	numbered.
#
# AUTHOR
#	Gary A. Johnson
#	<garyjohn@spk.agilent.com>
#
# REVISION HISTORY
#	2003-05-31
#		Between wv-0.5.42 and wv-0.7.4, wvHtml changed to insert
#		a <p> tag between each <li> tag and the following text,
#		causing the list-item text to start on the line
#		following the list-item bullet or number.  A perl
#		expression was added to this script to fix the problem.
# 	2003-02-19
#		The command-line arguments to wvHtml changed, requiring
#		the output file name as well as the input file name.
#		Therefore, '-' (stdout) was added to the command as the
#		output file name.

wvHtml "$1" - 2> /dev/null |
perl -0777 -p -e '
	s|<img .*?>||gs;		# Delete img tags.
	s|(<li.*?>)\s*<p>|\1|gs;	# Remove <p> tags immediately
					# following <li> tags.  (This
					# problem appeared somewhere
					# between wv-0.5.42 and
					# wv-0.7.4.)
' |
w3m -dump -T text/html |
perl -p -e '
	s/\n\s*\n/\n\n/gs;		# Delete extra whitespace
					# between lines.
	s/\xa0/ /gs;			# Change A0 spaces to ASCII
					# spaces.
'
