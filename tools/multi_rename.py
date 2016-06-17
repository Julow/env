#!/usr/bin/python
# **************************************************************************** #
#                                                                              #
#                                                         :::      ::::::::    #
#    multi_rename.py                                    :+:      :+:    :+:    #
#                                                     +:+ +:+         +:+      #
#    By: juloo <juloo@student.42.fr>                +#+  +:+       +#+         #
#                                                 +#+#+#+#+#+   +#+            #
#    Created: 2015/10/04 15:11:48 by juloo             #+#    #+#              #
#    Updated: 2016/06/17 10:55:33 by jaguillo         ###   ########.fr        #
#                                                                              #
# **************************************************************************** #

#
# Multi rename
#
# Use $EDITOR to edit file names
#

from sys import argv, exit
from tempfile import mkstemp
import os, shutil, subprocess

files = []
new_names = []
editor = os.environ.get('EDITOR')

if editor == None or len(editor) == 0:
	print "Please setup $EDITOR variable"
	exit(1)

for f in argv[1:]:
	files.append(f)

if len(files) == 0:
	print "Usage: %s [file1] [file2 ...]" % argv[0]
	exit(0)

tmp_f, tmp_file = mkstemp("", "multi_rename_")

with os.fdopen(tmp_f, "w") as f:
	for file_name in files:
		f.write("%s\n" % file_name)

subprocess.call("%s %s" % (editor, tmp_file), shell=True)

with open(tmp_file) as f:
	for l in f:
		new_names.append(l.rstrip('\n'))

modifs = 0

for i in range(0, len(files)):
	modifs += 1
	if i >= len(new_names) or (len(new_names[i]) == 0 and len(files[i]) > 0):
		print "Remove '%s'" % files[i]
	elif len(files[i]) == 0:
		print "Create '%s'" % new_names[i]
	elif files[i] != new_names[i]:
		print "Rename '%s' to '%s'" % (files[i], new_names[i])
	else:
		modifs -= 1

print "Press any key to validate %d modifs" % modifs
print "Or ctrl+C to cancel"

raw_input()

for i in range(0, len(files)):
	if i >= len(new_names) or (len(new_names[i]) == 0 and len(files[i]) > 0):
		os.remove(files[i])
	elif len(files[i]) == 0:
		print "warning: '%s' file is not created" % new_names[i]
	elif files[i] != new_names[i]:
		shutil.move(files[i], new_names[i])
