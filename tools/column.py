#!/usr/bin/python
# **************************************************************************** #
#                                                                              #
#                                                         :::      ::::::::    #
#    column.py                                          :+:      :+:    :+:    #
#                                                     +:+ +:+         +:+      #
#    By: jaguillo <jaguillo@student.42.fr>          +#+  +:+       +#+         #
#                                                 +#+#+#+#+#+   +#+            #
#    Created: 2015/10/14 09:21:51 by jaguillo          #+#    #+#              #
#    Updated: 2015/10/14 11:39:04 by jaguillo         ###   ########.fr        #
#                                                                              #
# **************************************************************************** #

from sys import argv
import os, mimetypes

MAX_COLUMN = 80

EXCLUDE_DIRS = [".git"]
EXCLUDE_FILES = [".o", ".a", ".tar", ".gz"]

def is_text_file(file_name):
	try:
		with open(file_name, "r") as f:
			for line in f:
				line.decode('UTF-8')
	except:
		return False
	return True

start = 1

if len(argv) > start and argv[start].isdigit():
	MAX_COLUMN = int(argv[start])
	start += 1

if len(argv) <= start:
	print "Not enougth arguments"
	exit(1)

last_error = -1

max_file_len = 0
files = []
for file_name in argv[start:]:
	if os.path.isdir(file_name):
		for curr_dir, dirs, ls in os.walk(file_name):
			if os.path.basename(curr_dir) in EXCLUDE_DIRS:
				del dirs[:]
			else:
				for file_name in ls:
					for ex in EXCLUDE_FILES:
						if file_name.endswith(ex):
							file_name = None
							break
					if file_name == None:
						continue
					file_name = os.path.join(curr_dir, file_name)
					if is_text_file(file_name):
						files.append(os.path.relpath(file_name))
	else:
		files.append(file_name)

for file_name in files:
	if len(file_name) > max_file_len:
		max_file_len = len(file_name)

for file_name in files:
	with open(file_name, "r") as f:
		line_n = 1
		if last_error > 0:
			tmp = max_file_len + 1 + 3 + 1 + 3 - 1
			while tmp > 0:
				print "-",
				tmp -= 2
			print
		last_error = -1
		for line in f:
			line = line.expandtabs(4)[:-1]
			line_len = len(line)
			if line_len > MAX_COLUMN or line.find('\r') >= 0:
				if last_error + 1 < line_n and last_error > 0:
					print "%*s..." % (max_file_len + 1 + 3 + 1 + 3, ' ')
				print "\033[36m%-*s\033[39m %3d\033[43m:\033[49m %s\033[41m%s\033[49m" % (
					max_file_len + 1 + 3,
					"%s:%d" % (file_name, line_n),
					line_len,
					line[:MAX_COLUMN].replace('\r', '\033[41m^M\033[49m'),
					line[MAX_COLUMN:].replace('\r', '^M')
				)
				last_error = line_n
			line_n += 1
		if last_error < 0:
			print "\033[92m%s\033[39m" % file_name
