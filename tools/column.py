#!/usr/bin/python
# **************************************************************************** #
#                                                                              #
#                                                         :::      ::::::::    #
#    column.py                                          :+:      :+:    :+:    #
#                                                     +:+ +:+         +:+      #
#    By: jaguillo <jaguillo@student.42.fr>          +#+  +:+       +#+         #
#                                                 +#+#+#+#+#+   +#+            #
#    Created: 2015/10/14 09:21:51 by jaguillo          #+#    #+#              #
#    Updated: 2015/10/14 10:36:47 by jaguillo         ###   ########.fr        #
#                                                                              #
# **************************************************************************** #

from sys import argv

MAX_COLUMN = 80

start = 1

if len(argv) > start and argv[start].isdigit():
	MAX_COLUMN = int(argv[start])
	start += 1

if len(argv) <= start:
	print "Not enougth arguments"
	exit(1)

ok = True
last_error = -1

max_file_len = 0
for file_name in argv[start:]:
	if len(file_name) > max_file_len:
		max_file_len = len(file_name)

for file_name in argv[start:]:
	with open(file_name, "r") as f:
		line_n = 1
		if last_error > 0:
			print "%*s- - - - -" % (max_file_len, ' ')
		last_error = -1
		for line in f:
			line = line.expandtabs(4)[:-1]
			line_len = len(line)
			if line_len > MAX_COLUMN or line.find('\r') >= 0:
				if last_error + 1 < line_n and last_error > 0:
					print "%*s..." % (max_file_len, ' ')
				print "%*s:%-3d %3d: %s\033[41m%s\033[49m" % (
					max_file_len,
					file_name,
					line_n,
					line_len,
					line[:MAX_COLUMN].replace('\r', '\033[41m^M\033[49m'),
					line[MAX_COLUMN:].replace('\r', '^M')
				)
				last_error = line_n
				ok = False
			line_n += 1

if ok:
	print "OK"
