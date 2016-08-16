#!/usr/bin/python
# **************************************************************************** #
#                                                                              #
#                                                         :::      ::::::::    #
#    gitt.py                                            :+:      :+:    :+:    #
#                                                     +:+ +:+         +:+      #
#    By: juloo <juloo@student.42.fr>                +#+  +:+       +#+         #
#                                                 +#+#+#+#+#+   +#+            #
#    Created: 2016/08/17 01:44:55 by juloo             #+#    #+#              #
#    Updated: 2016/08/17 01:45:00 by juloo            ###   ########.fr        #
#                                                                              #
# **************************************************************************** #

import re, sys

stats = {}

max_name_len = 25
total_add = 0
total_del = 0

for line in sys.argv[1].split("\n"):
	m = re.match("^\s*(\d+)\s+(\d+)\s+(.+)$", line)
	if m != None:
		add = int(m.group(1))
		rem = int(m.group(2))
		name = m.group(3)
		if len(name) > max_name_len:
			max_name_len = len(name)
		total_add += add
		total_del += rem
		stats[name] = (add, rem)

total_file = 0
total_untrack = 0

for line in sys.argv[2].split("\n"):
	if line.startswith("##"):
		print ("\033[97m##\033[0m %s" % line[3:])
	elif len(line) > 0:
		m = re.match("^(.)(.)\s+(.+)$", line)
		if m == None:
			print (line)
		else:
			if m.group(1) == "?":
				status = "\033[31m??\033[0m"
			else:
				status = "\033[32m%s\033[31m%s\033[0m" % (m.group(1), m.group(2))
			name = m.group(3)
			if name[0] == "\"":
				name = name[1:-1]
			if name in stats:
				add, rem = stats[name]
				print ("%s %-*s | \033[32m%2d+ \033[31m%2d-\033[0m" % (
					status,
					max_name_len, name,
					add, rem
				))
				total_file += 1
			else:
				print ("%s \033[90m%s\033[0m" % (status, name))
				total_untrack += 1

total_str = "%d files" % total_file
if total_untrack > 0:
	total_str += " \033[90m+ %d untracked\033[0m" % total_untrack
	max_name_len += len("\033[90m\033[0m")

if total_file > 0:
	print ("\033[97m##\033[0m %-*s | \033[32m%2d+ \033[31m%2d-\033[0m" % (max_name_len, total_str, total_add, total_del))
else:
	print ("\033[97m##\033[0m %s" % total_str)
