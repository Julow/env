#!/usr/bin/env python
# **************************************************************************** #
#                                                                              #
#                                                         :::      ::::::::    #
#    gitd.py                                            :+:      :+:    :+:    #
#                                                     +:+ +:+         +:+      #
#    By: juloo <juloo@student.42.fr>                +#+  +:+       +#+         #
#                                                 +#+#+#+#+#+   +#+            #
#    Created: 2016/08/17 00:11:54 by juloo             #+#    #+#              #
#    Updated: 2016/08/17 01:49:35 by juloo            ###   ########.fr        #
#                                                                              #
# **************************************************************************** #

import sys, re

DIFF_FILE = 1 # file name
DIFF_MOVE = 2 # (line1, line2)
DIFF_LINE = 3 # string
DIFF_DIFF = 4 # list of tuple (type, str) where type is DIFF_LINE_*

DIFF_LINE_STR = 1
DIFF_LINE_ADD = 2
DIFF_LINE_DEL = 3

# Generator that yield a tuple (type, value)
#  where type is DIFF_{FILE,MOVE,LINE} and value the corresponding value
def parse_diff(i):
	current_line = None
	for l in i:

		c, l = l[0], l[1:-1]

		if c == 'd':
			assert(current_line == None)
			m = re.match('^iff --git a/(.+) b/(.+)$', l)
			assert(m != None)
			assert(m.group(1) == m.group(2))
			yield (DIFF_FILE, m.group(1))
			next(i)
			next(i)
			next(i)

		elif c == '@':
			assert(current_line == None)
			m = re.match('^@ -(\d+),\d+ \+(\d+),\d+ @@.*$', l)
			assert(m != None)
			yield (DIFF_MOVE, (int(m.group(1)), int(m.group(2))))

		elif c == ' ':
			if current_line == None: current_line = []
			current_line.append((DIFF_LINE_STR, l))

		elif c == '+':
			if current_line == None: current_line = []
			current_line.append((DIFF_LINE_ADD, l))

		elif c == '-':
			if current_line == None: current_line = []
			current_line.append((DIFF_LINE_DEL, l))

		elif c == '~':
			assert(l == "")
			if current_line == None: current_line = []
			if any(map(lambda s: s[0] != DIFF_LINE_STR, current_line)):
				yield (DIFF_DIFF, current_line)
			else:
				yield (DIFF_LINE, "".join((s[1] for s in current_line)))
			current_line = None

		else:
			assert(False)

LINE_WIDTH=80

first_file = True

for t, v in parse_diff(sys.stdin):
	if t == DIFF_FILE:
		s = "\033[97m" + "="*LINE_WIDTH + "\033[0m"
		if not first_file:
			print("\n"*4)
		else:
			first_file = False
		print(s)
		print("\033[97m||    %-*s||\033[0m" % (LINE_WIDTH - 2 - 4 - 2, v))
		print(s)
	elif t == DIFF_MOVE:
		l1, l2 = v
		s = "line %d" % l1 if l1 == l2 else "line -%d, +%d" % v
		print("\033[90m@@ %s @@\033[0m" % s)
	elif t == DIFF_LINE:
		print(v)
	elif t == DIFF_DIFF:
		def diff_str(ignore, str_color):
			if not any(map(lambda s: s[0] != ignore, v)):
				return
			print("".join(({
				DIFF_LINE_STR: lambda s: "%s%s" % (str_color, s),
				DIFF_LINE_ADD: lambda s: "\033[92m%s" % s,
				DIFF_LINE_DEL: lambda s: "\033[91m%s" % s,
			}[t](s) if t != ignore else "" for t, s in v)) + "\033[0m")
		diff_str(DIFF_LINE_DEL, "\033[32m")
		diff_str(DIFF_LINE_ADD, "\033[31m")
