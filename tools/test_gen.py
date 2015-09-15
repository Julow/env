#!/usr/bin/python
# **************************************************************************** #
#                                                                              #
#                                                         :::      ::::::::    #
#    test_gen.py                                        :+:      :+:    :+:    #
#                                                     +:+ +:+         +:+      #
#    By: juloo <juloo@student.42.fr>                +#+  +:+       +#+         #
#                                                 +#+#+#+#+#+   +#+            #
#    Created: 2015/09/15 22:41:07 by juloo             #+#    #+#              #
#    Updated: 2015/09/15 23:33:09 by juloo            ###   ########.fr        #
#                                                                              #
# **************************************************************************** #

from sys import argv, stdout, stdin, stderr

#

INITIAL_CODE = """
from sys import stdout, stderr

def out(s):
	stdout.write(s)

def err(s):
	stderr.write(s)

"""

END_CODE = """
stdout.flush()
"""

COMMENT_START = "//"
COMMENT_END = "\n"

#

TEXT_START = "out(\"\"\""
TEXT_END = "\"\"\");\n"

ERR_OPEN = "Error: Cannot open %s\n"

#

class Generator():

	out = None
	in_text = False

	def __init__(self, out):
		self.out = out;

	# Output text
	def _out_text(self, text):
		if len(text) > 0:
			if not self.in_text:
				self.in_text = True
				self.out.write(TEXT_START)
			self.out.write(text.replace("\\", "\\\\"))

	# Output code
	def _out_code(self, code):
		if self.in_text:
			self.in_text = False
			self.out.write(TEXT_END)
		self.out.write(code)

	# Parse a comment
	def _parse_comment(self, inp, l):
		comment = ""
		try:
			while True:
				end_index = l.find(COMMENT_END)
				if end_index >= 0:
					comment += l[:end_index + len(COMMENT_END)]
					# WHAT TO DO WITH THE COMMENT
					self._out_text("\033[32m%s\033[0m" % comment)
					#
					return l[end_index + len(COMMENT_END):]
				comment += l
				l = inp.next()
		except:
			pass
		return ""

	# Start parsing and generating
	def start(self, inp):
		self._out_code(INITIAL_CODE)
		for l in inp:
			start_index = l.find(COMMENT_START)
			if start_index >= 0:
				if start_index > 0:
					self._out_text(l[:start_index])
				l = self._parse_comment(inp, l[start_index:])
			self._out_text(l)
		self._out_code(END_CODE)
		self.out.flush()

# main

gen = Generator(stdout)

if len(argv) == 1:
	gen.start(stdin)

for i in range(1, len(argv)):
	try:
		with open(argv[i], "r") as f:
			gen.start(f)
	except Exception as e:
		stderr.write(ERR_OPEN % argv[i])
		exit(1)
