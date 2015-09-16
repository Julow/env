#!/usr/bin/python
# **************************************************************************** #
#                                                                              #
#                                                         :::      ::::::::    #
#    test_gen.py                                        :+:      :+:    :+:    #
#                                                     +:+ +:+         +:+      #
#    By: juloo <juloo@student.42.fr>                +#+  +:+       +#+         #
#                                                 +#+#+#+#+#+   +#+            #
#    Created: 2015/09/15 22:41:07 by juloo             #+#    #+#              #
#    Updated: 2015/09/16 15:24:35 by jaguillo         ###   ########.fr        #
#                                                                              #
# **************************************************************************** #

from sys import argv, stdout, stdin, stderr

#

INITIAL_CODE = """#
# Generated script
#

from sys import stdout, stderr

#
#
#

# Write to the current output
def out(s):
	_output.write(s)

# Write to stderr
def err(s):
	stderr.write(s)

#
# Internal functions
#
_output = None

# Close current output
def _close_output():
	# global _output
	# if _output != None:
	# 	_output.close()
	# 	_output = None
	pass

# Change current output
def _set_output(file_name):
	global _output
	# try:
	# 	f = open(file_name, "w")
	# except:
	# 	err("Error: Cannot open %s\\n" % (file_name))
	# 	exit(1)
	# _close_output()
	# _output = f
	_output = stdout

#
#
#

"""

END_CODE = """
_close_output()
"""

COMMENT_START = "/*"
COMMENT_END = "*/"

MARKUP_END = "[end]"

#

TEXT_START = "\nout(\"\"\""
TEXT_END = "\"\"\")\n\n"

SET_OUT_CODE = "_set_output(\"%s\")\n"

ERR_OPEN = "Error: Cannot open %s\n"
ERR_NARGS = "Error: Not enougth argument\n"

#
# Generators
#

# Script generator

def _script_generator(code):
	for l in code:
		_out_code(l)

# Enum generator

def _enum_generator(code):
	_out_text("ENUM LOL\n")

#
GENERATORS = [
	{
		"markup": "[code]",
		"generator": _script_generator
	},
	{
		"markup": "[enum]",
		"generator": _enum_generator
	}
]

#
#
#

in_text = False

# Output text
def _out_text(text):
	global in_text
	if len(text) > 0:
		if not in_text:
			in_text = True
			stdout.write(TEXT_START)
		stdout.write(text.replace("\\", "\\\\"))

# Output code
def _out_code(code):
	global in_text
	if in_text:
		in_text = False
		stdout.write(TEXT_END)
	stdout.write("\033[33m") # TMP
	stdout.write(code)
	stdout.write("\033[0m") # TMP

#
def _exec_comment_util(f, (l, comment), (markup_index, markup_line), generator):
	while True:
		_out_text("\033[32m") # TMP
		for c in comment:
			_out_text(c)
		_out_text("\033[0m") # TMP
		# HERE OUTPUT CODE
		for i in range(markup_line + 1, len(comment)):
			comment[i] = comment[i][markup_index:]
		generator['generator'](comment[markup_line + 1:])
		# -
		try:
			while True:
				start_index = l.find(COMMENT_START)
				if start_index >= 0:
					l, comment = _parse_comment(f, l[start_index:])
					markup_line = 0
					for c in comment:
						if c.find(MARKUP_END) >= 0:
							return _exec_comment(f, (l, comment), markup_line)
						markup_line += 1
					break
				l = f.next()
		except:
			pass
		markup_line = -1
	return l

# Search code in a comment and exec it
# Return the string after the comments
def _exec_comment(f, (l, comment), start_line):
	for generator in GENERATORS:
		markup_line = start_line
		for i in range(start_line, len(comment)):
			markup_index = comment[i].find(generator['markup'])
			if markup_index >= 0:
				return _exec_comment_util(f, (l, comment), (markup_index, markup_line), generator)
			markup_line += 1
	_out_text("\033[31m") # TMP
	for c in comment:
		_out_text(c)
	_out_text("\033[0m") # TMP
	return l

# Parse a comment
# Return ("after comment", ["comment"])
def _parse_comment(f, l):
	comment = []
	try:
		while True:
			end_index = l.find(COMMENT_END)
			if end_index >= 0:
				c = end_index + len(COMMENT_END)
				if l[c] == '\n':
					c += 1
				comment.append(l[:c])
				return (l[c:], comment)
			comment.append(l)
			l = f.next()
	except:
		pass
	return ("", comment)

# Start parsing and generating
def start_gen(file_name):
	try:
		f = open(file_name, "r")
	except:
		exit(1)
	_out_code(SET_OUT_CODE % file_name)
	for l in f:
		start_index = l.find(COMMENT_START)
		if start_index >= 0:
			if start_index > 0:
				_out_text(l[:start_index])
			l = _exec_comment(f, _parse_comment(f, l[start_index:]), 0)
		_out_text(l)

# main

_out_code(INITIAL_CODE)

if len(argv) <= 1:
	stderr.write(ERR_NARGS)

for i in range(1, len(argv)):
	start_gen(argv[i])

_out_code(END_CODE)
stdout.flush()
