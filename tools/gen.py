#!/usr/bin/python

initialCode = """
from sys import stdout, stderr

def out(s):
	stdout.write(s)

def debug(s):
	stderr.write(s)

"""

endCode = """
stdout.flush()
"""

startFlag = "?start"
endFlag = "?end"

startComment = "/*"
endComment = "*/"

#

from sys import argv, stdout, stdin, stderr

last_output = ""
last_output_count = 0

last_start_offset = -1

def output_text(s):
	global last_output
	global last_output_count
	if len(s) > 0:
		last_output += s
		last_output_count += 1

def output_code(s):
	global last_output
	global last_output_count
	if last_output_count > 0:
		stdout.write("out(\"\"\"%s\"\"\")\n" % last_output.replace("\\", "\\\\"))
		last_output = ""
		last_output_count = 0
	if len(s) > 0:
		stdout.write(s)

def parse_comment(s):
	global last_start_offset
	code = ""
	output_text(s + '\n')
	for l in s.split("\n"):
		if last_start_offset < 0:
			last_start_offset = l.find(startFlag)
			if last_start_offset >= 0:
				l = l[len(startFlag):].lstrip()
		if last_start_offset >= 0:
			c = l[last_start_offset:]
			if c.startswith(endFlag):
				last_start_offset = -1
				continue
			code += c + "\n"
	if len(code) > 0:
		output_code(code)

# main

if len(argv) <= 1 or argv[1] == "-":
	f = stdin
else:
	try:
		f = open(argv[1], "r")
	except:
		print("#error Cannot open %s" % argv[1])
		exit(1)

stdout.write(initialCode)

commentIn = False
currentCode = ""
for line in f:
	startIndex = 0 if commentIn else line.find(startComment)
	if startIndex >= 0:
		commentIn = True
	if commentIn:
		endIndex = line.find(endComment) if len(endComment) > 0 else len(line)
		if endIndex < 0:
			endIndex = len(line)
		else:
			endIndex += len(endComment)
			commentIn = False
		if startIndex > 0:
			output_text(line[0:startIndex])
		if endIndex > startIndex:
			currentCode += line[startIndex:endIndex]
		if not commentIn and len(currentCode) > 0:
			parse_comment(currentCode)
			currentCode = ""
		output_text(line[endIndex:len(line)-1])
	else:
		if last_start_offset < 0:
			output_text(line)

output_code(endCode)

f.close()
stdout.flush()
