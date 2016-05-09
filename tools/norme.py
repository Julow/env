#!/usr/bin/python

import sys, re, subprocess, os

# Generate a tuple (file_name, [(line, col, error)], [warnings])
def iter_files(f_in):
	last = None
	for line in f_in:
		m = re.match("(Norme|Warning|Error)(?: \(line (\d+)(?:, col (\d+))?\))?: (.+)\n", line)
		assert(m != None)
		if m.group(1) == "Norme":
			if last != None:
				yield last
			last = (m.group(4), [], [])
		elif m.group(1).startswith("Error"):
			last[1].append((
					int(m.group(2)) if m.group(2) != None else -1,
					int(m.group(3)) if m.group(3) != None else -1,
					m.group(4)
				))
		elif m.group(1) == "Warning":
			if m.group(4) != "Not a valid file":
				last[2].append(m.group(4))
	yield last

def norme(f_in):
	for f_name, errors, warns in iter_files(f_in):
		if len(errors) == 0 and len(warns) == 0:
			continue
		print("%s:" % os.path.relpath(f_name))
		for w in warns:
			print("\tW: %s" % w)
		for line, col, e in errors:
			print("\t%s%s" % (
					e,
					(" (line %d%s)" % (
							line,
							(":%d" % col) if col >= 0 else ""
						)) if line >= 0 else ""
				))

if len(sys.argv) <= 1:
	for line in [
			("norme.py -", "Beautify the norminette's output (from stdin)"),
			("norme.py file ...", "Run the norminette and beautify it's output")
		]:
		print("%-24s %s" % line)
elif sys.argv[1] == "-":
	norme(sys.stdin)
else:
	p = subprocess.Popen(["norminette"] + sys.argv[1:], stdout=subprocess.PIPE)
	norme(p.stdout)
	p.wait()
