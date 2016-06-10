#
# Git Aliases
#
# Somes aliases for git
#
# gitt
# (Print the status in small format)
#
# gitc <msg> [...]
# (git commit)
#
# gitp [remote] [branch]
# (git push)
#
# gitp [remote] [branch]
# (git pull)
#
# gitamend
# (amend last commit without editing the commit message)
#
# gita [file...]
# (git add -A) + (Print the status)
#
# gitr <file1> [file2...]
# (Cancel (git add) for a file) + (Print the status)
#

alias gitamend="git commit --amend --no-edit"

function gitc()
{
	STR="$@";
	git commit -m "$STR"
};

function gitp()
{
	if [ "$#" -eq "0" ]; then
		git push origin : --tags
	elif [ "$#" -eq "1" ]; then
		git push "$1" : --tags
	else
		git push --tags $@
	fi
};

function gitpl()
{
	if [ "$#" -eq "0" ]; then
		git pull origin "`git rev-parse --abbrev-ref HEAD`" --tags
	elif [ "$#" -eq "1" ]; then
		git pull "$1" "`git rev-parse --abbrev-ref HEAD`" --tags
	else
		git pull $@
	fi
};

function gita()
{
	git add --all $@ && gitt
};

function gitu()
{
	git add -u && gitt
};

function gitr()
{
	git reset -- HEAD -q $@ && gitt
};

function gitt()
{
	python - "$(git diff --numstat HEAD)" "$(git status -sb --porcelain)" <<"GITT_SCRIPT_END"
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
GITT_SCRIPT_END
};
