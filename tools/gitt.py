#!/usr/bin/python
# **************************************************************************** #
#                                                                              #
#                                                         :::      ::::::::    #
#    gitt.py                                            :+:      :+:    :+:    #
#                                                     +:+ +:+         +:+      #
#    By: juloo <juloo@student.42.fr>                +#+  +:+       +#+         #
#                                                 +#+#+#+#+#+   +#+            #
#    Created: 2016/08/17 01:44:55 by juloo             #+#    #+#              #
#    Updated: 2017/01/09 21:33:39 by jaguillo         ###   ########.fr        #
#                                                                              #
# **************************************************************************** #

import re, sys, subprocess, sys

# git diff --numstat
# if 'cached' is true, add the '--cached' option
# :{ file_name => (add, del) }
# :{ file_name => None } if binary file
def git_stats(cached):
	stats = {}
	cmd = "git diff --numstat" + (" --cached" if cached else "")
	p = subprocess.Popen(cmd.split(), stdout=subprocess.PIPE)
	for line in p.stdout:
		a, b, file_name = line.split()
		stats[file_name] = (int(a), int(b)) if a != "-" else None
	if p.wait() != 0:
		raise Exception
	return stats

# git status -b --porcelain -u
# :branch_name, { file_name => status }
def git_status():
	status = {}
	branch = None
	cmd = "git status -bu --porcelain"
	p = subprocess.Popen(cmd.split(), stdout=subprocess.PIPE)
	branch = next(p.stdout).split()[1]
	for line in p.stdout:
		status[line[3:-1]] = line[:2]
	if p.wait() != 0:
		raise Exception
	return (branch, status)

#

STAGED_COLORS = ("\033[92m", "\033[91m")
UNSTAGED_COLORS = ("\033[32m", "\033[31m")

unstaged_stats, staged_stats = git_stats(False), git_stats(True)
branch, status = git_status()

def sorted_status(k):
	def s(s): return (sum(s[k]) if s[k] != None else -1) if k in s else 0
	return (s(staged_stats), s(unstaged_stats))

def stat_str(stats, colors):
	a, b = stats
	if (a + b) > 0:
		return " | %4s %4s" % (
			("%s%3d+\033[0m" % (colors[0], a) if a > 0 else ""),
			("%s%3d-\033[0m" % (colors[1], b) if b > 0 else "")
		)
	return ""

def stat_sum(stats):
	count, sum_a, sum_b = 0, 0, 0
	for f in status.keys():
		if stats.get(f) != None:
			sum_a += stats[f][0]
			sum_b += stats[f][1]
			count += 1
	return (count, sum_a, sum_b)

def count_untracked():
	untracked = 0
	for f in status.keys():
		if f not in unstaged_stats and f not in staged_stats:
			untracked += 1
	return untracked

max_file_name_len = max(36, max(map(len, status.keys())))

sys.stdout.write("\033[97m##\033[0m %s\n" % branch)

for file_name in sorted(status.keys(), key=sorted_status, reverse=True):
	s = status[file_name]
	if s == "??": s = "  "
	sys.stdout.write("\033[92m%c\033[31m%c\033[0m " % (s[0], s[1]))
	if file_name not in staged_stats and file_name not in unstaged_stats:
		sys.stdout.write("\033[90m%s\033[0m" % file_name)
	else:
		sys.stdout.write("%-*s" % (max_file_name_len, file_name))
		if file_name in staged_stats:
			sys.stdout.write(stat_str(staged_stats[file_name] or (0, 0), STAGED_COLORS))
		if file_name in unstaged_stats:
			sys.stdout.write(stat_str(unstaged_stats[file_name] or (0, 0), UNSTAGED_COLORS))
	sys.stdout.write("\n")

staged_count, staged_a, staged_b = stat_sum(staged_stats)
unstaged_count, unstaged_a, unstaged_b = stat_sum(unstaged_stats)
untracked_count = count_untracked()
sys.stdout.write("\033[97m##\033[0m %-*s%s%s\n" % (
	max_file_name_len,
	", ".join(filter(len, [
		"%d staged" % staged_count if staged_count > 0 else "",
		"%d unstaged" % unstaged_count if unstaged_count > 0 else "",
		"%d untracked" % untracked_count if untracked_count > 0 else "",
	])),
	stat_str((staged_a, staged_b), STAGED_COLORS),
	stat_str((unstaged_a, unstaged_b), UNSTAGED_COLORS),
))
