#!/usr/bin/env python3

import subprocess
from dateutil import parser as date_parser

FORMAT = "%(HEAD)\t%(refname:short)\t%(upstream:track,nobracket)\t%(upstream:short)\t%(authordate)\t%(subject)"

RED = "\033[31m"
GREEN = "\033[32m"
CYAN = "\033[36m"
GREY = "\033[30;1m"
RESET = "\033[0m"

class branch:

    def __init__(self, line):
        self.head, self.refname, self.track, self.upstream, date, subject = line.decode("utf-8").split("\t", 5)
        self.date = date_parser.parse(date)
        self.subject = subject.rstrip()

    def upstream_str(self):
        if self.upstream == "":
            return ""
        upname, upref = self.upstream.split("/", 1)
        # Only show the upstream name if the ref part is the same as local
        upstream_s = upname if upref == self.refname else self.upstream
        # Ahead/behind
        if self.track != "": upstream_s = f"[{CYAN}{self.track}{RESET}] {RED}{upstream_s}{RESET}"
        return upstream_s

    def pretty_print(self, refname_padding):
        ref_color = GREEN if b.head == '*' else RESET
        return f"{b.head} {ref_color}{b.refname:{max_refname_len}} {GREY}{b.date}{RESET} {b.subject} {b.upstream_str()}"

def git_branch():
    cmd = [ "git", "branch", "--format=" + FORMAT ]
    p = subprocess.Popen(cmd, stdout=subprocess.PIPE)
    branches = list(map(branch, p.stdout))
    if p.wait() != 0:
        exit(p.returncode)
    return branches

branches = sorted(git_branch(), key=lambda b: b.date)
max_refname_len = max(map(lambda b: len(b.refname), branches))
for b in branches:
    print(b.pretty_print(refname_padding=max_refname_len))
