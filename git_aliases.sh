#
# Git Aliases
#
# Somes aliases for git
#
# t
# (Print the status in small format)
#
# c <msg> [args...]
# (git commit)
#
# p [remote] [branch]
# (git push)
#
# amend
# (amend last commit without editing the commit message)
#
# a [file...]
# (git add -A) + (Print the status)
#
# r <file1> [file2...]
# (Cancel (git add) for a file) + (Print the status)
#
alias t="git status -sb"
alias c="git commit -m"
alias p="git push origin master --tags"
alias amend="git commit --amend --no-edit"

function a()
{
	git add --all $@ && git status -bs
};

function r()
{
	git reset -- HEAD -q $@ && git status -bs
};
