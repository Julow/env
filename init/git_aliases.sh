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
# p [remote] [branch]
# (git pull)
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
alias amend="git commit --amend --no-edit"

function p()
{
	if [ "$#" -eq "0" ]; then
		git push origin : --tags
	elif [ "$#" -eq "1" ]; then
		git push "$1" : --tags
	else
		git push --tags $@
	fi
};

function pl()
{
	if [ "$#" -eq "0" ]; then
		git pull origin "`git rev-parse --abbrev-ref HEAD`" --tags
	elif [ "$#" -eq "1" ]; then
		git pull "$1" "`git rev-parse --abbrev-ref HEAD`" --tags
	else
		git pull $@
	fi
};

function a()
{
	git add --all $@ && git status -bs
};

function r()
{
	git reset -- HEAD -q $@ && git status -bs
};
