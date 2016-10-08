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
	OPTIONS=("--tags")
	while [[ "$1" = "-"* ]]; do
		OPTIONS=($OPTIONS "$1")
		shift
	done
	if [ "$#" -eq "0" ]; then
		git push $OPTIONS origin HEAD
	elif [ "$#" -eq "1" ]; then
		git push $OPTIONS "$1" HEAD
	else
		git push $OPTIONS "$@"
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
	gitt.py "$(git diff --numstat HEAD)" "$(git status -sb --porcelain)"
};

function gitd()
{
	if type diff-so-fancy >/dev/null 2>/dev/null; then
		git diff --color "$@" | diff-so-fancy | less -R --tabs=4
	else
		git diff --word-diff=porcelain --no-color "$@" | gitd.py | less -R --tabs=4
	fi
};
