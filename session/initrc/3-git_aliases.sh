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
# gitl [...]
# (fancy git log)
#
# gitprev <file>
# (show previous version of a file (not the last version))
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
		git pull "$@"
	fi
};

function gita()
{
	git add --all "$@" && gitt.py
};

function gitu()
{
	git add -u && gitt.py
};

function gitr()
{
	git reset -- HEAD -q "$@" && gitt.py
};

alias gitt="gitt.py"

function gitd()
{
	if type diff-so-fancy >/dev/null 2>/dev/null; then
		git diff --color "$@" | diff-so-fancy | less -R --tabs=4
	else
		git diff --word-diff=porcelain --no-color "$@" | gitd.py | less -R --tabs=4
	fi
};

function gitl()
{
	git log --oneline --graph --decorate -n10 \
		--pretty=format:"%C(auto)%h %<(8,trunc)%C(cyan)%an%Creset%C(auto)%d %s %C(black bold)%ar" \
		"$@"
};

function gitprev()
{
	REV=$(git rev-list --max-count=1 --all -- "$1")
	if ! [[ -z "$REV" ]]
	then
		git show "$REV^:$1"
	fi
}
