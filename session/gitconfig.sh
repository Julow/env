# Git aliases and config

s() { git config --global "$@"; }

# Aliases

c() { s "alias.$1" "$2"; }

c k "checkout"
c b "branch -avv --sort=-refname"
c d "diff --stat --summary -p"
c ds "diff --stat --summary -p --staged"
c customlog "log --decorate --graph --format=custom"
c ll "customlog --all"
c l '!f() { : git log; if [ $# -eq 0 ]; then git --no-pager customlog -n15; else git customlog "$@"; fi; }; f'
c t "status --short -b -u"
c sh "show --summary --stat -p"
c a '!f() { : git add --all; git add --all "$@" && git t; }; f'
c u '!f() { : git add -u; git add -u "$@" && git t; }; f'
c r '!f() { : git reset; git reset -- HEAD -q "$@" && git t; }; f'
c c '!f() { if [[ $# -eq 0 ]]; then git commit -v; else git commit -m "$*"; fi; }; f'
c p "push"
c pu '!f() { : git push; git push -u "${1:-origin}" HEAD; }; f'
c logrf '!f () { BEFORE=`git rev-parse HEAD`; "$@" && git l "$BEFORE..HEAD"; }; f'
c rf '!f () { : git merge --ff-only; git logrf git merge --ff-only -v --stat "$@"; }; f'
c ft "fetch --all -t --progress -v"
c pl '!f () { : git pull; git logrf git pull --stat -v --ff-only "$@"; }; f'
c amend "commit --amend --no-edit"
c cln '!f() { : git clean; git clean -dn "$@";
	echo "y = yes, i = interactive"; read -N1 -s confirm; case "$confirm" in
		"y") git clean -df "$@";;
		"i") git clean -di "$@";;
		*) echo "Nothing done";; esac; }; f'
# Fetch a github pull request
c kpr '!f() {
	git checkout --detach HEAD;
	git fetch -f up "pull/$1/head:pr/$1" &&
	git checkout "pr/$1" && git l; }; f'

# Log format
s "pretty.custom" "tformat:%C(auto)%h %<(12,trunc)%C(cyan)%an%C(reset)%C(black bold)%>(13)%ar%C(auto)%d %s"

# Rebase
s "rebase.instructionFormat" "tformat:%<(8,trunc)%an%d %s # %ar"
s "rebase.stat" true

# Paging

GIT_PAGER="less"

DH_PATH=/usr/share/git/diff-highlight/diff-highlight

if [[ -e $DH_PATH ]]; then
	s --remove-section "color.diff-highlight"
	dh() { s "color.diff-highlight.$1" "$2"; }
	dh "oldNormal" "dim red"
	dh "oldHighlight" "nodim red"
	dh "newNormal" "dim green"
	dh "newHighlight" "nodim green"
	GIT_PAGER="$DH_PATH | less"
fi

s "pager.diff" "$GIT_PAGER"
s "pager.show" "$GIT_PAGER"
s "pager.status" "$GIT_PAGER"

# Global ignore

IGNORE_FILE="$HOME/.gitignore_global"

git config --global "core.excludesfile" "$IGNORE_FILE"

cat > "$IGNORE_FILE" <<"EOF"
*.swp
.vimrc
EOF
