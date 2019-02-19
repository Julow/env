# Git aliases and config

s() { git config --global "$@"; }

# Aliases

c() { s "alias.$1" "$2"; }

c k "checkout"
c b "branch -avv --sort=-refname"
c d "diff"
c ds "diff --staged"
c l "log --oneline --graph --decorate -n10 --pretty='format:%C(auto)%h %<(8,trunc)%C(cyan)%an%Creset%C(auto)%d %s %C(black bold)%ar'"
c t "status --short -b -u"
c a '!f() { : git add --all; git add --all "$@" && git t; }; f'
c u '!f() { : git add -u; git add -u "$@" && git t; }; f'
c r '!f() { : git reset; git reset -- HEAD -q "$@" && git t; }; f'
c c "commit -m"
c p "push origin HEAD"
c rf '!f () { : git merge --ff-only;
	BEFORE=`git rev-parse HEAD`;
	git merge --ff-only -v --stat "$@" &&
	git l "$BEFORE..HEAD"; }; f'
c pl '!f () { : git merge --ff-only; git fetch --all -t --progress -v &&
	git rf "$@"; }; f'
c amend "commit --amend --no-edit"
c cln '!f() { : git clean; git clean -dn "$@";
	echo "y = yes, i = interactive"; read -N1 -s confirm; case "$confirm" in
		"y") git clean -df "$@";;
		"i") git clean -di "$@";;
		*) echo "Nothing done";; esac; }; f'
# Fetch a github pull request
c kpr '!f() { git fetch up "pull/$1/head:#$1" && git checkout "#$1" && git l; }; f'

# diff-highlight

DH_PATH=/usr/share/git/diff-highlight/diff-highlight

if [[ -e $DH_PATH ]]; then

	s --remove-section "color.diff-highlight"

	dh() { s "color.diff-highlight.$1" "$2"; }

	dh "oldNormal" "dim red"
	dh "oldHighlight" "nodim red"
	dh "newNormal" "dim green"
	dh "newHighlight" "nodim green"

	s "pager.diff" "$DH_PATH | less"
	s "pager.show" "$DH_PATH | less"
fi

# Global ignore

IGNORE_FILE="$HOME/.gitignore_global"

git config --global "core.excludesfile" "$IGNORE_FILE"

cat > "$IGNORE_FILE" <<"EOF"
*.swp
EOF
