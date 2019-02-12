# Git aliases and config

# Aliases

c() { git config --global "alias.$1" "$2"; }

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

# Global ignore

IGNORE_FILE="$HOME/.gitignore_global"

git config --global "core.excludesfile" "$IGNORE_FILE"

cat > "$IGNORE_FILE" <<"EOF"
*.swp
EOF
