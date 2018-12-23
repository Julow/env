# Git aliases and config

# Use git cli instead of overwritting the .gitconfig file
c() { git config --global "alias.$1" "$2"; }

c k "checkout"
c b "checkout -b"
c d "diff"
c ds "diff --staged"
c l "log --oneline --graph --decorate -n10 --pretty='format:%C(auto)%h %<(8,trunc)%C(cyan)%an%Creset%C(auto)%d %s %C(black bold)%ar'"
c t "status --short -b -u"
c a '!f() { : git add --all; git add --all "$@" && git t; }; f'
c u '!f() { : git add -u; git add -u "$@" && git t; }; f'
c r '!f() { : git reset; git reset -- HEAD -q "$@" && git t; }; f'
c c "commit -m"
c p "push origin HEAD"
c pl "pull origin HEAD"
c amend "commit --amend --no-edit"
