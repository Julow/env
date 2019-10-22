# Git utils

available_dir ()
{
	local i=1 r="$1"
	while [[ -e "$r" ]]; do r="$1-$((i++))"; done
	echo "$r"
}

# Create a new worktree for the current git repository and cd to it
gitw ()
{
	if [[ $# -eq 0 ]]; then set HEAD; fi
	local dst="`available_dir "$PWD"`"
	git worktree add "$dst" "$@" && cd "$dst"
}
