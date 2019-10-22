GIT_COMPLETION=/usr/share/git/completion/git-completion.bash

if [[ ! -e $GIT_COMPLETION ]]; then
	exit
fi

cat "$GIT_COMPLETION"

echo '_git_l () { _git_log; }'
