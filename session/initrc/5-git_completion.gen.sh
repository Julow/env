COMP_FILE=.git_completion.bash
COMP_FILE_URL=https://raw.githubusercontent.com/git/git/master/contrib/completion/git-completion.bash

if ! [[ -e $COMP_FILE ]]
then
	curl -Lo "$COMP_FILE" "$COMP_FILE_URL"
fi

cat "$COMP_FILE"
