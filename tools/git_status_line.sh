#!/usr/bin/env bash

git status --short --branch --untracked-files --ahead-behind --no-renames | {

	read line

	# Header line
	if ! [[ $line =~ ^\#\#\ ([^\ .]+)(\.\.\.([^\ ]+))?(\ \[(ahead\ ([0-9]+))?(,\ )?(behind\ ([0-9]+))?\])? ]]
	then
		echo "Failed to parse the header" >&2
		exit 1
	fi

	BRANCH=${BASH_REMATCH[1]}
	REMOTE=${BASH_REMATCH[3]}
	AHEAD=${BASH_REMATCH[6]}
	BEHIND=${BASH_REMATCH[9]}

	# Status
	DIRTY=0
	STAGED=0
	UNTRACKED=0
	CONFLICT=0
	while IFS=\n read line; do
		if [[ $line = [^\ ?]* ]]; then STAGED=1; fi
		if [[ $line = ?[^\ ?]* ]]; then DIRTY=1; fi
		if [[ $line = \?\?* ]]; then UNTRACKED=1; fi
		if [[ $line = UU* ]]; then CONFLICT=1; fi
	done

	ESC=`printf '\033'`
	STATUS=""

	# Branch info
	# Remote branch hidden if same as local, then remote hidden if origin
	S_REMOTE=${REMOTE%origin/$BRANCH}
	S_REMOTE=${S_REMOTE%/$BRANCH}
	if [[ -n $S_REMOTE ]]; then S_REMOTE="...$S_REMOTE"; fi
	S_BRANCH="$ESC[35m($BRANCH$S_REMOTE)"

	# Status
	S_STAGED=""
	if [[ $STAGED -gt 0 ]]; then S_STAGED="$ESC[32m*"; fi
	S_DIRTY=""
	if [[ $DIRTY -gt 0 ]]; then S_DIRTY="$ESC[31m*"; fi
	S_UNTRACKED=""
	if [[ $UNTRACKED -gt 0 ]]; then S_UNTRACKED="$ESC[90m*"; fi
	S_CONFLICT=""
	if [[ $CONFLICT -gt 0 ]]; then S_CONFLICT="$ESC[31m!!"; fi

	# Tracking
	S_AHEAD=""
	if [[ -n $AHEAD ]]; then S_AHEAD="$ESC[32m+$AHEAD"; fi
	S_BEHIND=""
	if [[ -n $BEHIND ]]; then S_BEHIND="$ESC[31m-$BEHIND"; fi

	# Rebase and merge
	S_STATE=""
	if [[ -f .git/REBASE_HEAD ]]; then S_STATE="$ESC[33mRebase in progress";
	elif [[ -f .git/MERGE_HEAD ]]; then S_STATE="$ESC[33mMerge in progress"; fi

	echo $S_BRANCH $S_STATE $S_CONFLICT$S_STAGED$S_DIRTY$S_UNTRACKED $S_AHEAD $S_BEHIND "$ESC[0m"
}
