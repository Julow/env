#!/usr/bin/env bash

ESC=`printf '\033'`
C_RED=${C_RED:-"$ESC[31m"}
C_GREEN=${C_GREEN:-"$ESC[32m"}
C_YELLOW=${C_YELLOW:-"$ESC[33m"}
C_MAGENTA=${C_MAGENTA:-"$ESC[35m"}
C_GRAY=${C_GRAY:-"$ESC[37m"}
C_RESET=${C_RESET:-"$ESC[0m"}

git status --short --branch --untracked-files --ahead-behind 2>/dev/null | {

	if ! read line; then exit 1; fi

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

	STATUS=""

	# Branch info
	# Remote branch hidden if same as local, then remote hidden if origin
	S_REMOTE=${REMOTE%origin/$BRANCH}
	S_REMOTE=${S_REMOTE%/$BRANCH}
	if [[ -n $S_REMOTE ]]; then S_REMOTE="...$S_REMOTE"; fi
	S_BRANCH="$C_MAGENTA($BRANCH$S_REMOTE)"

	# Status
	S_STAGED=""
	if [[ $STAGED -gt 0 ]]; then S_STAGED="$C_GREEN*"; fi
	S_DIRTY=""
	if [[ $DIRTY -gt 0 ]]; then S_DIRTY="$C_RED*"; fi
	S_UNTRACKED=""
	if [[ $UNTRACKED -gt 0 ]]; then S_UNTRACKED="$C_GRAY*"; fi
	S_CONFLICT=""
	if [[ $CONFLICT -gt 0 ]]; then S_CONFLICT="$C_RED!!"; fi

	# Tracking
	S_AHEAD=""
	if [[ -n $AHEAD ]]; then S_AHEAD="$C_GREEN+$AHEAD"; fi
	S_BEHIND=""
	if [[ -n $BEHIND ]]; then S_BEHIND="$C_RED-$BEHIND"; fi

	# Rebase and merge
	S_STATE=""
	if [[ -f .git/REBASE_HEAD ]]; then S_STATE="${C_YELLOW}Rebase in progress";
	elif [[ -f .git/MERGE_HEAD ]]; then S_STATE="${C_YELLOW}Merge in progress";
	elif [[ -f .git/BISECT_START ]]; then S_STATE="${C_YELLOW}Bisect in progress";
	elif [[ -f .git/CHERRY_PICK_HEAD ]]; then S_STATE="${C_YELLOW}Cherry-pick in progress"; fi

	echo $S_BRANCH $S_STATE $S_CONFLICT$S_STAGED$S_DIRTY$S_UNTRACKED $S_AHEAD $S_BEHIND $C_RESET
}
