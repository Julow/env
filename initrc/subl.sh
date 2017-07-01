# sublime text

SUBL="subl"

if [[ "`uname`" == "Darwin" ]]; then
	if [[ -f "/Applications/Sublime Text 3.app/Contents/SharedSupport/bin/subl" ]]; then
		SUBL="/Applications/Sublime\ Text\ 3.app/Contents/SharedSupport/bin/subl"
	elif [[ -f "/Applications/Sublime Text.app/Contents/SharedSupport/bin/subl" ]]; then
		SUBL="/Applications/Sublime\ Text\.app/Contents/SharedSupport/bin/subl"
	elif [[ -f "/Applications/Sublime Text 2.app/Contents/SharedSupport/bin/subl" ]]; then
		SUBL="/Applications/Sublime\ Text\ 2.app/Contents/SharedSupport/bin/subl"
	fi
fi

echo 'export SUBL="'"$SUBL"'"

alias s="$SUBL -sa"

export EDITOR="$SUBL -w"
'
