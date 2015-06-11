#
# .juloorc
#
# https://github.com/Julow/My-Shell-Rc
#

export PATH="$HOME/.brew/bin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin"
export LD_LIBRARY_PATH="$HOME/.brew/lib"
export PS1="$SHLVL> "

#
# Reload .juloorc
#
alias rc="source $HOME/.juloorc"

#
# L
#
# Run ls -lAFh with a better output
# (ll is an alias for l)
#
# l [ls args...]
#
function l()
{
	ls -lAbFhgo $@ | sed -E "s/([^ ]+)( +)([^ ]+)( +)([^ ]+)( +[^ ]+ +[^ ]+ +[^ ]+) (.+)/[\1] `printf "\033[1;30m"`\6  `printf "\033[0;36m"`(\5 +\3)`printf "\033[0m"` \4\2\7/" | sed "s/ +1)/)   /"
};

alias ll="l"

#
# Sublime Text
#
# Open files/dirs in Sublime Text
# Open a new Sublime Text window if any
#
# s [file...]
#
# (Already exists on ubuntu: subl)
#
if [[ "`uname`" == "Darwin" ]]; then
	if [[ -f "/Applications/Sublime Text 2.app/Contents/SharedSupport/bin/subl" ]]; then
		alias subl="/Applications/Sublime\ Text\ 2.app/Contents/SharedSupport/bin/subl"
	elif [[ -f "/Applications/Sublime Text 3.app/Contents/SharedSupport/bin/subl" ]]; then
		alias subl="/Applications/Sublime\ Text\ 3.app/Contents/SharedSupport/bin/subl"
	elif [[ -f "/Applications/Sublime Text.app/Contents/SharedSupport/bin/subl" ]]; then
		alias subl="/Applications/Sublime\ Text.app/Contents/SharedSupport/bin/subl"
	fi
fi

alias s="subl -as"
