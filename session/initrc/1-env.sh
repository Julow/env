export PS1="$SHLVL> "

export PATH="$PATH:/usr/local/bin"

function l()
{
	ls -lAbFhgo "$@" | sed -E "s/([^ ]+)( +)([^ ]+)( +)([^ ]+)( +[^ ]+ +[^ ]+ +[^ ]+) (.+)/[\1] `printf "\033[1;30m"`\6  `printf "\033[0;36m"`(\5 +\3)`printf "\033[0m"` \4\2\7/" | sed "s/ +1)/)   /"
};

alias ll="l"

export LESS="-RiK -x 4"

shopt -s globstar

export EDITOR=vim
