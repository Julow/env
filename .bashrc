
#
# PS1
#
function _ps1_status()
{
	if [ $? -eq 0 ]; then
		printf "\033[32m$?"
	else
		printf "\033[31m$?"
	fi
};

function _ps1_git_rev()
{
	if [ "$1" -gt 0 ]; then
		printf "\033[32m-$1\033[0m "
	fi
	if [ "$2" -gt 0 ]; then
		printf "\033[32m+$2\033[0m "
	fi
};

function _ps1_git()
{
	BRANCH=`git rev-parse --abbrev-ref HEAD 2> /dev/null` > /dev/null
	if [ $? -eq 0 ]; then
		printf $BRANCH
		STATUS=$(git status --porcelain)
		COLUM1=`echo "$STATUS" | cut -c 1-1`
		COLUM2=`echo "$STATUS" | cut -c 2-2`
		if [[ "$COLUM1" == *"A"* ]]; then
			printf "\033[32mA"
		fi
		if [[ "$COLUM1" == *"D"* ]]; then
			printf "\033[32mD"
		fi
		if [[ "$COLUM1" == *"M"* ]]; then
			printf "\033[32mM"
		fi
		if [[ "$COLUM1" == *"R"* ]]; then
			printf "\033[32mR"
		fi
		if [[ "$COLUM2" == *"D"* ]]; then
			printf "\033[31mD"
		fi
		if [[ "$COLUM2" == *"M"* ]]; then
			printf "\033[31mM"
		fi
		if [[ "$COLUM2" == *"?"* ]]; then
			printf "\033[31m?"
		fi
		printf "\033[0m "
		_ps1_git_rev `git rev-list --left-right --count origin...HEAD 2> /dev/null || echo "0 0"`
	fi
};

export PS1="\$(_ps1_status) \033[36m\h \033[32m\w\033[0m \$(_ps1_git)"

#
# F
#
alias f="grep -r --color=always -C 3"

#
# Timeout
#
function timeout()
{
	$@ &
	PID=$!
	(sleep 10 ; kill $PID) &
	PID_SLEEP=$!
	wait $PID > /dev/null
	STATUS=$?
	if [ $STATUS -gt 128 ]; then
		kill $PID > /dev/null
	else
		kill $PID_SLEEP > /dev/null
	fi
};

#
# Norminette
#
function n()
{
	if [ $# -eq 0 ]; then
		ARGS="."
	else
		ARGS=$@
	fi
	norminette ${ARGS} | sed -E "s/((Error[^:]*:)|(Warning:?))(.+)$|(Norme:.+)/`echo "\033[0;31m"`\2`echo "\033[0;33m"`\3`echo "\033[0;0m"`\4`echo "\033[0;32m"`\5/"
};

#
# Header 42
#
function h()
{
    START_LINES="/* ************************************************************************** */
/*                                                                            */
/*                                                        :::      ::::::::   */"

    if [ -f "$1" ]; then
        HEAD="`head -3 "$1"`"
        if [ "$HEAD"="$START_LINES" ]; then
            CONTENT=`tail -n +13 "$1"`
        else
            CONTENT=`cat "$1"`
        fi
    else
        CONTENT=
    fi

    NOW=`date +"%Y/%m/%d %H:%M:%S"`

    if [ -n "$2" ]; then
        WHO="$2"
    else
        WHO=`whoami`
    fi

    printf "${START_LINES}\n" > $1
    printf "/*   %-50s :+:      :+:    :+:   */\n" "$1" >> $1
    printf "/*                                                    +:+ +:+         +:+     */\n" >> $1
    printf "/*   By: %-42s +#+  +:+       +#+        */\n" "${WHO} <${WHO}@student.42.fr>" >> $1
    printf "/*                                                +#+#+#+#+#+   +#+           */\n" >> $1
    printf "/*   Created: %s by %-17s #+#    #+#             */\n" "${NOW}" "${WHO}" >> $1
    printf "/*   Updated: %s by %-16s ###   ########.fr       */\n" "${NOW}" "${WHO}" >> $1
    printf "/*                                                                            */\n" >> $1
    printf "/* ************************************************************************** */\n\n" >> $1
    printf "${CONTENT}\n" >> $1

    cat $1
};

#
# Ls
#
function l()
{
	ls -lAbFhgo $@ | sed -E "s/([^ ]+)( +)([^ ]+)( +)([^ ]+)( +[^ ]+ +[^ ]+ +[^ ]+) (.+)/[\1] `printf "\033[1;30m"`\6  `printf "\033[0;36m"`(\5 +\3)`printf "\033[0m"` \4\2\7/" | sed "s/ +1)/)   /"
};

#
# Sublime Text
#
if [ "`uname`" = "darwin" ]; then
	alias s="/Applications/Sublime\ Text.app/Contents/SharedSupport/bin/subl"
else
	alias s="subl"
fi

#
# Gcc
#
alias g="gcc -Wall -Werror -Wextra"

#
# Git
#
alias t="git status -sb"
alias c="git commit -m"
alias p="git push origin master --tags"
alias amend="git commit --amend --no-edit"

function a()
{
	git add --all $@ && git status -bs
};

function r()
{
	git reset -- HEAD -q $@ && git status -bs
};

#
# Save Go
#
function save()
{
	if [ "$1" == "-g" ]; then
		cd "`save -i $2`"
	elif [ "$1" == "-i" ]; then
		if [ -f ~/.save_go ]; then
			cat ~/.save_go | grep -m 1 -i ^"$2"= | cut -d '=' -f 2
		fi
	elif [ "$1" == "-l" ]; then
		if [ "$2" == "" ]; then
			if [ -f ~/.save_go ]; then
				grep -v "^$" ~/.save_go
			fi
		else
			save -i $2
		fi
	elif [ "$1" == "-r" ]; then
		if [ -f ~/.save_go ]; then
			grep -iv "^$2=" ~/.save_go > ~/.save_go.tmp
			mv ~/.save_go.tmp ~/.save_go 2> /dev/null
		fi
	elif [ "$1" == "--help" ]; then
		save -h
	elif [ "$1" == "-h" ]; then
		echo "Save/Go"
		echo "    save -g <save>            Go to <save>"
		echo "    save -i <save>            Print the path of <save>"
		echo "    save -l                   Print the list of saves"
		echo "    save -l <save>            Alias for 'save -i'"
		echo "    save -r <save>            Remove <save>"
		echo "    save -h"
		echo "    save --help               Print this message"
		echo
		echo "    save <save>               Create <save> with the current dir"
		echo "    go <save>                 Alias for 'save -g'"
		echo "    saved                     Alias for 'save -l'"
		echo
		echo "A save can have any name"
		echo "If <save> is blank, it refer to a save with no name."
		echo "All the saves are stored in '~/.save_go'"
	else
		save -r $1
		echo >> ~/.save_go
		echo -n $1"=" >> ~/.save_go
		pwd >> ~/.save_go
		grep -v "^$" ~/.save_go > ~/.save_go.tmp
		mv ~/.save_go.tmp ~/.save_go 2> /dev/null
	fi
};

alias go="save -g"
alias saved="save -l"
