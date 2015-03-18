export PATH=$HOME/.brew/bin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/opt/X11/bin:/usr/texbin
export USER=`/usr/bin/whoami`
export GROUP=`/usr/bin/id -gn $user`
export MAIL="$USER@student.42.fr"

#
# PS1
#
function _ps1_status()
{
	STATUS="$?"
	if [[ "$SHLVL" -gt "1" ]]; then
		printf "$SHLVL> "
	fi
	if [[ "$STATUS" -eq "0" ]]; then
		printf "\033[32m$STATUS"
	else
		printf "\033[31m$STATUS"
	fi
};

function _ps1_git_rev()
{
	if [[ "$4" -gt "0" ]]; then
		printf "%s " "$2-$4$3"
	fi
	if [[ "$5" -gt "0" ]]; then
		printf "%s " "$1+$5$3"
	fi
};

function _ps1_git()
{
	BRANCH=`git rev-parse --abbrev-ref HEAD 2> /dev/null` > /dev/null
	if [[ $? -eq 0 ]]; then
		if [[ ! "$BRANCH" == "master" ]]; then
			PRINT="[$BRANCH] "
		else
			PRINT=""
		fi
		STATUS=$(git status --porcelain)
		COLUM1=`echo "$STATUS" | cut -c 1-1`
		COLUM2=`echo "$STATUS" | cut -c 2-2`
		if [[ "$COLUM1" == *"A"* ]]; then
			PRINT=$PRINT"$1A"
		fi
		if [[ "$COLUM1" == *"D"* ]]; then
			PRINT=$PRINT"$1D"
		fi
		if [[ "$COLUM1" == *"M"* ]]; then
			PRINT=$PRINT"$1M"
		fi
		if [[ "$COLUM1" == *"R"* ]]; then
			PRINT=$PRINT"$1R"
		fi
		if [[ "$COLUM2" == *"D"* ]]; then
			PRINT=$PRINT"$2D"
		fi
		if [[ "$COLUM2" == *"M"* ]]; then
			PRINT=$PRINT"$2M"
		fi
		if [[ "$COLUM2" == *"?"* ]]; then
			PRINT=$PRINT"$2?"
		fi
		if [[ "${#PRINT}" -gt "0" ]]; then
			printf "%s " "$PRINT$3"
		fi
		_ps1_git_rev "$1" "$2" "$3" `git rev-list --left-right --count origin...HEAD 2> /dev/null || echo "0 0"`
	fi
};

export PS1="\$(_ps1_status) \033[36m\h \033[32m\w\033[0m \$(_ps1_git '\033[32m' '\033[31m' '\033[0m')"

#
# PS1 - zsh
#
export PROMPT="%(?.%F{green}%?%f.%F{red}%?%f) %F{cyan}%m %F{green}%~%f $(_ps1_git '%F{green}' '%F{red}' '%f')"

if [[ "$SHLVL" -gt "1" ]]; then
	PROMPT="$SHLVL> "$PROMPT
fi

#
# Rc
#
alias rc="source ~/.zshrc"

#
# F
#
function f()
{
	if [[ "$2" == "" ]]; then
		F_DIRS="."
	else
		F_DIRS="$2"
	fi
	if [[ -t 1 ]]; then
		F_COLOR=always
	else
		F_COLOR=never
	fi
	grep -r -E --exclude-dir=".?*" --color="$F_COLOR" -C 3 "$1" "$F_DIRS"
};

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
	if [[ $STATUS -gt 128 ]]; then
		kill $PID > /dev/null
	else
		kill $PID_SLEEP > /dev/null
	fi
};

#
# Diff
#
function d()
{
	if [[ "$#" -lt 2 ]]; then
		echo "Error: d need 2 arguments"
	else
		printf "\033[0;32m$2\033[0;0m - \033[0;31m$1\033[0;0m\n"
		diff -- "$1" "$2" | sed -E "s/> (.*)|< (.*)/`printf "\033[0;32m"`\1`printf "\033[0;31m"`\2`printf "\033[0;0m"`/"
	fi
}

#
# Norminette
#
function n()
{
	if [[ $# -eq 0 ]]; then
		ARGS=**/*.[ch]
	else
		ARGS=$@
	fi
	norminette ${ARGS} 2> /dev/null | sed -E "s/((Error[^:]*:)|(Warning:?))(.+)$|(Norme:.+)/`printf "\033[0;31m"`\2`printf "\033[0;33m"`\3`printf "\033[0;0m"`\4`printf "\033[0;32m"`\5`printf "\033[0;0m"`/"
};

#
# Header 42
#
function h()
{
    START_LINES="/* ************************************************************************** */
/*                                                                            */
/*                                                        :::      ::::::::   */"

    if [[ -f "$1" ]]; then
        HEAD="`head -3 "$1"`"
        if [[ "$HEAD" == "$START_LINES" ]]; then
            CONTENT=`tail -n +13 "$1"`
        else
            CONTENT=`cat "$1"`
        fi
    else
        CONTENT=
    fi

    NOW=`date +"%Y/%m/%d %H:%M:%S"`

    if [[ -n "$2" ]]; then
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

alias ll="l"

#
# PS
#
alias ps="ps -e -o 'pid %cpu %mem etime tty command' | grep -E ' ttys[0-9]+ | +COMMAND$| pts/[0-9]'"

#
# Sublime Text
#
if [[ "`uname`" == "Darwin" ]]; then
	if [[ -f "/Applications/Sublime Text 3.app/Contents/SharedSupport/bin/subl" ]]; then
		alias subl="/Applications/Sublime\ Text\ 3.app/Contents/SharedSupport/bin/subl"
	elif [[ -f "/Applications/Sublime Text.app/Contents/SharedSupport/bin/subl" ]]; then
		alias subl="/Applications/Sublime\ Text.app/Contents/SharedSupport/bin/subl"
	fi
fi

alias s="subl ; subl"

#
# Gcc
#
alias g="gcc -Wall -Werror -Wextra"
alias gg="gcc -g -Wall -Wextra"

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
	if [[ "$1" == "-g" ]]; then
		cd "`save -i $2`"
	elif [[ "$1" == "-i" ]]; then
		if [[ -f ~/.save_go ]]; then
			cat ~/.save_go | grep -m 1 -i ^"$2"= | cut -d '=' -f 2
		fi
	elif [[ "$1" == "-s" ]]; then
		if [[ -f ~/.save_go ]]; then
			if [[ "$2" == "" ]]; then
				_SAVE="`pwd`"
			else
				_SAVE="$2"
			fi
			cat ~/.save_go | grep -m 1 -i ="$_SAVE"\$ | cut -d '=' -f 1
		fi
	elif [[ "$1" == "-l" ]]; then
		if [[ "$2" == "" ]]; then
			if [[ -f ~/.save_go ]]; then
				grep -v "^$" ~/.save_go
			fi
		else
			save -i $2
		fi
	elif [[ "$1" == "-r" ]]; then
		if [[ -f ~/.save_go ]]; then
			if [[ "$2" == "" ]]; then
				_SAVE="`save -s`"
			else
				_SAVE="$2"
			fi
			grep -iv ^"$_SAVE"= ~/.save_go > ~/.save_go.tmp
			mv ~/.save_go.tmp ~/.save_go 2> /dev/null
		fi
	elif [[ "$1" == "--help" ]]; then
		save -h
	elif [[ "$1" == "-h" ]]; then
		echo "Save/Go"
		echo "    save -g <save>            Go to <save>"
		echo "    save -i <save>            Print the path of <save>"
		echo "    save -s                   Search the save with the current dir"
		echo "    save -s <dir>             Search the save with <dir>"
		echo "    save -l                   Print the list of saves"
		echo "    save -l <save>            Alias for 'save -i'"
		echo "    save -r                   Search and remove the save with the current dir"
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

#
# Clean
#
function _cleandir()
{
	if [[ "$1" != "" ]]; then
		if [[ "`ls -1 "$1" 2> /dev/null | wc -l`" -gt "0" ]]; then
			rm -rf "$1"/*
			rm -rf "$1"/.*
		fi
	fi
};

function clean()
{
	printf "\033[0;32mCrash reports\033[0;0m\n"
	_cleandir ~/Library/Logs/DiagnosticReports/
	printf "\033[0;32mApplications caches\033[0;0m\n"
	_cleandir ~/Library/Caches/
	_cleandir ~/.cache/
	printf "\033[0;32mTrash\033[0;0m\n"
	_cleandir ~/.Trash/
	printf "\033[0;32mRuby cache\033[0;0m\n"
	_cleandir ~/.rbx/
};

#
# OK
#
function ok()
{
	_WHOAMI="`whoami`"
	_OK=1
	printf "auteur: "
	if [[ ! -f "auteur" ]]; then
		printf "\033[0;31mDon't exists\033[0;0m\n"
		_OK=0
	else
		if [[ "`cat -e auteur`\$" == "$_WHOAMI\$\$" ]]; then
			printf "\033[0;32mOK\033[0;0m\n"
		else
			printf "\033[0;31mNot well formated\033[0;0m\n"
			_OK=0
		fi
	fi
	printf "norme: "
	if [[ "`type norminette 2>&1 | grep "not found"`" == "" ]]; then
		_NORME="`norminette **/*.[ch] 2>/dev/null | grep -E -B 1 "Error|Warning"`"
		if [[ "$_NORME" == "" ]]; then
			printf "\033[0;32mOK\033[0;0m\n"
		else
			printf "\033[0;31mError:\033[0;0m\n"
			echo "$_NORME"
			_OK=0
		fi
	else
		printf "\033[0;31mCan't check\033[0;0m\n"
	fi
	printf "makefile: "
	if [[ -f "Makefile" ]]; then
		_MAKEOK=1
		if [[ "`grep -c -E "^all:" Makefile`" -eq 0 ]]; then
			if [[ "$_MAKEOK" -eq 1 ]]; then
				printf "\033[0;31mMissing rule:\033[0;0m all"
				_MAKEOK=0
			else
				printf ", all"
			fi
		fi
		if [[ "`grep -c -E "^clean:" Makefile`" -eq 0 ]]; then
			if [[ "$_MAKEOK" -eq 1 ]]; then
				printf "\033[0;31mMissing rule:\033[0;0m clean"
				_MAKEOK=0
			else
				printf ", clean"
			fi
		fi
		if [[ "`grep -c -E "^fclean:" Makefile`" -eq 0 ]]; then
			if [[ "$_MAKEOK" -eq 1 ]]; then
				printf "\033[0;31mMissing rule:\033[0;0m fclean"
				_MAKEOK=0
			else
				printf ", fclean"
			fi
		fi
		if [[ "`grep -c -E "^re:" Makefile`" -eq 0 ]]; then
			if [[ "$_MAKEOK" -eq 1 ]]; then
				printf "\033[0;31mMissing rule:\033[0;0m re"
				_MAKEOK=0
			else
				printf ", re"
			fi
		fi
		if [[ "`grep -c -E "\\( *whildcard|\\*[/\\.]|[/\\.]\\*" Makefile`" -gt 0 ]]; then
			if [[ "$_MAKEOK" -eq 0 ]]; then
				printf " ; \033[0;31mContains whildcard\033[0;0m"
			else
				printf "\033[0;31mContains whildcard\033[0;0m"
				_MAKEOK=0
			fi
		fi
		if [[ "$_MAKEOK" -eq 1 ]]; then
			printf "\033[0;32mOK\033[0;0m\n"
		else
			printf "\n"
			_OK=0
		fi
	else
		printf "\033[0;31mDon't exists\033[0;0m\n"
		_OK=0
	fi
	if [[ "$_OK" -eq 1 ]]; then
		echo "OK ! You can push"
	else
		echo "Noob there is errors"
	fi
}

return
