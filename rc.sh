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
	if [ ! -d ~/save_path ]; then
		mkdir ~/save_path
	fi
	pwd > ~/save_path/$1.save_path;
};

function go()
{
	if [ -f ~/save_path/$1.save_path ]; then
		cd "`cat ~/save_path/$1.save_path`";
	else
		echo "The save $1 does not exists.";
	fi
};

function saved()
{
	if [ -d ~/save_path ]; then
		ls -1 ~/save_path | grep ".save_path" | sed -E "s/.save_path$//"
	fi
};
