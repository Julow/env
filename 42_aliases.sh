#
# 42 Aliases
#

#
# Norminette
#
# Run the norminette and colorize the output
#
# n [files...]
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
# Write/Replace the 42 header in a file
#
# h <file> [username]
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
# OK
#
# Check auteur/Makefile/norme
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
