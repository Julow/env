#!/bin/bash
# **************************************************************************** #
#                                                                              #
#                                                         :::      ::::::::    #
#    redshift_util.sh                                   :+:      :+:    :+:    #
#                                                     +:+ +:+         +:+      #
#    By: juloo <juloo@student.42.fr>                +#+  +:+       +#+         #
#                                                 +#+#+#+#+#+   +#+            #
#    Created: 2016/11/13 00:02:57 by juloo             #+#    #+#              #
#    Updated: 2016/11/13 00:52:34 by juloo            ###   ########.fr        #
#                                                                              #
# **************************************************************************** #

PIDS=`pgrep -x redshift`

function run_redshift
{
	redshift -r &
}

function kill_redshift
{
	kill $PIDS >/dev/null
}

case "$1" in

	toggle)
		[[ -z "$PIDS" ]] && run_redshift || kill_redshift
	;;

	kill)
		[[ -n "$PIDS" ]] && kill_redshift
	;;

	"")
		[[ -z "$PIDS" ]] && run_redshift
	;;

esac
