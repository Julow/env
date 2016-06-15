#!/bin/sh
# **************************************************************************** #
#                                                                              #
#                                                         :::      ::::::::    #
#    prof.sh                                            :+:      :+:    :+:    #
#                                                     +:+ +:+         +:+      #
#    By: jaguillo <jaguillo@student.42.fr>          +#+  +:+       +#+         #
#                                                 +#+#+#+#+#+   +#+            #
#    Created: 2016/06/15 16:21:49 by jaguillo          #+#    #+#              #
#    Updated: 2016/06/15 16:28:54 by jaguillo         ###   ########.fr        #
#                                                                              #
# **************************************************************************** #

# Run valgrind (+callgrind) on a program + Output an image file with dot (+gprof2dot)
# Require valgrind, callgrind, graphviz, gprof2dot

set -e

PROF_DUMP_FILE=`mktemp`
PROF_IMG_FILE=`mktemp`.png

valgrind --tool=callgrind --callgrind-out-file="$PROF_DUMP_FILE" -q -- "$@"
gprof2dot -z main -n 0.01 -e 0.01 -f callgrind "$PROF_DUMP_FILE" | dot -Tpng -o "$PROF_IMG_FILE"

echo "## Output file: $PROF_IMG_FILE"
