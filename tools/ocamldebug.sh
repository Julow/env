#!/usr/bin/env bash

# How to use:
#  - Write prelude commands in a file
#    (set arguments, some breakpoints)
#  - :!ocamldebug.sh src/bin/main.bc %

set -e

BC="_build/default/$1"
shift

PRE_INPUT=()
if [[ $# -ge 1 ]]; then
  PRE_INPUT=(-P "`cat "$1"`")
fi
shift

dune build "$BC"

rlwrap "${PRE_INPUT[@]}" ocamldebug "$BC"
