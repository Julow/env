#!/usr/bin/env bash

set -e
BC="_build/default/$1"
shift
dune build "$BC"
dune exec -- rlwrap ocamldebug "$BC" "$@"
