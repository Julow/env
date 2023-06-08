#!/usr/bin/env bash

# This script finds the list of profiles under '/nix/var/nix/profiles' and runs
# 'nix-env --delete-generations $arg' on each of them.
#
# The argument to '--delete-generations' is taken as first argument to this
# script. The default is '+5', meaning keep only the 5 latest generations.
#
# It is intended to be run as root.

set -e

DELETE_GENERATIONS_ARG=${1:-+5}

profiles=()

find_profiles ()
{
  echo "Looking for profiles in $1" >&2
  for p in "$1"/*; do
    # This filters out generations of each profiles but also the 'per-user'
    # directory.
    if [[ -L $p ]] && ! [[ $p = *-link ]]; then profiles+=("$p"); fi
  done
}

find_profiles "/nix/var/nix/profiles"
for pu in /nix/var/nix/profiles/per-user/*; do
  find_profiles "$pu"
done

nix_env_print ()
{
  echo nix-env "$@"
  nix-env "$@"
}

for p in "${profiles[@]}"; do
  nix_env_print --profile "$p" --delete-generations "$DELETE_GENERATIONS_ARG"
done
