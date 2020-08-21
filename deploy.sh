#!/usr/bin/env bash

set -e

# Useful default arguments
if [[ $# -eq 0 ]]; then
  case $HOSTNAME in
    "jules-work") config=./configuration-work.nix ;;
    "jules-pc") config=./configuration-home.nix ;;

    *)
      echo "Unknown hostname $HOSTNAME" >&2
      exit 1
  esac

  set deploy local "$config"
fi

nixos-deploy "$@"
