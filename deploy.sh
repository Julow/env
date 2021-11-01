#!/usr/bin/env bash

set -e

# Useful default arguments
if [[ $# -eq 0 ]]; then
  case $HOSTNAME in
    "jules-work") config=host/work ;;
    "jules-pc") config=host/home ;;

    *)
      echo "Unknown hostname $HOSTNAME" >&2
      exit 1
  esac

  set deploy local "$config"
fi

nixos-deploy "$@"
