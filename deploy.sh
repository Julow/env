#!/usr/bin/env bash

set -e

case $HOSTNAME in
  "jules-work") config=configuration-work.nix ;;
  "jules-pc") config=configuration-home.nix ;;

  *)
    echo "Unknown hostname $HOSTNAME" >&2
    exit 1
esac

config=$(realpath "$(dirname "$0")/$config")

[[ -e $config ]]

cd /tmp

case "$1" in
  vm)
    NIXOS_CONFIG=$config nixos-rebuild build-vm
    result/bin/run-nixos-vm
    ;;

  trace)
    NIXOS_CONFIG=$config nixos-rebuild dry-build --show-trace 2>&1 | less
    ;;

  "")
    su -c "NIXOS_CONFIG='$config' nixos-rebuild switch"
    ;;

  *) echo "Usage: $0 [vm|trace]" >&2 ;;
esac
