#!/usr/bin/env bash

set -e

config=$(realpath "$(dirname "$0")/configuration.nix")

[[ -e $config ]]

cd /tmp

case "$1" in
  vm)
    NIXOS_CONFIG=$config nixos-rebuild build-vm
    result/bin/run-nixos-vm
    ;;

  "")
    su -c "NIXOS_CONFIG='$config' nixos-rebuild switch"
    ;;

  *) echo "Usage: $0 [vm]" >&2 ;;
esac
