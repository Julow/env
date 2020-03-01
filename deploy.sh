#!/usr/bin/env bash

set -e

config=$(realpath "$(dirname "$0")/configuration.nix")

[[ -e $config ]]

su -c "NIXOS_CONFIG='$config' nixos-rebuild switch"
