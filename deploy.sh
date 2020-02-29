#!/usr/bin/env bash

cd "`dirname "$0"`"

su -c '
cp configuration.nix /etc/nixos/configuration.nix
nixos-rebuild switch
'
