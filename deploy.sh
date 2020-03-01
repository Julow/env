#!/usr/bin/env bash

cd "`dirname "$0"`"

su -c '
cp -r . /etc/nixos/
nixos-rebuild switch
'
