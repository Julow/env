#!/usr/bin/env bash

set -e

case $HOSTNAME in
  "jules-work") config=./configuration-work.nix ;;
  "jules-pc") config=./configuration-home.nix ;;

  *)
    echo "Unknown hostname $HOSTNAME" >&2
    exit 1
esac

build ()
{
  local target="$1"
  shift
  echo "Building $config" >&2
  nix-build --no-out-link "$@" -A "$target" \
    -E "import nixpkgs/nixos { configuration = import $config; }"
}

case "$1" in
  vm)
    result=$(build vm)
    "$result"/bin/run-*-vm
    ;;

  trace)
    result=$(build system --show-trace)
    echo "Built $result"
    ;;

  "")
    result=$(build system)
    echo "Built $result"
    echo "Switching..."
    su -c \
"nix-env --profile /nix/var/nix/profiles/system --set '$result' && \
/nix/var/nix/profiles/system/bin/switch-to-configuration switch"
    ;;

  *) echo "Usage: $0 [vm|trace|]" >&2 ;;
esac
