#!/usr/bin/env bash

# EncFS + gpg

set -e

encfs --extpass="encfs-gpg-extpass.sh" "$@"
