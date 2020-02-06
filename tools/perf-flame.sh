#!/usr/bin/env bash

ensure_available ()
{
  if [[ -e "$1" ]]; then
    local d="$1.old"
    ensure_available "$d"
    echo "$1 already exists, renaming it to $d"
    mv "$1" "$d"
  fi
}

d=perf.svg
ensure_available "$d"

perf script | stackcollapse-perf.pl | flamegraph.pl > "$d"

echo "Generated $d"
