#!/usr/bin/env bash

VALUE="$1"

DIV=$((VALUE/10))
REM=$((10-DIV))

repeat() { printf "%$1s" "" | tr " " "$2"; }

echo "<span color=\"cyan\">$(repeat $DIV -)</span>$(repeat $REM -)"
