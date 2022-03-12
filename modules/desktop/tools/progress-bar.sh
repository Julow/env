#!/usr/bin/env bash

COLOR=${BAR_COLOR:-cyan}

VALUE="$1"
PREC=20

DIV=$((VALUE * PREC / 100))
if [[ $DIV -le $PREC ]]; then
  REM=$((PREC - DIV))
  OVER=0
else
  REM=0
  OVER=$((DIV - PREC))
  DIV=$PREC
fi

repeat() {
  x=`printf "%*s" "$1" ""`
  echo "${x// /$2}"
}

bar() { repeat "$1" "━"; }

echo "<span color=\"$COLOR\">`bar "$DIV"`</span>`bar "$REM"`<span color=\"red\">`bar "$OVER"`</span>"
