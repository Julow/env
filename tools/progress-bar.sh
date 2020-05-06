#!/usr/bin/env bash

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

printf "<span#color=\"cyan\">%*s</span>%*s<span#color=\"red\">%*s</span>" \
  "$DIV" "" "$REM" "" "$OVER" "" | tr " #" "- "
