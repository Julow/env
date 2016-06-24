#!/bin/sh

# Show weather from wttr.in

curl -s "wttr.in/$1" | less -RS
