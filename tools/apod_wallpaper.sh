#!/usr/bin/env bash

set -e

set_wallpaper () { feh --bg-fill "$1"; }

IMG_DST=/tmp/apod.jpg

# Temporarily use previous image
if [[ -e $IMG_DST ]]; then
  set_wallpaper "$IMG_DST" || true
fi

# Fetch lastest image
BASE_URL="https://apod.nasa.gov/apod"
PAGE_DST=/tmp/apod.html

curl -o "$PAGE_DST" "$BASE_URL/astropix.html"

IMG_PATH=`sed --quiet -E '/<IMG SRC="(.+)"/{s//\1/;p}' </tmp/apod.html`

curl -o "$IMG_DST" "$BASE_URL/$IMG_PATH"

set_wallpaper "$IMG_DST"

#
rm "$PAGE_DST"
