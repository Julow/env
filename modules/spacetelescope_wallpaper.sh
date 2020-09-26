#!/usr/bin/env bash

set -e

set_wallpaper () { feh --bg-fill "$1"; }

# Fetch with retry
fetch () { curl --silent -L --retry 10 -o "$1" "$2"; }

IMG_CACHE="$HOME/.cache/spacetelescope_wallpaper.jpg"
ID_CACHE="$HOME/.cache/spacetelescope_wallpaper.id"

# Temporarily use previous image
if [[ -e $IMG_CACHE ]]; then
  set_wallpaper "$IMG_CACHE" || true
fi

DST=/tmp/spacetelescope.org.html

fetch "$DST" "https://www.spacetelescope.org/images/archive/search/?wallpapers=on&type=Observation&sort=-release_date"

# Get the first 'id' field in:
#   <script>
#     var images = [
#
#       {
#         id: '...',
#         ...
#       },
ID=`sed -nE '0,/^\s*var images = \[$/ d; 0,/^\s*id: '\''(.+)'\'',$/ s//\1/ p' "$DST"`

CACHED_ID=`cat "$ID_CACHE" 2>/dev/null || true`
if [[ $ID = $CACHED_ID ]]; then
  echo "No new image"
  exit 0
fi

fetch "$IMG_CACHE" "https://cdn.spacetelescope.org/archives/images/wallpaper5/$ID.jpg"
echo "$ID" > "$ID_CACHE"

set_wallpaper "$IMG_CACHE"
