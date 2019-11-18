#!/usr/bin/env bash

set -e

set_wallpaper () { feh --bg-fill "$1"; }

IMG_DST="$HOME/.cache/apod.jpg"
DETAILS_DST="$HOME/.cache/apod.md"

# Temporarily use previous image
if [[ -e $IMG_DST ]]; then
  set_wallpaper "$IMG_DST" || true
fi

# Log details and evetually errors
exec &>"$DETAILS_DST"

# Fetch lastest image
API_URL="https://api.nasa.gov/planetary/apod?api_key=DEMO_KEY&date=2019-11-17"
JSON_DST="/tmp/apod.json"

curl --silent -L --retry 10 -o "$JSON_DST" "$API_URL"

eval `jq -r '@sh "
URL=\(.url)
HDURL=\(.hdurl)
MEDIA_TYPE=\(.media_type)
TITLE=\(.title)
EXPLANATION=\(.explanation)
DATE=\(.date)
"' "$JSON_DST"`

URL=${HDURL:=$URL}

echo "Astronomy Picture of the Day ($DATE)"
echo
if [[ $MEDIA_TYPE = image ]]; then
  echo "# $TITLE"
else
  echo "# $TITLE ($MEDIA_TYPE)"
fi
echo
echo "$URL"
echo
echo "$EXPLANATION"

case "$MEDIA_TYPE" in
  image)
    curl --silent -o "$IMG_DST" "$URL"
    set_wallpaper "$IMG_DST"
    ;;

  *)
    echo; echo "Cannot handle media type '$MEDIA_TYPE'" ;;
esac
