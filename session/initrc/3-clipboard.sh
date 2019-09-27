# Clipboard utils
#
# Usage:
#  copy     Copy from stdin into the clipboard
#  paste    Output clipboard to stdout

copy ()
{
  xclip -in -selection clipboard
}

paste ()
{
  xclip -out -selection clipboard
}
