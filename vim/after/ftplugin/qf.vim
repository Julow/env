" Settings for the quickfix window

" Improve status line
setlocal noruler
setlocal statusline=%{exists('w:quickfix_title')?w:quickfix_title:''}%= " Set by some commands
setlocal statusline+=%=%t " Right part
setlocal statusline+=\ %l/%L%(\ (%{S_selection()})%) " Cursor position and selection

setlocal number
